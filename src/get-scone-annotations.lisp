;;; -*- Mode:Lisp -*-
;;; ***************************************************************************
;;; get-scone-annotations.lisp
;;;
;;; Functions that read a piece of English text and return Scone elements found in the text.
;;; Includes Dynamic-programming and linear approaches to aligning words to Scone concepts.
;;;  
;;;
;;; AUTHOR: Alicia Tribble <atribble@cs.cmu.edu>
;;; ***************************************************************************

;;; Copyright (C) 2005, Carnegie Mellon University.

;;; The Scone software is made available to the public under the CPL 1.0
;;; Open Source license.  A copy of this license is distributed with the
;;; software.  The license can also be found at URL
;;; <http://www.opensource.org/licenses/cpl.php>.

;;; By using, modifying, reproducing, or distibuting the Scone
;;; software, you agree to be bound by the terms and conditions set
;;; forth in the CPL 1.0 Open Source License.  If you do not agree to
;;; these terms and conditions, or if they are not legally applicable
;;; in the jurisdiction where such use takes place, then you may not
;;; use the Scone software.

;;; Scone incoporates some parts of the NETL2 system, developed by
;;; Scott E. Fahlman for IBM Coporation between June 2001 and May
;;; 2003.  IBM holds the copyright on NETL2 and has made that software
;;; available to the author under the CPL 1.0 Open Source license.

;;; Development of Scone has been supported in part by the Defense Advanced
;;; Research Projects Agency (DARPA) under contract numbers NBCHC030029 and
;;; NBCHC030030.  Any opinions, findings and conclusions or recommendations
;;; expressed in this material are those of the author(s) and do not
;;; necessarily reflect the views of DARPA or the Department of
;;; Interior-National Business Center (DOI-NBC).

;;; ***************************************************************************
;;; GENERAL STYLE NOTE (from sef@cs): We want the runtime marker-scanning operations
;;; to be as fast as possible and to be non-consing.  So that code is
;;; optimized for performance first and for clarity second.  Code that
;;; builds or modifies the KB, or that does higher-level setup for marker
;;; ops, can be slower, so I have tried to optimize that for clarity and
;;; ease of maintenance..

;;; ***************************************************************************
;;; HOW TO USE THIS CODE
;;; 
;;; There are two exported functions:
;;;
;;;  (get-scone-annotations "string")   ; Returns a list of Scone elements that appear in 
;;;                                     ; STRING, formatted in XML
;;;
;;;  (set-use-null-annotations boolean) ; Sets the dp tokenizer to return all tokens, 
;;;                                     ; including non-element tokens (null tokens)
;;;  (set-scone-annotator-algorithm 
;;;          :viterbi nil 
;;;          :recurse t)               ; Sets the algorithm used to create scone tokens.
;;;                                    ; If VITERBI is set to t, uses dynamic-programming.
;;;                                    ; This is slower but gathers multi-word scone tokens.
;;;                                    ; Otherwise, uses recursion, which is faster but only
;;;                                    ; gives single-word scone tokens.
;;;
;;; ***************************************************************************

;; Uses split-sequence code
;(load "home:lib/split-sequence.lisp")

;; Global variable that determines whether we're 
;; parsing into Scone elements or WordNet concepts  -- this feature disabled for now
(defvar *make-token*)
(setf *make-token* 'make-scone-token)
;;(setf *make-token* 'make-wn-token)

;; Global variable that determines DP vs. Recursive tokenization
(defvar *use-dp-tokenizer*)
(setf *use-dp-tokenizer* t)
(defun set-scone-annotator-algorithm (&key (viterbi nil) (recurse t))
  "Set the algorithm used by the Scone annotator.  
   If :VITERBI is T, use Dynamic-programming approach.  
   If :RECURSE is T, use recursive approach.  
   Note that recurse is the default and in case of error 
   (i.e. both set to T or neither set to T), recurse will be used."
  (declare (ignore recurse))
  (if viterbi
      (setf *use-dp-tokenizer* t)
    (setf *use-dp-tokenizer* nil)))

;; Define a few of token types
(defvar *string-token*  0)  
(defvar *list-sep*      0)  
(defvar *iname-token*   1)  
(defvar *iname-token-end*  2)
(defvar *whspace-token* 3)   
(defvar *english-name*  4)   
(defvar *english-name-end* 5)
(defvar *pos-tag*       6)   
(defvar *pos-tag-end*   7)   
(defvar *integer-token* 8)   
(defvar *var-token*     9)

;; Punctuation 
(defvar *punctuation* (make-hash-table :test #'equal))
;(setf (gethash "'" *punctuation*) "*apostrophe*")
;(setf (gethash #\' *punctuation*) "*apostrophe*")
(setf (gethash "." *punctuation*) "*period*")
(setf (gethash #\. *punctuation*) "*period*")
(setf (gethash "?" *punctuation*) "*quest*")
(setf (gethash #\? *punctuation*) "*quest*")
(setf (gethash "," *punctuation*) "*comma*")
(setf (gethash #\, *punctuation*) "*comma*")
(setf (gethash "(" *punctuation*) "*oparen*")
(setf (gethash #\( *punctuation*) "*oparen*")
(setf (gethash ")" *punctuation*) "*cparen*")
(setf (gethash #\) *punctuation*) "*cparen*")
(defun punctuation? (w)
  (if (gethash w *punctuation*)
      t
    nil))
(defvar *char-punctuation*)
(setf *char-punctuation* nil)
(push "'" *char-punctuation*)
(push "." *char-punctuation*)
(push "?" *char-punctuation*)
(push "," *char-punctuation*)
(push "(" *char-punctuation*)
(push ")" *char-punctuation*)

;; Stop words
(defvar *stop-list*)
(setf *stop-list* '("the" "a" "an" "this" "these" "those"))

;; Scone tokens have an id, an offset, and a length.  They 
;; must correspond to Scone elements, otherwise make-scone-token
;; returns NIL rather than tokens.
;;
;; keyword arg ALLOW-DUPLICATES controls whether multiple meanings
;; are returned for ambiguous words.  
;;  :allow-duplicates :none  -- returns NIL if there are multiple meanings.
;;  :allow-duplicates nil    -- returns the first only if there are multiple meanings.
;;  :allow-duplicates t      -- returns all if there are multiple meanings.
;;
(defun make-scone-token (id type offset &key (allow-duplicates nil))
  "Rather than returning a token type, return any Scone elements 
   associated with ID along with the offset and length of this 
   piece of text."
  (declare (ignore type))
  (let ((defs (lookup-definitions id))
        (len  (length id))
	(return-value nil))
    (when defs
      (cond ((null allow-duplicates)          ;; ALLOW-DUPLICATES is NIL.
             (setf defs (first defs))
             (setf return-value 
                   (list (list (first defs) offset len))))
            ((eq allow-duplicates ':none)     ;; ALLOW-DUPLICATES is :none                 
             (when (< (length defs) 2)
              (setf return-value 
                    (list (list (first (first defs)) offset len)))))
            (t                                ;; ALLOW-DUPLICATES is 't                  
             (dolist (def defs)
               (push (list (first def) offset len) return-value)))))
    (return-from make-scone-token (nreverse return-value))))

;; Get-scone-annotations 
;;
"Finds all Scone tokens that correspond to words in S, and 
 returns them as a list of tokens.  Each token has a Scone element name,
 an offset (No. of characters into the string S where the token begins),
 and a length (No. of characters that are included in the token).
" 
 (defun get-scone-annotations (s)
   (tokenize s))

;; Tokenize -- Uses *use-dp-tokenizer* to determine algorithm
;;
(defun tokenize (string &key (offset -1) (allow-duplicates nil))
  "Convert a string into a stream of tokens: inames or words.
   Calls tokenize-recursive with proper args. 
   Assume that the starting offset is 0 unless OFFSET is given.
  "
  (if *use-dp-tokenizer*
      (find-best-concept-match string)
    (tokenize-recursive "" "" string *string-token* offset :allow-duplicates allow-duplicates)))



;;; ***************************************************************************
;;;  Recursive Tokenization (one word to one Scone concept)
;;;

;; tokenize-recursive
;;
;;(defun tokenize-recursive (nextchar currentword rem state offset 
;;                                    &key (allow-duplicates nil))
(defun tokenize-recursive (nextchar currentword rem state offset &key (allow-duplicates nil))
  "Convert a string into a list of tokens recursively.  
   NEXTCHAR is the lookahead character, 
   CURRENTWORD is the token we're currently building,
   REM is the remainder of the string we're tokenizing, after the current word
   STATE determines what to do with NEXTCHAR and CURRENTWORD.
   OFFSET is the current offset for the beginning of this token in the string.
  "
  ;;(format t "TOKENIZE-RECURSIVE \"~A\" \"~A\" \"~A\" ~A ~A~%" nextchar currentword rem state offset)
  (cond ((equal rem "")
	 ;; 
	 ;; We completed a token.
	 (cond ((equal currentword "") nil)
	       (t
		(if (or (eq state *english-name-end*) 
			(eq state *pos-tag-end*) 
			(eq state *iname-token-end*)) 
		    (setq state (- state 1)))
		(cond ((or (equal nextchar "}") (equal nextchar " ") (equal nextchar "]"))
                       
                       (apply *make-token* (list currentword 
                                                 state 
                                                 (- offset (length currentword)) 
                                                 :allow-duplicates allow-duplicates)))
		      (t 
                       (apply *make-token* (list (format nil "~a~a" currentword nextchar) 
                                                 state 
                                                 (- offset (length currentword)) 
                                                 :allow-duplicates allow-duplicates)))))))
	(t
	 (cond 
	  ;; Starting a new element iname: state = 1 and continue
	  ((equal nextchar "{")
	   (tokenize-recursive (subseq rem 0 1)
			  ""
			  (subseq rem 1)
			  *iname-token*
			  (+ 1 offset)
                          :allow-duplicates allow-duplicates))
	  ;; Ending an element iname: change state to reflect that.
	  ((equal nextchar "}")
	   (tokenize-recursive (subseq rem 0 1) 
			  currentword
			  (subseq rem 1)
			  *iname-token-end*
			  (+ 1 offset)
                          :allow-duplicates allow-duplicates))
	  ;; Starting an english name
	  ((equal nextchar "[")
	   (tokenize-recursive (subseq rem 0 1)
			  ""
			  (subseq rem 1)
			  *english-name*
			  (+ 1 offset)
                          :allow-duplicates allow-duplicates))
	  ;; Ending an english name, with or without POS tag
	  ((equal nextchar "]")
	   (cond ((eq state *english-name*)
		  (tokenize-recursive (subseq rem 0 1) 
				 currentword
				 (subseq rem 1)
				 *english-name-end*
				 (+ 1 offset)
                                 :allow-duplicates allow-duplicates))
		 ((eq state *pos-tag*)
		  (tokenize-recursive (subseq rem 0 1) 
				 currentword
				 (subseq rem 1)
				 *pos-tag-end*
				 (+ 1 offset)
                                 :allow-duplicates allow-duplicates))))
          ;;; NEW: aug 2005 commenting this out because we want to 
          ;;; catch TO: and FROM: tags in email more than :NOUN or other pos-tags.
	  ;; Starting a POS tag
	  #|((equal nextchar ":")
              (tokenize-recursive (subseq rem 0 1)
			  (format nil "~a~a" currentword nextchar)
			  (subseq rem 1)
			  *pos-tag*
			  (+ 1 offset)
                          :allow-duplicates allow-duplicates))
          |#
	  ;; Commas are list separators and they must end a token 
	  ((gethash nextchar *punctuation*)
	   (when (or (eq state *english-name-end*)
		     (eq state *iname-token-end*)
		     (eq state *pos-tag-end*)) 
	     (setf state (- state 1)))
           ;;
           ;; NEW! don't want a new token if we're in the middle of an email address.
           (if (equal (subseq rem 0 1) " ")
               (append (append 
                        (apply *make-token* 
                               (list currentword 
                                     state 
                                     (- offset (length currentword)) 
                                     :allow-duplicates allow-duplicates))
                        (apply *make-token*
                               (list (gethash nextchar *punctuation*) 
                                     *list-sep* 
                                     offset 
                                     :allow-duplicates allow-duplicates)))
                       (tokenize-recursive (subseq rem 0 1) 
                                           "" 
                                           (subseq rem 1) 
                                           *string-token* 
                                           (+ 1 offset)
                                           :allow-duplicates allow-duplicates))
	     (tokenize-recursive (subseq rem 0 1)
                                 (format nil "~a~a" currentword nextchar)
                                 (subseq rem 1)
                                 state
                                 (+ 1 offset)  ;; NEW!
                                 :allow-duplicates allow-duplicates)))
	  ;; If we're at the end of a word
	  ((equal nextchar " ")
	   (cond 
	    ((or (eq state *pos-tag-end*) (eq state *string-token*) 
		 (eq state *iname-token-end*) (eq state *english-name-end*))
	     ;;
	     ;;  Just completed a token. Return it and keep going.
	     (unless (eq state *string-token*) (setf state (- state 1)))
	     (cond ((equal currentword "")
		    (tokenize-recursive (subseq rem 0 1) 
                                        "" 
                                        (subseq rem 1) 
                                        *string-token* 
                                        (+ 1 offset)
                                        :allow-duplicates allow-duplicates))
		   (t
		    (append 
                     (apply *make-token* (list currentword 
                                               state 
                                               (- offset (length currentword)) 
                                               :allow-duplicates allow-duplicates))
			    (tokenize-recursive (subseq rem 0 1) 
                                                "" 
                                                (subseq rem 1) 
                                                *string-token* 
                                                (+ 1 offset)
                                                :allow-duplicates allow-duplicates)))))
	    ((or (eq state *iname-token*) (eq state *english-name*))
	     ;; In the middle of an iname
	     (tokenize-recursive (subseq rem 0 1)
                                 (format nil "~a~a" currentword nextchar)
                                 (subseq rem 1)
                                 state
                                 (+ 1 offset)  ;; NEW!
                                 :allow-duplicates allow-duplicates))))
	  ;; Otherwise we're in the middle of a word, keep building it
	  (t
	   (tokenize-recursive (subseq rem 0 1) 
                               (format nil "~a~a" currentword nextchar)
                               (subseq rem 1)
                               state
                               (+ 1 offset)
                               :allow-duplicates allow-duplicates))))))

;;; ***************************************************************************
;;;  Multi-word Tokenization 
;;;

(defvar *tokens-by-offset*)
(setf *tokens-by-offset* (make-hash-table :test #'equal))

(defun find-best-concept-match-multiple (str)
  ;; populate the tokens-by-offset hash
  (get-all-concept-tokens str)
  (let ((return-tokens nil))
    ;; greedy search for longest matches: start at 0 and find the 
    ;; longest token
    (do ((i 0))
        ((eq i (length str)))
      ;; Examine any tokens that start at this position
      (let ((toks (gethash i *tokens-by-offset*))
            (max-tok nil)
            (max-ln nil))
        (cond 
         ;; If there are tokens, find the longest one and 
         ;; increment i according to its length.
         (toks
          (setf max-ln 0)
          (dolist (tok toks)
            (when (>= (third tok) max-ln)
              (setf max-tok tok) 
              (push tok return-tokens)))
          (setf i (+ i (third max-tok))))
         ;; if there are no tokens, increment i by one and keep looking
         (t
          (incf i)))))
    return-tokens))

(defun find-best-concept-match (str)
  ;; populate the tokens-by-offset hash
  (get-all-concept-tokens str)
  (let ((return-tokens nil))
    ;; greedy search for longest matches: start at 0 and find the 
    ;; longest token
    (do ((i 0))
        ((eq i (length str)))
      ;; Examine any tokens that start at this position
      (let ((toks (gethash i *tokens-by-offset*))
            (max-toks nil)
            (max-ln nil))
        (cond 
         ;; If there are tokens, find the longest one(s) and 
         ;; increment i according to its length.
	 ;; NEW: if there is >1 with same offset/length, return all
	 ;; meanings 
         (toks
          (setf max-ln 0)
          (dolist (tok toks)
	    (cond 
	     ;; When the length is equal to the current best,
	     ;; just add this to the list of results.
	     ((= (third tok) max-ln)
	      (push tok max-toks))
	     ;; When the length is greater, we have a new max.
	     ;; Reset the list.
	     ((> (third tok) max-ln)
	      (setf max-toks (list tok))
	      (setf max-ln (third tok)))))
	  (dolist (max-tok max-toks) (push max-tok return-tokens))
          (setf i (+ i (third (first max-toks)))))
         ;; if there are no tokens, increment i by one and keep looking
         (t
          (incf i)))))
    return-tokens))

(defun find-partial-tokens (str)
  "Return a list of all the concepts that could appear in string STR.
   (DEPRECATED--> Each item in the list is a word index plus a list of concepts that 
   are the scone definitions for that word.)"
  (let* ((tokens (split-sequence:split-sequence #\Space str))
         (defs nil)
         (live-tokens nil)
         (all-tokens nil))
    (do* ((i 0 (+ i 1))
          (end (length tokens))
          (tok nil)
          (tok-plus-one nil nil)
          (tok-plus-two nil nil))
        ((eq i end))
      (setf tok (nth i tokens))
      (if (> i (- end 2))
          (setf tok-plus-one nil)
        (setf tok-plus-one 
              (concatenate 'string (nth i tokens) " " (nth (+ i 1) tokens))))
      (if (> i (- end 3))
          (setf tok-plus-two nil)
        (setf tok-plus-two 
              (concatenate 'string tok-plus-one " " (nth (+ i 2) tokens))))
      (setf live-tokens nil)
      ;; Get all definitions for tok with and without punctuation, and
      ;; for the 2- and 3-word windows around tok in STR.
      (setf defs (mapcar #'car (lookup-definitions tok)))
      (unless (equal tok (remove-punct tok))
        (setf defs (append defs (mapcar #'car (lookup-definitions (remove-punct tok))))))
      (when tok-plus-one
	;(format t "find-partial-tokens: checking window \"~A\"~%" tok-plus-one)
        (setf defs (append defs (mapcar #'car (lookup-definitions tok-plus-one)))))
      (when tok-plus-two
	;(format t "find-partial-tokens: checking window \"~A\"~%" tok-plus-two)
        (setf defs (append defs (mapcar #'car (lookup-definitions tok-plus-two)))))
      ;; For each definition, create a return item with the current 
      ;; text position and the definition.
      (dolist (def defs)
        (push (cons def i) live-tokens)
        (pushnew def all-tokens)))
    ;(format t "find-partial-tokens found candidate concepts: ~A~%" all-tokens)
    all-tokens))

(defun get-all-concept-tokens (str)
  "Go over all the candidate concepts that could appear in the string,
   and anchor them to positions in the text.  Put all anchored tokens
   into the *tokens-by-offset* global hash."
  (clrhash *tokens-by-offset*)
  (let* ((concepts (find-partial-tokens str))
         (str-ln (length str))
         (full-tokens nil)
         (full-token nil))
    (dolist (c concepts)
      ;; For every concept, get all the english defs
      (dolist (name (mapcar #'car (get-english-names c)))
        ;; if there is a match in STR, make a new offset-token
        (dolist (instance (search-all-instances name str))
          (let ((offset (car instance))
                (ln (cdr instance)))
            ;; Check whether there is a space or punctuation on both sides
            ;;
            ;; Beginning of string, or punct. or space preceding
            (when (or (eq offset 0)  
                      (punctuation-or-space? 
                       (subseq str (- offset 1) offset)))
              ;; End of string, or punct. or space following
              (if (>= str-ln (+ offset ln 1))
                  (when 
                      (punctuation-or-space?
                       (subseq str (+ offset ln) (+ offset ln 1)))
                    (progn
                      ;;(format t "creating new token: ~A offset: ~A length: ~A~%"
                      ;;        c  offset ln)
                      (setf full-token  (list c offset ln))
                      (push full-token full-tokens)
                      (push full-token (gethash offset *tokens-by-offset*))))
                (progn
                  (setf full-token  (list c offset ln))
                  (push full-token full-tokens)
                  (push full-token (gethash offset *tokens-by-offset*)))))))))
    full-tokens))
    
(defun punctuation-or-space? (str)
  (let ((return-val nil))
    (dolist (p *char-punctuation*)
      (if (equal str p)
          (setf return-val t)))
    (unless return-val
      (if (equal str " ")
          (setf return-val t)))
    return-val))

(defun search-all-instances (substring string &optional (offset 0))
  "Search for all instances of SUBSTRING in STRING.
   Return NIL if there is no match.  Return a list of 
   offsets if there is one match or more."
  (let ((matches nil)
        (ln (length substring)))
    (do* ((ofs offset)
          (match (search substring string :start2 ofs :test #'char-equal)
                 (search substring string :start2 ofs :test #'char-equal)))
        ((null match))
      (push (cons match ln)  matches)
      (setf ofs (+ match ln)))
    matches))

(defun remove-punct (str)
  (dolist (p *char-punctuation*)
    (setf str (replace-all str p "")))
  str)

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part 
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos))) 
