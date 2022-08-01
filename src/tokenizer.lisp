(defpackage css-parser.tokenizer
  (:use :cl :esrap)
  (:import-from :str
                :ascii-p)
  (:export #:comment
           #:newline
           #:whitespace
           #:hex-digit
           #:escape
           #:<whitespace-token>
           #:ws*
           #:<ident-token>
           #:<function-token>
           #:<at-keyword-token>
           #:<hash-token>
           #:<string-token>
           #:<url-token>
           #:<number-token>
           #:<dimension-token>
           #:<percentage-token>
           #:<CDO-token>
           #:<CDC-token>
           #:<colon-token>
           #:<semicolon-token>
           #:<comma-token>
           #:<[-token>
           #:<]-token>
           #:<\(-token>
           #:<\)-token>
           #:<{-token>
           #:<}-token>))
(in-package :css-parser.tokenizer)

(defun one-to-six-chars (s)
  (<= 1 (length s) 6))

(defrule comment-open (and #\/ #\*))
(defrule comment-close (and #\* #\/))
(defrule comment (and comment-open (* (not comment-close)) comment-close))

(defrule newline (+ (or #\newline #\return #\linefeed))
  (:constant nil))

(defrule whitespace (+ (or #\space #\tab newline))
  (:constant nil))

(defrule hex-digit (character-ranges (#\a #\f) (#\0 #\9) (#\A #\F)))

(defrule escape
    (and #\\ (or (not (or newline hex-digit))
                 (and (one-to-six-chars (+ hex-digit)) (? whitespace)))))

(defrule <whitespace-token> (+ whitespace))

(defrule ws* (? (+ <whitespace-token>)))

(defrule <ident-token>
    (and (or (and #\- #\-)
             (and (? #\-)
                  (or (or (character-ranges (#\a #\z) (#\A #\Z))
                          #\_
                          (not (ascii-p character)))
                      escape)))
         (? (or (+ (or (or (character-ranges (#\a #\z) (#\A #\Z) (#\0 #\9))
                           #\_
                           #\-
                           (not (ascii-p character)))
                       escape))))))

(defrule <function-token> (and <ident-token> #\())

(defrule <at-keyword-token> (and #\@ <ident-token>))

(defrule <hash-token>
    (and #\#
         (? (or (+ (or (or (character-ranges (#\a #\z) (#\A #\Z) (#\0 #\9))
                           #\_
                           #\-
                           (not (ascii-p character)))
                       escape))))))

(defrule string-boundary (or #\" #\'))
(defrule <string-token>
    (and string-boundary
         (+ (or (not (or #\" #\' #\\ newline))
                escape
                (and #\\ newlilne)))
         string-boundary))

(defrule <url-token>
    (and <ident-token>
         "url"
         #\(
         ws*
         (? (or (not (or string-boundary #\( #\) #\\ whitespace))
                     (? escape)))
         ws*
         #\)))

(defrule <number-token> (+ (numberp character)))

(defrule <dimension-token> (and <number-token> <ident-token>))

(defrule <percentage-token> (and <number-token> #\%))

(defrule <CDO-token> (and #\< #\! #\- #\-))
(defrule <CDC-token> (and #\- #\- #\>))

(defrule <colon-token> #\:)
(defrule <semicolon-token> #\;)
(defrule <comma-token> #\,)
(defrule <[-token> #\[)
(defrule <]-token> #\])
(defrule <\(-token> #\()
(defrule <\)-token> #\))
(defrule <{-token> #\{)
(defrule <}-token> #\})
