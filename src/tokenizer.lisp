(defpackage css-parser.tokenizer
  (:use :cl :esrap :str))
(in-package :css-parser.tokenizer)

(defrule comment-open (and #\/ #\*))
(defrule comment-close (and #\* #\/))
(defrule comment (and comment-open (* (not comment-close)) comment-close))

(defrule newline (+ (or #\newline #\return #\f)))

(defrule whitespace (+ (or #\space #\tab newline)))

(defrule hex-digit (character-ranges (#\a #\f) (#\0 #\9) (#\A #\F)))

(defun one-to-six-chars (string)
  (<= 1 (length string) 6))

(defrule escape
    (and #\\
         (or (not (or newline hex-digit))
             (and (one-to-six-chars (+ hex-digit))
                  (? whitespace)))))

(defrule whitespace-token (+ whitespace))

(defrule ws* (? (+ whitespace-token)))

;; TODO This needs a ton of work @see https://www.w3.org/TR/css-syntax-3/#ident-token-diagram
(defrule ident-token
  (and (or (character-ranges (#\a #\z #\_)) (not (ascii-p character)))))

(defrule function-token (and ident-token #\())

(defrule at-keyword-token (and #\@ ident-token))

(defrule string-token
    (and (or #\" #\')
         (+ (or (not #\" #\\ newline)
                escape
                (and #\\ newlilne)))
         (or #\" #\')))

(parse 'comment "/* yo dawg */ this is outside" :junk-allowed t)
(parse 'hex-digit "c")
(parse 'escape "\\aaaaaa")
