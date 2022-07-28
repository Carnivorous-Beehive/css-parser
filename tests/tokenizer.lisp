(defpackage css-parser/tests/tokenizer
  (:use :cl
        :css-parser.tokenizer
        :rove
        :esrap))
(in-package :css-parser/tests/tokenizer)

(deftest test-comment-rule
  (testing "\"/* comment */\" is parsed"
    (ok (equal '(("/" "*") (#\  #\c #\o #\m #\m #\e #\n #\t #\ ) ("*" "/"))
               (parse 'comment "/* comment */")))))

(deftest test-newline
  (testing "newline character is parsed"
    (ng (parse 'newline (format nil "~%")))))

(deftest test-whitespace
  (testing "whitespace character is parsed"
    (ng (parse 'whitespace " "))
    (ng (parse 'whitespace "    "))
    (ng (parse 'whitespace (format nil "~%")))))

(deftest test-hex-digit
  (testing "hex digits are parsed"
    (ok (equal #\a (parse 'hex-digit "a")))
    (ok (equal #\A (parse 'hex-digit "A")))
    (ok (equal #\0 (parse 'hex-digit "0")))
    (ok (signals (parse 'hex-digit "Z")))))

(deftest test-escape
  (testing "escaping characters are parsed"
    (ok (equal '("\\" #\') (parse 'escape "\\'")))
    (ok (equal '("\\" ((#\a #\a #\a #\a #\a #\a) nil)) (parse 'escape "\\aaaaaa")))
    (ok (signals (parse 'escape "\\aaaaaaa")))
    (ok (equal '("\\" ((#\a) nil)) (parse 'escape "\\a")))
    (ok (equal '("\\" ((#\a) nil)) (parse 'escape "\\a  ")))))

(deftest test-whitespace-token
  (testing "parses many whitespaces"
    (ok (equal '(nil) (parse '<whitespace-token> "     ")))))

(deftest test-ws*
  (testing "parses many <whitespace-tokens>"
    (ok (equal '((nil)) (parse 'ws* "   ")))))
