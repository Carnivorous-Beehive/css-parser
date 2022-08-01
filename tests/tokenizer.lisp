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

(deftest test-<ident-token>
    (testing "parses identity tokens"
             (ok (parse '<ident-token> "nono79"))
             (ok (parse '<ident-token> "ground-level"))
             (ok (parse '<ident-token> "-test"))
             (ok (parse '<ident-token> "--toto"))
             (ok (parse '<ident-token> "_internal"))
             (ok (parse '<ident-token> "\\22 toto"))
             (ok (parse '<ident-token> "bili\\.bob"))
             (ok (signals (parse '<ident-token> "34rem")))
             (ok (signals (parse '<ident-token> "-12rad")))
             (ok (signals (parse '<ident-token> "bili.bob")))
             (ok (signals (parse '<ident-token> "'bilibob'")))
             (ok (signals (parse '<ident-token> "\"bilibob\"")))))

(deftest test-<function-token>
  (testing "parses function tokens"
    (ok (parse '<function-token> "nono79("))
    (ok (signals (parse '<function-token> "nono79")))
    (ok (parse '<function-token> "bili\\.bob("))
    (ok (signals (parse '<function-token> "bili.bob(")))))

(deftest test-<at-keyword-token>
  (testing "parses at keyword tokens"
    (ok (parse '<at-keyword-token> "@nono89"))
    (ok (signals (parse '<at-keyword-token> "@asdf@")))))

(deftest test-<hash-token>
  (testing "parses hash tokens"
    (ok (parse '<hash-token> "#fff"))
    (ok (signals (parse '<hash-token> "fff")))))

(deftest test-<string-token>
  (testing "parses string tokens"
    (ok (parse '<string-token> "\"asdf\""))
    (ok (signals (parse '<string-token> "asdf")))
    (ok (parse '<string-token> "'asdf'"))))
