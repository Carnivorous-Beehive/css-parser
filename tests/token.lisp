(defpackage css-parser/tests/token
  (:use :cl
        :css-parser.token
        :rove
        :esrap))
(in-package :css-parser/tests/token)

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

(deftest test-<url-token>
  (testing "parses URL tokens"
    (ok (parse '<url-token> "url(pdficon.jpg)"))
    (ok (parse '<url-token> "url(https://example.com/images/myImg.jpg)"))
    (ok (parse '<url-token> "url(data:image/png;base64,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)"))
    (ok (parse '<url-token> "url(myFont.woff)"))
    (ok (parse '<url-token> "url(#IDofSVGpath)"))
    (ok (parse '<url-token> "url('fantasticfont.woff')"))
    (ok (parse '<url-token> "url(\"pdficon.jpg\")"))
    (ok (parse '<url-token> "url(\"masks.svg#mask1\")"))))

(deftest test-<number-token>
  (testing "parses number tokens"
    (ok (parse '<number-token> "1"))
    (ok (parse '<number-token> "0"))
    (ok (parse '<number-token> "0.0"))
    (ok (parse '<number-token> "1.1"))
    (ok (parse '<number-token> "-1"))
    (ok (parse '<number-token> "-0"))
    (ok (parse '<number-token> "-1.1"))
    (ok (parse '<number-token> "+0"))
    (ok (parse '<number-token> "4e4"))
    (ok (parse '<number-token> "-4e4"))
    (ok (parse '<number-token> "4E4"))
    (ok (parse '<number-token> "-4E4"))
    (ok (parse '<number-token> "4e-4"))
    (ok (parse '<number-token> "1.05E-10"))
    (ok (parse '<number-token> "-1.0e921"))
    (ok (parse '<number-token> "-1.2E+1"))))

(deftest test-<dimension-token>
  (testing "parses a dimension"
    (ok (parse '<dimension-token> "1rem"))
    (ok (parse '<dimension-token> "1.25px"))
    (ok (parse '<dimension-token> "1000em"))
    (ok (parse '<dimension-token> "30vw"))))

(deftest test-<percentage-token>
  (testing "parses percentages"
    (ok (parse '<percentage-token> "10%"))
    (ok (parse '<percentage-token> "1.25%"))
    (ok (signals (parse '<percentage-token> "100rem%")))))

(deftest test-<CDO-token>
  (testing "parses comment open token"
    (ok (parse '<CDO-token> "<!--"))
    (ok (signals (parse '<CDO-token> "<--")))))

(deftest test-<CDC-token
  (testing "parses comment close token"
    (ok (parse '<CDC-token> "-->"))
    (ok (signals (parse '<CDC-token "->")))))
