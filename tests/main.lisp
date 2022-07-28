(defpackage css-parser/tests/main
  (:use :cl
        :css-parser
        :rove))
(in-package :css-parser/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :css-parser)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
