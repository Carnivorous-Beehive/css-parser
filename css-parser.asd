(defsystem "css-parser"
  :version "0.1.0"
  :author "Evan Duncan"
  :license "LGPL 3.0 or later"
  :depends-on ("esrap"
               "str")
  :components ((:module "src"
                :components
                ((:file "parser" :depends-on ("tokenizer"))
                 (:file "tokenizer")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "css-parser/tests"))))

(defsystem "css-parser/tests"
  :author "Evan Duncan"
  :license "LGPL 3.0 or later"
  :depends-on ("css-parser"
               "rove"
               "esrap")
  :components ((:module "tests"
                :components
                ((:file "tokenizer")
                 (:file "main"))))
  :description "Test system for css-parser"
  :perform (test-op (op c) (symbol-call :rove :run c)))
