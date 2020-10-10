(defsystem "afp-series-workshop"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on ("series")
  :components ((:module "src"
                :components
                ((:file "test-series")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "afp-series-workshop/tests"))))

(defsystem "afp-series-workshop/tests"
  :author ""
  :license ""
  :depends-on ("afp-series-workshop"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for afp-series-workshop"
  :perform (test-op (op c) (symbol-call :rove :run c)))
