(defsystem "llace"
  :version "0.1.0"
  :author ""
  :license ""
  :serial t
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "scanner")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "llace/tests"))))

(defsystem "llace/tests"
  :author ""
  :license ""
  :depends-on ("llace"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for llace"
  :perform (test-op (op c) (symbol-call :rove :run c)))
