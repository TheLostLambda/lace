(defsystem "llace"
  :version "0.1.0"
  :author "Brooks J Rady"
  :license "AGPLv3"
  :serial t
  :depends-on ("serapeum"
               "clazy")
  :components ((:module "src"
                :components
                ((:file "lazy")
                 (:file "functional-parsing")
                 (:file "parsing")
                 (:file "main"))))
  :description "An interpreter for the Lace language"
  :in-order-to ((test-op (test-op "llace/tests"))))

(defsystem "llace/tests"
  :author "Brooks J Rady"
  :license "AGPLv3"
  :depends-on ("llace"
               "parachute")
  :components ((:module "tests"
                :components
                ((:file "functional-parsing")
                 (:file "main"))))
  :description "Test system for llace"
  :perform (test-op (op c) (symbol-call :parachute :test (symbol-call :parachute :test-packages))))
