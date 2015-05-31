
(asdf:defsystem :opticl-test
  :name "opticl-test"
  :description "Test library for opticl. Kept separate from opticl to keep opticl size down."
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :default-component-class cl-source-file
  :depends-on (opticl)
  :serial t
  :components
  ((:file "package")
   (:file "opticl-test")
   (:file "shapes-test")
   (:file "transform-test")
   (:file "threshold-test")
   (:file "gamma-test")))
