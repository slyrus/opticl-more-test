
(asdf:defsystem :opticl-test
  :name "opticl-test"
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
