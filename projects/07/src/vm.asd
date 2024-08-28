(asdf:defsystem :vm
  :build-operation program-op
  :build-pathname "vm"
  :entry-point "vm:main"
  :serial t
  :depends-on ("str" "alexandria")
  :components ((:file "package")
               (:file "main")
               (:file "parser")  
               (:file "code")))
