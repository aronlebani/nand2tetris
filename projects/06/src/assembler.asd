(asdf:defsystem :assembler
  :build-operation program-op
  :build-pathname "assembler"
  :entry-point "assembler:main"
  :serial t
  :depends-on ("str")
  :components ((:file "package")
               (:file "helpers")
               (:file "main")
               (:file "parser")
               (:file "symbol")
               (:file "code")))
