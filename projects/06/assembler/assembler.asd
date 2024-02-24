(asdf:defsystem :assembler
  :build-operation program-op
  :build-pathname "assembler"
  :entry-point "assembler:main"
  :serial t
  :components ((:file "package")
               (:file "main")
               (:file "parser")
               (:file "code")))
