;;;; package.lisp 
;;;;
;;;; An assembler for the hack platform.
;;;;
;;;; Aron Lebani

(defpackage :assembler
  (:use :cl)
  (:import-from :sb-ext
                :*posix-argv*)
  (:export :main))
