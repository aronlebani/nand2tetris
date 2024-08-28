;;;; package.lisp
;;;;
;;;; A VM translator for the hack platform.
;;;;
;;;; Aron Lebani

(defpackage :vm
  (:use :cl)
  (:import-from :sb-ext
                :*posix-argv*)
  (:import-from :str
                :split
                :trim
                :rsplit)
  (:import-from :alexandria
                :switch)
  (:export :main))
