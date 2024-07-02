(in-package :assembler)

(defmacro end (seq)
  `(- (length ,seq) 1))

(defmacro position-after (element seq)
  `(when (position ,element ,seq)
     (+ (position ,element ,seq) 1)))

(defun make-out-path (path)
  (make-pathname
    :directory (pathname-directory path)
    :name (pathname-name path)
    :type "hack"))
