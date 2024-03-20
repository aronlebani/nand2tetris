(in-package :vm)

(defun get-dest (path)
  (make-pathname
    :directory (pathname-directory path)
    :name (pathname-name path)
    :type "asm"))

(defun parse (path)
  (with-open-file (in path)
    (with-open-file (out (get-dest path) :direction :output :if-exists :supersede)
      (when in
        (loop for line = (read-line in nil)
              while line
              do (let ((parsed (parse-line (string-trim '(#\return) line))))
                   (when parsed
                     (write-line parsed out))))))))

(defun main ()
  (let ((path (nth 1 *posix-argv*)))
    (parse path)))
