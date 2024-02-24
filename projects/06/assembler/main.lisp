(in-package :assembler)

(defun get-dest (path)
  (make-pathname
    :directory (pathname-directory path)
    :name (pathname-name path)
    :type "hack"))

(defun parse (path)
  (with-open-file (in path)
    (with-open-file (out (get-dest path)  :direction :output :if-exists :supersede)
      (when in
        (loop for line = (read-line in nil)
              while line
              do (let ((parsed (parse-command (string-trim '(#\return) line))))
                   (when parsed
                     (write-line parsed out))))))))

(defun main ()
  (parse (nth 1 *posix-argv*)))
