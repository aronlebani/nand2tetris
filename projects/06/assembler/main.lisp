(in-package :assembler)

(defun get-dest (path)
  (make-pathname
    :directory (pathname-directory path)
    :name (pathname-name path)
    :type "hack"))

(defun parse-commands (path)
  (with-open-file (in path)
    (with-open-file (out (get-dest path)  :direction :output :if-exists :supersede)
      (when in
        (loop for line = (read-line in nil)
              while line
              do (let ((parsed (parse-command (string-trim '(#\return) line))))
                   (when parsed
                     (write-line parsed out))))))))

(defun parse-symbols (path)
  (with-open-file (in path)
    (when in
      (loop for line = (read-line in nil)
            while line
            do (parse-symbol (string-trim '(#\return) line))))))

(defun main ()
  (let ((path (nth 1 *posix-argv*)))
    (parse-symbols path)
    (parse-commands path)))
