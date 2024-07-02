(in-package :assembler)

(defun parse-commands (path)
  (with-open-file (in path)
    (with-open-file (out (make-out-path path)
                         :direction :output
                         :if-exists :supersede)
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
