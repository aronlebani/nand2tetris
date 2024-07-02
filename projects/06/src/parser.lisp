(in-package :assembler)

(defun a-command? (command)
  (char= (elt command 0) #\@))

(defun c-command? (command)
  (or (find #\= command) (find #\; command)))

(defun l-command? (command)
  (and (char= (elt command 0) #\()
       (char= (elt command (end command)) #\))))

(defun empty? (command)
  (equal command ""))

(defun const? (command)
  (parse-integer (extract-symbol command)
                 :junk-allowed t))

(defun extract-symbol (command)
  (cond ((a-command? command) (subseq command 1))
        ((l-command? command) (subseq command 1 (1- (length command))))))

(defun dest-mnemonic (command)
  (when (c-command? command)
    (if (find #\= command)
        (subseq command 0 (position #\= command))  
        "null")))

(defun comp-mnemonic (command)
  (when (c-command? command)
    (subseq command
            (or (position-after #\= command) 0)
            (or (position #\; command) (length command)))))

(defun jump-mnemonic (command)
  (when (c-command? command)
    (if (find #\; command)
        (subseq command (position-after #\; command) (length command))  
        "null")))

(defun parse-a-command (command)
  (let ((sym (extract-symbol command)))
    (if (const? command)
        (const-code (parse-integer sym))
        (if (in-symbol-table? sym)
            (const-code (get-from-symbol-table sym)) 
            (const-code (add-to-symbol-table sym))))))

(defun parse-c-command (command)
  (format nil "111~a~a~a~a"
          (address-code (comp-mnemonic command))
          (comp-code (comp-mnemonic command))
          (dest-code (dest-mnemonic command))
          (jump-code (jump-mnemonic command))))

(defun strip-whitespace (line)
  (replace-all " " "" line))

(defun strip-comments (line)
  (first (rsplit "//" line)))

(defun sanitize-command (line)
  (strip-comments (strip-whitespace line)))

(defun parse-command (line)
  "Parses 'line' and returns the binary code. Returns nil if 'line' is a comment,
   whitespace, or an invalid command."
  (let ((sanitized (sanitize-command line)))
    (cond ((empty? sanitized) nil)
          ((a-command? sanitized) (parse-a-command sanitized))
          ((c-command? sanitized) (parse-c-command sanitized)))))
