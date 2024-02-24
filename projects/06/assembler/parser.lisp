(in-package :assembler)

(defmacro end (seq)
  `(- (length ,seq) 1))

(defmacro position-after (element seq)
  `(when (position ,element ,seq)
     (+ (position ,element ,seq) 1)))

(defun a-command? (command)
  (char= (elt command 0) #\@))

(defun c-command? (command)
  (or (find #\= command) (find #\; command)))

(defun l-command? (command)
  (and (char= (elt command 0) #\()
       (char= (elt command (end command)) \#)))

(defun command-type (command)
  (cond ((a-command? command) 'a-command)
        ((c-command? command) 'c-command)
        ((l-command? command) 'l-command)))

(defun sym (command)
  (cond ((a-command? command) (subseq command 1))
        ((l-command? command) (subseq command 1 (length command)))))

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
  (format nil "0~15,'0b" (parse-integer (sym command))))

(defun parse-l-command (command)
  (format nil "0~15,'0b" (parse-integer (sym command))))

(defun parse-c-command (command)
  (format nil "111~a~a~a~a"
          (address-code (comp-mnemonic command))
          (comp-code (comp-mnemonic command))
          (dest-code (dest-mnemonic command))
          (jump-code (jump-mnemonic command))))

(defun parse-command (line)
  (cond ((equal line "") nil)
        ((a-command? line) (parse-a-command line))
        ((l-command? line) (parse-l-command line))
        ((c-command? line) (parse-c-command line))))
