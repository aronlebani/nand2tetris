(in-package :assembler)

(defparameter *pc* 0)

(defparameter *ram-counter* 16)

(defparameter *symbol-table*
  '(("SP" . 0)
    ("LCL" . 1)
    ("ARG" . 2)
    ("THIS" . 3)
    ("THAT" . 4)
    ("R0" . 0)
    ("R1" . 1)
    ("R2" . 2)
    ("R3" . 3)
    ("R4" . 4)
    ("R5" . 5)
    ("R6" . 6)
    ("R7" . 7)
    ("R8" . 8)
    ("R9" . 9)
    ("R10" . 10)
    ("R11" . 11)
    ("R12" . 12)
    ("R13" . 13)
    ("R14" . 14)
    ("R15" . 15)
    ("SCREEN" . 16384)
    ("KBD" . 16385)))

(defun in-symbol-table? (sym)
  (cdr (assoc sym *symbol-table* :test 'equal)))

(defun add-to-symbol-table (sym)
  (push (cons sym *ram-counter*) *symbol-table*)
  (incf *ram-counter*)
  (1- *ram-counter*))

(defun get-from-symbol-table (sym)
  (cdr (assoc sym *symbol-table* :test 'equal)))

(defun parse-symbol (line)
  "Parses 'line' and adds an entry to the *symbol-table* if it is an L command.
   Increases the program counter for A and C commands."
  (let ((sanitized (sanitize-command line)))
    (cond ((empty? sanitized) nil)
          ((or (a-command? sanitized) (c-command? sanitized)) (incf *pc*))
          ((l-command? sanitized) (push (cons (extract-symbol sanitized) *pc*)
                                        *symbol-table*)))))
