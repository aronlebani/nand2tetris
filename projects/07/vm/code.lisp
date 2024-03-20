(in-package :vm)

(defvar *command-id* 0)

(defparameter *operations*
  '(:add "D=M+D"
    :sub "D=M-D"
    :neg "D=-M"
    :eq "JEQ"
    :gt "JGT"
    :lt "JLT"
    :and "D=M&D"
    :or "D=M|D"
    :not "D=!M"))

(defparameter *segments*
  '(:local "LCL"
    :argument "ARG"
    :this "THIS"
    :that "THAT"
    :r5 "R5"
    :r6 "R6"
    :r7 "R7"
    :r8 "R8"
    :r9 "R9"
    :r10 "R10"
    :r11 "R11"
    :r12 "R12"
    :r13 "R13"
    :r14 "R14"
    :r15 "R15"))

(defun format-codes (&rest codes)
  (format nil "~{~a~%~}" codes))

(defun write-a (value)
  (format nil "@~a" value))

(defun jump-code (jump-condition)
  (format nil "D;~a" jump-condition))

(defun operation-code (operation)
  (getf *operations* operation))

(defun inc-sp ()
  (format-codes "@SP" "M=M+1"))

(defun dec-sp ()
  (format-codes "@SP" "M=M-1"))

(defun segment-address (segment index &key into-d from-a)
  (cond (into-d
          (format-codes (write-a index)
                        "D=A"
                        (write-a (getf *segments* segment))
                        (if from-a "D=D+A" "D=D+M")))))

(defun write-memory (segment &key from-d)
  (cond (from-d
          (format-codes (write-a (getf *segments* segment)) "M=D"))))

(defun push-stack (&key value from-d index from-segment from-reg)
  (cond (from-d 
          (format-codes "@SP" "A=M" "M=D" (inc-sp)))
        (value
          (format-codes (write-a value) "D=A" (push-stack :from-d t)))
        (from-segment
          (format-codes (segment-address from-segment index :into-d t) (push-stack :from-d t)))
        (from-reg
          (format-codes (segment-address from-segment index :into-d t :from-a t)
                        (push-stack :from-d t)))))

(defun pop-stack (&key into-d index into-segment into-reg)
  (cond (into-d 
          (format-codes (pop-stack) "D=M"))
        (into-segment
          (format-codes (segment-address into-segment index :into-d t)  
                        (write-memory :r13 :from-d t)
                        (pop-stack :into-d t)
                        (write-a "R13")
                        "A=M"
                        "M=D"))
        (into-reg
          (format-codes (segment-address into-segment index :into-d t :from-a t)
                        (write-memory :r13 :from-d t)
                        (pop-stack :into-d t)
                        (write-a "R13")
                        "A=M"
                        "M=D"))
        (t
          (format-codes (dec-sp) "@SP" "A=M"))))

(defun set-d-if (jump-condition)
  (incf *command-id*)
  (format-codes "D=M-D"
                (format nil "@TRUE_P.~a" *command-id*)
                (jump-code jump-condition)
                (format nil "@FALSE_P.~a" *command-id*)
                "0;JMP"
                (format nil "(TRUE_P.~a)" *command-id*)
                "D=-1"
                (format nil "@END.~a" *command-id*)
                "0;JMP"
                (format nil "(FALSE_P.~a)" *command-id*)
                "D=0"
                (format nil "@END.~a" *command-id*)
                "0;JMP"
                (format nil "(END.~a)" *command-id*)))

(defun arithmetic (&key operation unary)
  (if unary
      (format-codes (pop-stack) (operation-code operation) (push-stack :from-d t))
      (format-codes (pop-stack :into-d t)
                    (pop-stack)
                    (operation-code operation)
                    (push-stack :from-d t))))

(defun conditional (&key operation)
  (format-codes (pop-stack :into-d t)
                (pop-stack)
                (set-d-if (operation-code operation))
                (push-stack :from-d t)))
