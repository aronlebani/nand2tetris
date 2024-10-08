(in-package :vm)

(defmacro with-parts ((line command-name &optional (arg1-name 'arg1) (arg2-name 'arg2)) &body body)
  `(let* ((parts (split " " ,line)) (,command-name (first parts))
          ,(if arg1-name `(,arg1-name (second parts)))
          ,(if arg2-name `(,arg2-name (third parts))))
     ,@body))

(defun empty? (line)
  (equal line ""))

(defun c-arithmetic? (line)
  (with-parts (line command)
    (find command
          '("add" "sub" "neg" "eq" "gt" "lt" "and" "or" "not")
          :test #'equal)))

(defun c-push? (line)
  (with-parts (line command)
    (equal command "push")))

(defun c-pop? (line)
  (with-parts (line command)
    (equal command "pop")))

(defun parse-c-arithmetic (line)
  (with-parts (line command)
    (switch (command :test #'equal)
      ("add" (arithmetic :operation :add))
      ("sub" (arithmetic :operation :sub))
      ("neg" (arithmetic :operation :neg :unary t))
      ("eq" (conditional :operation :eq))
      ("gt" (conditional :operation :gt))
      ("lt" (conditional :operation :lt))
      ("and" (arithmetic :operation :and))
      ("or" (arithmetic :operation :or))
      ("not" (arithmetic :operation :not :unary t)))))

(defun parse-c-push (line)
  (with-parts (line command segment index)
    (switch (segment :test #'equal)
      ("constant" (push-stack :value index))
      ("local" (push-stack :index index :from-segment :local))
      ("argument" (push-stack :index index :from-segment :argument))
      ("this" (push-stack :index index :from-segment :this))
      ("that" (push-stack :index index :from-segment :that))
      ("pointer" (push-stack :index index :from-reg :pointer))
      ("temp" (push-stack :index index :from-segment :r5))
      ("static" ()))))

(defun parse-c-pop (line)
  (with-parts (line command segment index)
    (switch (segment :test #'equal)
      ("local" (pop-stack :index index :into-segment :local))
      ("argument" (pop-stack :index index :into-segment :argument))
      ("this" (pop-stack :index index :into-segment :this))
      ("that" (pop-stack :index index :into-segment :that))
      ("pointer" (pop-stack :index 0 :into-reg (if (= index 0) :this :that)))
      ("temp" (pop-stack :index index :into-reg :r5))
      ("static" ()))))

(defun strip-whitespace (line)
  (trim line))

(defun strip-comments (line)
  (first (rsplit "//" line)))

(defun sanitize-command (line)
  (strip-whitespace (strip-comments line)))

(defun parse-line (line)
  "Parses 'line' and returns the machine code. Returns nil if 'line' is a
   comment, whitespace, or an invalid command."
  (let ((sanitized (sanitize-command line)))
    (cond ((empty? sanitized) nil)
          ((c-arithmetic? sanitized) (parse-c-arithmetic sanitized))
          ((c-push? sanitized) (parse-c-push sanitized))
          ((c-pop? sanitized) (parse-c-pop sanitized)))))
