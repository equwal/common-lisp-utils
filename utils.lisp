;; Export read-token read-int
(defun numberp-char (char)
  (or (char= #\0 char)
      (char= #\1 char)
      (char= #\2 char)
      (char= #\3 char)
      (char= #\4 char)
      (char= #\5 char)
      (char= #\6 char)
      (char= #\7 char)
      (char= #\8 char)
      (char= #\9 char)))
(defun whitespacep-char (char)
  (or (char= #\Newline char) (char= #\Space char) (char= #\Tab char)))
(defun read-token (&optional (built-string ""))
  (let ((char (read-char)))
    (if (whitespacep-char char)
	built-string
	(read-token (concatenate 'string
				 built-string
				 (make-string 1 :initial-element char))))))
(defun read-int (&optional (built-string ""))
  (let ((char (read-char)))
    (if (numberp-char char)
	(read-int (concatenate 'string
			       built-string
			       (make-string 1 :initial-element char)))
	(read-from-string built-string))))
