(in-package :equwal)
(defun g!-symbol-p (s)
  (and (symbolp s)
       (> (length (symbol-name s)) 2)
       (or (string= (symbol-name s)
		    "G!"
		    :start1 0
		    :end1 2)
	   (string= (symbol-name s)
		    ",G!"
		    :start1 0
		    :end1 3))))
(defun prepare (str)
  (concatenate 'string (string #\() str (string #\))))
(defun backquote-kludge (str)
  (remove #\` str))
(defmacro with-macro-fn (char new-fn &body body)
  (once-only (char new-fn)
    (with-gensyms (old)
      `(let ((,old (get-macro-character ,char)))
	 (progn (set-macro-character ,char ,new-fn)
		(prog1 (progn ,@body) (set-macro-character ,char ,old)))))))
(defun read-atoms (str)
  (with-macro-fn #\, nil
    (flatten (read-from-string (backquote-kludge (prepare str)) nil nil))))
(defun read-to-string (stream &optional (acc (make-stack)) (parens 0))
  (aif (and (not (= -1 parens)) (read-char stream nil nil))
       (read-to-string stream (push-on it acc) (case it
						 (#\) (1- parens))
						 (#\( (1+ parens))
						 (t parens)))
       (concatenate 'string (subseq acc 0 (1- (length acc))))))
(defun gbang (stream no nope)
  (declare (ignore no nope))
  (let* ((str1 (read-to-string stream))
	 (term (aref str1 0))
	 (str (prepare str1))
	 (code (read-from-string str nil))
	 (syms (remove-duplicates (mapcar #'(lambda (x) (intern (remove #\, (symbol-name x))))
					  (remove-if-not #'g!-symbol-p (read-atoms str)))
				  :test #'(lambda (x y)
					    (string-equal (symbol-name x)
							  (symbol-name y))))))
    ;; a good thing.
    `(defmacro ,(car code) ,(cadr code)
       (let (,@(loop for x in syms
		  collect (list x '(gensym))))
	 ,@(cddr code)))))
;; Expansion
(defmacro name (y)
  (let ((g!x (gensym)))
     `(let ((,g!x ,y))
	(list ,g!x ,g!x))))
;; Dirty test case
;; #defmacro/g! name (z) `(let ((,g!y ,z)) (list ,g!y ,g!y)))
;; Clean test case
;#d{name (z) `(let ((,g!y ,z)) (list ,g!y ,g!y))}
(set-dispatch-macro-character #\# #\d #'gbang)
;(set-macro-character #\d #'g! t)
;; (defmacro defmacro/g! (name args &rest body)
;;   (let ((syms (remove-duplicates
;; 	       (remove-if-not #'g!-symbol-p
;; 			      (flatten body)))))
;;     `(defmacro ,name ,args
;;        (let ,(mapcar
;; 	      (lambda (s)
;; 		`(,s (gensym ,(subseq
;; 			       (symbol-name s)
;; 			       2))))
;; 	      syms)
;;          ,@body))))
;; (set-dispatch-character)
;; (defmacro/g! test () `(list ,g!a))
