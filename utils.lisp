;; Export read-token read-int
(defvar *buffer-char* "")
(defun numberp-char (char)
  "Aux function."
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
  "Aux function."
  (or (char= #\Newline char) (char= #\Space char) (char= #\Tab char)))
(defmacro make-read (name char-predicate end-predicate)
  "Note: End predicate character gets eaten so we VALUES it.
This is a token reader with character predicates."
  (with-gensyms (char str stream)
    `(defun ,name (&optional (,stream *standard-input*))
       (labels ((recur (,str)
		  (let ((,char (read-char ,stream)))
		    (if ,end-predicate
			(values ,str ,char)
			(if ,char-predicate
			    (recur (concatenate 'string
						,str
						(make-string 1 :initial-element ,char))))))))
	 (recur "")))))
(make-read read-token #'(lambda (x) t) #'whitespacep-char)
(make-read read-int #'numberp-char #'whitespacep-char)
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))
(abbrev mvbind multiple-value-bind)
(abbrev dbind destructuring-bind)
(defmacro once-only ((&rest names) &body body)
  "A macro-writing utility for evaluating code only once."
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))
(defmacro with-gensyms (symbols &body body)
  "Create gensyms for those symbols."
  `(let (,@(mapcar #'(lambda (sym)
		       `(,sym ',(gensym))) symbols))
     ,@body))
(defun y-comb (f)
  "The Y-combinator from the Lambda calculus of Alonzo Church."
  ((lambda (x) (funcall x x))
   (lambda (y)
     (funcall f (lambda (&rest args)
		  (apply (funcall y y) args))))))
;;; Example code for the y-combinator is in order:
#|(funcall (y-comb #'(lambda (FUNCTION) (lambda (n) (if (= 0 n)
						      (progn (print 0) 0)			;
						      (progn (print n) (funcall FUNCTION (- n 1))))))) 10)|#
;;; Yes it is a bit complex and redundant, so there is an anaphoric macro
(defmacro ay (lambda-list-args specific-args &rest code)
  "Like Y-combinator, but is a recursive macro. The anaphor is 'f'. Don't forget
to funcall f instead of trying to use it like an interned function."
  `(funcall (y-comb #'(lambda (f) #'(lambda ,lambda-list-args
				       ,code)))
	    ,@specific-args))
;;; Example code for the macro:
#|(y (a b) (10 0) 
			 (if (= 0 a) 
			     b
			     (funcall f (- a 1) (progn (print b) (+ b 1)))))|#
;;; It is peano arithmetic.
(defmacro aif (conditional then &optional else)
  `(let ((it ,conditional))
     (if it
	 ,then
	 ,else)))
