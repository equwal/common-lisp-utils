;; Export read-token read-int
(defpackage :utils
  (:use :cl :cl-user)
  (:export :make-read
	   :once-only
	   :with-gensyms
	   :y
	   :aif
	   :it ;; A feature of some anaphoric macros
	   :f ;; A feature of the y combinator anaphoric macro.
	   ))
(in-package :utils)
(defun y-comb (f)
  "The Y-combinator from the Lambda calculus of Alonzo Church."
  ((lambda (x) (funcall x x))
   (lambda (y)
     (funcall f (lambda (&rest args)
		  (apply (funcall y y) args))))))
;;; Example code for the y-combinator is in order:
#|(funcall (y-comb #'(lambda (FUNCTION) (lambda (n) (if (= 0 n)
						      (progn (print 0) 0)			
						      (progn (print n) (funcall FUNCTION (- n 1))))))) 10)|#
;;; Yes it is a bit complex and redundant, so there is an anaphoric macro.
(defmacro y (lambda-list-args specific-args &rest code)
  "Like Y-combinator, but is a recursive macro. The anaphor is 'f'. Don't forget
to funcall f instead of trying to use it like an interned function."
  ;; Funcall is the price we pay for being a Lisp-2. No way around this without language switch.
  `(funcall (y-comb #'(lambda (f) #'(lambda ,lambda-list-args
				      ,@code)))
	    ,@specific-args))
;;; Example code for the anaphoric y-combinator:
#|(y (a b) (10 0) 
(if (= 0 a) 				; ; ;
b					; ; ;
(funcall f (- a 1) (progn (print b) (+ b 1)))))|#

(defmacro with-gensyms (symbols &body body)
  "Create gensyms for those symbols."
  `(let (,@(mapcar #'(lambda (sym)
		       `(,sym ',(gensym))) symbols))
     ,@body))
(defun number-charp (char)
  "Aux function."
  (not (null (member char (list #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) :test #'char= ))))
(defun whitespace-charp (char)
  "Aux function."
  (not (null (member char (list #\Newline #\Space #\Tab) :test #'char=))))
(defmacro make-read (name charp endp)
  "The ending character gets eaten, so it is put into VALUES."
  (with-gensyms (char str stream)
    `(defun ,name (&optional (,stream *standard-input*))
       (y (,str) ("")
	  (let ((,char (read-char ,stream)))
	    (cond ((funcall ,endp ,char) (values ,str ,char))
		  ((funcall ,charp ,char)
		   (funcall f
			    (concatenate 'string
					 ,str
					 (make-string 1 :initial-element ,char))))
		  (t (values ,str ,char))))))))
(make-read read-token #'(lambda (x) (declare (ignore x)) t) #'whitespace-charp)
(make-read read-int #'number-charp #'whitespace-charp)
(defmacro once-only ((&rest names) &body body)
  "A macro-writing utility for evaluating code only once."
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))
(defmacro aif (conditional then &optional else)
  `(let ((it ,conditional))
     (if it
	 ,then
	 ,else)))
(defun group (list size)
  (labels ((recur (old-list new-list size-count this-node)
	     (cond ((null old-list) (reverse new-list))
		   ((= 1 size-count) (recur (cdr old-list)
					    (cons (reverse (cons (car old-list)
								 this-node))
						  new-list)
					    size
					    nil))
		   (t (recur (cdr old-list)
			     new-list
			     (1- size-count)
			     (cons (car old-list) this-node))))))
    (recur list nil size nil)))
(defmacro abbrev (short long)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))
(defmacro abbrevs (&rest names)
  `(progn
     ,@(mapcar #'(lambda (pair)
		   `(abbrev ,@pair))
	       (group names 2))))
