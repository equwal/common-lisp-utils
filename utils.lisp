;; Export read-token read-int
(defpackage :utils
  (:use :cl :cl-user)
  (:export :make-read
	   :once-only
	   :with-gensyms
	   :y
	   :aif
	   :it ;; for anaphoric macros
	   :f ;; for y-combinator macro
	   :abbrev
	   :abbrevs
	   :mapatoms
	   :group
	   :memoize
	   :defmemo
	   :prognil))
(in-package :utils)
;; Compose would work with with a reader macro for point-free notation.
;;; Memoization:
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig
;;; modified.
(defmacro defmemo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (make-hash-table :test test)))
    (setf (get name 'memo) table)
    #'(lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))
(defun compose (&rest fns)
  (let ((fns (butlast fns))
	(fn1 (car (last fns))))
    (if fns
	(lambda (&rest args)
	  (reduce #'funcall fns :from-end t
		  :initial-value (apply fn1 args)))
	#'identity)))

(defmacro y (lambda-list-args specific-args &rest code)
	 `(labels ((f ,lambda-list-args
		     . ,code))
	    (f ,@specific-args)))
#|Essentially:
(y (a b) (10 0)
     (if (= 0 a) b (f (- a 1) (progn (print b) (+ b 1 )))))
;; Named aster the y-combinator, though really it is just a wrapper over labels.
|#
(defun mapatoms (function tree)
  "Perform operations on the atoms of a tree."
  (y (tree) (tree)
     (cond ((null tree) nil)
 	   ((integerp tree) (funcall function tree))
 	   (t (cons (f (car tree))
 		    (f (cdr tree)))))))
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
		   (f
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
(defmacro prognil (&rest forms)
  "Tired for writing (progn terminal-crashing-return nil)?"
  `(progn ,@forms nil))
;; Reader macro:
;; Convert something like #Fa.b.c to #'(lambda (x) (a (b (c x))))

;; Want to calculate fibbinacci really fast?
(defun fib (n) 
  (if (<= n 2) 1
      (y (m p1 p2) (3 1 1)
	 (if (= n m) (+ p1 p2)
	     (f (1+ m) (+ p1 p2) p1)))))
;; Want to calculate fibbinacci at the same speed as the previous one but
;; memoize each one? Added bonus of being able to write it more idiomatically.
;; Takes only about twice as long as the other one, which makes sense since
;; there are now two major operations (addition and hashing) per call.
(defmemo fibsave (n) 
  (if (>= 0 n) 1
      (+ (fib (- n 1)) (fib (- n 2)))))
(prognil (time (fib     100000))); => .35 seconds

(prognil (time (fibsave 100000))); => .7 seconds
(prognil (time (tibsave 100000))); => 0 seconds
