;;;; My utilities and toys.
(defpackage :utils
  (:use :cl :cl-user :lol)
  (:export :once-only
	   :whitespacep
	   :with-gensyms
	   :y
	   :pop-off
	   :make-stack
	   :aif
	   :use
	   :awhen
	   :alist
	   :aand
	   :aor
	   :a+
	   :asetf
	   :it ;; for anaphoric macros
	   :f ;; for y-combinator macro
	   :abbrev
	   :abbrevs
	   :mapatoms
	   :nthcar
	   :y-trace
	   :memoize
	   :empty
	   :defmemo
	   :clear-memoize
	   :basic-profile
	   :prognil
	   :definline
	   :dolines
	   :push-on
	   :mvbind
	   :dbind
	   :last1
	   :single
	   :append1
	   :conc1
	   :pack
	   :longer
	   :filter
	   :flatten
	   :select
	   :before
	   :after
	   :duplicate
	   :split-if
	   :most
	   :best
	   :mostn
	   :map0-n
	   :map1-n
	   :mapa-b
	   :map->
	   :mapflat
	   :mappend
	   :mapcars
	   :rmapcar
	   :fand
	   :for
	   :lrec
	   :rfind-if
	   :ttrav
	   :trec
	   :>casex
	   :shuffle
	   :compose
	   :comp
	   :defmacro/g!
	   :defmacro!
	   :nlet
	   :dlambda
	   :in
	   :inq
	   :in-if
	   :>case
	   :while
	   :till
	   :allf
	   :nilf
	   :tf
	   :toggle
	   :defanaph
	   :make-reader))
(in-package :utils)
(defmacro with-gensyms (symbols &body body)
  "Create gensyms for those symbols."
  `(let (,@(mapcar #'(lambda (sym)
		       `(,sym ',(gensym))) symbols))
     ,@body))
;; Compose would work with with a reader macro for point-free notation.
;;; Memoization:
;;; Code from Paradigms of AI Programming 
;;; Copyright (c) 1991 Peter Norvig
;;; modified.

;;; Make CPS functions automagically
;;; Ex: (cps apply& apply (fn list)); => apply&
;;; Some might call this compile-time  intern uncool. Those people are lame.
(defmacro make-stack (&rest array-options)
  `(make-array 0 :adjustable t :fill-pointer 0 ,@array-options))
(defun use (package)
  (progn (asdf:load-system package) (use-package package)))
;; From OnLisp
 (defun comp (pred)
   (compose #'not pred))
(proclaim '(inline last1 single append1 conc1 empty))
(proclaim '(optimize speed))

(defun empty (array)
  (declare (type array array))
  (zerop (length array)))
(defun last1 (lst)
  (car (last lst)))
(defun single (lst)
  (and (consp lst) (not (cdr lst))))
(defun append1 (lst obj) 
  (append lst (list obj)))
(defun conc1 (lst obj)   
  (nconc lst (list obj)))
(defun pack (obj)
  (if (listp obj) obj (list obj)))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x) 
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))
(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
(defun select (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (select fn (cdr lst))))))
(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test)) 
          :test test))
(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max  score))))
        (values wins max))))
(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))
(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max)
                   (setq max    score
                         result (list obj)))
                  ((= score max)
                   (push obj result)))))
        (values (nreverse result) max))))
(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun mapflat (fn &rest lists)
  (apply #'append (apply #'maplist fn lists)))
(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar 
             #'(lambda (&rest args) 
                 (apply #'rmapcar fn args))
             args)))
(defun fand (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'for fns)))
        #'(lambda (x) 
            (and (funcall fn x) (funcall chain x))))))

(defun for (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'for fns)))
        #'(lambda (x)
            (or (funcall fn x) (funcall chain x))))))
(defun lrec (rec &optional base)
  (labels ((self (lst)
             (if (null lst)
                 (if (functionp base)
                     (funcall base)
                     base)
                 (funcall rec (car lst)
                              #'(lambda () 
                                  (self (cdr lst)))))))
    #'self))
(defun rfind-if (fn tree)
  (if (atom tree)
      (and (funcall fn tree) tree)
      (or (rfind-if fn (car tree))
          (if (cdr tree) (rfind-if fn (cdr tree))))))
(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
             (if (atom tree)
                 (if (functionp base)
                     (funcall base tree)
                     base)
                 (funcall rec (self (car tree))
                              (if (cdr tree) 
                                  (self (cdr tree)))))))
    #'self))
(defun trec (rec &optional (base #'identity))
  (labels 
    ((self (tree)
       (if (atom tree)
           (if (functionp base)
               (funcall base tree)
               base)
 (funcall rec tree 
                        #'(lambda () 
                            (self (car tree)))
                        #'(lambda () 
                            (if (cdr tree)
                                (self (cdr tree))))))))
    #'self))
(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #'(lambda (a)
                          `',a)
                      args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
       (or ,@(mapcar #'(lambda (c)
                         `(funcall ,fnsym ,c))
                     choices)))))

(defmacro >case (expr &rest clauses)
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ,@(mapcar #'(lambda (cl) (>casex g cl))
                       clauses)))))

(defun >casex (g cl)
  (let ((key (car cl)) (rest (cdr cl)))
    (cond ((consp key) `((in ,g ,@key) ,@rest))
          ((inq key t otherwise) `(t ,@rest))
          (t (error "bad >case clause")))))
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro till (test &body body)
  `(do ()
       (,test)
     ,@body))
(defun shuffle (x y)
  (cond ((null x) y)
        ((null y) x)
        (t (list* (car x) (car y)
                  (shuffle (cdr x) (cdr y))))))

;; p. 162
(defmacro allf (val &rest args)
  (with-gensyms (gval)
		`(let ((,gval ,val))
		   (setf ,@(mapcan #'(lambda (a) (list a gval))
				    args)))))
(defmacro nilf (&rest args) `(allf nil ,@args))
(defmacro tf (&rest args) `(allf t ,@args))
(define-modify-macro toggle () not)
(defmacro defanaphs (&rest pairs)
  ``(,,@(mapcar #'(lambda (p) `(defanaph ,@(if (consp p) p (list p)))) pairs)))
(defmacro defanaph (name &optional &key calls (rule :all))
  (let* ((opname (or calls (intern (subseq (symbol-name name) 1))))
	 (body (case rule
		 (:all `(anaphex1 args '(,opname)))
		 (:first `(anaphex2 ',opname args))
		 (:place `(anaphex3 ',opname args)))))
    `(defmacro ,name (&rest args)
       ,body)))
(defun anaphex1 (args call)
  (if args
      (let ((sym (gensym)))
	`(let* ((,sym ,(car args))
		(it ,sym))
	   ,(anaphex1 (cdr args)
		      (append call (list sym)))))
    call))
(defun anaphex2 (op args)
  `(let ((it ,(car args))) (,op it ,@(cdr args))))
(defun anaphex3 (op args)
  `(_f (lambda (it) (,op it ,@(cdr args))) ,(car args)))
(defanaphs a+ alist aand aor
	   (aif :calls if :rule :first)
	   (awhen :rule :first)
	   (asetf :rule :place))
(defmacro _f (op place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    `(let* (,@(mapcar #'list vars forms)
	       (,(car var) (,op ,access ,@args)))
       ,set)))
(defmacro pull (obj place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,obj)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete ,g ,access ,@args)))
	 ,set))))

(defmacro pull-if (test place &rest args)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (let ((g (gensym)))
      `(let* ((,g ,test)
	      ,@(mapcar #'list vars forms)
	      (,(car var) (delete-if ,g ,access ,@args)))
	 ,set))))

(defmacro popn (n place)
  (multiple-value-bind (vars forms var set access)
      (get-setf-expansion place)
    (with-gensyms (gn glst)
		  `(let* ((,gn ,n)
			  ,@(mapcar #'list vars forms)
			  (,glst ,access)
			  (,(car var) (nthcdr ,gn ,glst)))
		     (prog1 (subseq ,glst 0 ,gn)
		       ,set)))))
(defmacro make-reader (name (stream-var dispatch-1 dispatch-2) &body body)
  (with-gensyms (sub-char numarg)
    `(progn (defun ,name (,stream-var ,sub-char ,numarg)
	(declare (ignore ,sub-char ,numarg))
	,@body)
	    (set-dispatch-macro-character ,dispatch-1 ,dispatch-2 #',name))))
(make-reader my-string (stream #\# #\>)
  "Select a delimiter perl-style. #><TEST< -> 'TEST'"
  (do* ((prev #1=(read-char stream nil nil) curr)
	(first prev)
	(curr #1# #1#)
	(str (make-stack :element-type 'character)
	     (push-on prev str)))
       ((char= curr first) str)))
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
(defun whitespacep (char)
  "Aux function."
  (not (null (member char (list #\Newline #\Space #\Tab) :test #'char=))))
(defmacro dolines ((var stream) &body body)
  `(do ((,var #1=(read-line ,stream nil nil) #1#))
       ((null ,var))
     ,@body))
(defmacro doread((var stream) &body body)
  "Read character by character with the speed of line by line reading."
  (with-gensyms (line count len)
    `(dolines (,line ,stream)
       (do* ((,line (concatenate 'string ,line (format nil "~%~%")))
	     (,len (length ,line))
	     (,count 0 (1+ ,count))
	     (,var #2=(elt ,line ,count) #2#))
	    ((= (1- ,len) ,count))
	 ,@body))))
(defmacro once-only ((&rest names) &body body)
  "A macro-writing utility for evaluating code only once."
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
       `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
          ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
             ,@body)))))
(defun nthcar (n list)
  (if (or (null list) (<= n 0)) nil
      (cons (car list) (nthcar (1- n) (cdr list)))))
(defmacro y-trace (lambda-list-args specific-args &body code)
  "Short labels with some tracing. Same as y."
  (with-gensyms (count)
    `(progn (let ((,count 0))
	      (y ,lambda-list-args ,specific-args
		 (format *trace-output*  "~v~(f ~{ ~S~})~%" (incf ,count) (list ,@lambda-list-args))
		 (format *trace-output*  "~v~=> ~S~%" ,count  (progn ,@code)))))))
(defmacro abbrevs (&rest items)
  `(progn ,@(mapcar #'(lambda (p) (cons 'abbrev p)) (group items 2))))
(defmacro abbrev (long short)
  `(defmacro ,short (&rest args)
     `(,',long ,@args)))
(abbrevs multiple-value-bind mvbind
	 destructuring-bind dbind
	 vector-pop pop-off
	 set-dispatch-macro-character set-dispatch
	 get-dispatch-macro-character get-dispatch
	 get-macro-character get-char
	 set-macro-character set-char
	 symbol-macrolet sym-macrolet
	 define-setf-expander defexpand)
(defun push-on (elt stack)
  (vector-push-extend elt stack) stack)
(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char))
  (unless numarg (setq numarg 1))
  `(lambda ,(loop for i from 1 to numarg
                  collect (symb 'a i))
     ,(funcall
        (get-macro-character #\`) stream nil)))

(set-dispatch-macro-character
  #\# #\` #'|#`-reader|)
(defmacro prognil (&rest forms)
  "Tired for writing (progn terminal-crashing-return-val nil)?"
  `(progn ,@forms nil))
;;; Reader macro:
;;; Convert something like #Fa.b.c to #'(lambda (x) (a (b (c x))))

;;; For playing with profiling libs. Of paramount importance are:
;;;    (sb-sprof:with-profiling () code)
;;;    (utils:basic-profile () code)
;;; Want to calculate fibbinacci really fast?
(defun fib-fast (n) 
  (if (<= n 2) 1
      (y (m p1 p2) (3 1 1)
	 (if (= n m) (+ p1 p2)
	     (f (1+ m) (+ p1 p2) p1)))))
;;; Want to calculate fibbinacci really slow?
(defun fib-slow (n)
  (if (<= n 2) 1
      (+ (fib-slow (1- n)) (fib-slow (- n 2)))))
;;; Want to calculate fibbinacci at the same speed as the previous one but
;;; memoize each one? Added bonus of being able to write it more idiomatically.
;;; Takes only about twice as long as the other one, which makes sense since
;;; there are now two major operations (addition and hashing) per call.
(defmemo fib-save (n) 
  (if (>= 0 n) 1
      (+ (fib-save (- n 1)) (fib-save (- n 2)))))
#|(prognil (time (fib     100000))); => .35 seconds

(prognil (time (fibsave 100000))); => .7 seconds
(prognil (time (fibsave 100000)))|#; => 0 seconds
#+sbcl (require :sb-sprof)
#+sbcl (sb-profile:profile fib-save fib-slow fib-fast)
;#+sbcl 
#|(defmacro defwith (name bindings before after)
  `(defmacro ,name (,bindings . (&body body))
     ,before
     ,@body
     ,after))|#
#+sbcl (defmacro basic-profile ((&rest functions)
				&body body)
	 `(progn (sb-profile:reset)
		 (sb-profile:unprofile)
		 (sb-profile:profile ,@functions)
		 ,@body
		 (sb-profile:report)
		 (sb-profile:unprofile ,@functions)
		 (sb-profile:reset)))
;;; A common idiom is (declaim (inline function)) (defun function ...) so we
;;; so we impelement that here.
(defmacro definline (name lambda-list &body body)
  `(declaim (inline ,name))
  `(defun ,name ,lambda-list
     ,@body))
