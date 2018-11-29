#+sbcl
(defun backquote-kludge (str &optional (acc (make-array 0 :fill-pointer 0 :adjustable t)) in-string in-escape in-dispatch)
  (if (= 0 (length str))
      acc
      (let ((ch (elt str 0))
	    (newstr (subseq str 1)))
	(if in-escape
	    (backquote-kludge newstr (push-on ch acc) in-string nil nil)
	    (if in-dispatch
		(with-input-from-string (s newstr) (funcall (get-dispatch-macro-character #\# ch) s nil nil))
		(case ch
		  (#\` (backquote-kludge newstr (if in-string (push-on ch acc) acc) in-string nil nil))
		  (#\" (backquote-kludge newstr (push-on ch acc) (not in-string) nil))
		  (#\\ (backquote-kludge newstr (push-on ch acc) t t))
		  (#\# (if in-string
			   (backquote-kludge newstr (push-on ch acc) t nil nil)
			   (backquote-kludge newstr (push-on ch acc) nil nil t)))
		  (t (backquote-kludge newstr (push-on ch acc) in-string nil nil))))))))
#+sbcl
(defun read-atoms (str)
  (with-macro-fn #\, nil
    (mapcar #'(lambda (sym) (let ((sym (with-output-to-string (s) (princ sym s))))
			      (if (char= #\, (elt sym 0))
				  (intern (subseq sym 1)))))
	    (flatten (read-from-string (prepare (backquote-kludge str)) nil nil)))))
