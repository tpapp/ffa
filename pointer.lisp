(in-package :ffa)


(defun copy-array-to-pointer (array pointer cffi-type lisp-type
			      index-offset length)
  "Copy length elements from array (starting at index-offset) of type
lisp-type to the memory area that starts at pointer, coercing the
elements if necessary."
  (let ((matching-lisp-type (match-array-element-type cffi-type)))
    (unless matching-lisp-type
      (error "don't know how what array element type matches ~a" cffi-type))
    (if (equal matching-lisp-type lisp-type)
	(iter				; no coercion
	  (for pointer-index :from 0 :below length)
	  (for array-index :from index-offset)
	  (setf (cffi:mem-aref pointer cffi-type pointer-index)
		(row-major-aref array array-index)))
	(iter				; coercion
	  (for pointer-index :from 0 :below length)
	  (for array-index :from index-offset)
	  (setf (cffi:mem-aref pointer cffi-type pointer-index)
		(coerce (row-major-aref array array-index) matching-lisp-type))))))

(defun copy-array-from-pointer (array pointer cffi-type lisp-type
			      index-offset length)
  "Copy length elements from array (starting at index-offset) of type
lisp-type from the memory area that starts at pointer, coercing the
elements if necessary."
  (if (eq (match-array-element-type cffi-type) lisp-type)
      (iter				; no coercion
	(for pointer-index :from 0 :below length)
	(for array-index :from index-offset)
	(setf (row-major-aref array array-index)
	      (cffi:mem-aref pointer cffi-type pointer-index)))
      (iter				; coercion
	(for pointer-index :from 0 :below length)
	(for array-index :from index-offset)
	(setf (row-major-aref array array-index)
	      (coerce (cffi:mem-aref pointer cffi-type pointer-index)
		      lisp-type)))))

#+sbcl
(defmacro pin-to-pointer ((array pointer cffi-type length index-offset)
			  &body body)
  "Use SBCL's sb-sys:with-pinned-objects and sb-sys:vector-sap for
mapping an array to a memory location.  NOTE: checking that cffi-type
matches the type of the array is the responsibility of the user of
this macro.  The size of the array is checked.  The array is required
to have rank one."
  (once-only (array)
  `(sb-sys:with-pinned-objects (,array)
     (assert (<= (+ ,index-offset ,length) (length ,array)))
     (let ((,pointer (cffi:inc-pointer (sb-sys:vector-sap ,array)
				       (* ,index-offset 
					  (cffi:foreign-type-size 
					   ,cffi-type)))))
       ,@body))))

(defmacro copy-to-pointer ((array pointer cffi-type length index-offset direction)
			   &body body)
  "Allocate memory area and establish desired mapping between array
and pointer (copy in and/or out as needed).  Array will be available
at pointer, which is a local binding so you do whatever you want with
it (change its value etc)."
  (with-unique-names (hidden-pointer lisp-type)
    (once-only (array cffi-type length index-offset direction)
      `(let ((,lisp-type (array-element-type ,array)))
	 (cffi:with-foreign-object (,hidden-pointer ,cffi-type ,length)
	   (when (or (eq ,direction :copy-in) (eq ,direction :copy-in-out))
	     (copy-array-to-pointer ,array ,hidden-pointer ,cffi-type
				    ,lisp-type ,index-offset ,length))
	   (multiple-value-prog1 
	       (let ((,pointer ,hidden-pointer))
		 ,@body)
	     (when (or (eq ,direction :copy-in-out) (eq ,direction :copy-out))
	       (copy-array-from-pointer ,array ,hidden-pointer ,cffi-type
					,lisp-type ,index-offset ,length))))))))

(defun valid-direction-p (direction)
  "Test if the given direction is valid."
  (or (eq direction :copy-in) (eq direction :copy-out)
      (eq direction :copy-in-out)))

#+sbcl
(defmacro with-pointer-to-array ((array pointer cffi-type length direction)
				 &body body)
  "See the documentation."
  (assert (symbolp pointer))
  (once-only (array cffi-type direction)
    (with-unique-names (original-array index-offset lisp-type)
      `(bind (((values ,original-array ,index-offset) 
	       (find-original-array ,array))
	      (,lisp-type (array-element-type ,original-array)))
	 (assert (valid-direction-p ,direction))
	 (cond
	   ((and (typep ,original-array '(simple-array * (*)))
		 ,lisp-type 		; no nil arrays
		 (eq ,lisp-type (match-array-element-type ,cffi-type)))
	    (pin-to-pointer (,original-array ,pointer ,cffi-type
			     ,length ,index-offset)
	      ,@body))
	   (t
	    (warn "efficiency warning: array is copied and/or coerced")
	    (copy-to-pointer (,original-array ,pointer ,cffi-type
			      ,length ,index-offset ,direction)
	       ,@body)))))))


#-sbcl
(defmacro with-pointer-to-array ((array pointer cffi-type length direction)
				 &body body)
  "See the documentation."
  (assert (symbolp pointer))
  (once-only (array cffi-type)
    (with-unique-names (original-array index-offset)
      `(bind (((values ,original-array ,index-offset) 
	       (find-original-array ,array)))
	 (assert (valid-direction-p ,direction))
	 (copy-to-pointer (,original-array ,pointer ,cffi-type
			   ,length ,index-offset ,direction)
	       ,@body)))))
