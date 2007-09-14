(in-package :ffa)

(defun ignoring-nil (function)
  "From a bivariate function, create a function that calls the
original if both arguments are non-nil, otherwise returns the argument
which is non-nil, or nil when both are.  Useful for ignoring nil's
when calling reduce."
  #'(lambda (a b)
      (cond 
	((and a b) (funcall function a b))
	(a a)
	(b b)
	(t nil))))

(defun array-reduce (function array &optional key ignore-nil-p)
  "Apply (reduce function ...) to the flattened array.  If
ignore-nil-p is given, it behaves as if nil elements were removed from
the array."
  (bind (((values flat-array start length) (find-or-displace-to-flat-array array))
	 (end (+ start length)))
    (if ignore-nil-p
	(reduce (ignoring-nil function)
		flat-array
		:key key :start start :end end :initial-value nil)
	(reduce function
		flat-array
		:key key :start start :end end))))

(defun array-max (array &optional key ignoring-nil-p)
  "Find the maximum in array."
  (array-reduce #'max array key ignoring-nil-p))

(defun array-min (array &optional key ignoring-nil-p)
  "Find the minimum in array."
  (array-reduce #'min array key ignoring-nil-p))

(defun array-sum (array &optional key ignoring-nil-p)
  "Sum of the elements in array."
  (array-reduce #'+ array key ignoring-nil-p))

(defun array-product (array &optional key ignoring-nil-p)
  "Product of the elements in array."
  (array-reduce #'* array key ignoring-nil-p))

(defun array-count (array predicate)
  "Count elements in array satisfying predicate."
  (array-reduce #'+ array #'(lambda (x) (if (funcall predicate x) 1 0))))

(defun array-range (array &optional key ignoring-nil-p)
  "Minimum and maximum of an array.  !!!! Currently sugar for calling
array-min and array-max, should be replaced by an implementation that
traverses array only once."
  (values (array-min array key ignoring-nil-p)
	  (array-max array key ignoring-nil-p)))

(defun outer-product (function x y &optional (element-type (array-element-type x)))
  "Calculate the outer product of vectors x and y.  When not
specified, element-type will be the element-type of x."
  (declare ((vector * *) x y))
  (let* ((x-length (array-dimension x 0))
	 (y-length (array-dimension y 0))
	 (result (make-ffa (list x-length y-length) element-type)))
    (dotimes (i x-length)
      (dotimes (j y-length)
	(setf (aref result i j) (funcall function (aref x i) (aref y j)))))
    result))

(defun array-elementwise-operation (operator a b element-type)
  "Apply a bivariate operator on two arrays of the same dimension
elementwise, returning the resulting array, which has the given
element-type."
  (let ((dimensions (array-dimensions a)))
    (assert (equal dimensions (array-dimensions b)))
    (bind (((values a-flat a-index-offset length)
	    (find-or-displace-to-flat-array a))
	   ((values b-flat b-index-offset)
	    (find-or-displace-to-flat-array b))
	   (result (make-ffa dimensions element-type))
	   (result-flat (find-original-array result)))
      (iter
	(for index :from 0 :below length)
	(for a-index :from a-index-offset)
	(for b-index :from b-index-offset)
	(setf (aref result-flat index)
	      (funcall operator
		       (aref a-flat a-index)
		       (aref b-flat b-index))))
      result)))

(defun array+ (a b &optional (element-type :double))
  (array-elementwise-operation #'+ a b element-type))
(defun array- (a b &optional (element-type :double))
  (array-elementwise-operation #'- a b element-type))
(defun array* (a b &optional (element-type :double))
  (array-elementwise-operation #'* a b element-type))
(defun array/ (a b &optional (element-type :double))
  (array-elementwise-operation #'/ a b element-type))



