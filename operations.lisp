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

(defun array-reduce (function array &key key ignore-nil-p)
  "Apply (reduce function ...) to the flattened array.  If
ignore-nil-p is given, it behaves as if nil elements were removed from
the array."
  (bind (((:values flat-array start length) (find-or-displace-to-flat-array array))
	 (end (+ start length)))
    (if ignore-nil-p
	(reduce (ignoring-nil function)
		flat-array
		:key key :start start :end end :initial-value nil)
	(reduce function
		flat-array
		:key key :start start :end end))))

(defun array-max (array &key key ignore-nil-p)
  "Find the maximum in array."
  (array-reduce #'max array :key key :ignore-nil-p ignore-nil-p))

(defun array-min (array &key key ignore-nil-p)
  "Find the minimum in array."
  (array-reduce #'min array :key key :ignore-nil-p ignore-nil-p))

(defun array-sum (array &key key ignore-nil-p)
  "Sum of the elements in array."
  (array-reduce #'+ array :key key :ignore-nil-p ignore-nil-p))

(defun array-product (array &key key ignore-nil-p)
  "Product of the elements in array."
  (array-reduce #'* array :key key :ignore-nil-p ignore-nil-p))

(defun array-count (array predicate)
  "Count elements in array satisfying predicate."
  (array-reduce #'+ array #'(lambda (x) (if (funcall predicate x) 1 0))))

(defun array-range (array &key key ignore-nil-p)
  "Minimum and maximum of an array.  !!!! Currently sugar for calling
array-min and array-max, should be replaced by an implementation that
traverses array only once."
  (values (array-min array :key key :ignore-nil-p ignore-nil-p)
	  (array-max array :key key :ignore-nil-p ignore-nil-p)))

(defun array-mean (array &key key ignore-nil-p)
  "Calculate the mean of the elements in array."
  (/ (array-sum array :key key :ignore-nil-p ignore-nil-p)
     (if ignore-nil-p
	 (array-count array #'not)
	 (length array))))

(defun dot-product (x y &optional (function #'*))
  "Calculate the (generalized) dot product of vectors x and y."
  (let* ((n (length x))
	 (sum 0))
    (assert (= (length y) n))
    (dotimes (i n)
      (incf sum (funcall function (aref x i) (aref y i))))
    sum))

(defun outer-product (x y &key (function #'*) (element-type (array-element-type x)))
  "Calculate the (generalized) outer product of vectors x and y.  When
not specified, element-type will be the element-type of x."
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
    (bind (((:values a-flat a-index-offset length)
	    (find-or-displace-to-flat-array a))
	   ((:values b-flat b-index-offset)
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

;; elementwise operations

(defun array+ (a b &optional (element-type :double))
  (array-elementwise-operation #'+ a b element-type))
(defun array- (a b &optional (element-type :double))
  (array-elementwise-operation #'- a b element-type))
(defun array* (a b &optional (element-type :double))
  (array-elementwise-operation #'* a b element-type))
(defun array/ (a b &optional (element-type :double))
  (array-elementwise-operation #'/ a b element-type))

(defmacro define-array-scalar-binary-operation (name operator)
  `(defun ,name (a b &optional (element-type :double))
     (array-map (lambda (x) (,operator x b)) a element-type)))

(define-array-scalar-binary-operation array-scalar+ +)
(define-array-scalar-binary-operation array-scalar- -)
(define-array-scalar-binary-operation array-scalar* *)
(define-array-scalar-binary-operation array-scalar/ /)

(defun array-reciprocal (a &optional b (element-type :double))
  "For each element x of a, map to (/ x) or (/ b x) (if b is given)."
  (if b
      (array-map (lambda (x) (/ b x)) a element-type)
      (array-map (lambda (x) (/ x)) a element-type)))

(defun array-negate (a &optional b (element-type :double))
  "For each element x of a, map to (- x) or (- b x) (if b is given)."
  (if b
      (array-map (lambda (x) (- b x)) a element-type)
      (array-map (lambda (x) (- x)) a element-type)))

(defun array-map-list (function array n
		       &optional (element-type (array-element-type array)))
  "Apply function (which is supposed to return a list of length n) to
each element of array, returning the results in an array which has an
extra last dimension of n."
  (let* ((dimensions (array-dimensions array))
	 (total-size (array-total-size array))
	 (result (make-ffa (append dimensions (list n)) element-type))
	 (result-matrix (displace-array result (list total-size n) 0)))
    (dotimes (i total-size result)
      (let ((value (funcall function (row-major-aref array i))))
	(assert (= (length value) n))
	(iter
	  (for elt :in value)
	  (for j :from 0)
	  (setf (aref result-matrix i j) elt))))))

(defun array-map-values (function array n
			 &optional (element-type (array-element-type array)))
  "Apply function (which is supposed to return n values) to each
element of array, returning the results in an array which has an extra
last dimension of n."
  (flet ((list-function (x)
	   (multiple-value-list (funcall function x))))
    (array-map-list #'list-function array n element-type)))

(defun index-extrema (n key &optional (weak-relation #'<=))
    "Find the extrema (using weak-relation) of a (funcall key i) and
the positions (indexes i) at which it occurs.  Return the extremum and
the list of positions.

Notes: 

1) when (and (funcall weak-relation a b) (funcall weak-relation b a)),
a and b are considered equal.

2) the list of positions is in reversed order, you may use nreverse on
it as nothing shares this structure.

Examples:

 (index-extrema 5 (lambda (x) (abs (- x 2)))) => 2, (4 0)
"
  (let ((maximum nil)
	 (positions nil))
    (dotimes (i n)
      (let ((element (funcall key i)))
	(cond
	  ((null maximum)
	   (setf maximum element
		 positions (list i)))
	  ((funcall weak-relation maximum element)
	   (if (funcall weak-relation element maximum)
	       (setf positions (cons i positions)) ; equality
	       (setf maximum element		 ; strictly greater
		     positions (list i)))))))
      (values maximum positions)))


(defun array-extrema (array &key (key #'identity) (weak-relation #'<=))
  "Find the extrema (using weak-relation) of an array and the
positions at which it occurs.  The positions are flat, one-dimensional
vector indexes in the array (like the index used with
row-major-aref).  Return the extremum and the list of positions.

Notes: 

1) when (and (funcall weak-relation a b) (funcall weak-relation b a)),
a and b are considered equal.

2) the list of positions is in reversed order, you may use nreverse on
it as nothing shares this structure.

Examples:

 (array-extrema #(1 2 2 3 3 2)) => 3, (4 3)

 (array-extrema #2A((2 1) (3 1) (2 3))) => 3, (5 2)
"
  (bind (((:values flat-array index-offset total-size)
	  (find-or-displace-to-flat-array array)))
    (index-extrema total-size 
		    (lambda (i)
		      (funcall key
			       (aref flat-array (+ index-offset i))))
		    weak-relation)))

      
    
