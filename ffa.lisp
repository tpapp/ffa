(in-package :ffa)

(defun make-ffa (dimensions element-type &key
		 (initial-element 0 initial-element-p)
		 (initial-contents nil initial-contents-p))
  "Make an array that is either one-dimensional or displaced to a
one-dimensional array.  element-type can be a cffi type, see
*cffi-and-lisp-types*.  Array is filled with initial-element or
initial-contents, coerced to the given type."
  (assert (or (atom dimensions) (and (listp dimensions) (car dimensions))))
  (let* ((element-type (or (match-array-element-type element-type) element-type))
	 (dimensions (if (atom dimensions) (list dimensions) dimensions))
	 (length (reduce #'* dimensions))
	 (array (cond
		  ((and initial-element-p initial-contents-p)
		   (error "you can't supply both initial-element and ~
                           initial-contents"))
		  ;; initial element given
		  (initial-element-p
		   (make-array length :element-type element-type
			       :initial-element (coerce initial-element
							element-type)))
		  ;; contents given, copy or coerce
		  (initial-contents-p
		   (assert (= (length initial-contents) length))
		   (if (typep initial-contents (list 'vector element-type))
		       (copy-seq initial-contents)
		       (map (list 'vector element-type)
			    (lambda (x) (coerce x element-type)) initial-contents)))
		  ;; neither
		  (t (make-array length :element-type element-type)))))
    (if (cdr dimensions)
	(make-array dimensions :element-type element-type 
		    :displaced-to array)
	array)))

(defun find-original-array (array)
  "Find the original parent of a displaced array, return this and the
sum of displaced index offsets."
  (let ((sum-of-offsets 0))
    (tagbody
     check-displacement
       (multiple-value-bind (displaced-to displaced-index-offset)
	   (array-displacement array)
	 (when displaced-to
	   (setf array displaced-to)
	   (incf sum-of-offsets displaced-index-offset)
	   (go check-displacement))))
    (values array sum-of-offsets)))

(defun displace-array (array dimensions index-offset)
  "Make a displaced array from array with the given dimensions and the
index-offset and the same element-type as array.  Tries to displace
from the original array."
  (multiple-value-bind (original-array sum-of-offsets)
      (find-original-array array)
    (make-array dimensions 
		:element-type (array-element-type array)
		:displaced-to original-array
		:displaced-index-offset (+ sum-of-offsets index-offset))))

(defun flatten-array (array)
  "Return a flat (ie rank 1) displaced version of the array."
  (displace-array array (array-total-size array) 0))
  
(defun find-or-displace-to-flat-array (array)
  "Find a flat array that array is displaced to, or create one that is
displaced to the original array.  Also return the index-offset and
length (total size).  Useful for passing to reduce etc."
  (bind ((total-size (array-total-size array))
	 ((values original-array index-offset) (find-original-array array)))
    (if (= (array-rank original-array) 1)
	(values original-array index-offset total-size)
	(values (displace-array original-array total-size index-offset)
		0 total-size))))

(defun array-copy (array)
  "Copy the elements of array.  Does not copy the elements itself
recursively, if you need that, use array-map."
  (make-ffa (array-dimensions array)
	    (array-element-type array)
	    :initial-contents (find-or-displace-to-flat-array array)))

(defun array-map (function array 
		  &optional (element-type (array-element-type array)))
  "Map an array into another one elementwise using function.  The
resulting array has the given element-type."
  (bind ((result (make-ffa (array-dimensions array) element-type))
	 (result-flat (find-original-array result))
	 ((values array-flat index-offset length)
	  (find-or-displace-to-flat-array array)))
    (iter
      (for result-index :from 0 :below length)
      (for array-index :from index-offset)
      (setf (aref result-flat result-index)
	    (funcall function (aref array-flat array-index))))
    result))

(defun array-map! (function array)
  "Replace each element 'elt' of an array with (funcall function elt),
and return the modified array."
  (dotimes (i (length array) array)
    (setf (aref array i) (funcall function (aref array i)))))

(defun array-convert (element-type array)
  "Convert array to desired element type.  Always makes a copy, even
if no conversion is required."
  (let ((element-type (or element-type (match-array-element-type element-type))))
    (if (equal (array-element-type array) element-type)
	(array-copy array)
	(array-map #'(lambda (x) (coerce x element-type)) array element-type))))
