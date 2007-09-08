(in-package :ffa)

(defparameter *a* (make-array 19))
(defparameter *a* (make-array '(4 5)))
(dotimes (i (array-total-size *a*))
  (setf (row-major-aref *a* i) i))
(defparameter *b* (displace-array *a* '(4 4) 2))
(defparameter *c* (displace-array *b* 7 3))
(multiple-value-bind (original index-sum) (find-original-array *c*)
  (values (eq *a* original) index-sum)) ; => T,5
(describe (find-or-displace-to-flat-array *a*))

(= (array-sum *c*) (reduce #'+ *c*)) ; => T

(defparameter *b* (displace *a* '(4 4) 2))
(defparameter *c* (displace *b* 7 3))
