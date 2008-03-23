(require :cl-numlib)

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


;; elementwise operations


(defparameter *a* (cl-numlib:num-sequence :from 0 :to 8 :by 1))
(defparameter *b* (cl-numlib:num-sequence :from 8 :to 0 :by -1))
(defparameter *a-2d* (displace-array *a* '(3 3) 0))
(defparameter *b-2d* (displace-array *b* '(3 3) 0))
(array+ *a* *b*)
(array+ *a-2d* *b-2d*)

(copy-array *a-2d*)

(array-map #'(lambda (x) (coerce (* 2 x) 'single-float)) *a-2d* 'single-float)

;; array-map-list, array-map-values
(array-map-list #'(lambda (x) (multiple-value-list (round x))) #(1.4 1.7) 2)
(array-map-values #'round #(1.4 1.7) 2)
(defparameter *p* (array-map-values #'round #2A((0.1 0.2) (0.7 0.9)) 2))
