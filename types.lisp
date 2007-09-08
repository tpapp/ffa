(in-package :ffa)

;; !!!! this mapping works with 32-bit SBCL, need to check for other
;; !!!! implementations
(defparameter *cffi-and-lisp-types*
  '((:int8 . (signed-byte 8))
    (:uint8 . (unsigned-byte 8))
    (:int16 . (signed-byte 16))
    (:uint16 . (unsigned-byte 16))
    (:int32 . (signed-byte 32))
    (:uint32 . (unsigned-byte 32))
    (:float . single-float)
    (:double . double-float)))

(defun match-array-element-type (cffi-element-type)
  "Return the Lisp array element-type matching cffi-element-type, nil
of not found."
  (cdr (assoc cffi-element-type *cffi-and-lisp-types*)))
