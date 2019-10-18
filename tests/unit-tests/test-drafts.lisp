;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

#|
;; ASDF interface for running all tests
(asdf:test-system :cluster-rules)

(asdf:load-system :cluster-rules/tests)

;; (run :cluster-rules/tests :style :spec)
 |#

(in-package #:cluster-rules/tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Setting up
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (optimize (speed 0) (space 0) (debug 3)))


;; Cluster engine is silent
(setf ce:*verbose-i/o?* nil)


#|
;; NOTE: Not really needed...
(defun full-test ()
  "Run all tests."
  (run! 'cluster-rules-tests))
; (full-test)
|#


;; TODO: Avoid code dublication (these defs also in cluster engine test)
(defun gen-selection (&key (length (gen-integer :min 0 :max 10))
			   elements)
  "Return a generator that picks `length' values from `elements' without repeating them. Must be called less often than length of xs."
  (lambda ()
    (let ((elements-copy (copy-list elements)))
      (loop for i from (funcall length) downto 1
	 for pos = (random (length elements-copy))
	 collect (tu:pop-nth elements-copy pos)))))
#|
(setf my-gen (gen-selection :length (gen-integer :min 1 :max 3) :elements '((-1/2) (-1/4) (-1/8) (1/8) (1/4) (1/2))))
(funcall my-gen)
|#

(defparameter *rhythm-domain-template*
  (mapcar #'list
	  (sort (loop for i from 0 to 4
		   for x = (/ 1 (expt 2 i))
		   append (list (- x) x (* 3/2 x) (* -3/2 x)))
		#'<))
  "A range of standard rhythmic domain values to select from.")

(defparameter *pitch-domain-template*
  (loop for pitch from 36 to 84
     collect (list pitch))
  "A range of standard pitch domain values to select from.")

(defparameter *metric-domain-template*
  (loop for demon in '(2 4 8)
     append (loop for num in '(1 2 3 4 5 6 7 9 12)
	       collect (list num demon)))
  "A range of standard time signatures to select from.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Harmony rules tests
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite cluster-rules-tests
    :description "The top-level suite of all Cluster Rules tests.")

(def-suite harmony-rules-tests
    :description "Testing harmony rules etc."
    :in cluster-rules-tests)

(in-suite harmony-rules-tests)



;; TMP:
(test dummy-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))


