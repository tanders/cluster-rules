;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

#|
;; ASDF interface for running all tests
(asdf:test-system :cluster-rules)

(asdf:load-system :cluster-rules/tests)

(run! 'cluster-rules-tests)
|#

#|
;; Generating an html test coverage output and collecting profiling info.
;; See test file in cluster-engine tests
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

;; Interactive debugging 
(setf fiveam:*on-error* :DEBUG)
;; (setf *on-error* :BACKTRACE)
;; (setf *on-error* NIL)
(setf fiveam:*on-failure* :debug)

;; TMP: reduce number of trials for speeding up during test developments
(setf fiveam:*num-trials* 10)
;; (setf *num-trials* 100)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Profile rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite cluster-rules-tests
    :description "The top-level suite of all Cluster Rules tests.")

(def-suite profile-rules-tests
    :description "Testing profile rules."
    :in cluster-rules-tests)

(in-suite profile-rules-tests)

;; TODO: follow-profile-hr with intervals and absolute pitches -- this seems to largely work, but sometimes there are surprising deviations

;; TODO: repetitions-follow-profile ; in melody-rules


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Harmony rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite harmony-rules-tests
    :description "Testing harmony rules etc."
    :in cluster-rules-tests)

(in-suite harmony-rules-tests)

;; TODO: min/max-harmonic-interval (possibly multiple test cases)

;; TODO: only-scale-pcs

;; TODO: only-chord-pcs

;; TODO: number-of-sim-PCs

;; TODO: number-of-sim-pitches

;; TODO: stepwise-non-chord-tone-resolution

;; TODO: chord-tone-before/after-rest

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Conterpoint rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite conterpoint-rules-tests
    :description "Testing counterpoint rules etc."
    :in cluster-rules-tests)

(in-suite conterpoint-rules-tests)

;; TODO: no-voice-crossing



#|
;; TMP:
(test dummy-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))

|#
