;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

#|
;; ASDF interface for running all tests
(asdf:test-system :cluster-rules)

(progn
  (asdf:load-system :cluster-rules/tests)
  (run! 'rhythmic-domain))


(asdf:load-system :cluster-engine/tests)
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

(def-suite cluster-rules-tests
    :description "The top-level suite of all Cluster Rules tests.")
(in-suite cluster-rules-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  TMP:
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; TODO: This is not working: only simple cases are generated with such conditions. I better create a large list of possible values explicitly and then use gen-selection on that.
;; ;; TODO: turn rhythmic-domain generation into a higher-level generator, and the guard into some abstraction even-motif?
;; (test rhythmic-domain
;;   (for-all ((rhythmic-domain (gen-list :length (gen-integer :min 1 :max 5) 
;; 				       :elements (gen-list :length (gen-integer :min 1 :max 5)
;; 							   :elements (gen-ratio)))
;; 			     ;; Restrict to rhythmic motifs of even duration
;; 			     (every (lambda (motif)
;; 				      (let ((motif-dur (apply #'+ (mapcar #'abs motif))))
;; 					(member motif-dur '(1 1/2 1/4 1/8 1/16))
;; 					;; (and (member (numerator motif-dur) '(1 2 4))
;; 					;;      (member (denominator motif-dur) '(1 2 4))
;; 					;;      (<= motif-dur 1))
;; 					))
;; 				    rhythmic-domain)))
;;     (format T "Rhythmic domain: ~A~%" rhythmic-domain)
;;     (is (eql T T))
;;     ))

 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Profile rules
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


(test number-of-sim-PCs
  "Testing number-of-sim-PCs for three voices."
  (test-harmonic-constraint 
      (cr:number-of-sim-PCs :voices '(0 1 2) :pc-number 3 :rests-mode :reduce-no)
    (lambda (pitches)
      (let ((rest-no (length (remove-if #'identity pitches))) ;; filter out any non-NILs
	    (pc-no (length (remove-duplicates (tu:pitch->pc (remove NIL pitches))))))
	(= pc-no
	   (- (length pitches) rest-no))))
    :voice-number 3
    :pitch-domain (gen-selection :length (gen-integer :min 5 :max (length *pitch-domain-template*))
				 :elements *pitch-domain-template*)))


(test number-of-sim-pitches
  "Testing number-of-sim-pitches for three voices."
  (test-harmonic-constraint 
      (cr:number-of-sim-pitches :voices '(0 1 2) :pitch-number 3 :rests-mode :reduce-no :condition :min)
    (lambda (pitches)
      (let ((rest-no (length (remove-if #'identity pitches))) ;; filter out any non-NILs
	    (pitch-no (length (remove-duplicates (remove NIL pitches)))))
	(= pitch-no (- 3 rest-no))))
    :voice-number 3
    :pitch-domain (gen-selection :length (gen-integer :min 5 :max (length *pitch-domain-template*))
				 :elements *pitch-domain-template*)))


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

(test no-voice-crossing
  "Testing no-voice-crossing: no voice crossing between voices 1 and 2."
  (test-harmonic-constraint (no-voice-crossing :voices '(0 1))
    (lambda (pitches)
      (let ((p1 (first pitches))
	    (p2 (second pitches)))
	(when* (and p1 p2)
	  (>= p1 p2))))))

#|
;; TMP: Long form of the above
(test no-voice-crossing
  "Testing no-voice-crossing: no voice crossing between voices 1 and 2."
  (for-all ((no-of-variables (gen-integer :min 1 :max 15))
	    (rhythm-domain (gen-selection :length (gen-integer :min 1 :max (length *rhythm-domain-template*))
					  :elements *rhythm-domain-template*))
	    (pitch-domain (gen-selection :length (gen-integer :min 2 :max (length *pitch-domain-template*))
					 :elements *pitch-domain-template*)))
    (let* ((voices-solution (get-keyword-voices
			     (cluster-shorthand no-of-variables
						(no-voice-crossing :voices '(0 1))
						(list
						 ;; voice 1
						 rhythm-domain pitch-domain
						 ;; voice 2
						 rhythm-domain pitch-domain))))
	   (first-voice (first voices-solution))
	   (first-voice-pitches (mapcar #'get-pitch first-voice))	     
	   (matching-2nd-voice-pitches (mapcar #'get-pitch
					       (get-events-time-points (second voices-solution)
								       (mapcar #'get-start first-voice))))
	   (sim-pitch-pairs (tu:mat-trans (list first-voice-pitches matching-2nd-voice-pitches))))
      (is (every (lambda (pitches)
		   (let ((p1 (first pitches))
			 (p2 (second pitches)))
		     (when* (and p1 p2)
		       (>= p1 p2))))
		 sim-pitch-pairs)))))
|#




#|
;; TMP:
(test dummy-tests
  "Just a placeholder."
  (is (listp (list 1 2)))
  (is (= 5 (+ 2 3))))

|#
