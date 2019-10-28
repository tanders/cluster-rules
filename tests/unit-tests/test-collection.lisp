;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

#|
;; ASDF interface for running all tests
(asdf:test-system :cluster-rules)

(progn
  (asdf:load-system :cluster-rules/tests)
  (run! 'min/max-harmonic-interval_no-pitch-repetition))


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
;; (setf fiveam:*num-trials* 100)
;; (setf fiveam:*num-trials* 1000)

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

;; Bass is voice with highest voice number
(test min/max-harmonic-interval_no-voice-crossing-below-bass_no-solution
  "Testing min/max-harmonic-interval: No voice crossing between bass (highest voice number) and other voices -- CSP resulting in :no-solution."
  (for-all ((stop-time (gen-ratio :numerator (gen-integer :min 1 :max 8)
				  :denominator (gen-select-one :candidates '(1 2 4))))
	    (rhythm-domain (gen-selection
			    :length (gen-integer :min 2 :max 8)
			    :elements '((1/16) (1/8) (3/16) (1/4) (3/8) (1/2) (3/4) (1)))))
    (let* ((no-of-variables 1000)
	   (higher-pitch-domain '((60)))
	   ;; NOTE: Domain intended to fail -- above domain of higher voices
	   (bass-pitch-domain '((62)))
	   (solution (cluster-shorthand no-of-variables
					(ce:rules->cluster
					 (ce:stop-rule-time '(0 1 2) stop-time :and)
					 ;; Actual rule
					 (cr:min/max-harmonic-interval :min-interval 0 :voices '(0 1 2) :input-mode :all
								       :combinations :over-bass :abs-intervals? NIL))
					(list rhythm-domain higher-pitch-domain
					      rhythm-domain higher-pitch-domain
					      rhythm-domain bass-pitch-domain))))
      (is (equal solution :no-solution)))))

(test min/max-harmonic-interval_no-voice-crossing-below-bass
  "Testing min/max-harmonic-interval: No voice crossing between bass (highest voice number) and other voices.."
  (test-harmonic-constraint
      (cr:min/max-harmonic-interval :min-interval 0 :voices '(0 1 2 3) :input-mode :all
				    :combinations :over-bass :abs-intervals? NIL)
    (lambda (pitches)
      (let ((bass-pitch (tu:last-element pitches)))
	(every (lambda (higher-pitch)
		 (when* (and higher-pitch bass-pitch) ; skip rests
		   (<= bass-pitch higher-pitch)))
	       (butlast pitches))))
    :voice-number 4))


(test min/max-harmonic-interval_no-pitch-repetition_no-solution
  "Testing min/max-harmonic-interval: no pitch repetition (and no voice crossing) ensured by constraining interval of at least semitone between voices -- CSP resulting in :no-solution."
  (for-all ((rhythm-domain (gen-selection
			    :length (gen-integer :min 2 :max 4)
			    :elements '((1/16) (1/8) (3/16) (1/4) (3/8) (1/2) (3/4) (1)))))
    (let* ((stop-time 2)
	   (no-of-variables 1000)
	   ;; NOTE: intends to fail: simple pitch in all pitch domains does not allow one voice be higher than other voice
	   (pitch-domain '((62))) 
	   (solution (cluster-shorthand no-of-variables
					(ce:rules->cluster
					 (ce:stop-rule-time '(0 1 2) stop-time :and)
					 ;; Actual rule
					 (cr:min/max-harmonic-interval :min-interval 1 :voices '(0 1 2) :input-mode :all
								       :combinations :consecutive-voices :abs-intervals? NIL))
					(list rhythm-domain pitch-domain
					      rhythm-domain pitch-domain
					      rhythm-domain pitch-domain))))
      (is (equal solution :no-solution)))))


(test min/max-harmonic-interval_no-pitch-repetition
  "Testing min/max-harmonic-interval: no pitch repetition (and no voice crossing) ensured by constraining interval of at least semitone between voices."
  (test-harmonic-constraint
      (cr:min/max-harmonic-interval :min-interval 1 :voices '(0 1 2) :input-mode :all
				    :combinations :consecutive-voices :abs-intervals? NIL)
    (lambda (pitches)
      (tu:map-neighbours (lambda (p1 p2)
			   (when* (and p1 p2) ; skip rests
			     (> p1 p2)))
			 pitches))
    :voice-number 3
    :stop-time (gen-ratio :numerator (gen-integer :min 2 :max 3)
			  :denominator (gen-select-one :candidates '(1 2)))
    :pitch-domain (gen-selection :length (gen-integer :min 5 :max 10)
				 :elements *pitch-domain-template*)))


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
