;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-



(asdf:load-system :cluster-engine)
(in-package :cluster-engine)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod test ((x integer)) 
  (+ x 1))
(test 2)

;; BUG: A type spec not possible for a method, hm...
(defmethod test ((x '(satisfies evenp))) 
  (+ x 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Make the predicate
(defun small-number-seq-p (thing)
  (and (listp thing)
       (every #'numberp thing)
       (every (lambda (x) (< x 10)) thing)))

;; Test it
(setq yes '(1 2  4))
(setq no  '(1 20 4))
(small-number-seq-p yes) ;; ⇒ t

;; Register it
(deftype small-number-seq ()
  '(satisfies small-number-seq-p))

;; Use it
(typep yes 'small-number-seq) ;; ⇒ true
(typep no 'small-number-seq)  ;; ⇒ false


;; BUG: A custom type cannot be used for defining a method, hm...
(defmethod test ((x small-number-seq)) 
  (+ x 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






#|
;; BUG: With complex rhythm domains, r-pitch-pitch *can* result in Cluster Engine internal error even with a rule always returning T.
;; Try evaluating the example multiple times to see the problem...
;; Seemingly, this bug did not exist in PWGL version. Perhaps, PWGL (CCL) ignored certain type declarations, which SBCL now uses and which cause this error (NIL not of type number).
;; TODO: Try with an older SBCL version.
(loop for i from 1 to 100
   do (format T "Test: ~A~%" i)
   collect
     (let ((no-of-variables 4)
	   (rhythm-domain '((-1) (-3/4) (-3/8) (-3/16) (-1/8) (3/16) (3/8) (1/2) (3/4) (1)))
	   (pitch-domain '((62))))
       (ce:clusterengine no-of-variables T NIL
			 (ce:r-pitch-pitch (lambda (ignore) T)
					   '(0 1) '(0) :all :include-gracenotes :pitch)
			 '((4 4))
			 (list rhythm-domain pitch-domain
			       rhythm-domain pitch-domain
			       ))))

;; A simpler rhythm domain seemingly fixes the problem. However -- see examples further down, which demonstrate the problem also with simpler rhythm domains.
(loop for i from 1 to 100 
   do (format T "Test: ~A~%" i)
   collect
     (let ((no-of-variables 4)
	   (rhythm-domain '((-1 -3/4 -1/2 -3/8 -1/4 -1/8 -1/16 1 3/4 1/2 3/8 1/4 1/8 1/16)))
	   ;; (rhythm-domain '((1/4)))
	   (pitch-domain '((46) (47) (73) (74) (79) (81))))
       (ce:clusterengine no-of-variables T NIL
			 (ce:r-pitch-pitch (lambda (ignore) T)
					   '(0 1) '(0) :all :include-gracenotes :pitch)
			 ;; (no-voice-crossing :voices '(0 1))
			 '((4 4))
			 (list rhythm-domain pitch-domain
			       rhythm-domain pitch-domain))))


;; Potential error of first example above -- this is perhaps an error where a variable value contradicts its type declaration.
The value
  NIL
is not of type
  NUMBER
when binding CLUSTER-ENGINE::START-TIME-INCLUDE-EARLIER-VARIABLES
in a lambda expression within CLUSTER-ENGINE::TEST-RULES (but the offending lambda has likely been compiled on the fly, so it has likely to be identified by code-walking from ce:r-pitch-pitch, a stack trace is only a limited help due to the [unnecessary] dynamic compilation)
(CLUSTER-ENGINE::TEST-RULES 2 #2A((#(#1=#<FUNCTION # {22A874DB}>) #(#)) (#(#1#) #(#)) (#(#1#) #(#)) (#(#1#) #(#)) (NIL NIL)) #(#((# # # # # # ...) NIL NIL NIL) #(NIL NIL NIL NIL) #((# # # # # # ...) N..

|#

#|
;; BUG: If the rhythm domain only consists in rest, it also triggers the error above
(let ((no-of-variables 4)
      (rhythm-domain '((-1/4 -1/4) (-1/2)))
      (pitch-domain '((62))))
  (ce:clusterengine no-of-variables T NIL
		    (ce:r-pitch-pitch (lambda (ignore) T)
				      '(0 1) '(0) :all :include-gracenotes :pitch)
		    '((4 4))
		    (list rhythm-domain pitch-domain
			  rhythm-domain pitch-domain
			  )))

;; BUG: Even with these simple domain settings the problem can appear
(let ((no-of-variables 4)
      (rhythm-domain '((-1/2) (1/16) (1/4) (-1/16)))
      (pitch-domain '((62) (64) (67))))
  (ce:clusterengine no-of-variables T NIL
		    (ce:r-pitch-pitch (lambda (ignore) T)
				      '(0 1) '(0) :all :include-gracenotes :pitch)
		    '((4 4))
		    (list rhythm-domain pitch-domain
			  rhythm-domain pitch-domain
			  rhythm-domain pitch-domain)))

;; BUG: Even when all rhythm-domain values have the duration 1/4, the bug happens
(let ((no-of-variables 3)
      (rhythm-domain '((-1/16 -1/16 1/8) (1/16 1/8 1/16) (-1/12 1/6) (-1/4) (-1/8 -1/8)
		       (-1/12 -1/6) (3/16 1/16) (-3/16 1/16) (-1/6 -1/12) (-1/6 1/12)))
      (pitch-domain '((37) (79) (75) (62) (72) (48) (46) (84) (65) (38) (69) (52) (45) (77) (57)
		      (43) (76) (80) (58) (61) (71) (42) (36) (81) (73) (40) (59) (68) (74) (66)
		      (53) (51) (41) (63) (70) (82) (60) (78) (67) (55) (39) (50) (44) (64) (49)
		      (47) (83) (56))))
  (ce:clusterengine no-of-variables T NIL
		    (ce:r-pitch-pitch (lambda (ignore) T)
				      '(0 1) '(0) :all :include-gracenotes :pitch)
		    '((4 4))
		    (list rhythm-domain pitch-domain
			  rhythm-domain pitch-domain
			  rhythm-domain pitch-domain)))


;; NOTE: A CSP with the settings below also resulted in this error, but this is seemingly not reproducible here
;; Anyway, also in test runs, the error occurs only rarely, which indicates that perhaps it can also occur with rather "tamed" rhythm domains occasionally (e.g., 1 in 20 runs)
(let ((no-of-variables 12)
      (rhythm-domain '((1/4 -1/2 -1/4) (1/16) (1/8 1/16 1/16) (3/16 -1/2 -5/16) (-1/8 -1/4 -1/8)
		       (1/16 3/16 3/4) (1/8) (1/8 1/8 1/4) (-1/4 1/2 -1/16 -3/16)))
      (pitch-domain '((57) (66) (61) (83) (70) (71) (84) (44) (64) (80) (69) (74) (67) (63) (45)
		      (37) (81) (59) (46) (42) (65) (62) (47) (36) (40) (39) (43))))
  (ce:clusterengine no-of-variables T NIL
		    (cr:no-voice-crossing :voices '(0 1))
		    #(((4 4))
		      (((4 4) 0 1/16 1/12 1/8 1/6 3/16 1/4 5/16 1/3 3/8 5/12 7/16 1/2 9/16 7/12
			5/8 2/3 11/16 3/4 13/16 5/6 7/8 11/12 15/16 1))
		      (((4 4) 0 -1/4 -1/2 -3/4 1)))
		    (list rhythm-domain pitch-domain
			  rhythm-domain pitch-domain
			  )))

;; NOTE: Same problem
(let ((no-of-variables 6)
      (rhythm-domain '((-3/8 -1/8) (-5/8 -3/8) (-1/2 1/2) (-3/4 1/4) (3/4 1/4) (-3/8 -5/8)
		       (1/2 -1/2)))
      (pitch-domain '((45) (41) (51) (49) (75) (38) (71) (79) (42) (81) (65) (73) (63) (40) (37)
		      (56) (58) (68) (76) (72) (53) (74) (39) (69) (36) (54) (60) (43) (67) (78)
		      (44))))
  (ce:clusterengine no-of-variables T NIL
		    (cr:number-of-sim-PCs :voices '(0 1 2) :pc-number 3 :rests-mode :reduce-no)
		    #(((4 4))
		      (((4 4) 0 1/16 1/12 1/8 1/6 3/16 1/4 5/16 1/3 3/8 5/12 7/16 1/2 9/16 7/12
			5/8 2/3 11/16 3/4 13/16 5/6 7/8 11/12 15/16 1))
		      (((4 4) 0 -1/4 -1/2 -3/4 1)))
		    (list rhythm-domain pitch-domain
			  rhythm-domain pitch-domain
			  rhythm-domain pitch-domain)))

|#

