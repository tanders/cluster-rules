;;; *************************************************************
;;; Copyright (C) 2013 Torsten Anders (torsten.anders@beds.ac.uk) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 3
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; *************************************************************

(in-package :cluster-rules)

;;; no-voice-crossing 

(defun no-voice-crossing 
    (&key (voices 0)
       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice 
       (gracenotes? :no_grace) ; options: :gracenotes, :no_grace
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1))
  "Voices should not cross, i.e., the pitch of simultaneous note pairs in voices are always sorted in decreasing order (lower voice number means higher pitch).

Arguments are inherited from r-pitch-pitch."
  (let ((sorted-voices (sort (copy-list voices) #'<)))
    (mappend (lambda (voice1 voice2)
	       (r-pitch-pitch (lambda (pitches)
				(let ((p1 (first pitches))
				      (p2 (second pitches)))
				  (when* (and p1 p2) ; no rests
				    (>= p1 p2))))
			      (list voice1 voice2)
			      '(0)
			      input-mode
			      gracenotes?
			      :pitch
			      rule-type weight))
	     (butlast sorted-voices)
	     (rest sorted-voices))))


;; no-parallels

(defun no-parallels (&key
		       (mode :open) ; options: :open, :open-and-hidden
		       (intervals '(0 7))
		       (voices '(0 1))
		       (gracenotes? :no_grace) ; options: :gracenotes, :no_grace
		       (rule-type :true/false) ; options: :true/false :heur-switch
		       (weight 1))
  "Parallels of given intervals are prohibited between all combinations of the given voices.

Args:
  mode: Specifies whether only open or also hidden intervals should be avoided.
  intervals (list of ints): Specifies the intervals (as pitch classes) of of which parallels should be avoided. 

Other arguments are inherited from r-pitch-pitch."
  (map-pairwise
   (lambda (voice1 voice2)
       (r-pitch-pitch (lambda (pitches1 pitches2)
			(when* (every (lambda (p) p) (append pitches1 pitches2))  ; no rests -- no NILs
			  (let* ((pitch1a (first pitches1)) ; 1st and 2nd pitch of voice A and voice B
				 (pitch1b (second pitches1))
				 (pitch2a (first pitches2))
				 (pitch2b (second pitches2))
				 (harm-interval2 (mod (abs (- pitch2b pitch2a)) 12)) 
				 (matchingInterval2? (member harm-interval2 intervals)))
			    (case mode
			      (:open-and-hidden (if matchingInterval2? 
						    (let ((directionA (signum (- pitch1a pitch2a)))  
							  (directionB (signum (- pitch1b pitch2b))))
						      (/= directionA directionB))
						    T))
			      (:open (if matchingInterval2? 
					 (let ((harm-interval1 (mod (abs (- pitch1b pitch1a)) 12))) 
					   (/= harm-interval1 harm-interval2))
					 T))))))
		      (list voice1 voice2)
		      '(0)
		      :all
		      gracenotes?
		      :pitch
		      rule-type weight))
   voices))


