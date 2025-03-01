;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

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


;;; TODO:
;;; - Update all rules calling R-pitch-pitch in background and added  support for input-mode :at-timepoints
;;;   R-pitch-pitch supports input-mode :at-timepoints -- useful for specifying more complex positions where rule should be applied. 


(in-package :cluster-rules)


;;;
;;; Follow harmony rules 
;;;

;; TODO: Add keyword arg intervals that specifies which intervals above the first chord tone (root) are considered in harmony. This allows, e.g., to detect and treat suspension chords differently. OR arg substitute-intervals with mappings of intervals above the root to be replaced with another given interval. Example (sus fourth to be replaced by fifth): '((5 7))
(defun in-harmony? (pitches &key (harmony-positions :all))
  "The PC of (first pitches) is in the PCs of (second pitches). (first pitches) can be a chord.

harmony-positions is a list of positions (0-based chord degrees) in the harmony to take into account, useful, e.g., to control inversions by reducing the allowed positions for the bass to '(0 1). This requires that the pitches in the harmonies domain are sorted accordingly (e.g., '(<root> <third> ...). If set to :all, all harmony tones are taken into account.
"
  (let* ((voice-pitch (first pitches))
	 (harmony-pitches (second pitches))
	 (harmony-size (length harmony-pitches))
	 (reduced-harmony-pitches (if (eql harmony-positions :all)
				      harmony-pitches
				      (loop for pos in harmony-positions
					 ;; if (= pos 3) do (break)
					 when (>= (1- harmony-size) pos) ;; just in case...
					 collect (nth pos harmony-pitches)
					   ;; (if (= pos 3)
					   ;;     (progn (break)
					   ;; 	      (nth pos harmony-pitches))
					   ;;     (nth pos harmony-pitches)
					   ;;     )
					   ))))
    (when* (and voice-pitch harmony-pitches)  ; no rests
      (let ((harmony-pcs (mapcar (lambda (p) (mod p 12))
				 reduced-harmony-pitches)))
	(every (lambda (p) (member (mod p 12) harmony-pcs))
	       ;; handle both individual pitches and chords
	       (tu:ensure-list voice-pitch))))))
#|
(in-harmony? ' (44 (68 72 75 79)))
|#

;;; only-scale-PCs 

(defun only-scale-PCs 
    (&key
       (voices 2)
       (timepoints '(0))
       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints 
       (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1)
       (scale-voice 0))
  "Tones (PC) in the given voice must be a member of the underlying scale (its PCs). The scale is represented as a simultaneous chord in another voice (voice 0 by default). I is either given directly to the clusterengine's pitch domain of that scale voice, or using read-harmony-file, or controlled with other constraints on that voice. 

Args: 
voices (int or list of ints): the voice(s) to which this constraint is applied.
input-mode (either :all, :beat, :1st-beat, :1st-voice, :at-timepoints): if set to :all, the constraint is applied whenever a new voice note or scale starts, while the setting :1st-voice applies the constraint only at each now voice note and multiple underlying harmonies per voice note are ignored. See the doc of r-pitch-pitch for the meaning of the other settings.

Optional args:
scale-voice (int, default 0): the voice representing the underlying scale.

Other arguments are inherited from r-pitch-pitch.
"
  (mapcar (lambda (voice)
	    (r-pitch-pitch
	     ;; Wrapping in lambda only for ensuring only a single arg...
	     (lambda (pitches) (in-harmony? pitches)) 
	     (list voice scale-voice)
	     timepoints
	     input-mode
	     gracenotes?
	     :pitch
	     rule-type weight))
	  (if (listp voices) voices (list voices))))


;; only-chord-PCs

(defun only-chord-PCs (&key
			 (voices 2)
			 (timepoints '(0))
			 (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints 
			 (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
			 (chord-voice 1)
			 (harmony-positions :all)
			 (rule-type :true/false) ; options: :true/false :heur-switch
			 (weight 1))
  "Tones (PC) in the given voice must be a member of the underlying chord (its PCs). The chord is represented as a simultaneous chord in another voice (voice 1 by default). I is either given directly to the clusterengine's pitch domain of that scale voice, or using read-harmony-file, or controlled with other constraints on that voice. 

Args: 
 - voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
 - chord-voice (int, default 1): the voice representing the underlying chord.
 - harmony-positions (list of ints or :all): list of positions (chord degrees) that are allowed.  This is useful, e.g., to control inversions by reducing the allowed positions for the bass to '(0 1). This requires that the pitches in the harmonies domain are sorted accordingly (e.g., '(<root> <third> ...). If set to :all, all harmony tones are taken into account. 

Other arguments are inherited from r-pitch-pitch. For example, it is possible to control whether this constraint should be applied to all notes, or only specific notes (input-mode). By default, it is applied to notes starting on a beat."
  (mapcar (lambda (voice)
	    (r-pitch-pitch (lambda (pitches)
			     (in-harmony? pitches :harmony-positions harmony-positions))
			   (list voice chord-voice)
			   timepoints
			   input-mode
			   gracenotes?
			   :pitch
			   rule-type weight))
	  (if (listp voices) voices (list voices))))


(defun in-spectrum? (pitches)
  "The pitches of (first pitches) is in the list in (second pitches)."
  (let ((ps1 (first pitches))
	(ps2 (second pitches)))
    (if (and ps1 ps2) ;; no rests
	(progn 
	  ; (BREAK)
	  (member ps1 ps2))
	T)))

(defun only-spectrum-pitches (&key
				(voices 2)
				(timepoints '(0))
				(input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints 
				(gracenotes? :no_grace) ; options: :no_grace, :gracenotes
				(rule-type :true/false) ; options: :true/false :heur-switch
				(weight 1)
				(chord-voice 1))
  "Pitches in the given voice must be a member of the underlying spectrum (its absolute pitches). The spectrum is represented as a simultaneous chord in another voice (voice 1 by default). 

Args: 
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
spectrum-voice (int, default 1): the voice representing the underlying spectra (quasi underlying harmony).

Other arguments are inherited from r-pitch-pitch. For example, it is possible to control whether this constraint should be applied to all notes, or only specific notes (input-mode). By default, it is applied to notes starting on a beat.

This rule is very similar to only-chord-PCs, but instead of pitch classes absolute pitches are constrained to the pitches of the given spectrum (quasi chord). "  
  (mapcar (lambda (voice)
	      (r-pitch-pitch #'in-spectrum?
			     (list voice chord-voice)
			     timepoints
			     input-mode
			     gracenotes?
			     :pitch
			     rule-type weight))
	  (if (listp voices) voices (list voices))))



(defun long-notes-chord-PCs (&key
			       (voices 2)
			       (max-nonharmonic-dur 1/1)
			       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints 
			       (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
			       (rule-type :true/false) ; options: :true/false :heur-switch
			       (weight 1)
			       (chord-voice 1))
  "Every note in the given voice(s) with a duration exceeding max-nonharmonic-dur (on any metric position) must be a harmonic tone: the PC of such notes must be a member of the underlying chord (its PCs). The chord is represented as a simultaneous chord in another voice (voice 1 by default). I is either given directly to the clusterengine's pitch domain of that scale voice, or using read-harmony-file, or controlled with other constraints on that voice. 

Args: 
voices (int or list of ints): the voice(s) to which this constraint is applied.
max-nonharmonic-dur (int): the maximum duration for which non-harmonic pitches are permitted.

Optional args:
chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch. For example, it is possible to control whether this constraint should be applied to all notes, or only specific notes (input-mode). By default, it is applied to notes starting on a beat."
  (mapcar (lambda (voice)
	      (r-pitch-pitch (lambda (p_d_offs)
				 "The PC of (first pitches) is in the PCs of (second pitches)."
				 (destructuring-bind ((pitch1 dur1 offs1) (pitch2 dur2 offs2)) p_d_offs
				   (declare (ignore offs1 offs2 dur2))
				   (if (and (and pitch1 pitch2) ;; no rests
					    (> dur1 max-nonharmonic-dur)) ;; main condition
				       (member (mod pitch1 12)
					       (mapcar (lambda (p) (mod p 12))
						       pitch2))					    
				       T)))
			     (list voice chord-voice)
			     '(0)
			     input-mode
			     gracenotes?
			     :p_d_offs
			     rule-type weight))
	  (if (listp voices) voices (list voices))))


;; chord-tone-before/after-rest

(defun chord-tone-before/after-rest (&key
				       (voices 2)
				       (input-mode :all) ; options: :all, :beat, :1st-beat, :at-timepoints 
				       (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
				       (harmony-positions :all)
				       (rule-type :true/false) ; options: :true/false :heur-switch
				       (weight 1)
				       (chord-voice 1))
  "Tones (PC) in the given voice(s) after a rest must be a member of the underlying chord PCs.

Args: 
 - voices (int or list of ints): the voice(s) to which this constraint is applied.
 - harmony-positions (list of ints or :all): list of positions (chord degrees) that are allowed. This allows to consider certain positions as dissonances (e.g., positions > 2) and require dissonance treatment.

Optional args:
 - chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch."
  (mapcar (lambda (voice)
	    (r-pitch-pitch (lambda (pitches1 pitches2)	     
			     (let* ((rest1? (null (first pitches1))) ; 1st note is rest
				    (rest2? (null (first pitches2)))) ; 2nd note is rest
			       (cond ((and rest1? rest2?) T) ; in case there are two rests in a row
				     (rest1? (in-harmony? pitches2 :harmony-positions harmony-positions)) ; after rest
				     (rest2? (in-harmony? pitches1 :harmony-positions harmony-positions)) ; before rest
				     (T T))))
			   (list voice chord-voice)
			   '(0)
			   input-mode
			   gracenotes?
			   :pitch
			   rule-type weight))
	  (if (listp voices) voices (list voices))))


;;;
;;; Other pitch related rules
;;;

;; chord-PC-at-1st-tone

;; NOTE: hack
(defun chord-PC-at-1st-tone-HACK 
    (&key
       (chords ()) 
       (voices 2)
       (input-mode :position-for-pitches) ; option: :position-for-pitches, :index-for-cell
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1)
       )
	 "HACK: The very first tone (PC) in given voice(s) number must be a member of the first of the given chords (the list of list of chords from read-harmony-file).

NOTE: an index variant for a pitch-pitch constraint (which could access the sim chord PCs) is not available, therefore this workaround."
	 (mapcar (lambda (voice)
		     (r-index-pitches-one-voice (lambda (pitch) 
						    (if pitch ; no rest
							(member (mod pitch 12)
								(mapcar (lambda (chord-pitch) (mod chord-pitch 12)) 
									(first chords)))
							T))
						'(0) ; positions
						voice
						input-mode
						rule-type weight))
		 (if (listp voices) voices (list voices))))


;; stepwise-non-chord-tone-resolution

(defun stepwise-non-chord-tone-resolution 
    (&key
       (voices 2)
       (step-size 2) 	  
       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints
       (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
       (harmony-positions :all)
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1)
       (chord-voice 1))
  "Every tone (PC) that is not a chord tone (member of sim chord PCs, in voice 1 by default) is reached and left by a step of at most the given step size. Note repetitions are possible as well.

Args: 
 - step-size (int): maximum interval considered a step.
 - voices (int or list of ints): the voice(s) to which this constraint is applied.
 - harmony-positions (list of ints or :all): list of positions (chord degrees) that are allowed. This allows to consider certain positions as dissonances (e.g., positions > 2) and require them to be resolved as well.

Optional args:
 - chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch."
  (mapcar (lambda (voice)
	    (r-pitch-pitch (lambda (pitches1 pitches2 pitches3)
			     ;; Every pitchesN is a list of the form (voice-pitch chord-pitches) 
			     (let ((voice-pitch2 (first pitches2))
				   (chord-pitches2 (second pitches2)))
			       (when* (and chord-pitches2 voice-pitch2)  ;; no rests
				 (let ((voice-pitch1 (first pitches1))
				       (voice-pitch3 (first pitches3)))
				   (when* (not (in-harmony? pitches2 :harmony-positions harmony-positions))
				     ;; (break)
				     (and (if voice-pitch1 ; no rest
					      (<= (abs (- voice-pitch1 voice-pitch2)) step-size)
					      NIL) ; fail in case preceeded by rest
					  (if voice-pitch3 ; no rest
					      (<= (abs (- voice-pitch2 voice-pitch3)) step-size)
					      NIL)))))))
			   (list voice chord-voice)
			   '(0)
			   input-mode
			   gracenotes?
			   :pitch
			   rule-type weight))
	  (if (listp voices) voices (list voices))))



;;; chord-tone-follows-non-chord-tone

(defun chord-tone-follows-non-chord-tone
    (&key
       (voices 2)
       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints
       (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
       (harmony-positions :all)
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1)
       (chord-voice 1))
  "Every tone (PC) that is not a chord tone (member of sim chord PCs, in voice 1 by default) is followed by a chord tone.

Args: 
 - voices (int or list of ints): the voice(s) to which this constraint is applied.
 - harmony-positions (list of ints or :all): list of positions (chord degrees) that are allowed. This allows to consider certain positions as dissonances (e.g., positions > 2) and require dissonance treatment.

Optional args:
 - chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch."
  (mapcar (lambda (voice)
	    (r-pitch-pitch (lambda (pitches1 pitches2)
			     ;; Every pitchesN is a list of the form (voice-pitch chord-pitches) 
			     (when* (and (first pitches1) (first pitches2)
					 (second pitches1) (second pitches2))  ;; no rests
			       (when* (not (in-harmony? pitches1 :harmony-positions harmony-positions))
				 (in-harmony? pitches2 :harmony-positions harmony-positions))))
			   (list voice chord-voice)
			   '(0)
			   input-mode
			   gracenotes?
			   :pitch
			   rule-type weight))
	  (if (listp voices) voices (list voices))))



(defun unequal-sim-pitches-aux 
    (&key
       (voices 0)
       (timepoints '(0))
       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints
       (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1))
  "[Quasi aux def] The pitches of the 1st given voice differ from the sim pitches of the remaining voices."
  (r-pitch-pitch (lambda (pitches)
		     (if (first pitches) ; no rest
			 (not (member (first pitches) 
				      (rest pitches)))
			 T))
		 voices
		 timepoints
		 input-mode
		 gracenotes?
		 :pitch
		 rule-type weight))

;;; TODO: unfinished
(defun unequal-sim-pitches
    (&key
       (voices 0)
       (timepoints '(0))
       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints
       (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1))
  "Simultaneous pitches in all given voices differ.

Arguments are inherited from r-pitch-pitch.

TODO: Revise this definition -- can the interplay with unequal-sim-pitches-aux be simplified?
"
  (let* ((voices (sort (copy-list voices) #'<))
	 (len (length voices))
	 (rev (reverse voices)))
    (mapcar (lambda (i) 
		(unequal-sim-pitches-aux :voices (subseq rev i len)
					 :timepoints timepoints
					 :input-mode input-mode
					 :gracenotes? gracenotes?
					 :rule-type rule-type
					 :weight weight))
	    ;; Package/library pw not loaded -- no idea how this function came up here..
	    ;; (pw:arithm-ser 0 1 (- len 2))
	    (tu:arithmeric-series 1 0 (- len 2))
	    )))

;;; unequal-sim-PCs

#|
(defun unequal-sim-PCs-aux 
    (&key
       (voices 0)
       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice
       (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1))
  "[Quasi aux def] The PCs of the 1st given voice are unequal to the sim PCs of the remaining voices."
  (r-pitch-pitch (lambda (pitches)
		     (if (first pitches) ; no rest
			 (not (member (mod (first pitches) 12) 
				      (mapcar (lambda (p) (mod p 12)) 
					      (rest pitches))))
			 T))
		 voices
		 '(0)
		 input-mode
		 gracenotes?
		 :pitch
		 rule-type weight))

;;; TODO: unfinished
(defun unequal-sim-PCs
    (&key
       (voices 0)
       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice
       (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1))
  "Sim PCs in all given voices are unequal to each other.

Arguments are inherited from r-pitch-pitch.

TODO: Revise this definition -- can the interplay with unequal-sim-PCs-aux be simplified?
"
  (let* ((voices (sort voices #'<))
	 (len (length voices))
	 (rev (reverse voices)))
    (mapcar (lambda (i) 
		(unequal-sim-PCs-aux :voices (subseq rev i len)
				     :input-mode input-mode
				     :gracenotes? gracenotes?
				     :rule-type rule-type
				     :weight weight))
	    ;;; TODO: unfinished
	    ;; Package/library pw not loaded -- no idea how this function came up here..
	    (pw:arithm-ser 0 1 (- len 2))
	    ; (tu:arithmeric-series 1 0 (- len 2))
	    )))
|#


(defun number-of-sim-PCs (&key
			    (PC-number 2)
			    (condition :min) ; options: :min, :equal, :max
			    (rests-mode :reduce-no) ; options: :reduce-no, :ignore
			    (at-most-as-in-chord-voice NIL)
			    (voices '(0 1))
			    (redundant-constraints? :voice-accumulation)
			    (timepoints '(0))
			    (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints
			    (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
			    (rule-type :true/false) ; options: :true/false :heur-switch
			    (weight 1))
  "Controls the number of simultaneous pitch classes. Useful, for example, to require that some underlying harmony is expressed.

Args: 
See constrain-number-of-sim-pitches"
  (constrain-number-of-sim-pitches :pitch-number PC-number :condition condition :rests-mode rests-mode
				   :at-most-as-in-chord-voice at-most-as-in-chord-voice :voices voices
				   :redundant-constraints? redundant-constraints? :timepoints timepoints :input-mode input-mode
				   :gracenotes? gracenotes? :rule-type rule-type :weight weight
				   ;; Unique element of this rule
				   :key (lambda (p) (mod p 12))))


(defun number-of-sim-pitches (&key
				(pitch-number 2)
				(condition :min) ; options: :min, :equal, :max
				(rests-mode :reduce-no) ; options: :reduce-no, :ignore
				(at-most-as-in-chord-voice NIL)
				(voices '(0 1))
				(redundant-constraints? :voice-accumulation)
				(timepoints '(0))
				(input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints
				(gracenotes? :no_grace) ; options: :no_grace, :gracenotes
				(rule-type :true/false) ; options: :true/false :heur-switch
				(weight 1))
  "Controls the number of simultaneous pitches. Useful, for example, to require that all pitches of chord layer differ.

This function does the same as constrain-number-of-sim-pitches, but is kept for backward compatibility.

Args: 
See constrain-number-of-sim-pitches"
  (constrain-number-of-sim-pitches :pitch-number pitch-number :condition condition :rests-mode rests-mode
				   :at-most-as-in-chord-voice at-most-as-in-chord-voice :voices voices
				   :redundant-constraints? redundant-constraints? :timepoints timepoints :input-mode input-mode
				   :gracenotes? gracenotes? :rule-type rule-type :weight weight
				   ;; Unique element of this rule
				   :key #'identity))

;; TODO:
;; - ? Add additional input modes that not only take harmonic slices but all tones within a certain time frame into account, namely a full beat duration, a full bar duration, the beginning of a new harmony, and the whole duration of a harmony -- likely the current implementation of r-pitch-pitch does not allow for these cases.
;; - ?? Have pitch-number controlled with BPF? Possibly the current implementation of r-pitch-pitch does not allow for that. Also, that would restrict the would only work if underlying harmony allows for that, but could be useful 
;; - OK Redundant constraints for efficiency
;; - OK Automatically reduce pitch-number internally, if the number of sim pitches is snmaller due to rests (some pitch = NIL)
(defun constrain-number-of-sim-pitches (&key
					  (pitch-number 2)
					  (condition :min) ; options: :min, :equal, :max
					  (rests-mode :reduce-no) ; options: :reduce-no, :ignore
					  (at-most-as-in-chord-voice NIL)
					  (key #'identity)
					  (voices '(0 1))
					  (redundant-constraints? :voice-accumulation)
					  (timepoints '(0))
					  (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints
					  (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
					  (rule-type :true/false) ; options: :true/false :heur-switch
					  (weight 1))
  "Controls the number of simultaneous pitches or pitch-related parameters. Useful, for example, to require that pitches of a chord (expressed by multiple voices) differ.

Args: 
  pitch-number (int): the number of the simultaneous pitches. The meaning of this setting depends on the argument condition.
  condition: Whether the number of simultaneous pitch should be at least the given pitch-number (:min), or exactly that number (:equal), or at most that number (:max). 
  rests-mode: If set to :reduce-no, then the number of simultaneous rests is subtracted from pitch-number. For example, if there is only a single tone at a certain time and all other voices have rests, this rule can still be fulfilled. By contrast, if rests-mode is set to :ignore, then the remaining simultaneous pitch classes must still fullfil the condition expressed by the arguments pitch-number and condition.
  at-most-as-in-chord-voice (int or NIL): if set to an int, this int is the voice number of the chord voice (the underlying harmony). If set, the number of simultaneous pitches is limited to the number of chord tones in the underlying harmony.
  key (unary function): function expecting a pitch and somehow transforming the pitch. The condition will be applied to results of the key function. Example: if key computes the pitch class, the constraint controls the number of simultaneous pitch classes.
  voices: the list of voices to which the rule is applied.
  redundant-constraints? (:all-voice-combinations, :voice-accumulation or NIL): If non-NIL, redundant constraints with subsets of voices and with a reduced pitch-number are also applied to cause fails earlier and thus speed up the search. The setting :voice-accumulation applies redundant constraints for the combinations (voice1 voice2), (voice1 voice2 voice3) ..., but not (voice2 voice3) etc. This is suitable when voices are visited in increasing order of voice IDs (Cluster Engine engine variable), which is the Cluster Engine default. The setting :all-voice-combinations applies redundant constraints to all possible voice subsets instead.
    If NIL, no redundant constraints are applied. If condition is set to :max, then redundant constraints may also reduce the possible solutions, therefore this can be switched off.

Other arguments are inherited from r-pitch-pitch."
  ;; Improve efficiency with redundant constraints to cause fails earlier:
  ;; internally also call constrain-number-of-sim-pitches-aux with subsets of voices and with a reduced pitch-number
  (if redundant-constraints?
      (tu:mappend (lambda (args)
		    (destructuring-bind (&key voices-subset number-subset) args
		      (list voices-subset number-subset)
		      (constrain-number-of-sim-pitches-aux
		       :pitch-number number-subset :condition condition :rests-mode rests-mode
		       :at-most-as-in-chord-voice at-most-as-in-chord-voice :key key :voices voices-subset
		       :timepoints timepoints :input-mode input-mode :gracenotes? gracenotes?
		       :rule-type rule-type :weight weight)))
		  ;; Create args voices-subset number-subset for all incl. redundant constraints
		  (remove-if (lambda (x) (< (getf x :number-subset) 1)) 
			     (let ((l (length voices))
				   ;; Just in case...
				   (sorted-voices (sort (copy-list voices) #'<))
				   (min-voice-combination-no 2))
			       (case redundant-constraints?
				 ;; TODO: Consider with the argument redundant-constraints? (or renamed redundant-constraints) to be able switch between different cases of combinations of redundant constraints.
				 ;;
				 ;; Adding redundant constraints for all possible voice subset combinations
				 (:all-voice-combinations
				  (loop
				     for end from min-voice-combination-no to l
				     ;; a bit inefficient calling subseq multiple times, but fine for a short voices list
				     append (loop
					       for start from 0 to (- end min-voice-combination-no)
					       ;; for no from (- pitch-number l -2) to pitch-number		    
					       ;; for no = (+ pitch-number (- end start))
					       for no = (+ pitch-number
							   (- (- end start) (* 2 min-voice-combination-no)))
					       collect `(:voices-subset ,(subseq sorted-voices start end)
									:number-subset ,no))))
				 (:voice-accumulation
				  (loop
				     for i from min-voice-combination-no to l
				     for no from (- pitch-number l (- min-voice-combination-no)) to pitch-number
				     ;; a bit inefficient calling subseq multiple times, but fine for a short voices list
				     collect `(:voices-subset ,(subseq sorted-voices 0 i)
							      :number-subset ,no)))
				  ))))
      (constrain-number-of-sim-pitches-aux
       :pitch-number pitch-number :condition condition :rests-mode rests-mode
       :at-most-as-in-chord-voice at-most-as-in-chord-voice :key key :voices voices
       :timepoints timepoints :input-mode input-mode :gracenotes? gracenotes?
       :rule-type rule-type :weight weight)))


(defun constrain-number-of-sim-pitches-aux (&key
					      (pitch-number 2)
					      (condition :min) ; options: :min, :equal, :max
					      (rests-mode :reduce-no) ; options: :reduce-no, :ignore
					      (at-most-as-in-chord-voice NIL)
					      (key #'identity)
					      (voices '(0 1))
					      (timepoints '(0))
					      (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints
					      (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
					      (rule-type :true/false) ; options: :true/false :heur-switch
					      (weight 1))
  "See constrain-number-of-sim-pitches"
  (r-pitch-pitch (lambda (all-pitches)
		   (let* (;; underlying harmony in chord voice
			  (chord-pitches (if at-most-as-in-chord-voice
					     (remove-duplicates
					      ;; Remove rests in underlying harmony
					      (remove NIL (mapcar key (first all-pitches))))
					     NIL))
			  (pitches (if at-most-as-in-chord-voice
				       (rest all-pitches)
				       all-pitches))
			  ;; Take rests into account
			  (aux-number (case rests-mode
					(:reduce-no (- pitch-number
						       (length (remove NIL pitches :test (complement #'eql)))))
					(:ignore pitch-number)))
			  ;; (actual-number aux-number)
			  ;; Take underlying harmony into account
			  #|
			  ?? BUG: If the var actual-number does not
			  depend on chord-pitches (see version above
			  in comments), but arg
			  at-most-as-in-chord-voice is used, i.e. if
			  the only difference is that the lambda given
			  to r-pitch-pitch additionally receives the
			  underlying harmony as arg, then seemingly
			  that results in a change that fixes some
			  CSPs. Something is very fishy here -- I must
			  misunderstand something here.
			  |#
			  (actual-number (if (and chord-pitches ;; there is an underlying harmony (no rest)
			  			  at-most-as-in-chord-voice)
			  		     (min (length chord-pitches) aux-number)
			  		     aux-number))
			  ;; The chord resulting from the pitches in voices
			  (harm (remove-duplicates (mapcar key
							   ;; take out rests
							   (remove NIL pitches)))))
		     (assert (<= pitch-number (length pitches)))
		     ;; (when (> ce::*current-loop-n* 50000)
		     ;; 	 (break))
		     (if harm				  
			 (funcall (ecase condition
				    (:min #'>=)
				    (:equal #'=)
				    (:max #'<=))
				  (length harm) actual-number)
			 t)))
		 (if at-most-as-in-chord-voice
		     (cons at-most-as-in-chord-voice
			   voices)
		     voices)
		 timepoints
		 input-mode
		 gracenotes?
		 :pitch
		 rule-type weight))


(defun set-harmonic-intervals 
    (&key
       (voices '(0 1))
       (intervals NIL)
       (pcs? :pitches) ; options: :pitches, :pcs
       (exclude/only :only-given) ; options: :only-given, :exclude-given
       (combinations :over-bass) ; options: :over-bass, :consecutive-voices, :all-combinations
       (input-mode :beat) ; options: :beat, :all, :1st-beat, :1st-voice, :at-timepoints
       (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
       (timepoints '(0))
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1))
  "Restricts the harmonic intervals between all combinations of the given voices to only (or not) those intervals specified. For example, 'empty' perfect consonances in two-voice counterpoint can be excluded with this rule. 

Args:
  intervals (list of ints): Specified intervals.
  pcs?: whether absolute intervals or PC intervals should be used. 
  exclude/only: Controls whether to only use the given intervals (:only-given), or whether to only use intervals that are not given (:exclude-given).
  combinations: Controls whether to constrain only intervals between the bass and a higher voice (:over-bass), between pairs of consecutive voices such as soprano-alto, alto-tenor etc. (:consecutive-voices), or between all voice combinations (:all-combinations).

Other arguments are inherited from r-pitch-pitch.
"
  (tu:flat
   (flet ((rule (pitches)
	    (if (and (first pitches) (second pitches)) ; no rests 
		(let ((interval (case pcs?
				  (:pitches (abs (- (first pitches) (second pitches))))
				  (:pcs (mod (abs (- (first pitches) (second pitches))) 12)))))
		  (case exclude/only
		    (:exclude-given (not (member interval intervals)))
		    (:only-given (member interval intervals))))
		T)))    
     (let ((sorted-voices (sort (copy-list voices) #'>)))
       (case combinations 
	 (:over-bass 
	  (let ((bass-voice (first sorted-voices)))
	    (mapcar (lambda (voice)
			(r-pitch-pitch #'rule
				       (list bass-voice voice)
				       timepoints
				       input-mode
				       gracenotes?
				       :pitch
				       rule-type weight)) 
		    (rest sorted-voices))))
	 (:consecutive-voices
	  (mapcar (lambda (voice1 voice2)
		      (r-pitch-pitch #'rule
				     (list voice1 voice2)
				     timepoints
				     input-mode
				     gracenotes?
				     :pitch
				     rule-type weight)) 
		  (butlast sorted-voices) (rest sorted-voices)))
	 (:all-combinations
	  (map-pairwise (lambda (voice1 voice2)
			    (r-pitch-pitch #'rule
					   (list voice1 voice2)
					   timepoints
					   input-mode
					   gracenotes?
					   :pitch
					   rule-type weight)) 
			sorted-voices)))))))


;; TODO: Reduce rule by reducing code repetition
(defun min/max-harmonic-interval
    (&key
       (voices '(0 1))
       (min-interval NIL)
       (max-interval NIL)
       (abs-intervals? T)
       (input-mode :beat) ; options: :beat, :all, :1st-beat, :1st-voice, :at-timepoints
       (combinations :consecutive-voices) ; options: :over-bass, :consecutive-voices, :all-combinations
       (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
       (timepoints '(0))
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1))
  "Limit the minimum/maximum harmonic interval of simultaneous notes between given voices. 

Args: 
  voices (list of ints): the voices to constrain.
    Internally voice numbers are sorted, assuming higher voice numbers stand for lower pitches (if abs-intervals? is NIL).
  min-interval (number or NIL): minimum interval in semitones. Ignored if NIL.
  max-interval (number or NIL): maximum interval in semitones. Ignored if NIL. 
  abs-intervals? (Boolean): if T, only absolute intervals are compared, otherwise the interval direction is taken into account as well.
  combinations: Controls whether to constrain only intervals between the voice with highest note number and other voices (:over-bass), between pairs of consecutive voices such as soprano-alto, alto-tenor etc. (:consecutive-voices), or between all voice combinations (:all-combinations).

This rule assumes that the higher the voice number the lower the pitch generally (e.g., for combinations :over-bass, the bass is considered the voice with the highest voice number; and when abs-intervals? is NIL, again the voice number affects the interval direction).

Other arguments are inherited from r-pitch-pitch.
"
  (tu:flat
   (flet ((rule (pitches)
	    ;; (format T "min/max-harmonic-interval rule: pitches: ~A" pitches)
	    (let ((pitch1 (first pitches)) ;; higher voice ID, assumed lower pitch
		  (pitch2 (second pitches)))		  
	      (if (and pitch1 pitch2) ; no rests
		  (let* ((raw-interval (- pitch2 pitch1))
			 (interval (if abs-intervals?
				       (abs raw-interval)
				       raw-interval)))
		    (and (if min-interval
			     (progn
			       ;; (break)	
			       (<= min-interval interval))
			     T)
			 (if max-interval 
			     (<= interval max-interval)
			     T)))
		  T))))
     (let ((sorted-voices (sort (copy-list voices) #'>)))
       ;; (format T "min/max-harmonic-interval: sorted-voices: ~A, combinations: ~A, rule-type:~A~%" sorted-voices combinations rule-type)
       (case combinations 
	 (:over-bass 
	  (let ((bass-voice (first sorted-voices)))
	    (mapcar (lambda (voice)
			;; (format T "min/max-harmonic-interval :over-bass")
			(r-pitch-pitch #'rule
				       (list bass-voice voice)
				       timepoints
				       input-mode
				       gracenotes?
				       :pitch
				       rule-type weight)) 
		    (rest sorted-voices))))
	 (:consecutive-voices
	  (mapcar (lambda (voice1 voice2)
		      ;; (format T "min/max-harmonic-interval :consecutive-voices: voice1: ~A, voice2: ~A" voice1 voice2)
		      (r-pitch-pitch #'rule
				     (list voice1 voice2)
				     timepoints
				     input-mode
				     gracenotes?
				     :pitch
				     rule-type weight)) 
		  (butlast sorted-voices) (rest sorted-voices)))
	 (:all-combinations
	  (map-pairwise (lambda (voice1 voice2)
			    ;; (format T "min/max-harmonic-interval :all-combinations")
			    (r-pitch-pitch #'rule
					   (list voice1 voice2)
					   timepoints
					   input-mode
					   gracenotes?
					   :pitch
					   rule-type weight)) 
			sorted-voices))
	 )))))


;; Values stem from Aiva's music-engine: constant LOWEST_INTERVALS in constants.py
(defparameter *lowest-permitted-harmonic-intervals_Aiva* '(0 50 49 46 44 44 45 32 39 39 39 39)
  "Encoding of lowest harmonic intervals (within an octave) that permitted. The position encodes the interval, and the value the lowest pitch (MIDI keynumber) above which this interval is permitted. Example: a minor second (position 1) cannot occur below a D3 (MIDI keynumber 50). These are the values used by music-engine.")

(defparameter *lowest-permitted-harmonic-intervals_orig* '(0 52 51 48 46 46 47 34 41 41 41 41)
  "Encoding of lowest harmonic intervals (within an octave) that permitted. The position encodes the interval, and the value the lowest pitch (MIDI keynumber) above which this interval is permitted. Example: a minor second (position 1) cannot occur below a D3 (MIDI keynumber 50). These are the original values from Brad's teacher. See also https://www.sweetwater.com/insync/low-interval-limit/")

(defun restrict-low-harmonic-intervals (&key
					  (voices '(0 1))
					  (lowest-harmonic-intervals *lowest-permitted-harmonic-intervals_Aiva*)
					  (input-mode :all) ; options: :beat, :all, :1st-beat, :1st-voice, :at-timepoints
					  (gracenotes? :no_grace) ; options: :no_grace, :gracenotes
					  (timepoints '(0))
					  (rule-type :true/false) ; options: :true/false :heur-switch
					  (weight 1))
  "Reduce roughness by limiting the interval size possible between lower pitches (avoid/reduce critical band roughness). Rule constraints intervals between consecutive voices to not go below the pitch-dependent interval limits encoded by argument LOWEST-HARMONIC-INTERVALS.

Usually, voices with lower ID are assumed at higher pitch and there is no voice crossing (not a strict requirement, but with too much voice crossing it will not be sufficient to check only consecutive voices).

* Arguments:
  - voices (list of ints): the voices to constrain.
  - lowest-harmonic-intervals (list of 12 ints): encoded of lowest harmonic intervals (within an octave) that permitted. See doc of *lowest-permitted-harmonic-intervals_Aiva* for further details.

Other arguments are inherited from r-pitch-pitch.
"
  (let ((max-interval-limit (apply #'max lowest-harmonic-intervals))
	(lowest-intervals (apply #'vector lowest-harmonic-intervals)))
    (let ((sorted-voices (sort (copy-list voices) #'>)))
      (mapcar (lambda (voice1 voice2)
		(r-pitch-pitch (lambda (pitches)
				 (let ((pitch1 (first pitches)) ;; higher voice ID, assumed lower pitch
				       (pitch2 (second pitches)))
				   (when* (and pitch1 pitch2 ; no rests
					       (<= (min pitch1 pitch2) ; lower pitches
						   max-interval-limit))
				     (let ((interval (abs (- pitch2 pitch1))))
				       (when* (< 0 interval 12) ; only intervals 1-11
					 (>= (min pitch1 pitch2) (elt lowest-intervals interval)))))))
			       (list voice1 voice2)
			       timepoints
			       input-mode
			       gracenotes?
			       :pitch
			       rule-type weight)) 
	      (butlast sorted-voices) (rest sorted-voices)))))


	  
;;
;; Tintinnabuli rules 
;; 

#|
Ideas for using tintinnabuli in settings beyond the "Arvo Pärt sound". Doing the below in terms of software developmemt is easy -- think how to musically use this!?
 - M voice: allow for slightly larger skips (e.g., up to maj 3d?) 
 - T voice: control pitch with BPF?
 - ! Allow T voice and M voice to be pretty independent rhythmically, e.g.,
   - M-voice (very) slow like a cantus (i.e. stands out) -- by allowing for non-harmonic tones can form a  rich melody in terms of its [Tonvorrat]
   - T-voice much faster, like a figuration -- accompaniment (not Paert's ideal anymore, I guess, but in my own mind allow for that) 
   - There can be multiple somehow dependent or independent T-voices
   - M-voice and T-voice in different beat subdivisions or otherwise clearly separate
|#

(defun tintinnabuli-M-voice (&key
			       (voices 0)
			       (max-interval 2)
			       (rule-type :true/false) ; options: :true/false :heur-switch
			       (weight 1)
			       (scale-voice 0))
  "Rules for a tintinnabuli M-voice, inspired by Arvo Pärt (slightly generalised, because this rule is applicable over any harmony). The voice consists only of scale ;TODO: ones that move stepwise (max interval is whole tone).  

Args: 
voices (int or list of ints): the number of the voice(s) to constrain.

Optional:
max-interval (default 2): maximum interval in semitones.
scale-voice (default 0): the voice representing the underlying scale."
  (rules->cluster 
   (min/max-interval :voices voices :max-interval max-interval :rule-type rule-type :weight weight)
   (only-scale-PCs voices :all :include-gracenotes rule-type weight scale-voice)))

(defun tintinnabuli-T-voice (&key
			       (voices 0)
			       (min-interval 0)
			       (max-interval 12)
			       (rule-type :true/false) ; options: :true/false :heur-switch
			       (weight 1)
			       (chord-voice 1))
  "Rules for a tintinnabuli T-voice, inspired by Arvo Pärt (slightly generalised, because this rule is applicable over any harmony). The voice consists only of chord tones, and the minimum/maximum interval size can be controlled.  

Args: 
voices (int or list of ints): the number of the voice(s) to constrain.

Key args:
min-interval (default 3): minimum interval in semitones.
max-interval (default 12): maximum interval in semitones.
chord-voice (default 1): the voice representing the underlying chord."
  (rules->cluster 
   (min/max-interval :voices voices :min-interval min-interval :max-interval max-interval :rule-type rule-type :weight weight)
   (only-chord-PCs voices :all :include-gracenotes rule-type weight chord-voice)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Rules on underlying harmony (the actual chords)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun set-chord-at-positions (positions chord
			       &key
				 (chord-voice 1) 
				 (rule-type :true/false) ; options: :true/false :heur-switch
				 (weight 1))
  "Restricts the chords at the given positions (0-based) in the chord-voice to the given chord."
  (loop for pos in positions
     append (R-index-pitches-one-voice
	     (lambda (ps)
		 (if ps ; no rest
		     (equal ps chord)
		     T))
	     (list pos)
	     chord-voice
	     :position-for-pitches
	     rule-type
	     weight)))


(defun set-root-at-positions (positions root
			      &key
				(pc? T)
				(chord-voice 1) 
				(rule-type :true/false) ; options: :true/false :heur-switch
				(weight 1))
  "Restricts the chord roots (the lowest chord pitches) at the given positions (0-based) in the chord-voice to the given root.

If pc? is set to T, then this constraint compares pitch classes instead of actual pitches."
  (let ((root-pc (mod root 12)))
    (flet ((aux ()
	     (if pc?
		 (lambda (ps)
		   (if ps ; no rest
		       (= (mod (first ps) 12) root-pc)
		       T))
		 (lambda (ps)
		   (if ps ; no rest
		       (= (first ps) root)
		       T)))))
      (loop for pos in positions
	 append (R-index-pitches-one-voice
		 (aux)
		 (list pos)
		 chord-voice
		 :position-for-pitches
		 rule-type
		 weight)))))




;; Port from Strasheela
;; - VoiceLeadingDistance VoiceLeadingDistance_Percent
;; - Schoenberg rules

;;; TODO: Revised version (possibly switched on by arg): the current version always takes the shortest path between pitch classes, and does not enforce that each PC of each chord is involved in this calculation, which leads to a lower voice leading distance overall. A perhaps better algorithm might enforce that all PCs of each chord is involved, but that might need some optimisation technique, which needs a bit of extra work to implement. Hm -- but is that still min. voice leading distance?
;; I could try to find a better voice-leading algorithm in the literature, or perhaps some general distance algorithm finding the shortest path between multiple 1D points in space? 
(defun voice-leading-distance (chord1 chord2 &optional n)
  "Returns the voice leading distance (an integer, measured in semitones) between two given chords, each a lists of MIDI note numbers. The voice leading distance is the minimal sum of pitch class intervals between chord1 and chord2. The voice-leading distance is directionless in the sense that regardless whether a voice moves up or down, always the smaller interval is taken into account. The lower the voice leading distance, the more 'smooth' is the harmonic progression (a chord repetition is quasi most smooth).

  Currently, only 12-TET is supported.

  Example: voice leading distance between C major and Ab major triads
  (voice-leading-distance '(60 64 67) '(56 60 63))
  => 2
  C->C=0 + E->Eb=1 + G->Ab=1, so the sum is 2

  Note: Only the minimal intervals from all chord2 pitch classes to chord1 pitch classes are taking into account. There may be pitch classes in chord1 which are ignored as all pitch classes of chord2 may be closer to some other pitch classes of chord1.

  Example: C-maj -> F#-maj = 4
  (voice-leading-distance '(60 64 67) '(66 70 73))
  => 4
  C->C#=1, C->A#=2, G->F#=1 -- the E of C-maj is ignored in the computation  

  If `n' is set, only the first `n' pitch classes of the chords are taken into account.
"
  (let ((chord1-pcs (if n
			(first-n (remove-duplicates (pitch->pc chord1) :from-end T) n)
			(pitch->pc chord1)))
	(chord2-pcs (if n
			(first-n (remove-duplicates (pitch->pc chord2) :from-end T) n)
			(pitch->pc chord2))))
    (apply #'+
	   (mapcar (lambda (pc2)
		       (apply #'min
			      (mapcar (lambda (pc1)
					  (let* ((pc-int (pc-interval pc1 pc2))
						 (pc-int-complement (- 12 pc-int)))
					    (min pc-int pc-int-complement)))
				      chord1-pcs)))
		   chord2-pcs))))

; (voice-leading-distance '(60 64 67) '(56 60 63))
; (voice-leading-distance '(56 60 63) '(60 64 67))
; => 2 for both

; (voice-leading-distance '(60 64 67) '(60 64 67))
; => 0

(defun limit-voice-leading-distance (max-distance &key
						    (chord-voice 1)
						    (n nil)
						    (rule-type :true/false) ; options: :true/false :heur-switch
						    (weight 1))
  "Constrains the voice leading distance of consecutive chords in chord-voice to be at most max-distance. See the documentation of `voice-leading-distance' for details on what the voice leading distance is.

  Args: 
  - chord-voice (int): the voice representing the underlying chord.
  - n (int): only the first `n' pitch classes of chords are taken into account, if this argument is set.
"
  (R-pitches-one-voice (lambda (chord1 chord2)
			   (<= (voice-leading-distance chord1 chord2 n)
			       max-distance))
		       chord-voice
		       :pitches
		       rule-type
		       weight))
; (limit-voice-leading-distance 3)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; My generalisation of Schoenberg's chord progression rules
;;;

#|

Music representation convention: 

 - A chord/spectrum/scale is represented as a list of pitches or pitch classes
 - Pitches are represented as MIDI note numbers, pitch classes as an integers between 0 and 11 (currently limited to 12-TET)
 - The root of a chord/spectrum/scale is its first pitch (class)

|#

(defparameter *pitches-per-octave* 12
  "Number of equidistant pitches per octave, specifies an equal temperament. Currently, only 12-TET is supported.")

(defun common-pcs? (chord1 chord2 &optional n)
  "Returns true if `chord1' and `chord2' have common pitch classes.

  If `n' is set, only the first `n' pitch classes of the chords are taken into acount."
  (let ((chord1-firsts (if n
			   (first-n (remove-duplicates (pitch->pc chord1) :from-end T) n)
			   chord1))
	(chord2-firsts (if n
			   (first-n (remove-duplicates (pitch->pc chord2) :from-end T) n)
			   chord2)))
    (intersection (pitch->pc chord1-firsts) (pitch->pc chord2-firsts))))

; (common-pcs? '(60 64 67) '(59 62 67))

(defun common-pcs-size (chord1 chord2 &optional n)
  "Returns the number of common pitch classes between`chord1' and `chord2'.

  If `n' is set, only the first `n' pitch classes of the chords are taken into acount."
  (length (common-pcs? chord1 chord2 n)))

; (common-pcs-size '(60 64 67) '(59 62 67)) ; 1
; (common-pcs-size '(60 64 67) '(9 0 4)) ; 2
; (common-pcs-size '(60 64 67) '(62 66 69)) ; 0


(defun get-root-pc (chord)
  "Return the pitch class of the first pitch in chord."
  (pitch->pc (first chord)))

;;; TODO: If n is set, shall I test whether root of chord2 is not member of first n PCs, or of any PCs of chord1, or should this be user-controllable? The second option is more restrictive, and for large chords this option can be too restrictive.
(defun ascending-progression? (chord1 chord2 &optional n)
  "Returns true if the progression from `chord1' to `chord2' is ascending (or strong): `chord1' and `chord2' have common pitch classes, but the root of `chord2' does not occur in the set of pitch classes of `chord1'.

  Such a definition is less restrictive than Schoenberg's original guidelines (e.g., a root progression by a step upwards into a 7th chord also counts as an ascending progression here). Therefore, the rule supports the additonal argument `n': only the first n pitches of `chord1' and `chord2' are taken into account, if this argument is set.

  Example:

  By default, a root progression by a step upwards into a 7th chord also counts as an ascending progression.
  ;; (ascending-progression? '(60 64 67) '(62 66 69 72)) ; T

  That is not the case, if only the first 3 district pitch classes are taken into account, which can be set with `n'. Note that the order of the pitches (and thus pitch classes) matters here -- the seventh is the 4th pitch in the chord. If you do not want certain chord pitches to be taken into account, you have to make sure that these pitches occur above the threshold `n'.
  ;; (ascending-progression? '(60 64 67) '(62 66 69 72) 3) ; nil
"
  (let ((chord1-firsts (if n
			   (first-n (remove-duplicates (pitch->pc chord1) :from-end T) n)
			   chord1))
	(chord2-firsts (if n
			   (first-n (remove-duplicates (pitch->pc chord2) :from-end T) n)
			   chord2)))
    (and
     ;;; TODO: If n is set, shall I test whether root of chord2 is not member of first n PCs, or of any PCs of chord1, or should this be user-controllable? The second option is more restrictive.
     ;; (not (member (get-root-pc chord2) (pitch->pc chord1)))
     (not (member (get-root-pc chord2) (pitch->pc chord1-firsts)))
     (common-pcs? chord1-firsts chord2-firsts))))

; (ascending-progression? '(60 64 67) '(67 71 74)) ; nil
; (ascending-progression? '(67 71 74) '(60 64 67)) ; T
; (ascending-progression?  (pitch->pc '(77 81 83 84 87)) (pitch->pc '(79 83 85 86 89)) 3)


;;; TODO: I might add an argument n here as well, like in ascending-progression?, but ascending-progression? with the argument n likely does not need ascending-progression*? anymore...
(defun ascending-progression*? (chord1 chord2)
  "More strict variant of ascending-progression?: Returns true if root of `chord2' does not occur in the set of pitch classes of `chord1', but the root of `chord1' does occur in `chord2'. 
   Note: This definition is still less restrictive that Schoenberg's original guidelines (e.g., a root progression by a step upwards into a 7th chord also counts as an ascending progression here)."
  (and
   (not (member (get-root-pc chord2) (pitch->pc chord1)))
   (member (get-root-pc chord1) (pitch->pc chord2))))

; (ascending-progression*? '(60 64 67) '(67 71 74)) ; nil
; (ascending-progression*? '(67 71 74) '(60 64 67)) ; T

(defun descending-progression? (chord1 chord2 &optional n)
  "Returns true if the progression from `chord1' to `chord2' is descending (or weak): a non-root pitchclass of `chord1' is root in `chord2'.

  If `n' is set, only the first `n' pitches of `chord1' are taken into acount when testing membership of the root of `chord2' in `chord1'."
  (let ((chord1-firsts (if n
			   (first-n (remove-duplicates (pitch->pc chord1) :from-end T) n)
			   chord1)))
    (and
     (member (get-root-pc chord2) (pitch->pc chord1-firsts))
     (/= (get-root-pc chord1) (get-root-pc chord2)))))

; (descending-progression? '(60 64 67) '(67 71 74)) ; T
; (descending-progression? '(67 71 74) '(60 64 67)) ; nil

(defun superstrong-progression? (chord1 chord2 &optional n)
  "Returns true if the progression from `chord1' to `chord2' is superstrong in a Schoenbergian sense (cf. Harmonielehre): `chord1' and `chord2' have no common pitch classes.

  If `n' is set, only the first `n' pitch classes of the chords are taken into acount."
  (not (common-pcs? chord1 chord2 n)))

; (superstrong-progression? '(60 64 67) '(67 71 74)) ; nil
; (superstrong-progression? '(60 64 67) '(62 66 69)) ; T

(defun constant-progression? (chord1 chord2)
  "Returns true if two chords/scales `chord1' and `chord2' have a common root but their pitchclasses might differ. This case is ommited by Schoenberg discussing root progressions."
  (= (get-root-pc chord1) (get-root-pc chord2)))

; (constant-progression? '(60 64 67) '(67 71 74)) ; nil
; (constant-progression? '(60 64 67) '(60 64 67)) ; T


(defun progression-strength (chord1 chord2)
  "Expects two chords/scales `chord1' and `chord2', and returns and integer n expressing the 'strength' of the harmonic progression.

   More specifically, n can be used to distinguish between the following cases.
   - n = 0: `chord1' and `chord2' share the same root 
   - 0 < n < `*pitches-per-octave*': descending progression.
   - `*pitches-per-octave*' < n < `*pitches-per-octave*' * 2: ascending progression.
   - n = `*pitches-per-octave*' * 2: superstrong progression.

   Within the two categories descending and ascending progression, n is rated depending on the number of common pitch classes between `chord1' and `chord2' and the number of pitch classes of `chord2'. The progression strength is higher if a progression shares less pitch classes, and if `chord2' has less pitch classes. 

  Examples (`*pitches-per-octave*' is always 12):

  Two equal chords (constant-progression)
  (progression-strength '(60 64 67) '(60 64 67))
  => 0

  A descending progression where the triads `chord1' and `chord2' share two pitch classes. 
  (progression-strength '(0 4 7) '(4 7 11))
  => 4

  A descending progression where `chord2' is a tetrad, and `chord1' and `chord2' share two pitch classes.
  (progression-strength '(0 4 7) '(4 7 11 2))
  => 6

  A descending progression that shares a single pitch class, and `chord2' is a triad. 
  (progression-strength '(60 64 67) '(67 71 74))
  => 8

  A descending progression where `chord2' is a tetrad, and `chord1' and `chord2' share a single pitch class.
  (progression-strength '(60 64 67) '(67 71 74 77))
   => 9

  An ascending progression where `chord2' is a triad and `chord1' and `chord2' share two pitch classes.
  (progression-strength '(0 4 7) '(9 0 4))
  => 16

  etc.
"
  (let* ((descending? (descending-progression? chord1 chord2))
	 (ascending? (ascending-progression? chord1 chord2))
	 (superstrong? (superstrong-progression? chord1 chord2))
	 (chord2-size (length chord2))
	 (common-pcs-strength (- *pitches-per-octave*				 
				 (* (common-pcs-size chord1 chord2)
				    (/ *pitches-per-octave* chord2-size)))))
    (+ (* (boolean->int descending?) common-pcs-strength)
       (+ (* *pitches-per-octave* (boolean->int ascending?))
	  (* (boolean->int ascending?) common-pcs-strength)
       (* (boolean->int superstrong?) *pitches-per-octave* 2)))))


(defun resolve-descending-progression (&key
					 (allow-repetition nil)
					 (allow-interchange-progression nil)
					 (chord-voice 1) 
					 (n nil)
					 (rule-type :true/false) ; options: :true/false :heur-switch
					 (weight 1))
  "Rule that constrains a chord progression according to Schoenberg's recommendation. For any three successive chords/scales, if the first two chords form a descending progression, then the progression from the first to the third chord forms a strong progression (so the middle chord is quasi a 'passing chord'). Also, the last chord/scale pair forms always a strong progression.
  
  Args: 
  - allow-interchange-progression (Boolean): If true, then mere interchange progressions (e.g., I V I), are permitted as well. In any case, no two descending progressions must follow each other.
  - allow-repetition (Boolean): If true, two consecutive chords can have the same root. 
  - chord-voice (int): the voice representing the underlying chord.
  - n (int): only the first n pitch classes of chords are taken into account, if this argument is set.

  Music representation convention: 
  - A chord/spectrum/scale is represented as a list of pitches or pitch classes
  - Pitches are represented as MIDI note numbers, pitch classes as an integers between 0 and 11 (currently limited to 12-TET)
  - The root of a chord/spectrum/scale is its first pitch (class)
"
  (append
   (list 
    (if allow-interchange-progression
	(R-pitches-one-voice
	 (lambda (c1 c2 c3)
	     (if (descending-progression? c1 c2 n)
		 (or (ascending-progression? c1 c3 n)
		     (constant-progression? c1 c3))))
	 chord-voice
	 :pitches
	 rule-type
	 weight)
	(R-pitches-one-voice
	 (lambda (c1 c2 c3)
	     (if (descending-progression? c1 c2 n)
		 (ascending-progression? c1 c3 n)))
	 chord-voice
	 :pitches
	 rule-type
	 weight)))
   (when (not allow-repetition)
     (list 
      (R-pitches-one-voice
       (lambda (c1 c2)
	   (not (constant-progression? c1 c2)))
       chord-voice
       :pitches
       rule-type
       weight)))))


(defun ascending-progression (&key
				(chord-voice 1)
				(n nil)
				(rule-type :heur-switch) ; options: :true/false :heur-switch
				(weight 1))
  "Rule that constrains consecutive chords to an ascending progression, where  (see documentation of `ascending-progression?'). By default, this is an heuristic rule.

  Args: 
  - chord-voice (int): the voice representing the underlying chord.
  - n (int): only the first n pitch classes of chords are taken into account, if this argument is set.
"
  (R-pitches-one-voice 
   (lambda (c1 c2)
       (ascending-progression? c1 c2 n))
   chord-voice
   :pitches
   rule-type
   weight))

; (ascending-progression)


(defun schoenberg-progression-rule (&key
				      (progression '(:resolve-descending-progression)) ; options: :ascending, (:resolve-descending-progression &rest args), :harmonic-band, :common-pcs
				      (chord-voice 1) 
				      (n nil)
				      (rule-type :heur-switch) ; options: :true/false :heur-switch
				      (weight 1)) 
  "[Convenience constraint] Constraints the chord root progression of consecutive chords, but different values of `progression' set different variants of Schoenbergs rule set. Supported values for `progression' are as follows.

   - :ascending - only ascending chord progressions are permitted
   - (:resolve-descending-progression &rest args) - descending progressions are resolved (arguments to rule resolve-descending-progression can be given as further values in this list)
   - :harmonic-band - consecutive chords must share common pitch classes
   - :common-pcs - consecutive chords must share common pitch classes

  Args: 
  - chord-voice (int): the voice representing the underlying chord.
  - n (int): only the first n pitch classes of chords are taken into account, if this argument is set.

  Music representation convention: 
  - A chord/spectrum/scale is represented as a list of pitches or pitch classes
  - Pitches are represented as MIDI note numbers, pitch classes as an integers between 0 and 11 (currently limited to 12-TET)
  - The root of a chord/spectrum/scale is its first pitch (class)
"
  ;; - :ascending* - only slightly more strict variation of ascending chord progressions are permitted
  (if (keywordp progression)
      (case progression
	(:ascending (ascending-progression :chord-voice chord-voice :n n :rule-type rule-type :weight weight))
	;; (:ascending* (R-pitches-one-voice
	;; 	      (lambda (c1 c2)
	;; 		  (ascending-progression*? c1 c2))
	;; 	      chord-voice
	;; 	      :pitches
	;; 	      rule-type
	;; 	      weight))
	((:harmonic-band :common-pcs) (R-pitches-one-voice
				       (lambda (c1 c2)
					   (common-pcs? c1 c2))
				       chord-voice
				       :pitches
				       rule-type
				       weight)))
      ;; rule-type is (resolve-descending-progression &rest args), but first arg is 'resolve-descending-progression
      (apply #'resolve-descending-progression (append (rest rule-type) (list :n n)))))


