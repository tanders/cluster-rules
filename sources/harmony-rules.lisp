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
;;; General defs
;;;

#|
(ccl::add-box-type :gracenotes?-include-mbox 
		   ;; TODO: this is probably an unnecessary nesting of ccl::mk-menu-subview
		   `(ccl::mk-menu-subview :menu-list ,(ccl::mk-menu-subview :menu-list '(":include-gracenotes" ":exclude-gracenotes")) :value 1))

(ccl::add-box-type :gracenotes?-exclude-mbox 
		   ;; TODO: this is probably an unnecessary nesting of ccl::mk-menu-subview
		   `(ccl::mk-menu-subview :menu-list ,(ccl::mk-menu-subview :menu-list '(":include-gracenotes" ":exclude-gracenotes")) :value 1))
|#



;;;
;;; Follow harmony rules 
;;;

(defun in-harmony? (pitches)
  "The PC of (first pitches) is in the PCs of (second pitches). (first pitches) can be a chord."
  (let ((voice-pitch (first pitches))
	(scale-pitches (second pitches)))
    (if (and voice-pitch (second pitches))  ; no rests
	(let ((scale-pcs (mapcar #'(lambda (p) (mod p 12))
				 scale-pitches)))
	  (every #'(lambda (p)
		     (member (mod p 12) scale-pcs))
		 ;; handle both individual pitches and chords
		 (tu:ensure-list voice-pitch)))
	T)))

;;; only-scale-PCs 

(defun only-scale-PCs 
    (&key
       (voices 2)
       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice 
       (gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1)
       (scale-voice 0))
  "Tones (PC) in the given voice must be a member of the underlying scale (its PCs). The scale is represented as a simultaneous chord in another voice (voice 0 by default). I is either given directly to the clusterengine's pitch domain of that scale voice, or using read-harmony-file, or controlled with other constraints on that voice. 

Args: 
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
scale-voice (int, default 0): the voice representing the underlying scale.

Other arguments are inherited from r-pitch-pitch."
  (mapcar #'(lambda (voice)
	      (r-pitch-pitch #'in-harmony?
			     (list voice scale-voice)
			     '(0)
			     input-mode
			     gracenotes?
			     :pitch
			     rule-type weight))
	  (if (listp voices) voices (list voices))))


;; only-chord-PCs

(defun only-chord-PCs (&key
			 (voices 2)
			 (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice
			 (gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
			 (rule-type :true/false) ; options: :true/false :heur-switch
			 (weight 1)
			 (chord-voice 1))
  "Tones (PC) in the given voice must be a member of the underlying chord (its PCs). The chord is represented as a simultaneous chord in another voice (voice 1 by default). I is either given directly to the clusterengine's pitch domain of that scale voice, or using read-harmony-file, or controlled with other constraints on that voice. 

Args: 
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch. For example, it is possible to control whether this constraint should be applied to all notes, or only specific notes (input-mode). By default, it is applied to notes starting on a beat."
  (mapcar #'(lambda (voice)
	      (r-pitch-pitch #'in-harmony?
			     (list voice chord-voice)
			     '(0)
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
				(input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice
				(gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
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
  (mapcar #'(lambda (voice)
	      (r-pitch-pitch #'in-spectrum?
			     (list voice chord-voice)
			     '(0)
			     input-mode
			     gracenotes?
			     :pitch
			     rule-type weight))
	  (if (listp voices) voices (list voices))))



(defun long-notes-chord-PCs (&key
			       (voices 2)
			       (max-nonharmonic-dur 1/1)
			       (gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
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
  (mapcar #'(lambda (voice)
	      (r-pitch-pitch #'(lambda (p_d_offs)
				 "The PC of (first pitches) is in the PCs of (second pitches)."
				 (destructuring-bind ((pitch1 dur1 offs1) (pitch2 dur2 offs2)) p_d_offs
				   (if (and (and pitch1 pitch2) ;; no rests
					    (> dur1 max-nonharmonic-dur)) ;; main condition
				       (member (mod pitch1 12)
					       (mapcar #'(lambda (p) (mod p 12))
						       pitch2))					    
				       T)))
			     (list voice chord-voice)
			     '(0)
			     :all
			     gracenotes?
			     :p_d_offs
			     rule-type weight))
	  (if (listp voices) voices (list voices))))


;; chord-tone-before/after-rest

(defun chord-tone-before/after-rest (&key
				       (voices 2)
				       (input-mode :all) ; options: :all, :beat, :1st-beat
				       (gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
				       (rule-type :true/false) ; options: :true/false :heur-switch
				       (weight 1)
				       (chord-voice 1))
  "Tones (PC) in the given voice(s) after a rest must be a member of the underlying chord PCs.

Args: 
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch."
  (mapcar #'(lambda (voice)
	      (r-pitch-pitch #'(lambda (pitches1 pitches2)	     
				 (let* ((voice-pitch1 (first pitches1))
					(voice-pitch2 (first pitches2))
					(rest1? (null voice-pitch1)) ; 1st note is rest
					(rest2? (null voice-pitch2))) ; 2nd note is rest
				   (cond ((and rest1? rest2?) T) ; in case there are two rests in a row
					 (rest1? (PC-member voice-pitch2 (second pitches2))) ; after rest
					 (rest2? (PC-member voice-pitch1 (second pitches1))) 
					 (T T)))) ; before rest
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
	 (mapcar #'(lambda (voice)
		     (r-index-pitches-one-voice #'(lambda (pitch) 
						    (if pitch ; no rest
							(member (mod pitch 12)
								(mapcar #'(lambda (chord-pitch) (mod chord-pitch 12)) 
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
     (input-mode :all) ; options: :all, :beat, :1st-beat
     (gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
     (rule-type :true/false) ; options: :true/false :heur-switch
     (weight 1)
     (chord-voice 1))
  "Every tone (PC) that is not a chord tone (member of sim chord PCs, in voice 1 by default) is reached/left by a step of the given step size.

Args: 
step-size (int): maximum interval considered a step.
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch."
  (mapcar #'(lambda (voice)
	      (r-pitch-pitch #'(lambda (pitches1 pitches2 pitches3)
				 ;; Every pitchesN is a list of the form (chord-pitches voice-pitch) 
				 (if (and (first pitches2) (second pitches2))  ;; no rests
				     (let ((voice-pitch2 (second pitches2)))
				       (if (not (member (mod voice-pitch2 12) ; middle PC
							;; chord PCs
							(mapcar #'(lambda (p) (mod p 12)) (first pitches2))))
					   (and (if (second pitches1) ; no rest
						    (<= (abs (- (second pitches1) voice-pitch2)) step-size)
						    T)
						(if (second pitches3) ; no rest
						    (<= (abs (- voice-pitch2 (second pitches3))) step-size)
						    T))
					   T))
				     T))
			     (list chord-voice voice)
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
       (input-mode :all) ; options: :all, :beat, :1st-beat
       (gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1)
       (chord-voice 1))
  "Every tone (PC) that is not a chord tone (member of sim chord PCs, in voice 1 by default) is followed by a chord tone.

Args: 
voices (int or list of ints): the voice(s) to which this constraint is applied.

Optional args:
chord-voice (int, default 1): the voice representing the underlying chord.

Other arguments are inherited from r-pitch-pitch."
  (mapcar #'(lambda (voice)
	      (r-pitch-pitch #'(lambda (pitches1 pitches2)
				 ;; Every pitchesN is a list of the form (chord-pitches voice-pitch) 
				 (if (and (first pitches1) (first pitches2)
					  (second pitches1) (second pitches2))  ;; no rests
				     (let ((chord-pitches1 (first pitches1))
					   (voice-pitch1 (second pitches1))
					   (chord-pitches2 (first pitches2))
					   (voice-pitch2 (second pitches2)))
				       (if (not (member (mod voice-pitch1 12) 
							(mapcar #'(lambda (p) (mod p 12)) chord-pitches1)))
					   (member (mod voice-pitch2 12)
						   (mapcar #'(lambda (p) (mod p 12)) chord-pitches2))
					   T))
				     T))
			     (list chord-voice voice)
			     '(0)
			     input-mode
			     gracenotes?
			     :pitch
			     rule-type weight))
	  (if (listp voices) voices (list voices))))



;;; unequal-sim-PCs

(defun unequal-sim-PCs-aux 
    (&key
       (voices 0)
       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice
       (gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1))
  "[Quasi aux def] The PCs of the 1st given voice are unequal to the sim PCs of the remaining voices."
  (r-pitch-pitch #'(lambda (pitches)
		     (if (first pitches) ; no rest
			 (not (member (mod (first pitches) 12) 
				      (mapcar #'(lambda (p) (mod p 12)) 
					      (rest pitches))))
			 T))
		 voices
		 '(0)
		 input-mode
		 gracenotes?
		 :pitch
		 rule-type weight))


(defun unequal-sim-PCs
    (&key
       (voices 0)
       (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice
       (gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1))
  "Sim PCs in all given voices are unequal to each other.

Arguments are inherited from r-pitch-pitch.

TODO: Revise this definition -- can the interplay with unequal-sim-PCs-aux be simplified?
"
  (let* ((voices (sort voices #'<))
	 (len (length voices))
	 (rev (reverse voices)))
    (mapcar #'(lambda (i) 
		(unequal-sim-PCs-aux :voices (subseq rev i len)
				     :input-mode input-mode
				     :gracenotes? gracenotes?
				     :rule-type rule-type
				     :weight weight))
	    (arithm-ser 0 1 (- len 2)))))


;; TODO:
;; - ? Add additional input modes that not only take harmonic slices but all tones within a certain time frame into account, namely a full beat duration, a full bar duration, the beginning of a new harmony, and the whole duration of a harmony -- likely the current implementation of r-pitch-pitch does not allow for these cases.
;; - ? Improve efficiency: shall I internally also call r-pitch-pitch with subsets of voices and with a reduced PC-number? That will help the solver, but that possibly also restricts the range of possible solutions.
;; - ?? Have PC-number controlled with BPF? Possibly the current implementation of r-pitch-pitch does not allow for that. Also, that would restrict the would only work if underlying harmony allows for that, but could be useful 
;; - OK Automatically reduce PC-number internally, if the number of sim pitches is snmaller due to rests (some pitch = NIL)
(defun number-of-sim-PCs (&key
			    (PC-number 2)
			    (condition :min) ; options: :min, :equal, :max
			    (rests-mode :reduce-no) ; options: :reduce-no, :ignore
			    (voices '(0 1))
			    (timepoints '(0))
			    (input-mode :all) ; options: :all, :beat, :1st-beat, :1st-voice, :at-timepoints
			    (gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
			    (rule-type :true/false) ; options: :true/false :heur-switch
			    (weight 1))
  "Controls the number of simultaneous pitch classes. Useful, for example, to require that some underlying harmony is expressed.

Args: 
  PC-number (int): the number of the simultaneous PCs. The meaning of this setting depends on the argument condition.
  condition: Whether the number of simultaneous pitch classes should be at least the given PC-number (:min), or exactly that number (:equal), or at most that number (:max). 
  rests-mode: If set to :reduce-no, then the number of simultaneous pitch classes is subtracted from PC-number. For example, if there is only a single tone at a certain time and all other voices have rests, this rule can still be fulfilled. By contrast, if rests-mode is set to :ignore, then the remaining simultaneous pitch classes must still fullfil the condition expressed by the arguments PC-number and condition.
  voices: the list of voices to which the rule is applied.
  

Other arguments are inherited from r-pitch-pitch."
  (r-pitch-pitch #'(lambda (pitches)
		     (let ((actual-number (case rests-mode
					    (:reduce-no (- PC-number
							   (length (remove NIL pitches :test-not #'eql))))
					    (:ignore PC-number)))
			   (harm (remove NIL ;; take out rests
					 (remove-duplicates
					  (mapcar #'(lambda (p) (mod p 12)) pitches)))))
		       (if harm				  
			   (funcall (case condition
				      (:min #'>=)
				      (:equal #'=)
				      (:max #'<=))
				    (length harm) actual-number)
			   t)))
		 voices
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
       (gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
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
     (let ((sorted-voices (sort voices #'>)))
       (case combinations 
	 (:over-bass 
	  (let ((bass-voice (first sorted-voices)))
	    (mapcar #'(lambda (voice)
			(r-pitch-pitch #'rule
				       (list bass-voice voice)
				       timepoints
				       input-mode
				       gracenotes?
				       :pitch
				       rule-type weight)) 
		    (rest sorted-voices))))
	 (:consecutive-voices
	  (mapcar #'(lambda (voice1 voice2)
		      (r-pitch-pitch #'rule
				     (list voice1 voice2)
				     timepoints
				     input-mode
				     gracenotes?
				     :pitch
				     rule-type weight)) 
		  (butlast sorted-voices) (rest sorted-voices)))
	 (:all-combinations
	  (map-pairwise #'(lambda (voice1 voice2)
			    (r-pitch-pitch #'rule
					   (list voice1 voice2)
					   timepoints
					   input-mode
					   gracenotes?
					   :pitch
					   rule-type weight)) 
			sorted-voices)))))))


;; ? TODO: 
;; ? Add arg: absolution intervals or including neg? -- Do only if actually needed :)
(defun min/max-harmonic-interval
    (&key
       (voices '(0 1))
       (min-interval NIL)
       (max-interval NIL)
       (input-mode :beat) ; options: :beat, :all, :1st-beat, :1st-voice, :at-timepoints
       (combinations :consecutive-voices) ; options: :over-bass, :consecutive-voices, :all-combinations
       (gracenotes? :include-gracenotes) ; options: :include-gracenotes, :exclude-gracenotes
       (timepoints '(0))
       (rule-type :true/false) ; options: :true/false :heur-switch
       (weight 1))
  "Limit the minimum/maximum harmonic interval of simultaneous notes between given voices. 

Args: 
  voices (list of ints): the voices to constrain.
  min-interval (number or NIL): minimum interval in semitones. Ignored if NIL.
  max-interval (number or NIL): maximum interval in semitones. Ignored if NIL. 
  combinations: Controls whether to constrain only intervals between the voice with highest note number and other voices (:over-bass), between pairs of consecutive voices such as soprano-alto, alto-tenor etc. (:consecutive-voices), or between all voice combinations (:all-combinations).

Other arguments are inherited from r-pitch-pitch.
"
  (tu:flat
   (flet ((rule (pitches)
	    ;; (format T "min/max-harmonic-interval rule: pitches: ~A" pitches)
	    (let ((pitch1 (first pitches))
		  (pitch2 (second pitches)))		  
	      (if (and pitch1 pitch2) ; no rests
		  (let ((interval (abs (- pitch1 pitch2))))			   
		    (and (if min-interval 
			     (<= min-interval interval)
			     T)
			 (if max-interval 
			     (<= interval max-interval)
			     T)))
		  T))))
     (let ((sorted-voices (sort voices #'>)))
       ;; (format T "min/max-harmonic-interval: sorted-voices: ~A, combinations: ~A, rule-type:~A~%" sorted-voices combinations rule-type)
       (case combinations 
	 (:over-bass 
	  (let ((bass-voice (first sorted-voices)))
	    (mapcar #'(lambda (voice)
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
	  (mapcar #'(lambda (voice1 voice2)
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
	  (map-pairwise #'(lambda (voice1 voice2)
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
   (min/max-interval voices :max-interval max-interval :rule-type rule-type :weight weight)
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
   (min/max-interval voices :min-interval min-interval :max-interval max-interval :rule-type rule-type :weight weight)
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
	     #'(lambda (ps)
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
    (loop for pos in positions
       append (R-index-pitches-one-voice
	       #'(lambda (ps)
		   (if ps ; no rest
		       (if pc? ;;; TODO: do this test only once at the time this rule is created
			   (= (mod (first ps) 12) root-pc)
			   (= (first ps) root))
		       T))
	       (list pos)
	       chord-voice
	       :position-for-pitches
	       rule-type
	       weight))))



;; Port from Strasheela
;; - VoiceLeadingDistance VoiceLeadingDistance_Percent
;; - Schoenberg rules

;;; TODO: Revised version (possibly switched on by arg): the current version always takes the shortest path between pitch classes, and does not enforce that each PC of each chord is involved in this calculation, which leads to a lower voice leading distance overall. A better algorithm would enforce that all PCs of each chord is involved, but that might need some optimisation technique, which needs a bit of extra work to implement. Hm -- but is that still min. voice leading distance?
;; I could try to find a better voice-leading algorithm in the literature, or perhaps some general distance algorithm finding the shortest path between multiple 1D points in space? 
(defun voice-leading-distance (chord1 chord2 &optional n)
  "Returns the voice leading distance (an integer, measured in semitones) between two given chords, each a lists of MIDI note numbers. The voice leading distance is the minimal sum of intervals between chord1 and chord2. The voice-leading distance is directionless in the sense that regardless whether a voice moves up or down, always the smaller interval is taken into account. The lower the voice leading distance, the more 'smooth' is the harmonic progression (a chord repetition is quasi most smooth).

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
	   (mapcar #'(lambda (pc2)
		       (apply #'min
			      (mapcar #'(lambda (pc1)
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
						    (rule-type :true/false) ; options: :true/false :heur-switch
						    (weight 1))
  "Constrains the voice leading distance of consecutive chords in chord-voice to be at most max-distance. See the documentation of `voice-leading-distance' for details on what the voice leading distance is."
  (R-pitches-one-voice #'(lambda (chord1 chord2)
			   (<= (voice-leading-distance chord1 chord2)
			       max-distance))
		       chord-voice
		       :pitches
		       rule-type
		       weight))
; (limit-voice-leading-distance 3)



