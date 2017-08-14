;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

;;; *************************************************************
;;; Copyright (C) 2017 Torsten Anders (torsten.anders@beds.ac.uk) 
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 3
;;; of the License, or (at your option) any later version.
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; *************************************************************

; (require :ta-utilities)
; (require :tot)
; (asdf:load-system :tot)
(require :cluster-rules)

(in-package :om) ;; Opusmodus package (not OpenMusic)

#| ; would cause conflicts
(use-package :cluster-engine)
(use-package :cluster-rules)
|#



;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Testing cluster-rules 
;;;

;;;
;;; rhythmic rules
;;;

;; minimal demo example
(preview-cluster-engine-score
 (cr:cluster-engine
  20 
  ;; rules
  (ce::rules->cluster (cr::no-syncopation :metric-structure :beats)
                      (cr::no-repetition))
  '((3 4)) 
  '(((1/4) (1/8))
    ((60) (61))))
 )

;;; BUG: metric domain of multiple values seemingly always results in static meter
(preview-cluster-engine-score
 (cr:cluster-engine 
  20 
  () ; no rules
  '((3 4) (2 4)) 
  '(((1/4) (1/8))
    ((60))))
 )


;;;
;;; Profile test
;;;

;; cr:follow-profile-hr

(setf my-melody '((q c4 cs4 stacc fs4 -q) 
                  (e f4 b4 tr2 c4 -h e g4) 
                  (q d4 e4 f4 g4) 
                  (h a4 -h)))

;; OK: follow pitch sequence of input melody results in same melody
;; ... though solution is repeating melody
(preview-cluster-engine-score
 (cr:cluster-engine 
  (count-notes my-melody)
  ;; rules
  (ce::rules->cluster 
   (cr:follow-profile-hr my-melody :voices 0 :mode :pitch))
  '((4 4)) 
  `((,(flatten (omn :length my-melody))) ; set rhythm as rhythmic motif
    ,(mclist (gen-integer 60 84))))
 )


;; OK: test args :n and :start 
(preview-cluster-engine-score
 (cr:cluster-engine 
  20 
  ;; rules
  (ce::rules->cluster 
   (cr:follow-profile-hr my-melody :voices 0 :mode :pitch :n 4 :start 1)
   )
  '((4 4)) 
  `((,(flatten (omn :length my-melody)))
    ,(mclist (gen-integer 60 84))))
 )


;;; BUG: rhythm profile not working
(preview-cluster-engine-score
 (cr:cluster-engine 
  20 
  ;; rules
  (ce::rules->cluster 
   (cr:follow-profile-hr my-melody :mode :rhythm)
   )
  '((4 4)) 
  '(((1/2) (1/4) (1/8) (-1/2) (-1/4) (-1/8))
    ((60))))
 )



;; whole-tone ascending scale
(setf my-profile '(q c4 d4 e4 fs4 gs4 as4 c5 d5))

;; OK: using :constrain :intervals 
(preview-cluster-engine-score
 (cr:cluster-engine 
  20 
  ;; rules
  (ce::rules->cluster 
   (cr:follow-profile-hr my-profile :voices 0 :mode :pitch :constrain :intervals)
   )
  '((4 4)) 
  `((,(flatten (omn :length my-profile)))
    ,(mclist (gen-integer 60 84))))
 )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; More complex example harmonically transforming a melody (OMN expression) -- ported from Cinderella harmony patch
;;;

;; Input melody to revise -- replace with whatever melody
(setf galliard-mel 
      '((q e5 f slap+stacc -h q eb5 slap+stacc -h) 
        (q cs5 f slap+stacc eb5 mp ord f5) (q. g5 e gs5 q c6) (h bb5 q a5) (q fs5 - e d5 cs5) (h a5 q b5) (q. cs5 e bb5 q d6) (q cs6 d6 g5) (h e5 q cs6) (q g5 f slap+stacc a5 mp ord bb5) (q. c6 e cs6 q c6) (e b5 a5 q f5 e5) (h b5 q c6) (q eb5 f slap+stacc -e b5 mp ord d6 cs6) (q. eb6 e b5 q e5) (q gs5 d6 b5) (h gs5 q fs5)))

(setf galliard-harmonies
      '((h. c4eb4g4a4)
        (w. c4eb4g4a4)        
        (w. f4g4a4c4) (w. cs4f4g4b4) (w. b4eb4g4a4) (w. f4a4c4eb4) (w. eb4g4b4cs4) (w. a4cs4eb4g4) (w. cs4f4g4c4) (h. g4b4cs4f4)))

(setf galliard-harmonic-rhythm
      (flatten (omn :length galliard-harmonies)))

;; test
(preview-score  
 (list :flt galliard-mel
       :harm galliard-harmonies))

;; ... solution is repeating melody
(preview-cluster-engine-score
 (let ((time-sigs (PWGL-time-signatures 
                   (get-time-signature galliard-harmonies))))
   (cr:cluster-engine 
    (count-notes galliard-mel)
    ;; rules
    (let ((mel-voice 2))
    (ce::rules->cluster 
     (cr:follow-profile-hr 
      galliard-mel :voices mel-voice :mode :pitch :constrain :profile)
     (cr:follow-profile-hr 
      galliard-mel :voices mel-voice :mode :pitch :constrain :intervals)
     (ce:r-predefine-meter time-sigs)
     ;; more rules
     (cr:only-scale-pcs :voices mel-voice :input-mode :all 
                        :scale-voice 0)
     (cr:only-chord-pcs :voices mel-voice :input-mode :beat ; :1st-beat
                        :chord-voice 1) 
     (cr:long-notes-chord-pcs :voices mel-voice :max-nonharmonic-dur 1/4)
     (cr:stepwise-non-chord-tone-resolution
      :voices mel-voice :input-mode :all :step-size 3)
     (cr:chord-tone-before/after-rest :voices mel-voice :input-mode :all)
     (cr:chord-tone-follows-non-chord-tone :voices mel-voice :input-mode :all)
     (cr:no-repetition :voices mel-voice :window 3)
     (cr:resolve-skips :voices mel-voice :resolution-size 4)
     ))
    ;; meter domain
    (remove-duplicates time-sigs :test #'equal) 
    ;; voice domains
    `(
      ; scale rhythm
      (,(length-merge galliard-harmonic-rhythm))
      ; scale pitches
      (,(pitch-to-midi '((c4 cs4 ds4 f4 g4 a4 b4))))
      ; harmonic rhythm for chords 
      (,galliard-harmonic-rhythm)
      ; harmony: chords
      (,(pitch-to-midi
         (flatten (omn :pitch galliard-harmonies))))
      ; voice rhythm (input rhythm as motif) 
      (,(flatten (omn :length galliard-mel))) 
      ,(mclist (gen-integer 60 84))
      ))
   ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The more complex example above abstracted / cleaned up
;;;

;; Input melody to revise -- replace with whatever melody
;; format: headerless score
(setf galliard-mel 
      '(:|1| 
         ((q e5 f slap+stacc -h q eb5 slap+stacc -h) 
          (q cs5 f slap+stacc eb5 mp ord f5) (q. g5 e gs5 q c6) (h bb5 q a5) (q fs5 - e d5 cs5) (h a5 q b5) (q. cs5 e bb5 q d6) (q cs6 d6 g5) (h e5 q cs6) (q g5 f slap+stacc a5 mp ord bb5) (q. c6 e cs6 q c6) (e b5 a5 q f5 e5) (h b5 q c6) (q eb5 f slap+stacc -e b5 mp ord d6 cs6) (q. eb6 e b5 q e5) (q gs5 d6 b5) (h gs5 q fs5))))

(setf polyphonic-score
      `(:v1 ,(second galliard-mel)
        :v2 ,(append '((-h.)) (butlast (second galliard-mel) 3))
        :v3 ,(append '((-h.) (-w.)) (butlast (second galliard-mel) 4))))

; (preview-score polyphonic-score)

(setf galliard-harmonies
      '((h. c4eb4g4a4)
        (w. c4eb4g4a4)        
        (w. f4g4a4c4) (w. cs4f4g4b4) (w. b4eb4g4a4) (w. f4a4c4eb4) (w. eb4g4b4cs4) (w. a4cs4eb4g4) (w. cs4f4g4c4) (h. g4b4cs4f4)))

#|
(setf galliard-harmonic-rhythm
      (flatten (omn :length galliard-harmonies)))
|#

(setf galliard-scales
      (append (length-merge (flatten (omn :length galliard-harmonies))) 
              (chordize '(c4 cs4 ds4 f4 g4 a4 b4))))


; (length-merge (length-rest-invert (flatten (omn :length galliard-mel))))

#|
(preview-score 
 (mix-parts polyphonic-score
            `(:c ,galliard-harmonies
              :s ,(list galliard-scales))))
|#

#|

;;; tmp for debugging
(setf score galliard-mel)

(mclist (gen-integer 60 84))

(mclist (apply #'gen-integer (mapcar (lambda (p) (+ p 60)) (find-ambitus (get-part-omn :|1| score)))))
|#


;;; TODO: insert all velo and articulations in score into result
;; ... solution is repeating melody
(defun revise-score-harmonically (score harmonies scales
                                        ;;; TODO: consider allowing to overwrite pitch domain
                                        ; &key pitch-domains
                                        ;;; TODO: consider allowing to specify `other rules' as argument 
                                        )
  ;;; TODO: extend doc
  "CSP transforming the input `score' such that it follows the underlying harmony specified. 
  The rhythm of the input score is left unchanged. The pitches follow the melodic and intervallic profile of the input voices/parts, and various additional constraints are applied.
  
  Args:
  - score (headerless score)
  - harmonies (OMN expression): chords expressing the underlying harmony such as the harmonic rhythm and chord changes
  - scales (OMN expression): chords expressing the underlying harmony such as the rhythm of scales and scale changes 
  - pitch-domains (property list): specifying a pitch domain in the Cluster Engine format for every part in score, using the same instrument ID. If no domain is specified for a certain part then a chromatic domain of the ambitus of the input part is automatically generated.
  "
  (cluster-engine-score
   (let* ((parts (get-parts score))
          (first-part (first parts))
          (time-sigs (PWGL-time-signatures 
                      (get-time-signature first-part))))
     (cr:cluster-engine 
      ;;; TMP: use note no of first part, not part with most notes
      (count-notes first-part) 
      ;; rules
      (let (;; position of all voices in score starting from 2 after scales and chords
            (voice-ids (gen-integer 2 (+ (length (get-instruments score)) 1))))
        (ce::rules->cluster 
         (cr:follow-profile-hr parts 
                               :voices voice-ids :mode :pitch :constrain :profile)
         (cr:follow-profile-hr parts 
                               :voices voice-ids :mode :pitch :constrain :intervals)
         (ce:r-predefine-meter time-sigs)
         ;; more rules
         (cr:only-scale-pcs :voices voice-ids :input-mode :all 
                            :scale-voice 0)
         (cr:only-chord-pcs :voices voice-ids :input-mode :beat ; :1st-beat
                            :chord-voice 1) 
         (cr:long-notes-chord-pcs :voices voice-ids :max-nonharmonic-dur 1/4)
         (cr:stepwise-non-chord-tone-resolution
          :voices voice-ids :input-mode :all :step-size 3)
         (cr:chord-tone-before/after-rest :voices voice-ids :input-mode :all)
         (cr:chord-tone-follows-non-chord-tone :voices voice-ids :input-mode :all)
         (cr:no-repetition :voices voice-ids :window 3)
         (cr:resolve-skips :voices voice-ids :resolution-size 4)
         ))
      ;; meter domain
      (remove-duplicates time-sigs :test #'equal) 
      ;; voice domains
      `(
        ; scale rhythm
        (,(flatten (omn :length scales)))
        ; scale pitches
        (,(pitch-to-midi (flatten (omn :pitch scales))))
        ; harmonic rhythm for chords 
        (,(flatten (omn :length harmonies)))
        ; harmony: chords
        (,(pitch-to-midi (flatten (omn :pitch harmonies))))
        ,@(mappend #'(lambda (part)
                       `( 
                         ;; rhythm domain: predefined motif
                         (,(flatten (omn :length part)))
                         ;; pitch domain
                         ,(mclist
                           (apply #'gen-integer 
                                  (mapcar #'(lambda (p) (+ p 60)) 
                                          (find-ambitus part))))
                         ))
                   parts)
        ))
     )))

(setf *default-preview-score-instruments*
      '(;; silent harmony -- :volume 0
        :|1| (:program 'violin :sound 'gm :channel 16 :volume 0)
        :|2| (:program 'violin :sound 'gm :channel 16 :volume 0)
        :|3| (:program 'violin :sound 'gm :channel 1)
        :|4| (:program 'violin :sound 'gm :channel 1)
        :|5| (:program 'violin :sound 'gm :channel 1))
      )


(preview-score 
 (revise-score-harmonically galliard-mel galliard-harmonies galliard-scales))


(preview-score 
 (revise-score-harmonically polyphonic-score galliard-harmonies galliard-scales))
