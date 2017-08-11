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
(require :tot)
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
 (cr::cluster-engine
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
 (cr::cluster-engine 
  20 
  () ; no rules
  '((3 4) (2 4)) 
  '(((1/4) (1/8))
    ((60))))
 )


