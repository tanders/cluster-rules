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

(in-package :asdf-user)

(asdf:defsystem cluster-rules
  :description "Rules defined for the cluster-engine by Orjan Sandred." 
  :author "Torsten Anders <torsten.anders@beds.ac.uk>"
  :licence "GNU General Public License, Version 3"
  :version "0.1"
  ;; :default-component-class ccl::pwgl-source-file
  :serial t ;; the dependencies are linear.
  :components ((:file "sources/package")
	       (:file "sources/utils")
	       ;; depends on ENP
	       ;; (:file "sources/score")
	       (:file "sources/rhythm-rules")
	       (:file "sources/melody-rules")
	       (:file "sources/harmony-rules")
	       (:file "sources/counterpoint-rules")
	       (:file "sources/export")
	       ;; (:file "sources/menus")
	       )
  :depends-on (
               "ta-utilities" ;; e.g., tu:dx->x, tu:mat-trans, but so far only in #+opusmodus code (melody-rules.lisp)
	       "fenv"
	       "cluster-engine" 
               ;; #+opusmodus "tot" ; Torsten's Opusmodus Tools -- would result in circular dependencies
	       ;;; NOTE: This library (OpenMusic without the visual programming interface) is still unfinished and not shared. Only a few functions depend on it. I could try to make this some conditional library to load, but for now I better leave it simply out.
	       ;; "pw"
	       )
  :in-order-to ((test-op (test-op #:cluster-rules/tests))))


(defsystem #:cluster-rules/tests
  :depends-on (:cluster-rules :FiveAM)
  :components ((:module "tests"
			::components ((:module "unit-tests"
					       :serial t
					       :components ((:file "package")
							    (:file "test-drafts")
							    )))))
  :perform (test-op (o s)
		    (uiop:symbol-call :fiveam '#:run!
				      (uiop:find-symbol* '#:cluster-rules-tests :cluster-rules/tests))))
