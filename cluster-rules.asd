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

(asdf:defsystem cluster-rules
  :description "Rules defined for the cluster-engine by Orjan Sandred." 
  :author "Torsten Anders"
  :version "0.001"
  :default-component-class ccl::pwgl-source-file
  :serial t ;; the dependencies are linear.
  :components ((:file "sources/package")
	       (:file "sources/utils")
	       (:file "sources/score")
	       ;; (:file "sources/generic-rules")
	       (:file "sources/rhythm-rules")
	       (:file "sources/melody-rules")
	       (:file "sources/harmony-rules")
	       (:file "sources/counterpoint-rules")
	       (:file "sources/export")
	       (:file "sources/menus"))
  :depends-on ("cluster-engine" ; "ta-utilities"
	       ))

