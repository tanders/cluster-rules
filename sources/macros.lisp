;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; -*-

(in-package :cluster-rules)

(defmacro when* (bool &body forms)
  "If the first argument is true, one or more forms are executed in an implicit PROGN and the result of the last form is returned. Otherwise, T is returned."
  `(if ,bool (progn ,@forms) T))
#|
(when* T 1)
(when* NIL 1)
|#

