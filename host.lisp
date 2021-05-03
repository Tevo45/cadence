;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: CADENCE -*-

(defflavor plan9-host ()
  (net:host-unix-mixin fs:active-pathname-host net:host))

(defmethod (:pathname-flavor plan9-host) ()
  'fs:unix-pathname)

(defprop :plan9 plan9-host net:host-type-flavor)

(compile-flavor-methods plan9-host)
