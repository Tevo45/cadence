;;; -*- Mode: LISP; Syntax: Common-Lisp -*-

(defpackage 9p
  (:use future-common-lisp)
  (:import scl:*current-process*)
  (:export :9p-client
           :send
	   :new-tag)
  (:shadow :count))

(defpackage cadence
  (:use scl)
  (:import future-common-lisp:nth-value))

(defsystem cadence
    (:pretty-name "Plan 9 Interoperability Utilities"
     :short-name "Cadence"
     :default-package cadence
     :default-pathname "src:cadence;"
     :maintain-journals nil)
  (:serial "9p.lisp" (:parallel "host.lisp" "fs.lisp")))

