;;;; -*- Lisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; This is hokuspokus - a program that does feature transformation
;;;; for machine learning tasks.
;;;; 
;;;; Copyright (C) 2005 Mark Pflueger <mark@pfluegerworld.de>
;;;;
;;;; This program is free software; you can redistribute it and/or
;;;; modify it under the terms of the GNU General Public License
;;;; as published by the Free Software Foundation; either version 2
;;;; of the License, or (at your option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program; if not, write to the Free Software
;;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;;;; 02110-1301, USA. 
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage hokuspokus-system
  (:use :common-lisp :asdf))

(in-package hokuspokus-system)

(defsystem hokuspokus
    :description "feature transformation for machine learning tasks"
    :version "0.4"
    :author "Mark Pflueger <mark@pfluegerworld.de>"
    :licence "GNU General Public License"
    :components ((:doc-file "README.md")
		 (:doc-file "LICENSE")
		 (:file "packages")
		 (:file "utils" :depends-on ("packages"))
		 (:file "term-simplifier" :depends-on ("packages"))
                 (:file "data" :depends-on ("utils" "term-simplifier"))
		 (:file "constraints" :depends-on ("data"))
		 (:file "transform" :depends-on ("constraints"))
		 (:file "eq-generator" :depends-on ("transform"))))
