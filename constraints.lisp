;;;; -*- Lisp -*-

;;; Constraints
(in-package :hokuspokus)

(defun range-constr (op features)
  (case (operator-function op)
    ((sqrt fac) (not (find-if #'minusp
			(feature-datavec (car features)))))
    ((/ mod) (not (find-if #'zerop
			   (feature-datavec (second features)))))
    ((asin acos) (not (find-if #'(lambda (n) (> (abs n) 1))
			       (feature-datavec (car features)))))
    ((my-and my-or my-not my-xor my-nxor)
     (every #'(lambda (n) (or (= n 1.0) (= n 0.0))) (feature-datavec (car features))))
    (log (every #'(lambda (n) (not (and (minusp n) (zerop n))))
		(feature-datavec (car features))))
    (otherwise t)))
  
;; checking heavily constrained system

(defun robot-constraint ()
  (constraints-insert #'robot-constr)
  (inheritance-insert #'robot-inherit)
  (feature-properties-insert :unit "x-pos" (feature-find "x" *feature-space*))
  (feature-properties-insert :unit "x-pos" (feature-find "xg" *feature-space*))
  (feature-properties-insert :unit "y-pos" (feature-find "y" *feature-space*))
  (feature-properties-insert :unit "y-pos" (feature-find "yg" *feature-space*))
  (feature-properties-insert :unit "angle" (feature-find "phi" *feature-space*))
  (feature-properties-insert :unit "angle" (feature-find "phig" *feature-space*)))

(defun robot-constr (op features)
  (case (operator-function op)
    ;; operations allowed only for operators with same units
    (- 
     (let ((unit (feature-properties-value :unit (car features))))
       (every #'(lambda (f) (string-equal (feature-properties-value :unit f) unit))
	      features)))
    (+
     (let ((unit (feature-properties-value :unit (car features))))
       (and
	(not (string-equal (feature-name (car features))
			   (feature-name (cadr features))))
	(every #'(lambda (f) (string-equal (feature-properties-value :unit f) unit))
	       features))))
    ;; two different operators and angles required
    ;; also only valid if both formuals have the same length
    ;; e.g. (min (- x y) (+ x y))
    (min
     (and
      (equal (length (feature-name (car features)))
	     (length (feature-name (cadr features))))
      (not (string-equal (feature-name (car features))
			 (feature-name (cadr features))))
      (every #'(lambda (f) (string-equal (feature-properties-value :unit f) "ABS (angle)"))
	     features)))
    ;; atan only for dist-y/dist-x
    (atan
      (and (string-equal (feature-properties-value :unit (car features)) "dist-y")
	   (string-equal (feature-properties-value :unit (cadr features)) "dist-x")))
    ;; abs only if some values negative
    (abs (some #'minusp (feature-datavec (car features))))
    ;; angle-sub only for two angles
    (angle-sub (every #'(lambda (f) (string-equal (feature-properties-value :unit f) "angle"))
		      features))
    (otherwise t)))

(defun robot-inherit (op features)
  (case (operator-function op)
    ;; for these operators units have to be the same and they inherit
    ;; that unit without any change
    ((+ min) (cons :unit (feature-properties-value :unit (car features))))
    ;; - indicates distance
    (- (cond
	 ((string-equal (feature-properties-value :unit (car features)) "x-pos")
	  (cons :unit "dist-x"))
	 ((string-equal (feature-properties-value :unit (car features)) "y-pos")
	  (cons :unit "dist-y"))
	 (t (cons :unit "angle"))))
    ;; atan yields an angle
    (atan (cons :unit "angle"))
    ;; angle-sub yields an angle
    (angle-sub (cons :unit "angle"))
    ((/) (if (string-equal (feature-properties-value :unit (car features))
			   (feature-properties-value :unit (cadr features)))
	     (cons :unit :none)
	     (cons :unit (format nil "~A ~A" (operator-function op)
				 (mapcar #'(lambda (f) (feature-properties-value :unit f))  features)))))
    ((*)  (cond
	    ((eq (feature-properties-value :unit (car features)) :none)
	     (cons :unit (feature-properties-value :unit (cadr features))))
	    ((eq (feature-properties-value :unit (cadr features)) :none)
	     (cons :unit (feature-properties-value :unit (car features))))
	    (t
	     (cons :unit (format nil "~A ~A" (operator-function op)
				 (mapcar #'(lambda (f) (feature-properties-value :unit f))  features))))))
    (otherwise
     (cons :unit (format nil "~A ~A" (operator-function op)
			 (mapcar #'(lambda (f) (feature-properties-value :unit f))  features))))))

(defun units-constraint ()
  (constraints-insert #'units-constr)
  (inheritance-insert #'units-inherit)
  (feature-properties-insert :unit "meter" (feature-find "x" *feature-space*))
  (feature-properties-insert :unit "meter" (feature-find "xg" *feature-space*))
  (feature-properties-insert :unit "meter" (feature-find "y" *feature-space*))
  (feature-properties-insert :unit "meter" (feature-find "yg" *feature-space*))
  (feature-properties-insert :unit "angle" (feature-find "phi" *feature-space*))
  (feature-properties-insert :unit "angle" (feature-find "phig" *feature-space*)))

(defun units-constr (op features)
  (case (operator-function op)
    ((+ -) 
     (let ((unit (feature-properties-value :unit (car features))))
       (every #'(lambda (f) (string-equal (feature-properties-value :unit f) unit)) features)))
    (otherwise t)))

(defun units-inherit (op features)
  (case (operator-function op)
    ((+ -) (cons :unit (feature-properties-value :unit (car features))))
    ((/) (if (string-equal (feature-properties-value :unit (car features))
			   (feature-properties-value :unit (cadr features)))
	     (cons :unit :none)
	     (cons :unit (format nil "~A ~A" (operator-function op)
				 (mapcar #'(lambda (f) (feature-properties-value :unit f))  features)))))
    ((*)  (cond
	    ((eq (feature-properties-value :unit (car features)) :none)
	     (cons :unit (feature-properties-value :unit (cadr features))))
	    ((eq (feature-properties-value :unit (cadr features)) :none)
	     (cons :unit (feature-properties-value :unit (car features))))
	    (t
	     (cons :unit (format nil "~A ~A" (operator-function op)
				 (mapcar #'(lambda (f) (feature-properties-value :unit f))  features))))))
    (otherwise
     (cons :unit (format nil "~A ~A" (operator-function op)
			 (mapcar #'(lambda (f) (feature-properties-value :unit f))  features))))))

(defun axis-constraint ()
  (constraints-insert #'axis-constr)
  (inheritance-insert #'axis-inherit)
  (feature-properties-insert :axis :x (feature-find "x" *feature-space*))
  (feature-properties-insert :axis :x (feature-find "xg" *feature-space*))
  (feature-properties-insert :axis :y (feature-find "y" *feature-space*))
  (feature-properties-insert :axis :y (feature-find "yg" *feature-space*)))

(defun axis-constr (op features)
  (let ((axis (feature-properties-value :axis (car features))))
    (case (operator-function op)
      ((+ -) (every #'(lambda (f) (eq (feature-properties-value :axis f) axis)) features))
      (otherwise t))))

(defun axis-inherit (op features)
  (let ((axis (feature-properties-value :axis (car features))))
    (when (null axis)
      (return-from axis-inherit nil))
    (if (every #'(lambda (f) (eq (feature-properties-value :axis f) axis)) features)
	(cons :axis axis)
	nil)))
