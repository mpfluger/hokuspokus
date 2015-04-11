;;;; -*- Lisp -*-

;;; Utility Functions
(in-package :hokuspokus)

;; (proclaim '(optimize speed))
;; turning this on gives some nice compiler warnings where to optimize

;;; Small Lisp Utils

(proclaim '(inline append1 last1 mapcl vec-constp))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun last1 (lst)
  (car (last lst)))

(defun (setf last1) (val lst)
  (setf (car (last lst)) val))

(defun mapcl (op list)
  "Like map but takes a list of lists"
  (apply #'map (cons `(simple-array single-float
		       (,(length (car list))))
		     (cons op list))))

(defun vec-constp (vec)
  "True if vector consists only of same values"
  (declare (type (simple-array single-float) vec))
  (let ((val (aref vec 0)))
    (dotimes (pos (length vec))
      (when (not (=more-or-less (aref vec pos) val))
	(return-from vec-constp nil))))
  t)

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
    ,@body))

(defun count-ops (exp)
  "Count number of operators in a lisp expression"
  (labels ((cnt (exp num)
	     (cond
	       ((atom exp) num)
	       (t (+ 1 (cnt (cadr exp) num) (cnt (caddr exp) num))))))
    (cnt exp 0)))

(defmacro runtime (&body form)
  "Returns the real runtime of form as a float" 
  `(parse-simple-float (nth 5 (tokens
			       (with-output-to-string (stream)
				 (let ((*trace-output* stream))
				   (time ,@form))) #\ ))))

;;; List Functions

(proclaim '(inline select-random))

(defun make-random-list (length min max)
  "Create a random list in the range [min, max["
  (labels ((mk-rnd-lst (length lst)
	     (cond
	       ((= length 0) lst)
	       (t
		(let ((rnd (+ (random (- max min)) min)))
		  (when (< (abs rnd) 0.1)
		    (setf rnd (- max 0.1)))
		  (mk-rnd-lst
		   (1- length) (cons
				rnd
				lst)))))))
    (mk-rnd-lst length nil)))

(defun select-random (lst)
  "Select a random element from lst"
  (nth (random (length lst)) lst))

;;; String Functions

(defun tokens (str delim &optional (start 0))
  "Returns list of tokens delimited by delim from string"
  (declare (type simple-string str))
  (labels ((tokenizer (str delim start acc)
	     (declare (type simple-string str))
	     (let* ((p1 (position delim str :start start))
		    (token (subseq str start p1)))
	       (if (null p1)
		   (cons token acc)
		   (tokenizer str delim (1+ p1) (cons token acc))))))
    (nreverse (tokenizer str delim start nil))))

(defun parse-simple-float (str)
  "Parse float from string, the fast version"
  (declare (type simple-string str))
  (let* ((dot (position #\. str))
	 (int (parse-integer (subseq str 0 dot))))
    (declare (type (signed-byte 32) int))
    (if (null dot)
	int
	(multiple-value-bind (x e) (parse-integer (subseq str (+ dot 1)))
	  (cond
	    ((minusp int) (- int (* (expt 0.1 e) x)))
	    ((char= #\- (char str 0)) (- (+ int (* (expt 0.1 e) x))))
	    (t (+ int (* (expt 0.1 e) x))))))))

;;; Mathematical Functions

(proclaim '(inline avg variance correlation-coefficient))

(defun derivative (vec1 h)
  "Compute derivative of vector with stepping h"
  (declare (type (simple-array single-float) vec1))
  ;; simple version using just the right derivative
  (let* ((len (1- (length  vec1)))
	 (res (make-array (1+ len) :element-type 'single-float)))
    (declare (type (simple-array single-float) res))
    (dotimes (pos len res)
      (setf (aref res pos) (/ (- (aref vec1 (1+ pos))
				 (aref vec1 pos))
			      h)))
    (setf (aref res len) (aref res (1- len)))
    res))

(defun avg (vec)
  "Compute the average of a vector of numbers"
  (declare (type (simple-array single-float) vec))
  (labels ((sum (vec)
	     (let ((s 0.0))
	       (declare (type single-float s))
	       (dotimes (r (length vec))
		 (incf s (aref vec r)))
	       s)))
    (/ (sum vec) (length vec))))

(defun list-avg (lst)
  "Compute the average of a list of numbers"
  (labels ((avg1 (lst &optional (acc 0))
	     (if (null lst)
		 acc
		 (avg1 (rest lst) (+ acc (first lst))))))
    (/ (avg1 lst)
       (length lst))))

(defun variance (vec)
  "Variance of a vector"
  (declare (type (simple-array single-float) vec))
  (labels ((var (vec mean)
	     (let ((res 0.0))
	       (declare (type single-float res))
	       (dotimes (pos (length vec))
		 (incf res (expt (- (aref vec pos) mean) 2)))
	       res)))
    (/ (var vec (avg vec)) (1- (length vec)))))

(defun lst-variance (lst)
  "Variance of a list"
  (cond
    ((null lst) 0)
    ((= (length lst) 1) 0)
    (t
     (let ((mean (list-avg lst)))
       (/ (apply #'+ (mapcar #'(lambda (x) (expt (- x mean) 2))
			     lst))
	  (- (length lst) 1))))))

;; (defun correlation-coefficient (vec1 vec2)
;;   "Compute the linear correlation coefficient r"
;;   (let ((mean1 (avg vec1))
;; 	(mean2 (avg vec2)))
;;     (/ (reduce #'+ (map `(simple-array single-float (,(length vec1))) #'(lambda (x y) (* (- x mean1) (-  y mean2))) vec1 vec2))
;;        (* (sqrt (reduce #'+ (map `(simple-array single-float (,(length vec1))) #'(lambda (x) (expt (- x mean1) 2)) vec1)))
;; 	  (sqrt (reduce #'+ (map `(simple-array single-float (,(length vec1))) #'(lambda (y) (expt (- y mean2) 2)) vec2)))))))

(defun correlation-coefficient (vec1 vec2)
  "Compute the linear correlation coefficient r"
  (declare (type (simple-array single-float) vec1))
  (declare (type (simple-array single-float) vec2))
  (let ((mean1 (avg vec1))
	(mean2 (avg vec2))
	(e1 0.0)
	(e2 0.0)
    	(e3 0.0))
    (declare (type single-float mean1))
    (declare (type single-float mean2))
    (declare (type single-float e1))
    (declare (type single-float e2))
    (declare (type single-float e3))
    (dotimes (pos (length vec1))
      (incf e1 (* (- (aref vec1 pos) mean1)
		  (- (aref vec2 pos) mean2)))
      (incf e2 (expt (- (aref vec1 pos) mean1) 2))
      (incf e3 (expt (- (aref vec2 pos) mean2) 2)))
    (setf e1 (/ e1 (* (sqrt e2) (sqrt e3))))
    e1))

(defun partial-correlation-coefficient (vec1 vec2 splits)
  "Compute the correlation coefficient for splits of vector"
  (do* ((len (1- (length vec1)))
	(start 0 (+ start (floor (/ len splits))))
	(end (floor (/ len splits)) (+ end (floor (/ len splits))))
	(runs 0 (1+ runs))
	(best 0.0))
       ((= runs splits) best)
    (when (= runs (1- splits))
      (setf end len))
    (unless (vec-constp (subseq vec1 start end))
      (let ((corr (abs (correlation-coefficient (subseq vec1 start end)
						(subseq vec2 start end)))))
	(when (> corr best)
	  (setf best corr))))))

(defun mae (vec1 vec2)
  "Compute the mean absolute error"
  (let ((res 0.0))
    (dotimes (pos (length vec1))
      (incf res (abs (- (aref vec1 pos)(aref vec2 pos)))))
    (setf res (/ res (length vec1)))
    res))

(defun rmse (vec1 vec2)
  "Compute the root mean squared error"
  (let ((res 0.0))
    (dotimes (pos (length vec1))
      (incf res (expt (- (aref vec1 pos)(aref vec2 pos)) 2)))
    (setf res (sqrt (/ res (length vec1))))
    res))

(defun =more-or-less (num1 num2)
  "num1 is in the vicinity of num2"
  (< (abs (- num1 num2)) 0.000001))

(defun =more-or-less-big-error (num1 num2)
  "num1 is in the vicinity of num2"
  (< (abs (- num1 num2)) 0.01))

(defun positive? (number)
  (if (plusp number)
      1.0
      0.0))

(defun my-eq? (num1 num2)
  (if (=more-or-less-big-error num1 num2)
      1.0
      0.0))

(defun my-gt? (num1 num2)
  (if (> num1 num2)
      1.0
      0.0))

(defun my-and (val1 val2)
  (if (and (= val1 1.0) (= val2 1.0))
      1.0
      0.0))

(defun my-or (val1 val2)
  (if (or (= val1 1.0) (= val2 1.0))
      1.0
      0.0))

(defun my-not (val1)
  (if (= val1 0.0)
      1.0
      0.0))

(defun my-xor (val1 val2)
  (if (= val1 val2)
      0.0
      1.0))

(defun my-nxor (val1 val2)
  (if (= val1 val2)
      1.0
      0.0))

(defun angle-sub (a1 a2)
  "Subtract two angels and returned normalized result [-pi,pi]"
  (min (abs (- a1 a2)) (abs (+ a1 a2))))

(defun fac (n)
  "Faculty of n"
  (cond
    ((= 0 n) 1)
    (t (* n (fac (- n 1))))))
