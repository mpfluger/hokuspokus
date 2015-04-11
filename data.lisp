;;;; -*- Lisp -*-

;;; Functions to deal with the training data
(in-package :hokuspokus)

;; (proclaim '(optimize speed))
;; turning this on gives some nice compiler warnings where to optimize

;; Data structures for features
(defparameter *feature-space* nil)

(defparameter *gen-space* nil)

(defparameter *lookahead-space* nil)

(defparameter *dependent-var* nil)

(defparameter *feature-runs* nil)

(defstruct (feature (:print-object feature-print))
  name
  parents
  properties
  data
  endpointer
  datavec
  correlation)

(defun feature-print (feature stream)
  (declare (type stream stream))
  (format stream "#<~(~A~):~,3F>"
	  (prefix->infix (simplify (read-from-string (feature-name feature))))
	  (feature-correlation feature)))

(defun feature-append (name)
  "Insert new feature in global *feature-space* data structure"
  (let ((f (make-feature :name name
			 :properties nil
			 :data nil
			 :endpointer nil)))
    (setf *feature-space* (append1 *feature-space* f))
    f))

(defun feature-prepend (feature space)
  "Insert new feature in features data structure"
  (if feature
      (cons feature space)
      space))

(defun feature-remove (name space)
  "Remove feature called name from space"
  (delete-if #'(lambda (x) (string-equal name (feature-name x))) space))

(defun feature-find (name space)
  "Find feature called name and return it"
  (find-if #'(lambda (x) (string-equal name (feature-name x))) space))

(defmacro feature-map (fn &rest lists)
  "Apply function to every feature"
  (if (null lists)
      `(mapcar ,fn *feature-space*)
      `(mapcar ,fn *feature-space* ,@lists)))

(defun feature-runs-update ()
  "Add location of run-end to list of runs-0"
  (setf *feature-runs*
	(nconc *feature-runs*
		(list (length (feature-data (car *feature-space*)))))))

(defun feature-class (feature)
  "Return list of values at class registered in *feature-runs* from feature"
  (let ((acc nil)
	(data (feature-datavec feature)))
    (dolist (pos *feature-runs* acc)
      (push (aref data pos) acc))
    (nreverse acc)))

;; (defun feature-class (feature class)
;;   "Return list of values at t=class from feature"
;;   (let ((acc nil)
;; 	(data (feature-datavec feature))
;; 	(last-zero 0))
;;     (dolist (zeros *feature-runs* acc)
;;       (if (> (- zeros last-zero) class)
;; 	  (push (aref data (- zeros class)) acc))
;;       (setf last-zero zeros))
;;     (nreverse acc)))

(defun feature-correlation-compute (feature)
  "Based on correlation-coefficient r plus variance at class 0"
  (setf (feature-correlation feature)
	(+ (if *feature-runs* (lst-variance (feature-class feature)) 0)
	   (- 1 (abs (correlation-coefficient (feature-datavec feature)
					      (feature-datavec *dependent-var*)))))))

;; (defun feature-correlation-compute (feature)
;;   "Based on variance of the derivative"
;;   ;; this uses fixed stepping size h (0.1)
;;   ;; and therefore is only valid for linear time
;;   (setf (feature-correlation feature)
;; 	(variance (derivative (feature-datavec feature) 0.1))))

;; (defun feature-correlation-compute (feature)
;;   "Based on partial-correlation-coefficient (splits of data)"
;;   ;; check out different split nums
;;   (setf (feature-correlation feature)
;; 	(+ (if *feature-runs* (lst-variance (feature-class feature)) 0)
;; 	   (- 1 (abs (partial-correlation-coefficient
;; 		      (feature-datavec feature)
;; 		      (feature-datavec *dependent-var*)
;; 		      3))))))

;; (defun feature-correlation-compute (feature)
;;   "Based on Root Mean Squared Error"
;;   ;; this is not too good compared to mine, but similar to r
;;   (setf (feature-correlation feature)
;;     (rmse (feature-datavec feature) (feature-datavec *dependent-var*))))

;; (defun feature-correlation-compute (feature)
;;   "Based on sum of variance of all classes"
;;   (let ((corr 0))
;;     (dotimes (pos (length (feature-class feature 0)) corr)
;;       (incf corr (lst-variance (feature-class feature pos))))
;;     (setf (feature-correlation feature) corr)))

(defun feature-datum-insert (obj feature)
  "Insert datum in feature-data (using a queue for speed)"
  (if (null (feature-data feature))
      (setf (feature-endpointer feature) (setf (feature-data feature) (list obj)))
      (setf (cdr (feature-endpointer feature)) (list obj)
	    (feature-endpointer feature) (cdr (feature-endpointer feature))))
  (feature-data feature))

(defun feature-data-insert (lst feature)
  "Insert a vector of data in feature-datavec from a list"
  (setf (feature-datavec feature) (make-array (length lst)
					      :element-type 'single-float
					      :initial-contents lst)))

(defun feature-properties-insert (prop value feature)
  "Insert a property in the properties list of a feature"
  (setf (feature-properties feature)
	(cons (cons prop value) (feature-properties feature))))

(defun feature-properties-value (prop feature)
  "Find property and return value"
  (cdr (assoc prop (feature-properties feature))))

;; Read in an arff file (format for weka)
;; The format is described at
;;    http://www.cs.waikato.ac.nz/~ml/weka/arff.html
;; String attributes, date attributes, and sparse instances are
;; excluded, so are missing fields denoted by '?' and strings with
;; whitespace as names.
;; 
;; Therefore the grammar is the following:
;; arff      := header
;;             | data
;; header    := comment
;;             | relation
;;             | attribute
;; relation  := (@relation name)
;; attribute := (@attribute name numeric)
;; data      := (@data {num_line}*)
;; num_line  := num
;;             | (num, num)
;; num       := [0-9]+{.[0-9]+}?
;; 
;; The last feature is expected to be the dependent variable!
;; 
;; Basic implementation as a state machine without parser.

(defun read-data (file &key (class nil) (lines -1))
  "Read in an arff file and store data in memory"
  (with-open-file (stream file :direction :input)
    (do ((line (read-line stream nil :eof) (read-line stream nil :eof))
	 (numlines 0 (1+ numlines)))
	((or (eql line :eof) (= lines numlines))
	 ;; convert data to vectors
	 (feature-map #'(lambda (f) (feature-data-insert (feature-data f) f)))
	 (feature-map #'(lambda (f) (setf (feature-data f) nil)))
	 (feature-map #'(lambda (f) (setf (feature-endpointer f) nil)))
	 ;; move dep var to *dependent-var*
	 (setf *dependent-var* (last1 *feature-space*))
	 (rplacd (last *feature-space* 2) nil)
	 ;; compute correlation for all features
	 (feature-map #'(lambda (f) (feature-correlation-compute f)))
	 )
      (unless (= (length line) 0)	; ignore empty lines
	  (let ((head (char line 0)))
	    (cond
	      ((eql head #\%) nil)	; ignore comments
	      ((eql head #\@) (read-special line))
	      ((or (digit-char-p head) (eql head #\-)) (read-data-line line class))
	      (t (error "parse-error: unknown character in input"))))))))

(defun read-special (line)
  "Read in line beginning with @ - arff command"
  (let* ((tokens (tokens line #\ ))
	 (token (car tokens)))
    (cond
      ((or (string-equal token "@relation") (string-equal token
							 "@data")) nil)
      ((string-equal token "@attribute")
       (feature-append (second tokens)))
      (t (error "parse-error: unknown @command")))))

(defun read-data-line (line class)
  "Read and insert one line of numerical data"
  (let ((numlist (mapcar #'parse-simple-float (tokens line #\,))))
    (when (and class (= (last1 numlist) class))
      (feature-runs-update))
    ;; all data is coerced to single-floats here!
    (feature-map #'(lambda (f e) (feature-datum-insert (coerce e 'single-float)  f))
		 numlist)))

(defun write-data (file orig-file)
  "Write the feature-space data to an arff file"
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (format stream "@relation 'Generated by Hokuspokus from ~A'~%~%~
                    ~{@attribute ~(~A~) numeric~^~%~}~
                    ~%~%@data~%~%"
	    (file-namestring orig-file)
	    (append1 (feature-map #'(lambda (f)
				      (remove #\Newline (remove #\ (format nil "~a" (prefix->infix (simplify (read-from-string (feature-name f)))))))))
		     (feature-name *dependent-var*)))
    ;; output data
    (dotimes (pos (length (feature-datavec (car *feature-space*))))
      (format stream "~{~,4F~^,~}~%"
	      (append1 (feature-map #'(lambda (f) (aref (feature-datavec f) pos)))
		       (aref (feature-datavec *dependent-var*) pos))))))
