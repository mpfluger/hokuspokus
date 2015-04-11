;;;; -*- Lisp -*-

;;; Functions for generating terms and features
(in-package :hokuspokus)

;; (proclaim '(optimize speed))
;; turning this on gives some nice compiler warnings where to optimize

(defparameter *debug* nil)

(defparameter *constraints* nil)

(defparameter *inheritance* nil)

(defstruct (operator (:print-object operator-print))
  function
  (arity 2)
  (commutativep t))

(defparameter *operators* nil)

(defparameter *all-operators* (list
			       (make-operator :function '+)
			       ;; this is only for eq rediscovery
			       ;(make-operator :function '- :commutativep nil)
			       ;; minus is commutative as far as correlation
			       ;; is concerned
			       (make-operator :function '-)
			       ;(make-operator :function '- :arity 1)
			       (make-operator :function '*)
			       (make-operator :function '/ :commutativep nil)
			       (make-operator :function 'min)
			       (make-operator :function 'max)
			       (make-operator :function 'mod :commutativep nil)
			       (make-operator :function 'positive? :arity 1)
			       (make-operator :function 'my-eq?)
			       (make-operator :function 'my-gt?)
			       (make-operator :function 'my-and)
			       (make-operator :function 'my-or)
			       (make-operator :function 'my-not :arity 1)
			       (make-operator :function 'my-xor)
			       (make-operator :function 'my-nxor)
			       (make-operator :function 'abs :arity 1)
			       (make-operator :function 'sqrt :arity 1)
			       (make-operator :function 'sin :arity 1)
			       (make-operator :function 'asin :arity 1)
			       (make-operator :function 'cos :arity 1)
			       (make-operator :function 'acos :arity 1)
			       (make-operator :function 'tan :arity 1)
			       (make-operator :function 'atan :commutativep nil)
			       (make-operator :function 'angle-sub)
			       (make-operator :function 'exp :arity 1)
			       (make-operator :function 'log :arity 1)
			       (make-operator :function 'fac :arity 1)
			       ))

(defun operator-print (operator stream)
  (format stream "#<~A>" (operator-function operator)))

(defun operator-find (fn)
  (find-if #'(lambda (op) (equal fn (operator-function op))) *all-operators*))

(defun set-operators (&rest ops)
  "Set *operators* to given operators (as function-names)"
  (setf *operators* (mapcar #'operator-find ops)))

;; constraints
;; features can have properties
;; these properties are inherited from the features they were generated
;; from using the functions in the list *inheritance*

;; constraints are a list of functions to check if the operation is valid
;; these functions return t if the operation is valid, nil otherwise

;; examples:
;; subtraction and addition only of data on same axis
;; same range for subtraction and addition
;; relate controllable and uncontrollable vars only - problem - hill climbing

(defun inheritance-insert (fun)
  "Insert a new function into the inheritance list"
  (setf *inheritance* (cons fun *inheritance*)))

(defun properties-inherit (op features)
  "Return list of properties according to op and features"
  (mapcar #'(lambda (f) (funcall f op features)) *inheritance*))

(defun constraints-insert (constraint)
  "Insert a new function into the constraints list"
  (setf *constraints* (cons constraint *constraints*)))

(defun constraints-check (op features)
  "Check all constraints of an operator with the given features (as names)"
  (every #'(lambda (constr) (funcall constr op features))
	 *constraints*))

(defun feature-generate (name op features)
  "Generate a feature from features and a function"
  (block nil
    (let ((feature (make-feature :name name
		      :properties nil
		      :data nil
		      :endpointer nil)))
      (handler-case
	  (progn
	    (setf (feature-datavec feature)
		  (mapcl (operator-function op)
			 (mapcar #'feature-datavec features)))
	    ;; checking for obliterate features
	    ;; this *should* not be needed, but only if the
	    ;; term-transformer catches everything
	    (when (vec-constp (feature-datavec feature))
	      (when *debug*
		(format t "; [Constant feature generated.~%;   ~
                           Ignoring feature:~%;    ~A]~%"
			name))
	      (return-from feature-generate nil))
	    (feature-correlation-compute feature)
	    (setf (feature-properties feature) (properties-inherit op features))
	    (setf (feature-parents feature) features)
	    feature)
	(floating-point-overflow ()
	  (progn
	    (when *debug*
	      (format t "; [Condition Floating-Point-Overflow signalled.~%;   ~
                           Ignoring feature:~%;    ~A]~%"
		      name))
	    nil))
	(floating-point-invalid-operation ()
	  (progn
	    (when *debug*
	      (format t "; [Condition Floating-Point-Invalid-Operation signalled.~%;   ~
                           This probably means Division-by-Zero-Error~%;   ~
                           Ignoring feature:~%;    ~A]~%"
		      name))
	    nil))	  
	))))

(proclaim '(inline term-simplify term-make-name generate-feature-from-term))

(defun term-simplify (term)
  "Normalizes a term by sorting the operands"
  (cons (car term) (sort (cdr term) #'(lambda (a b) (string< (feature-name a)
							     (feature-name b))))))

(defun term-make-name (term)
  "Produce a name in prefix notation for a term"
  (format nil "(~A~{ ~A~})" (operator-function (car term))
	  (mapcar #'feature-name (cdr term))))

(defun generate-feature-from-term (term)
  "Generate a feature from the given term"
  (feature-generate (term-make-name term) (car term) (cdr term)))

(defun generate-terms (space1 &rest spaces)
  "Generate all terms possible with the given feature spaces"
  (let ((ops *operators*)
	(terms nil))
    (dolist (op ops terms)
      (dolist (f1 space1 terms)
	(if (= (operator-arity op) 1)
	    (setf terms (adjoin `(,op ,f1) terms))
	    (dolist (space spaces terms)
	      (dolist (f2 space terms)
		     (cond
		       ((operator-commutativep op)
			(setf terms (adjoin (term-simplify `(,op ,f1 ,f2)) terms :test #'equal)))
		       ((and (not (operator-commutativep op)) (not (eq f1 f2)))
			(setf terms (adjoin `(,op ,f1 ,f2) terms :test #'equal))
			(setf terms (adjoin `(,op ,f2 ,f1) terms :test #'equal)))
		       (t
			nil)))))))
      terms))

(defmacro generate-level (keep-data? save-space space1 &rest spaces)
  "Call generate-terms and insert the resulting features into save-space"
  `(dolist (term (generate-terms ,space1 ,@spaces))
    (when (and (constraints-check (car term) (cdr term))
	       (>= (count-ops (simplify (read-from-string (term-make-name term))))
		   (1+ (apply #'+ (mapcar #'(lambda (f) (count-ops (read-from-string (feature-name f))))
					  (cdr term))))))
      (let ((feature (generate-feature-from-term term)))
	(when (and (not ,keep-data?) feature)
	  (setf (feature-datavec feature) nil))
	(setf ,save-space (feature-prepend feature ,save-space))))))

(defun selection-count (method num len)
  (case method
	 (:abs num)
	 (:percent (if (> (round (* num len)) 0)
		       (round (* num len))
		       1))
	 (:all len)))

(defun selection-order (feature)
  "Determine the fitness of a feature"
  (feature-correlation feature))

;; (defun selection-order (feature)
;;   "Determine the fitness of a feature"
;;   (rmse (feature-datavec feature)
;; 	(feature-datavec *dependent-var*)))

;; (defun selection-order (feature)
;;   "Determine the fitness of a feature"
;;   (/ (feature-correlation feature)
;;      (apply #'max (mapcar #'feature-correlation (feature-parents feature)))))

(defmacro select-best (method num space)
  "Select best features in space without lookahead"
  (let ((len (gensym)))
    `(let ((,len (length ,space)))
      (subseq
       ; destructive! because of sort
       (sort ,space #'< :key #'selection-order)
       0 (selection-count ,method ,num ,len)))))

(defun post-select-best (method num)
  "Select best features with lookahead"
  (let ((buf nil)
	(len (length *gen-space*)))
    (do ((count 0 (length buf))
	 (runs  0 (1+ runs)))
	((or (>= count (selection-count method num len))
	     (> runs (length *gen-space*))))
;; this could make sense sometimes, but it's buggy
;; and it did not help where it should have (balance-scale)
;; possibly it works if we exchange car and nth runs
;;       (when (< (feature-correlation (nth runs *gen-space*))
;; 	       (feature-correlation (car *lookahead-space*)))
;; 	(setf buf (adjoin (feature-name (nth runs *gen-space*))
;; 			  buf :test #'string-equal)))
      (mapcar #'(lambda (f)
		  (unless (find f *feature-space*)
		    (setf buf (adjoin (feature-name f) buf :test #'string-equal))))
	      (feature-parents (nth runs *lookahead-space*))))
    (mapcar #'(lambda (name) (feature-find name *gen-space*)) buf)))

(defun generate-features (pre-method pre-best post-method post-best depth)
  (dotimes (n depth)
    ;; this is a workaround for equation rediscovery
    ;; in case we generate an equation that equals to one single feature
    ;; it is already found when we start - right here
    (dolist (f *feature-space*)
      (when (< (rmse (feature-datavec f)
		     (feature-datavec *dependent-var*)) 0.1)
	(setf *feature-space* (list f))
	(return-from generate-features 'done)))
    (setf *gen-space* (select-best pre-method pre-best *gen-space*))
    ;; if we found the perfect feature...
    (dolist (f *gen-space*)
      (when (< (rmse (feature-datavec f)
		     (feature-datavec *dependent-var*)) 0.1)
	(setf *feature-space* (list f))
	(return-from generate-features 'done)))
;;     (when (< (feature-correlation (car *gen-space*)) 0.0001)
;;       (setf *feature-space* (list (car *gen-space*)))
;;       (return-from generate-features 'done))
    (generate-level nil *lookahead-space* *gen-space* *gen-space* *feature-space*)
    (setf *lookahead-space* (sort *lookahead-space* #'< :key #'selection-order))
    ;; if all features in lookahead are worse than in gen-space...
;;     (when (< (feature-correlation (car *gen-space*))
;; 	     (feature-correlation (car *lookahead-space*)))
;;       (mapcar #'(lambda (f) (setf *feature-space* (feature-prepend f *feature-space*)))
;; 	      (select-best pre-method pre-best *gen-space*))
;;       (when *debug*
;; 	(format t "Debug: Exiting. All features in looakhead are worse.~%"))
;;       (return-from generate-features 'done))
    (let ((best (post-select-best post-method post-best))
	  (tmp nil))
      (generate-level t tmp best best *feature-space*)
      (setf *gen-space* tmp)
      (mapcar #'(lambda (f) (setf *feature-space* (feature-prepend f *feature-space*)))
	      best)
      (setf *lookahead-space* nil))))

(defmacro with (&rest args)
  "Call the respective functions with the given settings"
  `(progn
    (setf *feature-space* nil)
    (setf *gen-space* nil)
    (setf *lookahead-space* nil)
    (setf *dependent-var* nil)  
    (setf *feature-runs* nil)
    (setf *constraints* nil)
    (setf *inheritance* nil)
    (init-simplification-rules)
    ,(if (assoc 'debug args)
	   `(setf *debug* t)
	   `(setf *debug* nil))
    (read-data ,@(cdr (assoc 'in-file args)))
    ,(when (assoc 'operators args)
	   `(set-operators ,@(cdr (assoc 'operators args))))
    (constraints-insert #'range-constr)
    ,(when (assoc 'constraints args)
	   `(mapcar #'funcall ',(cdr (assoc 'constraints args))))
    ;; maybe do this for a number of general constants
    ,(when (assoc 'pi args)
	   `(let ((f (make-feature :name "pi")))
	     (setf (feature-datavec f) (make-array (length (feature-datavec (car *feature-space*)))
					:initial-element (coerce pi 'single-float)
					:element-type 'single-float))
	     (setf *feature-space* (feature-prepend f *feature-space*))))
     ;; generate the first level of features (for search init)
    (generate-level t *gen-space* *feature-space* *feature-space*)
    (generate-features ,@(cdr (assoc 'pre-select args)) ,@(cdr (assoc 'post-select args)) ,@(cdr (assoc 'depth args)))
    (setf *simplification-rules* 
      (append *simplification-rules* (mapcar #'simp-rule 
				      '((x + x  = 2 * x)
					(x * x  = x ^ 2)
					(x * (y / z) = (x * y) / z)
					((x / y) / z = x / (y * z))
					((x / y) + (z / y) = (x + z) / y)))))
    ;; deal with parentheses and spaces...
    ,(when (assoc 'final-select args)
	   `(setf *feature-space* (select-best ,@(cdr (assoc 'final-select args)) *feature-space*)))
    ,(when (assoc 'out-file args)
	   `(write-data ,@(cdr (assoc 'out-file args)) ,(cadr (assoc 'in-file args))))
    ,(when (assoc 'final-words args)
	   `(eval ,@(cdr (assoc 'final-words args))))
    'done))
