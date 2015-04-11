;;;; -*- Lisp -*-

;;; The term simplifier
;;; Ripped off from Norvigs PAIP
(in-package :hokuspokus)

;; (proclaim '(optimize speed))
;; turning this on gives some nice compiler warnings where to optimize


;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

(defun starts-with (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))

(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

;;;; PATTERN MATCHING FACILITY

(defconstant fail nil)
(defvar no-bindings '((t . t)))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun make-binding (var val) (cons var val))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

;;;; File pat-match.lisp: Pattern matcher from section 6.2

;;; A bug fix By Richard Fateman, rjf@cs.berkeley.edu  October 92.

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)                
         (segment-matcher pattern input bindings))  
        ((single-pattern-p pattern)                 ; ***
         (single-matcher pattern input bindings))   ; ***
        ((and (consp pattern) (consp input)) 
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) 
                               bindings)))
        (t fail)))


(setf (get '?is  'single-match) 'match-is)
(setf (get '?or  'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?*  'segment-match) 'segment-match)
(setf (get '?+  'segment-match) 'segment-match+)
(setf (get '??  'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern)) 
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
            (not (funcall pred input)))
        fail
        new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pat-match (first patterns) input
                                 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns) 
                                     input bindings)))
        (if (eq new-bindings fail)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  (if (match-or patterns input bindings)
      fail
      bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        (let ((pos (first-match-pos (first pat) input start)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                          pat (subseq input pos)
                          (match-variable var (subseq input 0 pos)
                                          bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((<= start (length input)) start) ;*** fix, rjf 10/1/92 (was <)
        (t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
  ;; *** fix, rjf 10/1/92 (used to eval binding values)
  (and (progv (mapcar #'car bindings)
              (mapcar #'cdr bindings)
          (eval (second (first pattern))))
       (pat-match (rest pattern) input bindings)))  

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev) 
    (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
        ((atom pat) pat)
        (t (cons (expand-pat-match-abbrev (first pat))
                 (expand-pat-match-abbrev (rest pat))))))

(defun rule-based-translator 
       (input rules &key (matcher 'pat-match) 
        (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the first rule in rules that matches input,
  and apply the action to that rule."
  (some 
    #'(lambda (rule)
        (let ((result (funcall matcher (funcall rule-if rule) 
                               input)))
          (if (not (eq result fail))
              (funcall action result (funcall rule-then rule)))))
    rules))

;;;; File macsyma.lisp: The implementation of MACSYMA in Chapter 8

;; (defun variable-p (exp)
;;   "Variables are the symbols M through Z."
;;   ;; put x,y,z first to find them a little faster
;;   (member exp '(x y z m n o p q r s t u v w)))

;;; From student.lisp:
(defstruct (rule (:type list)) pattern response)
(defstruct (expr (:type list)
		 (:conc-name exp-)
		 (:constructor mkexp (lhs op rhs)))
  op lhs rhs)

(defun exp-p (x) (consp x))
(defun exp-args (x) (rest x))

(defun binary-exp-p (x)
  (and (exp-p x) (= (length (exp-args x)) 2)))

(defun prefix->infix (exp)
  "Translate prefix to infix expressions."
  (if (atom exp) exp
      (mapcar #'prefix->infix
              (if (binary-exp-p exp)
                  (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
                  exp))))

;; Define x+ and y+ as a sequence:
(pat-match-abbrev 'x+ '(?+ x))
(pat-match-abbrev 'y+ '(?+ y))

;; Define n and m as numbers; s as a non-number:
(pat-match-abbrev 'n '(?is n numberp))
(pat-match-abbrev 'm '(?is m numberp))
(pat-match-abbrev 's '(?is s not-numberp))

(defparameter *infix->prefix-rules*
  (mapcar #'expand-pat-match-abbrev
    '(((x+ = y+) (= x y))
      ((- x+)    (- x))
      ((+ x+)    (+ x))
      ((x+ + y+) (+ x y))
      ((x+ - y+) (- x y))
      ((d y+ / d x) (d y x))        ;*** New rule
      ((Int y+ d x) (int y x))      ;*** New rule
      ((x+ * y+) (* x y))
      ((x+ / y+) (/ x y))
      ((x+ ^ y+) (^ x y)))))

(defun infix->prefix (exp)
  "Translate an infix expression into prefix notation."
  ;; Note we cannot do implicit multiplication in this system
  (cond ((atom exp) exp)
        ((= (length exp) 1) (infix->prefix (first exp)))
        ((rule-based-translator exp *infix->prefix-rules*
           :rule-if #'rule-pattern :rule-then #'rule-response
           :action
           #'(lambda (bindings response)
               (sublis (mapcar
                         #'(lambda (pair)
                             (cons (first pair)
                                   (infix->prefix (rest pair))))
                         bindings)
                       response))))
        ((symbolp (first exp))
         (list (first exp) (infix->prefix (rest exp))))
        (t (error "Illegal exp"))))

(defvar *simplification-rules* nil) ;Rules are in file macsymar.lisp

(defun ^ (x y) "Exponentiation" (expt x y))

(defun simplifier ()
  "Read a mathematical expression, simplify it, and print the result."
  (loop
    (print 'simplifier>)
    (print (simp (read)))))

(defun simp (inf) (prefix->infix (simplify (infix->prefix inf))))

(defun simplify (exp) 
  "Simplify an expression by first simplifying its components."
  (if (atom exp) exp
      (simplify-exp (mapcar #'simplify exp))))

;;; simplify-exp is redefined below
;(defun simplify-exp (exp)
;  "Simplify using a rule, or by doing arithmetic."
;  (cond ((rule-based-translator exp *simplification-rules*
;           :rule-if #'exp-lhs :rule-then #'exp-rhs
;           :action #'(lambda (bindings response)
;                       (simplify (sublis bindings response)))))
;        ((evaluable exp) (eval exp))
;        (t exp)))

(defun evaluable (exp)
  "Is this an arithmetic expression that can be evaluated?"
  (and (every #'numberp (exp-args exp))
       (or (member (exp-op exp) '(+ - * /))
           (and (eq (exp-op exp) '^)
                (integerp (second (exp-args exp)))))))

(defun not-numberp (x) (not (numberp x)))

(defun simp-rule (rule)
  "Transform a rule into proper format."
  (let ((exp (infix->prefix rule)))
    (mkexp (expand-pat-match-abbrev (exp-lhs exp))
	   (exp-op exp) (exp-rhs exp))))

(defun simp-fn (op) (get op 'simp-fn))
(defun set-simp-fn (op fn) (setf (get op 'simp-fn) fn))

(defun simplify-exp (exp)
  "Simplify using a rule, or by doing arithmetic,
  or by using the simp function supplied for this operator."
  (cond ((simplify-by-fn exp))                             ;***
        ((rule-based-translator exp *simplification-rules*
           :rule-if #'exp-lhs :rule-then #'exp-rhs
           :action #'(lambda (bindings response)
                       (simplify (sublis bindings response)))))
        ((evaluable exp) (eval exp))
        (t exp)))

(defun simplify-by-fn (exp)
  "If there is a simplification fn for this exp,
  and if applying it gives a non-null result,
  then simplify the result and return that."
  (let* ((fn (simp-fn (exp-op exp)))
         (result (if fn (funcall fn exp))))
    (if (null result)
        nil
        (simplify result))))

(defun factorize (exp)
  "Return a list of the factors of exp^n,
  where each factor is of the form (^ y n)."
  (let ((factors nil)
        (constant 1))
    (labels
      ((fac (x n)
         (cond
           ((numberp x)
            (setf constant (* constant (expt x n))))
           ((starts-with x '*)
            (fac (exp-lhs x) n)
            (fac (exp-rhs x) n))
           ((starts-with x '/)
            (fac (exp-lhs x) n)
            (fac (exp-rhs x) (- n)))
           ((and (starts-with x '-) (length=1 (exp-args x)))
            (setf constant (- constant))
            (fac (exp-lhs x) n))
           ((and (starts-with x '^) (numberp (exp-rhs x)))
            (fac (exp-lhs x) (* n (exp-rhs x))))
           (t (let ((factor (find x factors :key #'exp-lhs
                                  :test #'equal)))
                (if factor
                    (incf (exp-rhs factor) n)
                    (push `(^ ,x ,n) factors)))))))
      ;; Body of factorize:
      (fac exp 1)
      (case constant
        (0 '((^ 0 1)))
        (1 factors)
        (t `((^ ,constant 1) .,factors))))))

(defun unfactorize (factors)
  "Convert a list of factors back into prefix form."
  (cond ((null factors) 1)
        ((length=1 factors) (first factors))
        (t `(* ,(first factors) ,(unfactorize (rest factors))))))

(defun divide-factors (numer denom)
  "Divide a list of factors by another, producing a third."
  (let ((result (mapcar #'copy-list numer)))
    (dolist (d denom)
      (let ((factor (find (exp-lhs d) result :key #'exp-lhs
                          :test #'equal)))
        (if factor
            (decf (exp-rhs factor) (exp-rhs d))
            (push `(^ ,(exp-lhs d) ,(- (exp-rhs d))) result))))
    (delete 0 result :key #'exp-rhs)))

(defun free-of (exp var)
  "True if expression has no occurrence of var."
  (not (find-anywhere var exp)))

(defun find-anywhere (item tree)
  "Does item occur anywhere in tree?  If so, return it."
  (cond ((eql item tree) tree)
        ((atom tree) nil)
        ((find-anywhere item (first tree)))
        ((find-anywhere item (rest tree)))))

(defun integrate (exp x)
  ;; First try some trivial cases
  (cond
    ((free-of exp x) `(* ,exp x))          ; Int c dx = c*x
    ((starts-with exp '+)                  ; Int f + g  = 
     `(+ ,(integrate (exp-lhs exp) x)      ;   Int f + Int g
         ,(integrate (exp-rhs exp) x)))
    ((starts-with exp '-)              
     (ecase (length (exp-args exp))        
       (1 (integrate (exp-lhs exp) x))     ; Int - f = - Int f
       (2 `(- ,(integrate (exp-lhs exp) x) ; Int f - g  =
              ,(integrate (exp-rhs exp) x)))))  ; Int f - Int g
    ;; Now move the constant factors to the left of the integral
    ((multiple-value-bind (const-factors x-factors)
         (partition-if #'(lambda (factor) (free-of factor x))
                       (factorize exp))
       (identity ;simplify
         `(* ,(unfactorize const-factors)
             ;; And try to integrate:
             ,(cond ((null x-factors) x)
                    ((some #'(lambda (factor)
                               (deriv-divides factor x-factors x))
                           x-factors))
                    ;; <other methods here>
                    (t `(int? ,(unfactorize x-factors) ,x)))))))))

(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun deriv-divides (factor factors x)
  (assert (starts-with factor '^))
  (let* ((u (exp-lhs factor))              ; factor = u^n
         (n (exp-rhs factor))
         (k (divide-factors 
              factors (factorize `(* ,factor ,(deriv u x))))))
    (cond ((free-of k x)
           ;; Int k*u^n*du/dx dx = k*Int u^n du
           ;;                    = k*u^(n+1)/(n+1) for n/=1
           ;;                    = k*log(u) for n=1
           (if (= n -1)
               `(* ,(unfactorize k) (log ,u))
               `(/ (* ,(unfactorize k) (^ ,u ,(+ n 1)))
                   ,(+ n 1))))
          ((and (= n 1) (in-integral-table? u))
           ;; Int y'*f(y) dx = Int f(y) dy
           (let ((k2 (divide-factors
                       factors
                       (factorize `(* ,u ,(deriv (exp-lhs u) x))))))
             (if (free-of k2 x)
                 `(* ,(integrate-from-table (exp-op u) (exp-lhs u))
                     ,(unfactorize k2))))))))

(defun deriv (y x) (simplify `(d ,y ,x)))

(defun integration-table (rules)
  (dolist (i-rule rules)
    ;; changed infix->prefix to simp-rule - norvig Jun 11 1996
    (let ((rule (simp-rule i-rule)))
      (setf (get (exp-op (exp-lhs (exp-lhs rule))) 'int)
            rule))))

(defun in-integral-table? (exp)
  (and (exp-p exp) (get (exp-op exp) 'int)))

(defun integrate-from-table (op arg)
  (let ((rule (get op 'int)))
    (subst arg (exp-lhs (exp-lhs (exp-lhs rule))) (exp-rhs rule))))

(set-simp-fn 'Int #'(lambda (exp)
		      (unfactorize
		       (factorize
			(integrate (exp-lhs exp) (exp-rhs exp))))))

;;;; File macsymar.lisp: The rewrite rules for MACSYMA in Chapter 8

(defun init-simplification-rules ()
(setf *simplification-rules* (mapcar #'simp-rule '(
  (x + 0  = x)
  (0 + x  = x)
  ;(x + x  = 2 * x)
  ((x + x) - x = x)        ;; added by mp
  (x - 0  = x)
  (0 - x  = - x)
  (x - x  = 0)
  (- - x  = x)
  (x * 1  = x)
  (1 * x  = x)
  (x * 0  = 0)
  (0 * x  = 0)
  ;(x * x  = x ^ 2)
  (x / 0  = undefined)
  (0 / x  = 0)
  (x / 1  = x)
  (x / x  = 1)
  (0 ^ 0  = undefined)
  (x ^ 0  = 1)
  (0 ^ x  = 0)
  (1 ^ x  = 1)
  (x ^ 1  = x)
  (x ^ -1 = 1 / x)
  (x * (y / x) = y)
  ((y / x) * x = y)
  ((y * x) / x = y)
  ((x * y) / x = y)
  ((x * y) / (x * z) = y / z)  ;; added by mp
  ((y * x) / (x * z) = y / z)  ;; added by mp
  ((x * y) / (z * x) = y / z)  ;; added by mp
  ((y * x) / (z * x) = y / z)  ;; added by mp
  (x / (x * y) = 1 / y)    ;; added by mp
  (x / (y * x) = 1 / y)    ;; added by mp
  (x / (x / y) = y)        ;; added by mp
  (x / (y / x) = 1 / y)    ;; added by mp
  ((x / y) / x = 1 / y)    ;; added by mp
  ((x / y) / x = y)        ;; added by mp
  ((y / x) / x = y)        ;; added by mp
  ((x / y) / (z / y) = x / z)    ;; added by mp
  ((x / y) / (z * x) = 1 / (y * z))    ;; added by mp
  ((x / y) / (x / z) = z / y)    ;; added by mp
  ((x / y) / (z / y) = x / z)    ;; added by mp
  (x * (x / y) = (x * x) / y)    ;; added by mp
  ((x / y) * x = (x * x) / y)    ;; added by mp
  ;; removed the line below again, because sometimes hokuspokus
  ;; only find features one way and not the other and this eliminates
  ;; one way	   
  ;; ((x / y) + (z / y) = (x + z) / y)    ;; added by mp
  ((x / y) * z  = (x * z) / y)    ;; added by mp ; only changes order
  (x + - x = 0)
  ((- x) + x = 0)
  (x + y - x = y)
  ((x + y) - x = y)        ;; added by mp
  )))

(setf *simplification-rules* 
 (append *simplification-rules* (mapcar #'simp-rule
  '((s * n = n * s)
    (n * (m * x) = (n * m) * x)
    (x * (n * y) = n * (x * y))
    ((n * x) * y = n * (x * y))
    (n * (x / y) = (n * x) / y)	     ;; added by mp
    (n + s = s + n)
    ((x + m) + n = x + n + m)
    (x + (y + n) = (x + y) + n)
    ((x + n) + y = (x + y) + n)))))

(setf *simplification-rules* 
 (append *simplification-rules* (mapcar #'simp-rule '(
  (log 1         = 0)
  (log 0         = undefined)
  (log e         = 1)
  (sin 0         = 0)
  (sin pi        = 0)
  (cos 0         = 1)
  (cos pi        = -1)
  (sin(pi / 2)   = 1)
  (cos(pi / 2)   = 0)
  (log (e ^ x)   = x)
  (e ^ (log x)   = x)
  ((x ^ y) * (x ^ z) = x ^ (y + z))
  ((x ^ y) / (x ^ z) = x ^ (y - z))
  (log x + log y = log(x * y))
  (log x - log y = log(x / y))
  ((sin x) ^ 2 + (cos x) ^ 2 = 1)
  ))))


(setf *simplification-rules* 
 (append *simplification-rules* (mapcar #'simp-rule '(
  (d x / d x       = 1)
  (d (u + v) / d x = (d u / d x) + (d v / d x))
  (d (u - v) / d x = (d u / d x) - (d v / d x))
  (d (- u) / d x   = - (d u / d x))
  (d (u * v) / d x = u * (d v / d x) + v * (d u / d x))
  (d (u / v) / d x = (v * (d u / d x) - u * (d v / d x)) 
                     / v ^ 2) ; [This corrects an error in the first printing]
  (d (u ^ n) / d x = n * u ^ (n - 1) * (d u / d x))
  (d (u ^ v) / d x = v * u ^ (v - 1) * (d u / d x)
                   + u ^ v * (log u) * (d v / d x))
  (d (log u) / d x = (d u / d x) / u)
  (d (sin u) / d x = (cos u) * (d u / d x))
  (d (cos u) / d x = - (sin u) * (d u / d x))
  (d (e ^ u) / d x = (e ^ u) * (d u / d x))
  (d u / d x       = 0)))))
)
       
(integration-table
  '((Int log(x) d x = x * log(x) - x)
    (Int exp(x) d x = exp(x))
    (Int sin(x) d x = - cos(x))
    (Int cos(x) d x = sin(x))
    (Int tan(x) d x = - log(cos(x)))
    (Int sinh(x) d x = cosh(x))
    (Int cosh(x) d x = sinh(x))
    (Int tanh(x) d x = log(cosh(x)))
    ))
