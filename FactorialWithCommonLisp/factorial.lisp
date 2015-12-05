;; definition file of the article
;; HOW MANY WAYS TO DEFINE FACTORIAL ?

;; helper iota 1 to n
(defun iota1-n (n)
  (loop for i from 1 to n
        collecting i))

;; dotimes
(defun factorial-dotimes (n)
  (let ((fact 1))
    (dotimes (i n)
      (setf fact (* fact (1+ i))))
    fact))

;; dotimes - variation
;; note : do wrong with call like that : (factorial-dotimes-ex1 1 5)
(defun factorial-dotimes-ex1 (n &optional (fact 1))
  (dotimes (k n)
    (setf fact (* fact (1+ k))))
  fact)

;; dotimes - variation 2 - too redundant
(defun factorial-dotimes-ex2 (n)
  (let ((k 1))
    (let ((acc 1))
      (dotimes (i n)
        (setf acc (* acc k))
        (incf k))
      acc)))


;; dolist
(defun factorial-dolist (n)
  (let ((fact 1))
    (dolist (m (iota1-n n))
      (setf fact (* fact m)))
    fact))

;; dolist - variation
(defun factorial-dolist-ex1 (n &optional (fact 1))
  (dolist (m (iota1-n n))
    (setf fact (* fact m)))
  fact)

;; dolist variation
(defun factorial-dolist-ex2 (n)
  (let ((iota-lst (iota1-n n))
        (acc 1))
    (dolist (m iota-lst)
      (setf acc (* acc m)))
    acc))

;; dolist variation
(defun factorial-dolist-ex3 (n)
  (let ((iota-lst (iota1-n n)))
    (let ((acc 1))
      (dolist (m iota-lst)
        (setf acc (* acc m)))
       acc)))

;; simple-loop - by integer
(defun factorial-simple-loop-int (n)
  (let ((counter 1)
        (fact 1))
    (loop
      (when (> counter n) (return fact))
      (setf fact (* fact counter))
      (incf counter))))

;; simple-loop - by list
(defun factorial-simple-loop-list (n)
  (let ((factors (iota1-n n))
        (fact 1))
    (loop
      (when (null factors) (return fact))
      (setf fact (* fact (car factors)))
      (setf factors (cdr factors)))))

;; do
(defun factorial-do (n)
  (do ((counter 1 (1+ counter))
       (fact 1 (* fact counter)))
      ((> counter n) fact)))

;; do*
(defun factorial-do* (n)
  (do* ((counter 1 (1+ counter))
        (fact 1 (* fact counter)))
       ((>= counter n) fact)))

;; do with let
(defun factorial-do-let (n)
  (let ((fact 1))
    (do ((counter 1 (1+ counter)))
        ((> counter n) fact)
      (setf fact (* fact counter)))))

;; recursive way
(defun factorial-recursive (n)
  (if (<= n 1)
      1
      (* n (factorial-recursive (1- n)))))

;; short coding (recursive ex-1)
(defun f(n)(if(<= n 1)1(* n(f(1- n)))))

;; labels local func
(defun factorial-labels (n)
  (labels ((rec (n)
             (if (< n 1)
                 1
                 (* n (rec (1- n))))))
    (rec n)))

;; * function have its solution
(defun factorial-apply (n)
  (apply #'* (iota1-n n)))

;; reduce list
(defun factorial-reduce (n)
  (reduce #'* (iota1-n n)))

;; loop macro with let
(defun factorial-loop-macro-with-let (n)
  (let ((fact 1))
    (loop for factor from 1 to n
          do (setf fact (* fact factor)))
    fact))

;; loop macro
(defun factorial-loop-macro (n)
  (loop for factor from 1 to n
        for fact = 1 then (* fact factor)
        finally (return fact)))

;; loop macro with list
(defun factorial-loop-macro-with-list (n)
  (loop for factor in (iota1-n n)
        for fact = 1 then (* factor fact)
        finally (return fact)))

;; labels recursive + list
(defun factorial-labels-and-list (n)
  (labels ((rec (factors)
             (if (null factors)
                 1
                 (* (car factors) (rec (cdr factors))))))
    (rec (iota1-n n))))

;; do + list
(defun factorial-do-and-list (n)
  (do ((factors (iota1-n n) (cdr factors))
       (fact 1 (when factors (* fact (car factors)))))
      ((null factors) fact)))

;; tail recursion
(defun factorial-with-tail-recursion (n &optional (acc 1))
  (if (<= n 1)
      acc
      (factorial-with-tail-recursion (1- n) (* n acc))))

;; macro : target => (* 1 2 3 .. n)
(defmacro factorial-with-macro (n)
  `(* ,@(iota1-n n)))

;; macro : final target => (* n (* n-1 (* n-2 (* ... (* 2 1))))))
(defmacro factorial-with-recursive-macro (n)
  (if (<= n 1)
      `1
      `(* ,n (factorial-with-recursive-macro ,(1- n)))))

;; tagbody
(defun factorial-tagbody (n)
  (let ((fact 1)
        (factor 1))
    (tagbody
      factorial   (when (> factor n)
                        (return-from factorial-tagbody fact))
                  (setf fact (* fact factor))
                  (incf factor)
                  (go factorial))))

;; block
;; note : very looks like "simple loop"
(defun factorial-named-block (n)
  (let ((fact 1)
        (factor 1))
    (block factorial-core
      (loop
        (block meanningless
          (when (> factor n)
                (return-from factorial-core fact))
          (setf fact (* fact factor))
          (incf factor))))))

;; CLOS method
;; implicitly creating new geenric function
(defmethod factorial-clos-method ((n integer))
  (apply #'* (iota1-n n)))

#| - 残る題材
- decrimental-do
- macrolet
- closure
- memorise ?
- lambda only
- map で何とか処理
- tagbody, block - spagetti
|#
