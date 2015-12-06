;; -----------------------------------------------------------------------------
;; Factorial functions
;; -----------------------------------------------------------------------------

;;; dotimes
(defun factorial-dotimes (n)
  (let ((acc 1))
    (dotimes (k-1 n)
      (setf acc (* acc (1+ k-1))))
    acc))

;;; simple-loop
(defun factorial-simple-loop (n)
  (let ((k 1)
        (acc 1))
    (loop
      (when (> k n) (return acc))
      (setf acc (* acc k))
      (incf k))))

;;; do
(defun factorial-do (n)
  (do ((k 1 (1+ k))
       (acc 1 (* acc k)))
      ((> k n) acc)))

;;; do*
(defun factorial-do* (n)
  (do* ((k 1 (1+ k))
        (acc 1 (* acc k)))
       ((>= k n) acc)))

;;; recursive
(defun factorial-recursive (n)
  (if (<= n 1)
      1
      (* n (factorial-recursive (1- n)))))

;;; labels
(defun factorial-labels (n)
  (labels ((rec (n)
             (if (< n 1)
                 1
                 (* n (rec (1- n))))))
    (rec n)))

;;; labels - tail recursion
(defun factorial-labels-tail-recursion (n)
  (labels ((rec (n &optional (acc 1))
             (if (< n 1)
                 acc
                 (rec (1- n) (* n acc)))))
    (rec n)))


;;; loop macro
(defun factorial-loop (n)
  (loop for k from 1 to n
        for acc = 1 then (* acc k)
        finally (return acc)))

;;; loop macro - decrement order
(defun factorial-loop-decrement (n)
  (loop for k downfrom n downto 1
        for acc = n then (* acc k)
        finally (return acc)))

;;; tagbody
(defun factorial-tagbody (n)
  (let ((acc 1)
        (k 1))
    (tagbody
      factorial   (when (> k n)
                        (return-from factorial-tagbody acc))
                  (setf acc (* acc k))
                  (incf k)
                  (go factorial))))

; -----------------------------------------------------------------------------
; special variation
; -----------------------------------------------------------------------------

;;; accumlate value keeps with optional argument
;;; NOTE : this function work wrong when acc value has passed
;;;   ex ) (factorial-args-optional 3 2)
(defun factorial-args-optional (n &optional (acc 1))
  (dotimes (k-1 n)
    (setf acc (* acc (1+ k-1))))
  acc)

;;; * function has simple solution
(defun factorial-apply (n)
  (apply #'* (iota1-n n)))

;;; reduce list
(defun factorial-reduce (n)
  (reduce #'* (iota1-n n)))

;; CLOS method
;; implicitly creating new geenric function
(defmethod factorial-clos-method ((n integer))
  (apply #'* (iota1-n n)))

;;; macro : target => (* 1 2 3 .. n)
(defmacro factorial-with-macro (n)
  `(* ,@(iota1-n n)))

;;; macro : target => (* n (* n-1 (* n-2 (* ... (* 2 1))))))
(defmacro factorial-with-recursive-macro (n)
  (if (<= n 1)
      `1
      `(* ,n (factorial-with-recursive-macro ,(1- n)))))

;;; macro : target => (* 1 (* 2 (* 3 (* ... (* n-1 n)..))))
(defmacro factorial-with-outside-macro (n &body body)
  (if (<= n 1)
      `(* 1 ,@body)
      `(factorial-with-outside-macro ,(1- n) (* ,n ,@body))))

;;; length as factor
(defun factorial-length (n)
  (do ((n-lst '(nil) (push 'nil n-lst))
       (acc 1 (* acc (length n-lst))))
      ((>= (length n-lst) n) acc)))

;; -----------------------------------------------------------------------------
;; sub functions
;; -----------------------------------------------------------------------------
;;; helper iota 1 to n
(defun iota1-n (n)
  (loop for i from 1 to n
        collecting i))

; -----------------------------------------------------------------------------
; list process variation
; -----------------------------------------------------------------------------
;;; dolist
(defun factorial-dolist (n)
  (let ((acc 1))
    (dolist (m (iota1-n n))
      (setf acc (* acc m)))
    acc))

;;; simple-loop - with list
(defun factorial-simple-loop-list (n)
  (let ((factor-lst (iota1-n n))
        (acc 1))
    (loop
      (when (null factor-lst) (return acc))
      (setf acc (* acc (car factor-lst)))
      (setf factor-lst (cdr factor-lst)))))

;;; do - with list
(defun factorial-do-list (n)
  (do ((factor-lst (iota1-n n) (cdr factor-lst))
       (acc 1 (when factor-lst (* acc (car factor-lst)))))
      ((null factor-lst) acc)))

;;; labels recursive - with list
(defun factorial-labels-list (n)
  (labels ((rec (factor-lst)
             (if (null factor-lst)
                 1
                 (* (car factor-lst) (rec (cdr factor-lst))))))
    (rec (iota1-n n))))

;;; labels tail recursion - with list
(defun factorial-labels-tail-recursion-list (n)
  (labels ((rec (factor-lst &optional (acc 1))
             (if (null factor-lst)
                 acc
                 (rec (cdr factor-lst) (* acc (car factor-lst))))))
    (rec (iota1-n n))))

;;; loop macro - with list
(defun factorial-loop-macro-list (n)
  (loop for m in (iota1-n n)
        for acc = 1 then (* m acc)
        finally (return acc)))

;;; tagbody - with list
(defun factorial-tagbody-list (n)
  (let ((acc 1)
        (factor-lst (iota1-n n)))
    (tagbody
      factorial   (unless factor-lst
                        (return-from factorial-tagbody-list acc))
                  (setf acc (* acc (car factor-lst)))
                  (setf factor-lst (cdr factor-lst))
                  (go factorial))))

; -----------------------------------------------------------------------------
; NG
; -----------------------------------------------------------------------------

;;; x - dotimes - too redundant - mediator loop variable i
(defun factorial-dotimes-too-redundant (n)
  (let ((k 1))
    (let ((acc 1))
      (dotimes (i n)
        (setf acc (* acc k))
        (incf k))
      acc)))

;;; x - dotime - use as index
(defun factorial-dotimes-as-index (n)
  (let ((factor-lst (iota1-n n))
        (acc 1))
    (dotimes (index n)
      (setf acc (* acc (nth index factor-lst))))
    acc))

;;; x - dolist - too redundant - let variation 1
(defun factorial-dolist-too-redundant1 (n)
  (let ((factor-lst (iota1-n n))
        (acc 1))
    (dolist (m factor-lst)
      (setf acc (* acc m)))
    acc))

;;; x - dolist - too redundant - let variation 2
(defun factorial-dolist-too-redundant2 (n)
  (let ((factor-lst (iota1-n n)))
    (let ((acc 1))
      (dolist (m factor-lst)
        (setf acc (* acc m)))
      acc)))

;;; x - dolist - use index of item
(defun factorial-dolist-index-of-item (n)
  (let ((factor-lst (iota1-n n))
        (acc 1))
    (dolist (m factor-lst)
      (setf acc (* acc (1+ (search (list m) factor-lst)))))
    acc))

;;; x - simple-loop + optional argument
;;; NOTE : this function work wrong when k, acc value has passed
;;;   ex ) (factorial-args-optional 3 5 7)
(defun factorial-simple-loop-variables-on-argument (n &optional (k 1) (acc 1))
  (loop
    (when (> k n) (return acc))
    (setf acc (* acc k))
    (incf k)))

;;; x - do with let - accumulates as let variable
(defun factorial-do-let (n)
  (let ((acc 1))
    (do ((k 1 (1+ k)))
        ((> k n) acc)
      (setf acc (* acc k)))))

;;; x - do + optional argument
;;; NOTE : this function work wrong when acc value has passed
;;;   ex ) (factorial-args-optional 3 5)
(defun factorial-do-variables-on-argument (n &optional (acc 1))
  (do ((k 1 (1+ k)))
      ((> k n) acc)
    (setf acc (* acc k))))

;;; x - do - decrement order
(defun factorial-do-decrement (n)
  (do ((k n (1- k))
       (acc 1 (* acc k)))
      ((<= k 0) acc)))

;;; x - tail recursion
;;; NOTE : this function work wrong when acc value has passed
;;;   ex ) (factorial-tail-recursion 3 2)
(defun factorial-tail-recursion (n &optional (acc 1))
  (if (<= n 1)
      acc
      (factorial-tail-recursion (1- n) (* n acc))))

;;; x - loop macro with let
(defun factorial-loop-with-let (n)
  (let ((acc 1))
    (loop for k from 1 to n
          do (setf acc (* acc k)))
    acc))

;;; x - macro dotimes
(defmacro factorial-macro-dotimes (n)
  `(let ((acc 1))
     (dotimes (k-1 ,n)
       (setf acc (* acc (1+ k-1))))
     acc))
