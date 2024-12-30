; not allow to use reduce/loop/member/sort
(defun TODO (thing)
  (error "Unimplemented: ~A" thing))

;; Return T if item is a member of set.
;; Return NIL if item is not a member of set.
;; The type of set is list.
;;
;; Examples:
;;  (set-member '(1 2) 1) => T
;;  (set-member '(1 2) 3) =>  NIL
(defun set-member (set item)
  (cond
   ((null set) nil)
   ((equal (first set) item) t)
   (t (set-member (rest set) item))))

;; Return the union of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
; function set-union (set-1, set-2):
;   check if set-2 is empty:
;       if it is, return set-1
;   check if set-1 is empty:
;       if it is, return set-2
;   for each item in set-2:
;       if item is not in set-1:
;       add item to set-1
;   return set-1

;; Examples:
;;   (set-union '(1 2) '(2 4)) => '(1 2 4)
(defun set-union (set-1 set-2)
    (cond
    ((null set-2) set-1) ; if set-2 is empty, return set-1
    ; ((null set-1) set-2) ; if set-1 is empty, return set-2
    ((set-member set-1(first set-2)) ; if the first element of set-2 is in set-1
         (set-union set-1 (rest set-2))) ;
     (t (set-union (cons (first set-2) set-1) (rest set-2)))))  

;; Return the intersection of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;;
;; Examples:
;;   (set-intersection '(1 2) '(2 4)) => '(2)
(defun set-intersection (set-1 set-2)
  (cond
   ((null set-1) nil) ;if set-1 is empty, return nil(becuase there will be no intersection)
   ((set-member set-2 (first set-1)) ; if the first element of set-1 is in set-2
    (cons (first set-1) ; add the first element of set-1 
      (set-intersection (rest set-1) set-2)))
    (t (set-intersection (rest set-1) set-2))))



(defun not-in-list (to-search to-find)
  (if (car to-search)
    (if (equal (car to-search) to-find) ;; Recursive case with optional early return
      nil
      (not-in-list (cdr to-search) to-find)
    )
      
    (if (cdr to-search) ;; Should handle if a NIL is in the set but but not corresponding to the end of the set
      (if (equal (car to-search) to-find) ;; Recursive case with optional early return
        nil
        (not-in-list (cdr to-search) to-find)
      )
      to-find ;; Base case
    )
  )  
)

(defun build-set-diff (set-a set-b diff-set)
  (if (car set-a)
    (let
      (
        (ret (not-in-list set-b (car set-a)))
      )
        
      (if ret
          (build-set-diff (cdr set-a) set-b (cons ret diff-set)) ;; If ret exists add it to the front of the diff-set
          (build-set-diff (cdr set-a) set-b diff-set)  ;; If ret doesn't exist don't add it
      )
    ) ;; Recursive case when set-a is not empty
      
    (if (cdr set-a) ;; Should handle if a NIL is in the set but but not corresponding to the end of the set
      (let
        (
          (ret (not-in-list set-b (car set-a)))
        )
          
        (if ret
            (build-set-diff (cdr set-a) set-b (cons ret diff-set)) ;; If ret exists add it to the front of the diff-set
            (build-set-diff (cdr set-a) set-b diff-set)  ;; If ret doesn't exist don't add it
        )
      ) ;; Recursive case when set-a is not empty
      diff-set
    )
  )
)

;; Return the difference of set-1 and set-2.
;; The result should contain no duplicates.
;; Assume set-1 contains no duplicates and set-2 contains no duplicates.
;; Charles
;; Examples:
;;   (set-diff '(1 2) '(2 4)) => '(1)
(defun set-diff (set-1 set-2)
#| Return the elements in set-1 not in set-2. 
  General idea I have is to call a function not-in-list which recursively calls itself until there are no elements left in the list to search in for the element to find.
  This function is called over every element in set-1 and set-2 and the results are merged together into a list by another function, .
|#
  (build-set-diff set-1 set-2 '())
)

;; Return the exclusive or of a and b
;; Charles
;; Examples:
;;  (boolean-xor t nil) => t
;;  (boolean-xor nil nil) => nil
(defun boolean-xor (a b)
  (or (and (not a) b) (and a (not b))
  )
)

;; Return the implication of a and b
;; Charles
;; Examples:
;;  (boolean-implies t nil) => nil
;;  (boolean-implies nil nil) => t
(defun boolean-implies (a b)
  (or (not a) b
  )
)

; (boolean-xor nil nil)
; (boolean-xor nil T)
; (boolean-xor T nil)
; (boolean-xor T T)

; (boolean-implies nil nil)
; (boolean-implies nil T)
; (boolean-implies T nil)
; (boolean-implies T T)

; (set-diff '(1 2) '(2 3)) ;; Should return (1)
; (set-diff '('() 2) '(2 3)) ;; Should return ('())
; (set-diff '(1 '()) '('() 3)) ;; Should return (1)
; (set-diff '('() 3 2) '(2 '())) ;; Should return (3) or some permutation
; (set-diff '(3 2 NIL) '(2 '())) ;; Should return '(NIL 3) or some permutation
; (set-diff '(NIL 3 2) '(2 '())) ;; Should return '(NIL 3) or some permutation
; (set-diff '(NIL 3 2) '(nil 2 '())) ;; Should return '(3) or some permutation
; (set-diff '(3 2 NIL) '(nil 2 '())) ;; Should return '(3) or some permutation
; (set-diff '(3 2 '() NIL) '(2 nil '())) ;; Should return '(3) or some permutation
; (set-diff '(3 2 '() NIL) '(2 '() nil)) ;; Should return '(3) or some permutation
; (set-diff '(1 2) '()) ;; Should return (1 2) or some permutation
; (set-diff '() '(2 3)) ;; Should return '()
; (set-diff '(1 2 3 4 5) '(3 4 5 6 7)) ;; Should return (1 2) or some permutation
; (set-diff '('() 2 3 4 '(()) '('('()))) '(3 4 5 '('()) '(('())) '((())) 7)) ;; Should return some permutation of (NIL 2 (NIL) ('('NIL)))
; (set-diff '(NIL 'NIL 2 3 4 '(NIL) '('('NIL))) '(NIL 3 4 5 '('()) '(('())) '((())) 7)) ;; Should return some permutation of (NIL 2 (NIL) ('('NIL)))

;; Return the bi-implication (if and only if) of a and b
;;
;; Examples:
;;  (boolean-iff t nil) => nil
;;  (boolean-iff nil nil) => t
(defun boolean-iff (a b)                                                           
  (cond
    ((and a b) t)                ; Case 1: Both are true, return T
    ((and (not a) (not b)) t)     ; Case 2: Both are false, return T
    (t nil)))                    ; Default case: one is true, the other is false, return NIL

;; Evaluate a boolean expression.
;; Handle NOT, AND, OR, XOR, IMPLIES, and IFF.
;;
;; Examples:
;;  (boolean-eval '(and t nil)) => nil
;;  (boolean-eval '(and t (or nil t)) => t
  (defun boolean-eval (exp)
  (cond
    ((equal t exp) t)
    ((equal nil exp) nil)
    ((equal 'not (car exp)) (not (boolean-eval (cadr exp))))
    ((equal 'and (car exp)) (and (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal 'or (car exp)) (or (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal 'xor (car exp)) (boolean-xor (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal 'implies (car exp)) (boolean-implies (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    ((equal 'iff (car exp)) (boolean-iff (boolean-eval (cadr exp)) (boolean-eval (caddr exp))))
    (t (error "Unknown operator in expression: ~A" (car exp)))))


(boolean-eval '(and t t (or nil t (implies t nil)) t))
;; Perform the left fold on the list
;;
;; Examples:
;;   (fold-left #'- 1 '(2 3)) => -4
(defun fold-left (function initial-value list)
  (if (null list)
      initial-value
      (fold-left function 
                 (funcall function initial-value (car list))  
                 (cdr list)))) 


;; Perform the right fold on the list
;;
;; Examples:
;;   (fold-right #'- 1 '(2 3)) => 0
(defun fold-right (function initial-value list)
  (if (null list)
      initial-value
      (funcall function (car list) (fold-right function initial-value (cdr list)))))

;; Perform merge sort on the lists.
;;
;; Parameters:
;;   list: The list to sort
;;   predicate: A function to compare elements of the list
;;
;; Examples:
;;     (merge-sort '(2 1 5 0) #'<) => '(0 1 2 5)
(defun merge-sort (list predicate)
  (if (or (null list) (null (cdr list)))  ; Base case: if list is empty or has 1 element
      list                               ; Return the list as it's already sorted
      (let* ((mid (floor (/ (length list) 2)))   ; Find the midpoint, make sure it's integer, is floor okay???
             (left (subseq list 0 mid))  ; Left half of the list
             (right (subseq list mid)))  ; Right half of the list
        ;; Merge the two sorted halves using recursion directly
        (labels ((merge2 (left right)       ; Define merge as a local recursive function
                   (cond
                     ((null left) right)  ; If left list is empty, return right
                     ((null right) left)  ; If right list is empty, return left
                     ((funcall predicate (car left) (car right))  ; Compare the first elements
                      (cons (car left) (merge2 (cdr left) right)))  ; Take from left, recurse
                     (t
                      (cons (car right) (merge2 left (cdr right)))))))  ; Take from right, recurse
          ;; Call the merge function after recursively sorting left and right halves
          (merge2 (merge-sort left predicate)
                 (merge-sort right predicate))))))


;; In mathematics, the fixpoint of a function f is where:
;;    f(y) = y
;; That is, where the output equals the input
;;
;; A function may have zero or more fixpoints. For example,
;;    f(x) = x^2
;; has fixpoints:
;;    f(0) = 0
;;    f(1) = 1
;;
;; Parameters:
;;    f: the function to find the fixpoint of
;;    initial-guess: a floating point number to start at
;;    close-enough?: a predicate function
;;    decimal-places: number of decimal places to round to
;;
;; Your solution should repeatedly apply f until:
;;    (close-enough? (f next-guess) previous-guess)
;;      returns true 
;;
;; And then round the answer to the specified precision
;;
;; In pseudocode, this looks like this:
;; procedure find-fixpoint(f, initial-guess, close-enough?, precision) (
;;     previous-guess = initial-guess
;;     repeat (
;;         next-guess <- f(previous-guess)
;;         if close-enough?(next-guess, previous-guess) (
;;             return round-to-precision(next-guess, decimal-places)
;;         )
;;         previous-guess <- next-value
;;     )
;; )
;;
;; Note that there are inputs which may make this function diverge.
;;
;; Examples:
;;    (find-fixpoint (lambda (x) (* x x)) 0.9 (lambda (a b) (< (abs (- a b)) 0.0001)) 1)
;;      => 0.0
;;    (find-fixpoint (lambda (x) x) 10.0 (lambda (a b) (= a b)) 1)
;;      => 10.0
;;    (find-fixpoint #'cos 1.0 (lambda (a b) (< (abs (- a b)) 0.001)) 3)
;;      => 0.739 
;;    (find-fixpoint (lambda (x) (+ (* 0.5 x) (* 0.5 (exp (- x))))) 1.0 (lambda (a b) (< (abs (- a b)) 0.000001)) 6)
;;      => 0.567143
(defun round-to-precision (x precision)
  (let ((factor (float (expt 10 precision))))
    (/ (float (round (* x factor))) factor)))

(defun find-fixpoint (f initial-guess close-enough? precision)
  (let ((next-guess (funcall f initial-guess)))
    (if (funcall close-enough? next-guess initial-guess)
        (round-to-precision next-guess precision)
        (find-fixpoint f next-guess close-enough? precision))))


; (defun p1 (x)
;   (- (* x x) 2))  ;; p(x) = x^2 - 2

; (defun p1-prime (x)
;   (* 2 x))  ;; p'(x) = 2x

; (defun newton-f1 (x)
;   (- x (/ (funcall #'p1 x) (funcall #'p1-prime x))))  ;; Newton's method formula for p(x) = x^2 - 2


; (defun p2 (x)
;   (- (* x x x) x 2))  ;; p(x) = x^3 - x - 2

; (defun p2-prime (x)
;   (- (* 3 x x) 1))  ;; p'(x) = 3x^2 - 1

; (defun newton-f2 (x)
;   (- x (/ (funcall #'p2 x) (funcall #'p2-prime x))))  ;; Newton's method formula for p(x) = x^3 - x - 2

; (defun p3 (x)
;   (- (+ (* x x x) (* -6 (* x x)) (* 11 x)) 6))  ;; p(x) = x^3 - 6x^2 + 11x - 6

; (defun p3-prime (x)
;   (+ (* 3 (* x x)) (* -12 x) 11))  ;; p'(x) = 3x^2 - 12x + 11

; (defun newton-f3 (x)
;   (- x (/ (funcall #'p3 x) (funcall #'p3-prime x))))  ;; Newton's method formula for p(x) = x^3 - 6x^2 + 11x - 6


