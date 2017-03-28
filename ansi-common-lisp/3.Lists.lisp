;3.1 Conses

;Conceptually, a cons is a pair of pointers
;the first one is the car and the second is the cdr.

;The function consp returns true if its argument is a cons. 
(defun our-listp (x)
  (or (null x) (consp x)))

(defun our-atom (x)
  (not (consp x)))


; 3.2 Equality
; Each time you call cons, Lisp allocates a new piece of memory with room
; for two pointers. So if we call cons twice the same arguments,
; we get back two values that look the same, but are in fact distinct objects:

; eql : 실제로 같은 오브젝트일 때 참 반환
; equal : 프린트되는 값이 같으면 참 반환


; 3.3 Why Lisp Has No Pointers
; One of the secrets to understanding Lisp is to realize that variables
; have values in the same way that lists have elements.

; As conses have pointers to their elements,
; variables have pointers to their values. 

; The reason Lisp has no pointers is that every value is conceptually a pointer.




; 3.6 Access
(defun our-nthcdr (n lst)
  (if (zerop n)
      lst
      (our-nthcdr (- n 1)(cdr lst))))


; 3.7 Mapping Functions
(maplist #'(lambda (x) x)
         '(a b c))


; 3.8 Trees
; Conses can also be considered as binary trees, 
; copy-tree takes a tree and returns a copy of it.
(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr))
            (our-copy-tree (cdr tr)))))

(and (integerp x) (zerop (mod x 2)))

; 3.9 Understanding Recursion
; A programmer defining a recursive function usually does not think explicitly
; about the sequence of invocations that results from calling it.

; The secret to understanding recursion is a lot like the secret for dealing
; with parentheses. How do you see which parenthesis matches which?
; You don't have to. How do you visualize all those invocations?
; You don't have to.

; When a recursive function doesn't behabe as you intended, it is usually because
; the base case is wrong.

; 3.10 Sets
; subseq
(subseq '(a b c d) 1 )

(defun mirror? (s)
  (let ((len (length s)))
    (and (evenp len)
         (let ((mid (/ len 2)))
           (equal (subseq s 0 mid)
                  (reverse (subseq s mid)))))))


; Sort
; You have to be careful when using sort, because it's destructive.

(defun nthmost (n lst)
  (nth (- n 1)
       (sort (copy-list lst) #'>)))
    
         
; 3.12 Stacks
; The representation of lists as conses makes it natural to use them as
; pushdown stacks.

    
