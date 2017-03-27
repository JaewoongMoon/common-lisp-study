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
