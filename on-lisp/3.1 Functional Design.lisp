
(defun good-reverse (lst)
  (labels ((rev (lst acc)
			 (if (null lst)
				 acc
				 (rev (cdr lst) (cons (car lst) acc)))))
	(rev lst nil)))


; 두 개의 값을 반환하는 예제
(multiple-value-bind (int frac) (truncate 26.21875)
  (list int frac))


;  values 연산자를 써서 여러개의 값을 반환할 수 있다.
(defun powers (x) 
   (values x (sqrt x) (expt x 2)))

(multiple-value-bind (base root square) (powers 4)
  (list base root square))


(defun fun (x)
  (list 'a (expt (car x) 2)))

(fun '(1 2))

(defun imp (x)
  (let (y sqr)
	(setq y (car x))
	(setq sqr (expt y 2))
	(list 'a sqr)))

(imp '(2 5))

; 3.3 Functional Interfaces

(defun qualify (expr)
  (nconc (copy-list expr) (list 'maybe)))


