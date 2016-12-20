;/*********************************************************************************************************/
;/****************************************** 장소(NODES) **************************************************/
;/*********************************************************************************************************/
;-- STEP 1. 세 장소에 대한 설명을 담은 기본적인 변수인 *nodes* 선언
(defparameter *nodes* '((living-room (you are in the living-room.
                          a wizard is snoring loudly on the couch.))
                        (garden (you are in a beautiful garden'
                                 there is a well in front of you.))
                         (attic (you are in the attic.
                                 there is a giant welding torch in the corner.))))

;-- 리스트에서 해당 아이템을 찾기위해서 assoc 함수를 사용해보자.  
(assoc 'garden *nodes*)            
;=> 결과 : (garden (you are in a beautiful garden' there is a well in front of you.))

;-- STEP 2. assoc 함수를 써서 장소 설명 함수 describe-location 함수를 작성
(defun describe-location (location nodes)
     (cadr (assoc location nodes)))
     

;-- 테스트 : living-room 심볼을 *nodes* 에서 찾는다.      
(describe-location 'living-room *nodes*)
;=> (cadr (assoc 'living-room *nodes*)) 와 동일함. 
;=> 결과 : (you are in a beautiful garden' there is a well in front of you.)


/**********************************************************************************************************/
/****************************************** 경로(EDGES) ***************************************************/
/**********************************************************************************************************/
;-- STEP 1. 경로를 표현하기 위한 *edges* 변수 선언. 
;-- 형식 :  [ 현재장소  (다른장소1 가는방향 가는방법) (다른장소2 가는방향 가는방법) ] 
(defparameter *edges* '((living-room (garden west door)
                                      (attic upstairs ladder))
                         (garden  (living-room east door))
                         (attic (living-room downstairs ladder))))

;-- STEP 2. 경로를 설명하기 위한 describe-path 함수를 작성
(defun describe-path (edge)
   `(there is a , (caddr edge) going , (cadr edge) from here.))

;-- 설명 THERE IS A (가는방법) GOING (가는방향) FROM HERE. 
;-- 테스트 : 거실에서 정원으로 가기위한 엣지(garden west door) 를 설명한다. 
(describe-path '(garden west door))
;=> 결과 : (THERE IS A DOOR GOING WEST FROM HERE.)
)


;-- STEP 3. 한 장소는 하나 이상의 경로를 가지므로, 모든 경로를 표현하는 함수를 작성. 
(defun describe-paths (location edges)
   (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;/**
;설명 : describe-paths 함수는 다음과 같은 과정을 거친다. 
 ;; 1. 관련된 에지들을 찾는다. =>  (cdr (assoc location edges))
; 2. 이 에지를 설명으로 바꾼다. => (mapcar #'describe-path (에지 리스트))
; 3. 설명을 하나로 합친다. => (apply #'append (에지 설명 리스트))
;*/

;-- 테스트 : (describe-paths 'living-room *edges*)
;-- 결과 : (THERE IS A DOOR GOING WEST FROM HERE. THERE IS A LADDER GOING UPSTAIRS FROM HERE.)


																		
																	   																		
;/***********************************************************************************************************/
;/******************************************* 물건(OBJECT) **************************************************/
;/***********************************************************************************************************/
;-- 집을 수 있는 물건들의 나열 
(defparameter *objects* '(whiskey bucket frog chain))

;-- 플레이어가 특정 장소에서 집을 수 있는 물건을 묘사
(defparameter *object-locations* '((whiskey living-room)
								   (bucket living-room)
								   (chain garden)
								   (frog garden)))

;-- 특정 장소에서 볼 수 있는 물건의 목록을 구하는 함수																		
(defun objects-at (loc objs obj-locs)
    (labels ((at-loc-p (obj)
             (eq (cadr (assoc obj obj-locs)) loc)))
    (remove-if-not #'at-loc-p objs)))
 


; -- 눈에 보이는 물건을 묘사하는 함수
(defun describe-objects (loc objs obj-loc)
  (labels ((describe-obj (obj)
						 `(you see a, obj on the floor.)))
		  (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-loc)))))

;-- 플레이어의 현재 위치를 저장하는 *location* 변수
(defparameter *location* 'living-room)


;-- 전체를 둘러보는 함수  
(defun look ()
  (append (describe-location *location* *nodes*)
		  (describe-paths *location* *edges*)
		  (describe-objects *location* *objects* *object-locations*))) 
 				 					
;-- 걷기 동작 함수 			 					
(defun walk (direction)
  (let ((next (find direction
					(cdr (assoc *location* *edges*))
					:key #'cadr)))
	(if next
		(progn (setf *location* (car next))
			   (look))
	  '(you cannot go that way.))))
		  	 
;-- 물건집기 동작 함수 
(defun pickup (object)
  (cond ((member object
				 (objects-at *location* *objects* *object-locations*))
		 (push (list object 'body) *object-locations*)
		 `(you are now carrying the ,object))
		(t '(you cannot get that.))))
				 
;-- 보관함 확인하기 함수
(defun inventory ()
		(cons 'items- (objects-at 'body *objects* *object-locations*)))
		
		
-----------------------------------------------------------------------------------------------------------

(defun game-repl ()
   (let ((cmd (game-read)))
        (unless (eq (car cmd) 'quit)
                (game-print (game-eval cmd))
                (game-repl))))
                
(defun game-read ()
    (let ((cmd (read-from-string
                    (concatenate 'string "(" (read-line) ")"))))
           (flet ((quote-it (x)
                            (list 'quote x)))
                 (cons (car cmd) (mapcar #'quote-it (cdr (cmd)))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
       (if (member (car sexp) *allowed-commands*)
           (eval sexp)
           '(i do not know that command.)))   
           
(defun tweak-text (lst caps lit)
  (when lst
  (let ((item (car lst))
        (rest (cdr lst)))
  (cond ((eql item #\space) (cons item (tweak-text rest caps lit)))
         ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
         ((eql item #\") (tweak-text rest caps (not lit)))
         (lit (cons item (tweak-text rest nil lit)))
         (caps (cons (char-upcase item) (tweak-text rest nil lit)))
         (t (cons (car-downcase item) (tweak-text rest nil nil)))))))
         
 (defun game-print (lst)
      (princ (coerce (tweak-text (coerce (string-trim "()"
                                                        (prin1-to-string lst))
                                          'list)
                                  t
                                  nil)
                      'string))
      (fresh-line))    
      
      
;==========================================================================================
;-- dot-name 함수
   (defun dot-name (exp)
    (substitute-if #\_ (complement #'alphanumericp) (print1-to-string exp)))
    
    
