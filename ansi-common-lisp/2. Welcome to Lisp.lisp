; 2. Welcome to Lisp

; This chapter aims to get you programming as soon as possible. By the end
; of it you will know enough Common Lisp to begin writing programs.

; 2.1 Form
; It is particularly true of Lisp that you learn it by using it, because Lisp is an
; interative language. Any Lisp system will include an interactive front-end called
; the toplevel. You type Lisp expressions into the toplevel, and the system displays
; their values.

; Lisp usually displays a prompt to tell you that it's waiting for you to type
; something. Many implementations of Common Lisp use > as the toplevel prompt.
; That's what we'll use here.

; One of the simplest kinds of Lisp expression is an integer. If we enter 1 after
; the prompt,

; the system will print its value, followed by another prompt, to say that it's
; ready for more.

; In this case, the value displayed is the same as what we typed. A number like 1
; is said to evaluate to itself. Life gets more interesting when we enter
; expressions that take some work to evaluate. For example, if we want to add two
; numbers together, we type something like:

(+ 2 3)

; In the expression (+ 2 3), the + is called the operator, and the numbers 2 and 3
; are called the arguments.

; In everyday life, we would write this expression as 2 + 3, but in Lisp we
; put the + operator first, followed by the arguments, with the whole expression
; enclosed in a pair of parentheses: (+ 2 3). This is called prefix notation,
; because the operator comes first. It may at first seem a strange way to write
; expressions, but in fact this notation is one of the best things about Lisp.

; For example, if we want to add three numbers together, in ordinary notation
; we have to use + twice,
; 2 + 3 + 4
; while in Lisp, we just add another argument:
(+ 2 3 4)
; The way we ordinarily use +, it must have exactly two arguments: one on the left
; and one on the right. The flexibility of prefix notation mean that,in Lisp, +
; can take any numbers of arguments, including none:

; Because operators can take varying numbers of arguments, we need parentheses to
; show where an expression begins and ends.

; Expressions can be nested. That is, the arguments in an expression may themselves
; be complex expressions:

(/ (- 7 1) (- 4 2))

; In English, this is even minus one, divide by four minus two.

; As we will see, all Lisp code takes this form. A language like C has a more
; complicated syntax: arithmetic expressions use infix notation; function calls
; use a sort of prefix notation, with the arguments delimited by commas;
; expressions are delimited by semicolons; and blocks of code are delimited by
; curly brackets. In Lisp, we use a single notation to express all these  ideas.


; 2.2 Evaluation
; In the previous section, we typed expressions into the toplevel, and Lisp
; displayed their values. In the section we take a closer look how expressions
; are evaluated.

; In Lisp, + is a function, and an expression like (+ 2 3) is a functionc call.
; When Lisp evaluates a function call, it does so in two steps:

; 1. First the arguments are evaluated, from left to right. In this case, each
; argument evaluates to itself, so the values of the arguments are 2 and 3,
; respectively.

; 2. The values fo the arguements are passed to the function named by the operator.
; In this case, it is the + functions, which returns 5.

; If any of the arguments are themselves funciton calls, they are evaluated
; according to the same rules. So when (/ (- 7 1) (- 4 2)) is evaluated, this is
; what happens:

; 1. Lisp evaluates (- 7 1): 7 evaluates to 7 and 1 evaluates to 1. These values
; are passed to the function -, which returns 6.

; 2. Lisp evaluates (- 4 2): 4 evaluates to 4 and 2 evaluates 2. These values are
; passed to the function -, which reutrn 2.

; 3. The values 6 and 2 are sent to the function /, which return 3.

; Not all the operator in Common Lisp are functions, but most are. And function
; calls are always evaluated this way. The arguments are evaluated left-to-right,
; and their values are passed to the function, which reutrn the value of the
; expression as a whole. This is called the evaluation rule for Common Lisp.

; One operator that doesn't follow the Common Lisp evaluation rule is quote.
; The quote operator is a special operator, meaning that it has a distinct
; evaluation rule of its own. And the rule is : do nothing. The quote operator
; takes a single argument, and just returns it verabtim:

(quote (+ 3 5))

; For convenience, Common Lisp defines ' as an abbreviation for quote.
; You can get the effect of calling quote by affixing a ' to the front of any
; expressions:

; It is much more common to use the abbreviation than to write out the whole
; quote expression.

; Lisp provides the quotes as a way of protecting expressions from evaluation.
; The next section will explain how such protection can be useful.

; 2.3 Data
; Lisp offers all the data types we find in most other languages, along with
; several others that we don't. One data ty0pew e have used already is the
; integer, which is written as a series of digis: 256. Another data type Lisp
; has in common with most other languages is the string, which is represented
; as a series of characters surrounded by double-quotes: "ora et labroa".
; Integers and strings both evaluate to themselves.

; Two Lisp data types that we don't commonly find in other languages are symbols
; and lists. Symbols are words. Ordinarily they are converted to uppercase,
; regardless of how you type them:

'Arichoke

; Symbols do not (usunally) evaluate to themselves, so if you want to refer to a
; symbol, you should quote it, as above.

; Lists are represented as zero or more elements enclosed in parentheses.
; The elements can be of any type, including lists. You have to quote lists,
; or Lisp would take them for function calls:

'(my 3 "Sons")

'(the list (a b c) has 3 elements)

; Notice that one quote proectes a whole expression, including expressions within
; it.

; You can build lists by calling list. Since list is a function, its arguments
; are evaluated. Here we see a call to + within a call to list:

(list 'my (+ 2 1) "Sons")

; We are now in a position to appreciate one of the most remarkable features of
; Lisp. Lisp programs are expressed as lists. If the arguments of flexibility
; and elegance did not convince you that Lisp notation is a valuable tool, this
; point should. It means that Lisp programs can generate Lisp code. Lisp
; programmers can (and often do) write programs to write their programs for them.

; Such programs are not considered till Chapter 10, but it is important even at
; this stage to understand the relation between expressions and lists, if only
; to aovid being confused by it. This is why we need the quote. If a list is
; quoted, evaluation returns the list itself; if it is not quoted, the list is
; treated as code, and evaluation returns its vale:

(list '(+ 2 1) (+ 2 1))


; 2.4 List Operations
; The function cons builds lists. If its second argument is a list, it returns
; a new list with the first argument added to the front:

(cons 'a '(b c d))

; Whe can build up lists by consing new elements onto an empyty list. The list
; function that we saw in the previous section is just a more convenient way of
; consing several thigns onto nil:


(cons 'a (cons 'b nil))

(list 'a 'b)

; The primitive functions for extracting the elements of lists are car and cdr.
; The car of a list is the first element, and the cdr is everything after the
; first element:
(car '(a b c))

(cdr '(a b c))

; You can use combinations of car and cdr to reach any element of a list.
; If you want to get the third element, you could say:
(car (cdr (cdr '(a b c e))))

; However, you can do the same thing more easily by calling third:
(third '(a b c d))

; 2.5 Truth

; In Common Lisp, the symbol t is the default representation for truth. Like nil,
; t evaluates to itself. The function listp returns true if its argument is a
; list:

(listp '(a b c))

; A function whose return value is intended to be interpreted as truth of falsity
; is called a predicate. Common Lisp predicates often have names that end with p.
; Falsity in Common Lisp is represented by nil, the empty list. If we give listp
; an argument that isn't a list, it returns nil:
(listp 27)

; Because nil plays two roles in Common Lisp, the function null, which returns
; true of the empty list.

(null nil)

; and the function not, which returns true if its argument is false.
(not nil)

; do exactly the same thing.

; The simplest conditional in Common Lisp is if. It usually takes three arguments:
; a test expression, a then expression, and an else expression. The test expression
; is evaluated. If it returns true, the then expression is evaluated and its value
; is returned. If the test expression returns false, the else expression is
; evaluated and its value is returned:

(if (listp '(a b c))
    (+ 1 2)
    (+ 5 6))

(if (listp 26)
    (+ 1 2)
    (+ 5 6))
; Like quote, if is a special operator. It could not possibly be implemented as a
; function, because the arguments in a function call are always evaluated, and the
; whole point of it is that only one of the last two arguments is evaluated.
; The last argument to if is optional. If you omit it, it defaults to nil:

(if (listp 27)
    (+ 2 3))

; Although t is the default representation for truth, everything except nil
; also counts as true in a logical context:

(if 27 1 2)

; The logical operators and and or resemble conditionals. Both take any number
; of arguements, but onyl evaluate as many as they need to in order to decide
; what to return. If all its arguments are true (this is, not null), then and
; returns the value of the last one:

(and t (+ 1 2))

; But if one of the arguments turns out to be false, none of the arguments after
; that get evaluated. Similarly for or, which stops as soon as it finds an
; argument that is true.

; These two operators are macros. Like special operators, macros can circumvent
; the usual evaluation rule. Chapter 10 explains how to write macros of your own.


; 2.6 Functions
; You can define new functions with defun. It usually takes three or more argument:
; a name, a list of parameters, and one or more expression that will make up the
; body of the function. Here is how we might define third:

(defun our-third (x)
  (car (cdr (cdr x))))

; The first argument says that the name of this function will be our-third.
; The second argument, the list (x), says that the function will take exactly
; one argument: x. A symbol used as a placeholder in this way is called a
; variable.
; When the variable represents an argument to a function, as x does, it is also
; called a parameter.

; The rest of the definition, (car (cdr (cdr x))), is known as the body of the
; function. It tells Lisp what it has to do to calculate the return value of the
; function. So a call to our-third returns (car (cdr (cdr x))), for whatever x
; we give as the argument:

; 2.7 Recursion

; 2.8 Reading Lisp
; Read by indent , not by parentethis

; 2.9 Input and Output
(format t "~A plus ~A equals ~A.~%" 2 3 (+ 2 3))

(defun askem (string)
  (format t "~A" string)
  (read))

; read is very powerful. read is a complete Lisp parser.
; It parses what it reads, and returns the Lisp object that results.

; 2.10 Variables
; One of the most frequently used operators in Common Lisp is let, which allows
; you to introduce new local variables:

(let ((x 1) (y 2))
  (+ x y))

; A let expression has two parts.
;

(defun ask-number ()
  (format t "Please enter a number. ")
  (let ((val (read)))
    (if (numberp val)
        val
        (ask-number))))

(defparameter *glob* 1)
(defconstant limit (+ *glob* 1))
; check whether some symbol is the name of a global variable or constant
(boundp '*glob*)


; 2.11 Assignment

; setf
(setf *glob* 98)

(let ((n 10))
  (setf n 2)
  n)

(setf x (list 'a 'b 'c))

(setf (car x) 'n)

; You can give any(even) numbers of arguments to setf. 
(setf a b
      c d
      e f)

; 2.12 Functional Programming
; Functional programming mean writing programs that work by returning values,
; instead of by modifying things. 
(setf lst '(c a r a t))

; 2.13 Iteration
(defun show-squares (start end)
  (do ((i start (+ i 1)))
      ((> i end) 'done)
    (format t "~A ~A~%" i (* i i))))

; recursive version
(defun show-squares (i end)
  (if (> i end)
      'done
      (progn
        (format t "~A ~A~%" i (* i i))
        (show-squares (+ i 1) end))))

(defun our-length (lst)
  (let ((len 0))
    (dolist (obj lst)
      (setf len (+ len 1)))
    len))

; recursive version
(defun our-length (lst)
  (if (null lst)
      0
      (+ (our-length (cdr lst)) 1)))

; 2.14 Functions as Objects
(function +)

; 2.15 Types
; The function typep takes an object and a type specifier, and returns true
; if the object is of that type:
(typep 27 'integer)

; 2.16 Looking Forward

