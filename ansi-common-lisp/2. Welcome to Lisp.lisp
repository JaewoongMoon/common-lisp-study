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




