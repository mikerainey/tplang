#lang racket
(require redex)

(define-language TPLANG

  (v ::= integer)      ; values
  (b ::= boolean)      ; booleans
  (pl ::=
      variable-not-otherwise-mentioned)
  (p ::= pl)           ; parameters
  (l ::= pl)           ; locals
  (r ::=               ; reducers
     variable-not-otherwise-mentioned)
  (f ::=               ; functions
     variable-not-otherwise-mentioned)
  
  (⊙ ::=               ; binary operators
     + - * / % < > <= >= == <>)             

  (eb ::=              ; boolean expressions:
      (f e ...)        ;   function call
      b)               ;   boolean value
  (e ::=               ; expressions:
     v                 ;   value
     l                 ;   local
     p                 ;   parameter
     eb                ;   boolean expression
     (f e ...))        ;   function application
  (sb ::=              ; base statements:
      (return e ...)   ;   return from function
      (sb sb)          ;   sequence
      (:= l e)         ;   local assignment
      (if eb sb sb)    ;   conditional branch
      (while eb sb)    ;   while loop
      (reduce r e))    ;   reduction
  (si ::=              ; inductive statements:
      (return e ...)   ;   return from function
      (si si)          ;   sequence
      (:= l e)         ;   local assignment
      (if eb si si)    ;   conditional branch
      (while eb si)    ;   while loop
      (spawn f e ...)) ;   parallel call
  (m ::=               ; function definitions:
     ((f p ...)        ;   function name and parameters
      (l ...)          ;   locals
      (if eb sb si) #:refers-to (p ... l ...))) ; body

  (M ::= (m ...))      ; function-definition maps
  (PL ::=              ; environments for parameters and locals
      (pl v))
  (R ::=               ; environments for reducers
     (r v v))
  (P ::=
     (M (f e ...))))

(define-judgment-form TPLANG
  #:mode (⇓-e I I I I O O O)
  #:contract (⇓-e M PL R e PL R v)

  [--------------------- "boolean"
   (⇓-e M PL R b PL R b)]

  [(where/error (v ...) (1))
   --------------------- "call"
   (⇓-e M PL R (f e ...) PL R (v ...))]

  )

(define-judgment-form TPLANG
  #:mode (⇓-b I I I I O O O)
  #:contract (⇓-b M PL R sb PL R (v ...))

  [-------------------------- "return"
   (⇓-b M PL R (return) PL R ())])