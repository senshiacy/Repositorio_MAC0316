#|
MAC0316 - EP00
Nathália Yukimi Uchiyama Tsuno
14600541
|#

#lang plai-typed

(define-type ArithC ;; Construção de um tipo, um template para a nossa linguagem
  ;; Construtor do numC
  [numC (n : number)] ;; Subtipo <- Recebe um número e devolve como numC, Todo number que ele receber, vai transformar num numC
  ;; Construtor boolC
  [boolC (b : boolean)] ;; Subtipo <- Recebe um boolean e devolve com boolC, Todo boolean que ele receber vai transformar num boolC

  ;; Se quisermos operar nossos numC e boolC, usamos essas operações
  ;; Construtor andC
  [andC (l : ArithC) (r : ArithC)] ;; Expressão andC
  ;; Construtor plusC
  [plusC (l : ArithC) (r : ArithC)] ;; Expressão plusC
  ;; Construtor multC
  [multC (l : ArithC) (r : ArithC)] ;; Expressão multC
  ;; Construtor lessC
  [lessC (l : ArithC) (r : ArithC)] ;; Expressão lessC
  ;; Construtor ifC
  [ifC (c : ArithC) (t : ArithC) (e : ArithC)] ;; Expressão ifC

    #|
  Inclusão do EP
  |#
  ;; Item 2 - 'Divisão'
  [divC (l : ArithC) (r : ArithC)]
  ;; Item 3 - 'Or'
  [orC    (l : ArithC) (r : ArithC)]
  ;; Item 4 - 'Igualdade'
  [equalC    (l : ArithC) (r : ArithC)]
  ;; Item 6 - 'Not'
  [notC (l : ArithC)]
  )

#|
Interpretador de ArithC
|#

(define (interp [a : ArithC]) : number ;; Tipo de parâmetro, tipo de retorno 
  (type-case ArithC a ;; Recebe um a e verifica o que ele é/ subtipo de ArithC
    [numC (n) n] ;; Se a expr é um numC, retorne ele mesmo
    [boolC (b) (if b 1 0)] ;; Se a expr é um boolC, retorne 1, se b é true. E 0, caso contrário
    #|
    Transforma l num booleano e compara com o false
    - Se for igual (l é false), então o valor retorna true, mas, o not deixa false
    - Se for diferente (l é true), então, o valor retorna false, mas,  o not deixa true
    |#
    [andC (l r) (if (and (not (= (interp l) 0)) (not (= (interp r) 0)))
                    1 ;; Se ambos forem verdadeiros
                    0)] ;; Caso contrário
    [plusC (l r) (+ (interp l) (interp r))]
    [multC (l r) (* (interp l) (interp r))]
    #|
    Tranforma l e r em números
    - Se l for menor do que r, retorne 1
    - Se l for maior do que r, retorne 0
    |#
    [lessC (l r) (if (< (interp l) (interp r))
                     1
                     0)]
    [ifC (c t e) (if (not (= (interp c) 0))
                     (interp t) ;; ArithC -> Recursivamente, pode ser aplicado até que se reste um bool ou um num
                     (interp e) ;; ArithC -> Recursivamente, pode ser aplicado até que se reste um bool ou um num
                     )]
    
   #|
   Inclusão do EP
   |#
    ;; Item 2 - 'Divisão'
    [divC (l r) (/ (interp l) (interp r))]
    ;; Item 3 - 'Or'
    [orC (l r) (if (or (not (= (interp l) 0)) (not (= (interp r) 0)))
                    1 ;; Se ambos forem verdadeiros
                    0)] ;; Caso contrário
    ;; Item 4 - 'Igualdade'
    [equalC (l r) (if (eq? (interp l) (interp r))
                    1 ;; Se ambos forem verdadeiros
                    0)] ;; Caso contrário
    ;; Item 6 - 'Not'
    [notC (l) (if (= (interp l) 0) ;; Se l = 0, isso retorna 1
                  1 ;; l é 0, not l é 1
                  0)]
   ))

; Tipo definido com Sinal
(define-type ArithS
  ;; Construtor do numS
  [numS (n : number)]
  ;; Construtor do boolS
  [boolS (b : boolean)]

  ;; Se quisermos operar nossos numS e boolS, usamos estas operações
  [andS    (l : ArithS) (r : ArithS)]
  [plusS   (l : ArithS) (r : ArithS)]
  [multS   (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]
  [lessS   (l : ArithS) (r : ArithS)]
  [ifS  (c : ArithS) (t : ArithS) (e : ArithS)]

  #|
  Inclusão do EP
  |#
  ;; Item 1 - 'Menos Unário'
  [uminusS (l : ArithS)]
  ;; Item 2 - 'Divisão'
  [divS (l : ArithS) (r : ArithS)]
  ;; Item 3 - 'Or'
  [orS    (l : ArithS) (r : ArithS)]
  ;; Item 4 - 'Igualdade'
  [equalS    (l : ArithS) (r : ArithS)]
  ;; Item 6 - 'Not'
  [notS (l : ArithS)]
  ;; Item 7 - 'Maior'
  [moreS (l : ArithS) (r : ArithS)]
  )

#|
Interpretador de ArithS
|#

(define (desugar [as : ArithS]) : ArithC
  (type-case ArithS as
    [numS (n) (numC n)]
    [boolS (b) (boolC b)]
    #|
    Aplique recursivamente o desugar
    |#
    [andS (l r) (andC (desugar l) (desugar r))]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [lessS (l r) (lessC (desugar l) (desugar r))]
    [ifS (c t e) (ifC (desugar c) (desugar t) (desugar e))]

    #|
  Inclusão do EP
  |#
    ;; Item 2 - 'Divisão'
    [divS (l r) (divC (desugar l) (desugar r))]
    ;; Item 3 - 'Or'
    [orS (l r) (orC (desugar l) (desugar r))]
    ;; Item 4 - 'Igualdade'
    [equalS (l r) (equalC (desugar l) (desugar r))]
    ;; Item 6 - 'Not'
    [notS (l) (notC (desugar l))]

    ;; Açúcar Sintático
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]

    #|
   Inclusão do EP
   |#
    ;; Item 1 - 'Menos Unário'
    [uminusS (l) (multC (numC -1) (desugar l))]
    ;; Item 7 - 'Maior'
    [moreS (l r) (ifC (lessC (desugar l) (desugar r))
                      (numC 0) ;; Se é menor, não é maior
                      (ifC (equalC (desugar l) (desugar r))
                         (numC 0) ;; Se é igual, não é maior
                       (numC 1))
                      )]
   ))

(define (interpS [s : ArithS]) : number
  (interp (desugar s)))

#|
Parser
|#

(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-boolean? s) (boolS (s-exp->boolean s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(and) (andS (parse (second sl)) (parse (third sl)))]
         [(<) (lessS  (parse (second sl)) (parse (third sl)))]
         [(if)(ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         #|
  Inclusão do EP
  |#
         ;; Item 1 - 'Menos Unário'
         [(~) (uminusS (parse (second sl)))]
         ;; Item 2 - 'Divisão'
         [(/) (divS (parse (second sl)) (parse (third sl)))]
         ;; Item 3 - 'Or'
         [(or) (orS (parse (second sl)) (parse (third sl)))]
         ;; Item 4 - 'Igualdade'
         [(=) (equalS (parse (second sl)) (parse (third sl)))]
         ;; Item 6 - 'Not'
         [(not) (notS (parse (second sl)))]
         ;; Item 7 - 'Maior'
         [(>) (moreS (parse (second sl)) (parse (third sl)))]
         
         [else
          (error 'parse "invalid input")]
         )
       )
     ]
    [else (error 'parse "invalid input")]
   ))
