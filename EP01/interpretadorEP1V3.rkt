#lang plai-typed

#|
Nathália Yukimi Uchiyama Tsuno
14600541
|#

#|
 | interpretador simples, sem variáveis ou funções
 |#

#| primeiro as expressões "primitivas", ou seja, diretamente interpretadas
 |#

(define-type ExprC ;; AST : Abstract Syntax Tree
  [numC    (n : number)]
  [idC     (s : symbol)]
  [plusC   (l : ExprC) (r : ExprC)]
  [multC   (l : ExprC) (r : ExprC)]
  [lamC    (arg : symbol) (body : ExprC)]
  [appC    (fun : ExprC) (arg : ExprC)]
  [ifC     (cond : ExprC) (y : ExprC) (n : ExprC)]
  [consC   (car : ExprC) (cdr : ExprC)]; Creates cell with a pair
  [carC    (pair : ExprC)]; Gets 1st element of a pair
  [cdrC    (pair : ExprC)]; Gets 2nd element of a pair
  #|
  EP01
  |#
  [letrecC (x1 : symbol) (v1 : ExprC) (b : ExprC)]
  [quoteC  (exp : symbol)] ;; Espero que não tenha algo como (quote (quote alan))...
  [read-loopC ]
  #|
  Extras
  |#
  [eqC     (l : ExprC) (r : ExprC)]
  [gtC     (l : ExprC) (r : ExprC)]
  [ltC     (l : ExprC) (r : ExprC)]
  )
#| agora a linguagem aumentada pelo açúcar sintático
 | neste caso a operação de subtração e menus unário
 |#

(define-type ExprS ;; AST : Abstract Syntax Tree
  [numS    (n : number)]
  [idS     (s : symbol)]
  [lamS    (arg : symbol) (body : ExprS)]
  [appS    (fun : ExprS) (arg : ExprS)]
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [consS   (car : ExprS) (cdr : ExprS)]
  [carS    (pair : ExprS)]
  [cdrS    (pair : ExprS)]
  #|
  EP01
  |#
  [letS    (x1 : symbol) (v1 : ExprS) (b : ExprS)]
  [let*S   (x1 : symbol) (v1 : ExprS) (x2 : symbol) (v2 : ExprS) (b : ExprS)]
  [letrecS (x1 : symbol) (v1 : ExprS) (b : ExprS)]
  [quoteS  (exp : symbol)]
  [read-loopS ]
  #|
  Extras
  |#
  [eqS     (l : ExprS) (r : ExprS)]
  [gtS     (l : ExprS) (r : ExprS)]
  [ltS     (l : ExprS) (r : ExprS)]
  )


(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [idS     (s)        (idC s)]
    [lamS    (a b)      (lamC a (desugar b))]
    [appS    (fun arg)  (appC (desugar fun) (desugar arg))]
    [plusS   (l r)      (plusC (desugar l) (desugar r))]
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [bminusS (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [ifS     (c y n)    (ifC (desugar c) (desugar y) (desugar n))]
    [consS   (b1 b2)    (consC (desugar b1) (desugar b2))]
    [carS    (c)        (carC (desugar c))]
    [cdrS    (c)        (cdrC (desugar c))]
    #|
    EP01
    |#
    [letS    (x1 v1 b)  (appC (lamC x1 (desugar b)) (desugar v1))]
    [let*S   (x1 v1 x2 v2 b) (appC (lamC x1 (appC (lamC x2 (desugar b)) (desugar v2))) (desugar v1))]
    [letrecS (x1 v1 b) (letrecC x1 (desugar v1) (desugar b))] ;; Mudar para lambda
    [quoteS  (exp)       (quoteC exp)]
    [read-loopS () (read-loopC)]
    #|
    Extras
    |#
    [eqS     (l r)       (eqC (desugar l) (desugar r))]
    [gtS     (l r)       (gtC (desugar l) (desugar r))]
    [ltS     (l r)       (ltC (desugar l) (desugar r))]
    ))



; We need a new value for the box
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [consV (car : Value) (cdr : Value)]
  #|
  EP01
  |#
  [symV  (exp : symbol)]
  [endV] ;; Para saída do read-loop
  )


; Bindings associate symbol with Boxes
; we need this to be able to change the value of a binding, which is important
; to implement letrec.

(define-type Binding
        [bind (name : symbol) (val : (boxof Value))])


; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)


; Storage's operations are similar to Env's
;   bind <-> cell
;   mt-env <-> mt-store
;   extend-env <-> override-store


; lookup changes its return type
(define (lookup [varName : symbol] [env : Env]) : (boxof Value); lookup returns the box, we need this to change the value later
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string varName) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                    [(symbol=? varName (bind-name (first env)))   ; achou!
                     (bind-val (first env))]
                    [else (lookup varName (rest env))])]))        ; vê no resto



; Primitive operators
(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "Um dos argumentos não é número")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "Um dos argumentos não é número")]))
#|
Extras
|#
(define (num= [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (if (= (numV-n l) (numV-n r))
         (numV 1)
         (numV 0))]
    [else
             (error 'num+ "Um dos argumentos não é número")]
    ))

(define (num> [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (if (> (numV-n l) (numV-n r))
         (numV 1)
         (numV 0))]
    [else
             (error 'num+ "Um dos argumentos não é número")]
    ))

(define (num< [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (if (< (numV-n l) (numV-n r))
         (numV 1)
         (numV 0))]
    [else
             (error 'num+ "Um dos argumentos não é número")]
    ))

#|
EP01
|#
(define (numRead-loop) : Value
  (let ([input(read)])
    (if(and(s-exp-symbol? input)(eq? (s-exp->symbol input) '@end))
       (begin
        (display 'FINISHED-INTERPRETER)
        (endV)) ;; Valor de retorno
       (begin
         (display "\nintepret-command:")
              (display input)
              (display "\nresult:")
              (display (interpS input))
              (display "\n")
              (numRead-loop)
              )
       )
    )
  )

; Return type for the interpreter, Value


(define (interp [a : ExprC] [env : Env] ) : Value
  (type-case ExprC a
    [numC (n) (numV n) ]
    [idC (n)  (unbox (lookup n env))]; we need to unbox the value in the environment before using it
    [lamC (a b) (closV a b env) ]

 
    ; application of function
    [appC (f a)
          (let ((closure (interp f env))
                (argvalue (interp a env)))
            (type-case Value closure
              [closV (parameter body env)
                     (interp body (extend-env (bind parameter (box argvalue)) env))]
              [else (error 'interp "operation app aplied to non-closure")]
              ))]
   
    ;I left plusC without error-checking
    [plusC (l r)
             (let ((left (interp l env))
                   (right (interp r env)))
               (num+ left right))]
    ;multC
    [multC (l r)
           (let ( (left (interp l env))
                  (right (interp r env)))
             ;in this case type cheking is a little different
             (if (numV? left)
                 (if (numV? right)
                     (num* left right)
                     (error 'interp "second argument of multiplication not a number value"))
                 (error 'interp "first argument of multiplication not a number value"))
                 )]
    ; ifC serializes
    [ifC (c s n) (type-case Value (interp c env)
                   [numV (value)
                        (if (zero? value)
                            (interp n env )
                            (interp s env ))]
                   [else (error 'interp "condition not a number")]
                   )]

    ; Working with lists
    [consC (b1 b2) (let ( (car (interp b1 env))
                          (cdr (interp b2 env)))
                     (consV car cdr))]
    [carC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       car]
                [else (error 'interp "car applied to non-cell")]
                )]
    [cdrC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       cdr]
                [else (error 'interp "cdr applied to non-cell")]
                )]
                 
    #|
    EP01
    |#
    [letrecC (x1 v1 body)
             (let* (
                   [env2 (extend-env (bind x1 (box (numV 0))) env)]
                   [g (interp v1 env2)]
                   )
               (begin
                 (set-box! (lookup x1 env2) g)
                 (interp body env2)))]
    
    [quoteC (exp) (symV exp)]

    [read-loopC () (numRead-loop)]

    #|
    Extras
    |#
    [eqC (l r) (let ((left (interp l env))
                     (right (interp r env)))
                 (num= left right))]

    [gtC (l r) (let ((left (interp l env))
                     (right (interp r env)))
                 (num> left right))]

    [ltC (l r) (let ((left (interp l env))
                     (right (interp r env)))
                 (num< left right))]
    ))


; Parser with funny instructions for boxes
(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(cons) (consS (parse (second sl)) (parse (third sl)))]
         [(car) (carS (parse (second sl)))]
         [(cdr) (cdrS (parse (second sl)))]
         #|
         EP01
         |#
         [(let) (let ([lista (s-exp->list (first (s-exp->list (second sl))))])
                  (letS (s-exp->symbol (first lista)) (parse (second lista)) (parse (third sl))))]
         [(let*) (let ([arg1 (s-exp->list (first (s-exp->list (second sl))))] [arg2 (s-exp->list (second (s-exp->list (second sl))))])
                  (let*S (s-exp->symbol (first arg1)) (parse (second arg1)) (s-exp->symbol (first arg2)) (parse (second arg2)) (parse (third sl))))]
         [(letrec) (let ([lista (s-exp->list (first (s-exp->list (second sl))))])
                  (letrecS (s-exp->symbol (first lista)) (parse (second lista)) (parse (third sl))))]
         [(quote) (quoteS (s-exp->symbol (second sl)))]
         [(read-loop) (read-loopS)]
         #|
         Extras
         |#
         [(=) (eqS (parse (second sl)) (parse (third sl)))]
         [(>) (gtS (parse (second sl)) (parse (third sl)))]
         [(<) (ltS (parse (second sl)) (parse (third sl)))]
         ;; Modificação extra
         [else (error 'parse 
                (string-append
                "invalid list input: unrecognized expression start: "
                (symbol->string (s-exp->symbol (first sl)))))]))]
    [else (error 'parse "invalid input: unknown s-expression type")]))


; Facilitator
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env))


; Examples
(interpS '(+ 10 (call (lambda x (car x)) (cons 15 16))))

(interpS '(call (lambda x (+ x 5)) 8))


(interpS '(call (lambda f (call f (~ 32))) (lambda x (- 200 x))))


; Tests
(test (interp (carC (consC (numC 10) (numC 20)))
              mt-env)
      (numV 10))

#|
Bateria de Testes (Meu)
|#
(interpS '(let ([x1 2]) (+ x1 3))) ;; Teste do let
(interpS '(let* ([x1 2] [x2 (+ x1 1)]) (* x2 x1))) ;; Teste do let*
(interpS '(letrec ([x1 (lambda n (if (= n 1) 1 (* n (call x1 (- n 1)))))])
         (call x1 6))) ;; Teste do letrec

(interpS '(< 1 2))
(interpS '(> (+ 12 3) 20))
(interpS '(= 3 (+ 1 2)))

(interpS '(quote alan)) ;; Teste quote
(interpS '(quote ana)) ;; Teste quote
(interpS '(read-loop)) ;; Teste do read-loop

#|
Bateria de Testes (Renan)
|#

(test (interpS '(let ((x 100)) x)) (numV 100))

(test (interpS '(let ((x 100)) (+ 1 x))) (numV 101))

(test (interpS '(let ((x 100)) (let ((y (+ x 1))) (+ x y)))) (numV 201))

(test (interpS '(let ((f (lambda x x))) (call f 10))) (numV 10))

(test (interpS '(let ((plus (lambda x (lambda y (+ x y)))))
    (call (call plus 10) 20) )) (numV 30))

(test (interpS '(let* ((x 100) (y (+ x 1))) (+ x y))) (numV 201))

(test (interpS 
    '(let* ((inc (lambda n (+ n 1))) (inc2 (lambda n (call inc (call inc n))))) (call inc2 10)))
    (numV 12))


(test (interpS '(letrec ((f (lambda n (if n (* 2 (call f (- n 1))) 1)))) (call f 10))) (numV 1024))

(test (interpS '(letrec ((f (lambda n (if n (* 2 (call f (- n 1))) 1)))) 10)) (numV 10))

(test (interpS '(letrec ((f (lambda n n))) (call f 7))) (numV 7))

