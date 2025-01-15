#lang plai-typed

#|
苺
Nathália Yukimi Uchiyama Tsuno (幸美 内山 津野)
14600541

Nessa versão, objetos apontam para as definições da classe
classes apontam para as definições de superclasses
|#

#| primeiro as expressões "primitivas", ou seja, diretamente interpretadas
 |#

(define-type ExprC
  [numC    (n : number)]
  [idC     (s : symbol)]
  [plusC   (l : ExprC) (r : ExprC)]
  [multC   (l : ExprC) (r : ExprC)]
  [ifC     (cond : ExprC) (y : ExprC) (n : ExprC)]
  [letC (var : symbol) (expression : ExprC) (body : ExprC)]
  [quoteC  (sym : symbol)]
  [readloopC (placeholder : symbol)]
  [nullC]
  [seqC  (statement1 : ExprC) (statement2 : ExprC)]
  [setC  (varName : symbol) (statement : ExprC)]
  #|
  EP03
  |#
  ;; a
  [classC  (superClass : symbol) (instVar : symbol) (method1 : ExprC) (method2 : ExprC)]
  ;; b
  [regularMethodC (name : symbol) (arg : symbol) (body : ExprC)]
  [primitiveMethodC (name : symbol) (primNumber : number)]
  ;; e
  [newC    (class : symbol) (value : ExprC)]
  ;; f
  [sendC   (receiver : ExprC) (method : symbol) (arg : ExprC)]
  )

#| agora a linguagem aumentada pelo açúcar sintático
 | neste caso a operação de subtração e menus unário
 |#

(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)]
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [letS    (var : symbol) (exp : ExprS) (body : ExprS)]
  [quoteS  (sym : symbol)]
  [readloopS (placeholder : symbol)]
  [seqS (statement1 : ExprS) (statement2 : ExprS)]
  [setS (variable : symbol) (statement : ExprS)]
  #|
  EP03
  |#
  ;Deixei as opções aqui, para facilitar, basta retirar o ";" 
  ;; a
  [classS  (superClass : symbol) (instVar : symbol) (method1 : ExprS ) (method2 : ExprS)]
  ;; b
  [regularMethodS (name : symbol) (arg : symbol) (body : ExprS)]
  [primitiveMethodS (name : symbol) (primNumber : number)]
  ; e
  [newS    (class : symbol) (value : ExprS)]
  ; f
  [sendS   (receiver : ExprS) (method : symbol) (arg : ExprS)]
  [nullS ]
  )


(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [idS     (s)        (idC s)]
    [plusS   (l r)      (plusC (desugar l) (desugar r))]
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [bminusS (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [ifS     (c y n)    (ifC (desugar c) (desugar y) (desugar n))]
    [letS    (v e b)    (letC v (desugar e) (desugar b))]
    [quoteS  (sym) (quoteC sym)]
    [readloopS (s) (readloopC s)]
    [nullS  ()  (nullC)]
    [seqS (st1 st2) (seqC (desugar st1) (desugar st2))]
    [setS (var st)  (setC var (desugar st))]

    #|
    EP03
    |#
    ;; a
    [classS (superClass instVar method1 method2) (classC superClass instVar (desugar method1) (desugar method2))]
    ;; b
    [regularMethodS (name arg body) (regularMethodC name arg (desugar body))]
    [primitiveMethodS (name primNumber) (primitiveMethodC name primNumber)]
    ;; e
    [newS (nameClasse value) (newC nameClasse (desugar value))]
    ;; f
    [sendS (receiver method arg) (sendC (desugar receiver) method (desugar arg))]
    ))
#|
EP03
|#

(define-type MethodDefinition
  [regularMethod (par : symbol) (body : ExprC)]
  [primitiveMethod (num : number)]
)

; We need a new value for the box
(define-type Value
  [numV  (n : number)]
  ;[closV (arg : symbol) (body : ExprC) (env : Env)]
  ;[consV (car : Value) (cdr : Value)]
  [symV (sym : symbol)]
  [nullV ]
  [methodV (nameMethod : symbol) (defMethod : MethodDefinition)]
  #|
  EP03
  |#
  ;; a
  [classV (superClasse : Value) (instVar : symbol) (method1 : Value) (method2 : Value)]
  ;; c
  [objectV (class : Value) (innerState : Env)]
  )

; Bindings associate symbol with Values
(define-type Binding
        [bind (name : symbol) (val : (boxof Value))])
; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)


; auxiary functions for messageLookup                                         
(define (lookup [varName : symbol] [env : Env]) : (boxof Value)
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

;; (extend-env (bind 'Object (box Object)) mt-env)
#|
class -> busca e insere a vi no ambiente do objeto -> vai para a próxima classe
|#
(define (new+ [nameClasse : Value][objectEnv : Env]) : Env
  (type-case Value nameClasse
    [classV (superClasse instVar method1 method2)
            (if (equal? superClasse (nullV))
                (extend-env (bind instVar (box (nullV))) mt-env)
                (extend-env (bind instVar (box (nullV))) (new+ superClasse objectEnv))
                )
            ]
    [else (error 'interp "condition not a classV")]
    )
  )
#|
(define (findClass [nameClasse : symbol][objectEnv : Env][innerState : Env]) : Env
  (type-case Value (unbox (lookup nameClasse objectEnv))
                   [classV (superClasse instVar method1 method2)
                          (if (eq? nameClasse 'Object)
                              (extend-env (bind nameClasse (lookup nameClasse objectEnv)) innerState)
                              (extend-env (bind nameClasse (lookup nameClasse objectEnv)) (findClass superClasse objectEnv innerState))
                              )
                          ]
                   [else (error 'interp "condition not a classV")]
                   )
  )
|#
(define (methodLookup [nameClasse : Value][nameMethod : symbol][objectEnv : Env]) : Value
  (type-case Value nameClasse
                   [classV (superClasse instVar method1 method2)
                           (if (equal? nameMethod (methodV-nameMethod method1))
                              method1
                               (if (equal? nameMethod (methodV-nameMethod method2))
                                   method2
                                   (if (equal? superClasse (nullV))
                                       (nullV)
                                       (methodLookup superClasse nameMethod objectEnv)
                                       )
                                  )
                               )
                          ]
                   [else (error 'interp "condition not a classV")]
                   ))

#|
EP03
|#

(define PrimitiveMethodVector
  (make-vector 2 (lambda ([ x : Value] ) : Value
(error 'primitive "invalid primitive method")))); 0
;add primitive 1 for 'mensagemDesconhecida
(vector-set! PrimitiveMethodVector 1
             (lambda ([methodName : Value])
               (type-case Value methodName
                 [symV (symbolValue)
                       (error 'messaging
                              (string-append "mensagemDesconhecida:"
                                             (symbol->string symbolValue)))]
                 [else (error 'wrongArgument
                              "Wrong Argument: primitive 1 should receive a symV")])))
#|
(vector-set! PrimitiveMethodVector 2
             (lambda ([methodName : Value])
               (begin
                 (display methodName)
                 (display " ")
                 (nullV)
                 )))
|#
#|
Caso teste : Precisa mudar o valor no make-vector para 3, 4, 5 ou sei lá :P
(vector-set! PrimitiveMethodVector 2
             (lambda ([methodName : Value])
               (type-case Value methodName
                 [symV (symbolValue)
                       (error 'messaging
                              (string-append "erro:"
                                             (symbol->string symbolValue)))]
                 [else (error 'wrongArgument
                              "Wrong Argument: primitive 1 should receive a symV")])))
|#
; Return type for the interpreter, Value

(define (interp [a : ExprC] [objectEnv : Env]) : Value
  (type-case ExprC a
    [nullC () (nullV)]
    [numC (n) (numV n) ]
    [idC (n)  (unbox (lookup n objectEnv))]; cascading search, first in env then in sto
    ;I left plusC without error-checking
    [plusC (l r)
             (let ((left (interp l objectEnv ))
                   (right (interp r objectEnv )))
               (num+ left right))]
    ;multC
    [multC (l r)
           (let ( (left (interp l objectEnv ))
                  (right (interp r objectEnv )))
             ;in this case type cheking is a little different
             (if (numV? left)
                 (if (numV? right)
                     (num* left right)
                     (error 'interp "second argument of multiplication not a number value"))
                   (error 'interp "first argument of multiplication not a number value"))
                 )]
    ; ifC serializes
    [ifC (c s n) (type-case Value (interp c objectEnv )
                   [numV (value)
                        (if (zero? value)
                            (interp n objectEnv )
                            (interp s objectEnv ))]
                   [else (error 'interp "condition not a number")]
                   )]
    [quoteC  (s) (symV s)]
    [readloopC (ph) (letrec ( (read-till-end (lambda ()
                                              (let ( (input (read)))
                                                (if (and (s-exp-symbol? input )
                                                         (eq? (s-exp->symbol input) '@END))
                                                    (begin (display 'FINISHED-READLOOP)
                                                           (symV  'END_OF_loop))
                                                    (begin (display (interp (desugar (parse input)) objectEnv ))
                                                           (read-till-end)))))))
                     (read-till-end))]
    [letC (variable expression body)
          (let ((value (interp expression objectEnv )))
            (interp body
                    (extend-env (bind variable (box value)) objectEnv)
                    ))]
    [seqC (firstCommand secondCommand)
          (begin (interp firstCommand objectEnv)
                 (interp secondCommand objectEnv))]
    [setC  (variableName statement)
           (let ((varBox (lookup variableName objectEnv))
                 (value (interp statement objectEnv)))
             (begin (set-box! varBox value)
                    value))]
    #|
    EP03
    |#
    ;; a
    [classC (superClasse instVar method1 method2)
            (let ([superClass (unbox (lookup superClasse objectEnv))])
              (classV superClass instVar (interp method1 objectEnv) (interp method2 objectEnv))
              )
            ]
    ;; b
    [regularMethodC (name arg body)
                    (methodV name (regularMethod arg body))]
    [primitiveMethodC (name primNumber)
                    (methodV name (primitiveMethod primNumber))]
    ;; e
    #|
    classe aponta para o pai, não o nome dele
    |#
    [newC (nameClasse value)
          (let ([innerState (new+ (unbox (lookup nameClasse objectEnv)) objectEnv)]
                [nameVar (classV-instVar (unbox (lookup nameClasse objectEnv)))])
            (begin
              (set-box! (lookup nameVar innerState) ;; Pega o nome da variável na classe, mas, pega a variável no innerstate
                        (interp value objectEnv)) ;; Atualiza a variável e retorna o ambiente
              ;; set-box! atualiza todo box que tem determinado nome
              (objectV (unbox (lookup nameClasse objectEnv)) innerState) ;; Def Classe + estado interno
              )
            )
          ]
    ;; f
    #|
(define-type MethodDefinition
  [regularMethod (par : symbol) (body : ExprC)]
  [primitiveMethod (num : number)]
)
    ;; Colocar métodos, varInst, self e args na ordem pai -> filho
#|
(interpS '(let classe1 (class Object i (regularMethod m1 x i) (regularMethod m2 x (send self m3 x)))
            (let classe2 (class classe1 j (regularMethod m1 x (quote subclassregularMethod)) (regularMethod m3 y y))
              (let object2 (new classe2 200) (send object2 m2 55)))))
|#
    |#
    ;; object varInst params
    ;; 
    [sendC (receiver method arg) ;; methodLookup [nameclass] [namemethod] [ambiente]
             (let* ([objeto (interp receiver objectEnv)]
                  [env (extend-env (bind 'self (box objeto)) (objectV-innerState objeto))])
             (type-case Value (methodLookup (objectV-class objeto) method objectEnv)
               [methodV (nameMethod defMethod)
                        (type-case MethodDefinition defMethod
                          [regularMethod (par body)
                                         (let ([env2 (extend-env (bind par (box (nullV))) env)])
                                           (begin
                                             (set-box! (lookup par env2) (interp arg objectEnv)) ;; É suposto que o argumento não seja a própria variável
                                             (interp body env2)
                                             )
                                           )
                                         ]
                          [primitiveMethod (num)
                                           ((vector-ref PrimitiveMethodVector num) (interp arg objectEnv))
                                           ]
                          )
                        ]
               [nullV () (interp (sendC receiver 'mensagemDesconhecida (quoteC method)) objectEnv)
                      ]
               [else (error 'interp "condition not a methodV neither a nullV")] ;; :-(
               )
             )
           ]
    ))
;;(send object1 m1 5)

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
          [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(quote) (quoteS (s-exp->symbol (second sl)))]
         [(let) (letS (s-exp->symbol (second sl))
                      (parse (third sl))
                      (parse (fourth sl)))]
          [(set!) (setS (s-exp->symbol (second sl))
                      (parse (third sl)))]
         [(seq) (seqS (parse (second sl))
                      (parse (third sl)))]
         #|
         EP03
         |#
         ;; a
         [(class) (let ([fifth_sl (first (rest (rest (rest (rest sl)))))])
                    (classS (s-exp->symbol (second sl)) (s-exp->symbol (third sl)) (parse (fourth sl)) (parse fifth_sl))
                    )]
         ;; b
         [(regularMethod) (regularMethodS (s-exp->symbol (second sl))
                                          (s-exp->symbol (third sl))
                                          (parse (fourth sl)))]
         [(primitiveMethod) (primitiveMethodS (s-exp->symbol (second sl))
                                          (s-exp->number (third sl)))]
         ;; e
         [(new) (newS (s-exp->symbol (second sl))
                                          (parse (third sl)))]
         ;; f
         [(send) (sendS (parse (second sl)) (s-exp->symbol (third sl)) (parse (fourth sl)))]
        [else (error 'parse "invalid list input")]
         ))]
    [else (error 'parse "invalid input")]
    ))

(define Object
  (classV (nullV) ;; A classe aponta para vazio
          'i ;; Não possui argumentos
          ;;(methodV 'mensagemDesconhecida (primitiveMethod 404))
          (methodV 'mensagemDesconhecida (primitiveMethod 1))
          (methodV 'mensagemDesconhecida (primitiveMethod 1))
          )
  )
;; Método manda mensagem para o objeto original
;; begin, set, olha, imprime
;; Superclasse : set, olha
;; Instância é superclasse, muda o valor da
;; Vê se manda a mensagem não reconhecível
;; Sempre 2 métodos + 1 VI
;; Redefinição não daria se fosse 1 método

; Facilitator
; Enviromnent needs to be intialzed with the association for the Object class, which needs to be defined elsewhere 
(define initialObjectEnv (extend-env (bind 'Object (box Object)) mt-env)) ;; Incluindo o self no objeto
(define (interpS [s : s-expression]) : Value
  (interp (desugar (parse s)) initialObjectEnv ))

#|
; Examples
(interpS '(class Object i (regularMethod m1 x x) (regularMethod m2 x i)))
(interpS '(let classe1 (class Object i (regularMethod m1 x x) (regularMethod m2 x i))
            (let object1 (new classe1 1) (send object1 m1 5))))
(interpS '(let classe1 (class Object i (regularMethod m1 x x) (regularMethod m2 x i))
            (let object1 (new classe1 1) (send object1 m2 5))))

;(interpS '(let classe1 (class Object i (regularMethod m1 x x) (primitiveMethod m2 2))
;            (let object1 (new classe1 1) (send object1 m2 5))))
; no proximo exemplo definimos um novo m1 em uma subclasse, instanciamos e mandamos a mensagem m2 para no novo
;objeto. O interpretador deve voltar o resultado de m1 da subclasse. (

(interpS '(let classe1 (class Object i (regularMethod m1 x i) (regularMethod m2 x (send self m3 x)))
            (let classe2 (class classe1 j (regularMethod m1 x (quote subclassregularMethod)) (regularMethod m3 y y))
              (let object2 (new classe2 200) (send object2 m2 55)))))

(interpS '(let classe1 (class Object i (regularMethod m1 x i) (regularMethod m2 x (send self m1 x)))
            (let classe2 (class classe1 j (regularMethod m1 x (quote subclassregularMethod)) (regularMethod m3 y y))
              (let object2 (new classe2 200) (send object2 m2 55)))))

(interpS '(let classe1 (class Object i (regularMethod m1 x i) (regularMethod m2 x (send self m1 x)))
            (let classe2 (class classe1 j (regularMethod mensagemDesconhecida x (quote Erro)) (regularMethod m3 y y))
              (let object2 (new classe2 200) (send object2 m22 55)))))

(interpS '(let classe1 (class Object i (regularMethod m4 x (set! i 4)) (regularMethod m2 x (send self m1 x)))
            (let classe2 (class classe1 j (regularMethod m1 x (quote subclassregularMethod)) (regularMethod m3 y (+ (send self m4 y) 3)))
              (let object2 (new classe2 200) (send object2 m3 55)))))

#|
Caso que precisa mudar lá em cima ;-;
(interpS '(let classe1 (class Object i (regularMethod m4 x (set! i 4)) (regularMethod m2 x (send self m1 x)))
            (let classe2 (class classe1 j (regularMethod m1 x (quote subclassregularMethod)) (regularMethod m3 y (+ (send self m4 y) 3)))
              (let classe3 (class classe2 k (primitiveMethod erro 2) (regularMethod m5 x x))
               (let object3 (new classe3 200) (send object3 erro (quote abobrinha)))))))
|#

(interpS '(let classe1 (class Object i
                         (regularMethod m1 x i)
                         (regularMethod m2 x (send self m1 x)))
            (let classe2 (class classe1 j
                           (regularMethod m1 x (quote subclassregularMethod))
                           (regularMethod m3 y y))
              (let object2 (new classe2 200)
                (send object2 m22 55)))))
|#
; Tests
;(interpS '(let classe1 (class Object i (regularMethod m1 x x) (regularMethod m2 x i))
;            (let object1 (new classe1 1) 9)))
;(interpS '(let classe1 (class Object i (regularMethod m1 x x) (regularMethod m2 x i))
;            (let object1 (new classe1 1) (send object1 m3 5))))
#|
; Examples
(interpS '(class Object i (regularMethod m1 x x) (regularMethod m2 x i)))
(interpS '(let classe1 (class Object i (regularMethod m1 x x) (regularMethod m2 x i))
            (let object1 (new classe1 1) (send object1 m1 5))))
(interpS '(let classe1 (class Object i (regularMethod m1 x x) (regularMethod m2 x i))
            (let object1 (new classe1 1) (send object1 m2 5))))
; no proximo exemplo definimos um novo m1 em uma subclasse, instanciamos e mandamos a mensagem m2 para no novo
;objeto. O interpretador deve voltar o resultado de m1 da subclasse. (
(interpS '(let classe1 (class Object i (regularMethod m1 x i) (regularMethod m2 x (send self m1 x)))
            (let classe2 (class classe1 j (regularMethod m1 x (quote subclassregularMethod)) (regularMethod m3 y y))
              (let object2 (new classe2 200) (send object2 m2 55)))))
|#
#|
(interpS '(let classe1 (class Object i (regularMethod m1 x i) (regularMethod m2 x (send self m1 x)))
            (let classe2 (class classe1 j (regularMethod m1 x (quote subclassregularMethod)) (regularMethod m3 y y))
              (let object2 (new classe2 200) (send object2 m22 55)))))
|#
#|
(interpS '(let classe1 (class Object i (regularMethod set-i x (set! i x)) (regularMethod m1 x (* x 4)))
            (let classe2 (class classe1 j (regularMethod mensagemDesconhecida i (quote Erro)) (regularMethod m2 x (send self m1 j)))
              (let classe3 (class classe2 k (regularMethod m1 x (* i k)) (regularMethod m3 x (send self m1 5)))
                (let object3 (new classe3 200)
                  (seq (send object3 set-i 5)
                       (seq (send object3 m1 50)
                            (let object2 (new classe3 90)
                              (seq (send object2 set-i 4)
                                   (send object2 m11 12))
                              )
                            )
                       )
                  )
                )
              )
            )
         )
|#

#|
(test ; Métodos e variáveis de instância funcionam
  (interpS '(let Classezinha (class Object i
                         (regularMethod beep x x)
                         (regularMethod boop _ i))

            (let objetinho (new Classezinha 1)
              (+ (send objetinho beep 10)
                 (send objetinho boop 0)
                 ))))
  (numV 11))

(test ; Self funciona
  (interpS '(let Classezinha (class Object i 
                          (regularMethod beep x (+ 10 x))
                          (regularMethod boop x (send self beep (+ 1 x))))

              (let objetinho (new Classezinha 0) 
                (send objetinho boop 0))))
  (numV 11))

(test ; Sobrescrita funciona
  (interpS '(let Classezinha1 (class Object i 
                          (regularMethod beep _ 9)
                          (regularMethod boop _ 10))

              (let Classezinha2 (class Classezinha1 j
                            (regularMethod beep _ 1)
                            (regularMethod unused _ 0))

                (let objetinho (new Classezinha2 0) 
                  (+ (send objetinho beep 0)
                     (send objetinho boop 0))))))
  (numV 11))

(test ; Sobrescrita com self funciona
  (interpS '(let Classezinha1 (class Object i 
                          (regularMethod beep x 9)
                          (regularMethod boop x (send self beep x)))

              (let Classezinha2 (class Classezinha1 j
                            (regularMethod beep _ 11)
                            (regularMethod unused _ 0))

                (let objetinho (new Classezinha2 0) 
                  (send objetinho boop 0)))))
  (numV 11))

(test ; Set com variáveis de instância funciona
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod setVariable x (set! i x))
                           (regularMethod getVariable x i))

              (let objetinho (new Classezinha1 9)
                (seq (send objetinho setVariable 11)
                     (send objetinho getVariable 0)))))
  (numV 11))


(test ; Variáveis de instância classes filhas funcionam corretamente
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod getI _ i)
                           (regularMethod beep x x))

              (let Classezinha2 (class Classezinha1
                             j
                             (regularMethod getJ _ j)
                             (regularMethod beep x x))

                (let objetinho (new Classezinha2 11)
                  (send objetinho getJ 0)))))
  (numV 11))

(test ; Acessar de instância de classes ancestrais devolve null
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod getI _ i)
                           (regularMethod beep x x))

              (let Classezinha2 (class Classezinha1
                             j
                             (regularMethod getJ _ j)
                             (regularMethod beep x x))

                (let objetinho (new Classezinha2 9)
                  (send objetinho getI 0)))))
  (nullV))


(test ; É possível acessar e modificar variáveis de instância dos pais
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod getI _ i)
                           (regularMethod setI x (set! i x)))

              (let Classezinha2 (class Classezinha1
                             j
                             (regularMethod getJ _ j)
                             (regularMethod setJ x (set! j x)))

                (let Classezinha3 (class Classezinha2 
                                    k 
                                    (regularMethod getK _ k)
                                    (regularMethod setK x (set! k x))
                                    )

                (let objetinho (new Classezinha3 9)
                  (seq 
                    (send objetinho setI 1)

                    (seq 
                       (send objetinho setJ 10)

                       (seq 
                         (send objetinho setK 100)

                         (+ (send objetinho getI 0)
                            (+ 
                              (send objetinho getJ 0)
                              (send objetinho getK 0)))
                         ))))))))
  (numV 111))

(test ; É possível modificar variáveis de instância dos pais pelos métodos dos filhos
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod getI _ i)
                           (regularMethod unused _ 0))

              (let Classezinha2 (class Classezinha1
                             j
                             (regularMethod setI x (set! i x))
                             (regularMethod getJ _ j))

                (let Classezinha3 (class Classezinha2 
                                    k 
                                    (regularMethod setJ x (set! j x))
                                    (regularMethod unused _ 0))

                (let objetinho (new Classezinha3 9)
                  (seq 
                    (send objetinho setI 1)

                    (seq 
                       (send objetinho setJ 10)

                       (+ 
                         (send objetinho getI 0)
                         (send objetinho getJ 0))
                       )))))))
  (numV 11))

(test ; Objetos são independentes
  (interpS '(let Classezinha1 (class Object
                           i
                           (regularMethod getI _ i)
                           (regularMethod setI x (set! i x)))

              (let Classezinha2 (class Classezinha1
                             j
                             (regularMethod getJ _ j)
                             (regularMethod setJ x (set! j x)))

                (let objetinho1 (new Classezinha2 1)
                  (seq 
                    (send objetinho1 setI 10)

                    (let objetinho2 (new Classezinha2 100) 
                      (seq 
                        (send objetinho2 setI 1000)

                        (+ (send objetinho1 getI 0)
                           (+ 
                             (send objetinho1 getJ 0)
                             (+ (send objetinho2 getI 0)
                                (send objetinho2 getJ 0))))
                        )))))))
  (numV 1111))
|#