#lang scheme
(require racket/stream)

#|
Nathália Yukimi Uchiyama Tsuno
14600541
|#

#|
Funções Auxiliares
|#

(define mapcar (lambda (f l)
                 (if (stream-empty? l)
                     '()
                     (stream-cons (f (stream-first l))
                           (mapcar f (stream-rest l)))
                     )
                   ))

(define mapcar2 (lambda (f l1 l2)
                  (if (or (null? l1) (null? l2))
                      '()
                      (stream-cons (f (stream-first l1) (stream-first l2))
                                   (mapcar2 f (stream-rest l1) (stream-rest l2))
                                   )
                      )
                    ))

(define mapcar4 (lambda (f x1 x2 x3 x4)
                  (if (or (or (null? x1) (null? x2)) (or (null? x3) (null? x4)))
                      '()
                      (stream-cons (f (stream-first x1) (stream-first x2) (stream-first x3) (stream-first x4))
                               (mapcar4 f (stream-rest x1) (stream-rest x2) (stream-rest x3) (stream-rest x4)))
                      )
                  )
  )

(define first-n (lambda (n l)
                  (if (stream-empty? l)
                      #t
                      (if (eq? n 0)
                          #t
                          (begin
                            (display (stream-first l))
                            (display " ")
                            (first-n (- n 1) (stream-rest l))
                                     )
                            )
                          )
                      )
                  )

#|
Questão 1
|#

;; Item A

#|
Implementação 2
(define itera (lambda (f x)
                  (letrec ([composto (stream-cons x (mapcar f composto))])
                    composto)
               ))
|#
(define itera (lambda (f x)
                  (stream-cons x (mapcar f (itera f x)))
               ))
#|
;;Testes
(define resA (itera (lambda (y) (+ y 1)) 1))
(define resB (stream-rest resA))
(stream-first resB)
(stream-first (stream-rest resB))
(define resC (stream-rest resB))
(stream-first resC)
(define resD (stream-rest resC))
(stream-first resD)
|#

;; Item B

#|
;;Implementação 2
(define ciclo (lambda (l)
                (letrec ([repete (lambda (x l)
                                   (if (stream-empty? x)
                                       (repete l l)
                                       (stream-cons (stream-first x) (repete (stream-rest x) l))
                                       )
                                    )]
                        )
                  (repete l l))
                ))
|#

#|
(define ciclo (lambda (l)
                (letrec ([repete (lambda (x)
                                   (if (stream-empty? x)
                                       (stream-fold (lambda (x y) (stream-cons x y)) l (repete l))
                                       (stream-cons (stream-first x) (repete (stream-rest x)))
                                       )
                                    )]
                        )
                  (repete l))
                ))
|#


(define ciclo (lambda (l)
                (if (stream-empty? l)
                    '()
                    (letrec ([repete (lambda (x)
                                       (if (stream-empty? x)
                                           (repete l)
                                           (stream-cons (stream-first x) (repete (stream-rest x)))
                                           )
                                       )]
                             )
                      (repete l))
                    )
                ))
#|
;; Testes
(define resCiclo (ciclo '(A B C D E F G H I)))
(first-n 100 resCiclo)
(define resCicloVazio (ciclo '()))
(first-n 100 resCicloVazio)
(define respC (ciclo (cons (list 1 2 3) '())))
(first-n 30 respC)
|#
;; Item C
#|
;; A fórmula geral para esquemas de recursão do tipo
;; x0, x1, x2, xi+3

;; Mapcar4 já foi definido acima:

(define ints-from (lambda (i)
                    (stream-cons i (ints-from (+ i 1)))
                    ))

(define f3 (lambda (f x0 x1 x2)
             (stream-cons x0 (stream-cons x1 (stream-cons x2 (mapcar4 f
                                                 (f3 f x0 x1 x2)
                                                 (stream-rest (f3 f x0 x1 x2))
                                                 (stream-rest (stream-rest (f3 f x0 x1 x2)))
                                                 (ints-from 3)
                                                 ))))
             ))
|#
#|
Testes

(1 2 3 ((1 + 2 + 3) * 3) ((18 + 2 + 3) * 4))

(define resMapcar (f3 (lambda (x y z i) (* i (+ x (+ y z)))) 1 2 3))
(first-n 5 resMapcar)
|#

(define fib (lambda ()
  (stream-cons 1 (stream-cons 1 (mapcar2 (lambda (x y) (+ x y))
                                     (fib)
                                     (stream-rest (fib))
                                     )))))
#|
(fib)
;; Testes
(define resFib fib)
(stream-first (stream-rest (stream-rest (stream-rest (fib)))))
(stream-first (stream-rest (stream-rest (stream-rest (stream-rest (fib))))))
(first-n 10 (fib))
|#

#|
Questão 2
|#

(define merge (lambda (l1 l2)
                (if (stream-empty? l1)
                    l2
                    (if (stream-empty? l2)
                        l1
                        (stream-cons (stream-first l1) (stream-cons (stream-first l2) (merge (stream-rest l1) (stream-rest l2))))
                        )
                    )
                )
  )

#|
;; Testes
(define mult2 (stream-cons 0 (mapcar (lambda (x) (+ x 2)) mult2)))
(define zeros (stream-cons 0 (mapcar (lambda (x) 0) zeros)))

(define resMerge (merge mult2 zeros))
(first-n 20 resMerge)
|#

#|
Questão 3
|#

(define foreach-inf (lambda (lista f)
                              (merge (mapcar f (stream-first lista)) (mapcar f (stream-first (stream-rest lista))))
                      )
  )

#|
;; Testes
(define square (lambda (x) (* x x)))

;;(first-n 20 (foreach-inf '((1 2 4) (3 8 9)) square))

(define ints-from (lambda (i)
                    (stream-cons i (mapcar (lambda (x) (+ x 1)) (ints-from i)))
                    ))

(define ints-from2 (ints-from 2))
(first-n 30 ints-from2)
(define ints-from10 (ints-from 10))
(first-n 30 ints-from10)
(first-n 30 (stream-cons ints-from2 (stream-cons ints-from10 '())))

(first-n 30 (foreach-inf (stream-cons ints-from2 (stream-cons ints-from10 '())) square))
|#