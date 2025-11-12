#lang racket
;; EJERCICIO 1 
(define (ej1-contar-positivos xs)
  (length (filter (lambda (x) (> x 0)) xs)))


;; EJERCICIO 2 
(define (ej2-cuadrados-pares xs)
  (filter even? (map (lambda (x) (* x x)) xs)))


;; EJERCICIO 3 
(define (ej3-factorial n)
  (cond [(zero? n) 1]
        [else (* n (ej3-factorial (sub1 n)))]))


;; EJERCICIO 4 
(define (ej4-cubos xs)
  (map (lambda (x) (* x x x)) xs))


;; EJERCICIO 5 
(define (ej5-suma-impares xs)
  (foldl + 0 (filter odd? xs)))


;; EJERCICIO 6 
(define (ej6-contiene-negativos? xs)
  (ormap (lambda (x) (< x 0)) xs))


;; EJERCICIO 7 
(define (ej7-suma-acumulada xs)
  (define result
    (foldl (lambda (x acc)
             (define suma (+ (car acc) x))
             (cons suma (cons suma (cdr acc))))
           (cons 0 '())
           xs))
  (reverse (cdr result)))


;; EJERCICIO 8 
(define (ej8-concat cadenas)
  (foldl (lambda (x acc) (string-append acc x)) "" cadenas))
;; Alternativa: (foldr string-append "" cadenas)


;; EJERCICIO 9 
(define (ej9-dobles-mayores-5 xs)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) xs)))


;; EJERCICIO 10
(define (ej10-invertir xs)
  (foldl (lambda (x acc) (cons x acc)) '() xs))


;; EJERCICIO 11 
(define (ej11-aplicar f xs) (map f xs))
(define (cuadrado x) (* x x))


;; EJERCICIO 12 
(define (ej12-promedio-mayores-5 xs)
  (let* ([mayores (filter (lambda (x) (> x 5)) xs)]
         [suma    (foldl + 0 mayores)]
         [n       (length mayores)])
    (if (zero? n)
        (error 'ej12 "no hay elementos > 5")
        (/ (exact->inexact suma) n))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(module+ main
  (displayln "=== TALLER 2 DIEGO FERNANDO ALVARADO SIBRIAN 00127522 ===")

  (displayln (format "E1: ~a => ~a"
                     '(3 -2 7 0 -5 9)
                     (ej1-contar-positivos '(3 -2 7 0 -5 9))))

  (displayln (format "E2: ~a => ~a"
                     '(1 2 3 4 5 6 7 8)
                     (ej2-cuadrados-pares '(1 2 3 4 5 6 7 8))))

  (displayln (format "E3: 5 => ~a"
                     (ej3-factorial 5)))

  (displayln (format "E4: ~a => ~a"
                     '(2 3 4)
                     (ej4-cubos '(2 3 4))))

  (displayln (format "E5: ~a => ~a"
                     '(1 2 3 4 5 6 7)
                     (ej5-suma-impares '(1 2 3 4 5 6 7))))

  (displayln (format "E6: ~a => ~a"
                     '(5 9 -3 2)
                     (ej6-contiene-negativos? '(5 9 -3 2))))

  (displayln (format "E7: ~a => ~a"
                     '(1 2 3 4)
                     (ej7-suma-acumulada '(1 2 3 4))))

  (displayln (format "E8: ~a => ~a"
                     '("Hola" " " "Mundo")
                     (ej8-concat '("Hola" " " "Mundo"))))

  (displayln (format "E9: ~a => ~a"
                     '(3 6 8 2 10)
                     (ej9-dobles-mayores-5 '(3 6 8 2 10))))

  (displayln (format "E10: ~a => ~a"
                     '(1 2 3 4)
                     (ej10-invertir '(1 2 3 4))))

  (displayln (format "E11: cuadrado en ~a => ~a"
                     '(1 2 3 4)
                     (ej11-aplicar cuadrado '(1 2 3 4))))

  (displayln (format "E12: ~a => ~a"
                     '(3 8 10 4 9 2 7)
                     (ej12-promedio-mayores-5 '(3 8 10 4 9 2 7)))))


