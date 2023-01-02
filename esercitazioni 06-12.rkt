;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |esercitazioni 06-12|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))

;ES 3 LCS
(define lcs ; valore: lista di terne
  (lambda (u v) ; u, v: stringhe
    (lcs-rec 1 u 1 v)
))

(define lcs-rec
  (lambda (i u j v)
    (cond ((or (string=? u "") (string=? v ""))
           null)
          ((char=? (string-ref u 0) (string-ref v 0))
           (cons (list i j (substring u 0 1))
                (lcs-rec (+ i 1) (substring u 1) (+ j 1) (substring v 1)) ))
          (else
           (better (lcs-rec (+ i 1) (substring u 1) j v)
                   (lcs-rec i u (+ j 1) (substring v 1)))
          )
)))

(define better
  (lambda (x y)
    (if (< (length x) (length y)) y x)
))


;ES 2 INCREMENTO DI UN NUMERO RAPPRESENTATO SOTTO FORMA DI STRINGHE
(define offset (char->integer #\0))

(define last-digit
  (lambda (base) (integer->char (+ (- base 1) offset)) ))

(define next-digit
  (lambda (dgt) (string (integer->char (+ (char->integer dgt) 1))) ))

(define increment
  (lambda (num base) ; 2 <= base <= 10
    (let ((digits (string-length num)))
      (if (= digits 0)
          "1"
          (let ((dgt (string-ref num (- digits 1))))
            (if (char=? dgt (last-digit base))
                (string-append (increment (substring num 0 (- digits 1)) base)
                 "0")
                (string-append (substring num 0 (- digits 1)) (next-digit dgt) )
             ))
))))


;ES 4 STRINGHE CICLICHE




;ES 7 NUMERI COMUNI A DUE LISTE

(define shared  ;val: lista 
  (lambda (u v) ;u, v: liste di interi ordinati
    (cond ((or (null? v) (null? u))
           null)
          ((= (car u) (car v))
            (cons (car u) (shared (cdr u) (cdr v))))  
          ((< (car u) (car v))
           (shared (cdr u) v))
          (else
           (shared u (cdr v)))
)))


;ES8 PARITY CHECK FAILS

(define parity-check-fails
  (lambda (lst)
    ""
  )
)

(define parity-sum
  (lambda (lst)
    (if (null? (cdr lst))
        '()
        (let ((v (sum (car lst) 0))) 
          (if (= (remainder v 2) 0)
              ((cons true (parity-sum (cdr lst))))
              ((cons false (parity-sum (cdr lst))))
         )
    ))
))

;Fa la somma delle singole parole dentro la lista
(define sum
  (lambda (s acc)
    (if (string=? s "")
        acc
        (if (char=? (string-ref s 0) #\1)
            (sum (substring s 1) (+ acc 1))
            (sum (substring s 1) acc)
        ))
  )
)









