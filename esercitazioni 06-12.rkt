;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |esercitazioni 06-12|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))

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
    )
  )
)



