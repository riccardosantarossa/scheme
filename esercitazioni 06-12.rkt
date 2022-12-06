;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |esercitazioni 06-12|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))

;ES 4 STRINGHE CICLICHE


;ES 7 NUMERI COMUNI A DUE LISTE

(define belong? ;val: true/false
  (lambda (x S) ;x: intero, S: lista di interi
     (cond
       ((null? S)
        false)
       ((= (car S) x)
        true)
       (else
        (belong? x (cdr S))
       )
      )
   ))

(define L '() )

(define shared
  (lambda (u v)
      (if (belong? (car u) v)
          (begin
           (cons (car u) L)
           (shared (cdr u) v)
          (shared (cdr u) v)
      )
))



