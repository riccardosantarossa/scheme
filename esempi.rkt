;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#lang racket
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |prime prove con variabili|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;PROCEDURE BASE DI ESEMPIO PER I COSTRUTTI

;CALCOLO SUPERFICIE CILINDRO
(define supCil            ; valore restituito: reale
  (lambda (r h)           ; r,h: float (misure)
   (* (* 2 3.14159 r) (+ r h))
 )
)

;FA IL PLURALE DI UN SOSTANTIVO MASCHILE
(define pluraleM           ; valore restituito: stringa
  (lambda (sm)             ; sm: stringa
    (string-append (substring sm 0 (- (string-length sm ) 1)) "i")
  )
)

;FA IL PLURALE DI UN SOSTANTIVO FEMMINILE
(define pluraleF           ; valore restituito: stringa
  (lambda (sf)             ; sm: stringa
    (string-append (substring sf 0 (- (string-length sf ) 1)) "e")
  )
)

;CONTROLLA SE UNA STRINGA è FEMMINILE
(define femminile?            ; valore booleano
  (lambda (s)                 ; s: stringa
    (string=?(substring s (- (string-length s) 1))"a")
    ;(char=? (string-ref s (- (string-length s) 1))#\a)
  )
)

;FUNZIONE CHE COMBINA LE ALTRE
(define alPlurale  ;stringa
  (lambda (s)
    (if (femminile? s) ;controlla se la stringa finisce con "a"
        (pluraleF s)   ;se finisce con "a" usa la funzione pluraleF
        (pluraleM s)   ;se finisce con "o" usa la funzione pluraleM
     ))   
)

