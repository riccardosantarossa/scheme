;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ES9 Cesare e funzioni|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
; Parte I: FATTA
; data una chiave (integer) di crittazione
; restituire la procedura di crittazione
; (usa l'alfabeto latino dell'epoca repubblicana)
; A B C D E F G H I L M N O P Q R S T V X

; Alfabeto di riferimento
(define alfabeto-latino "ABCDEFGHILMNOPQRSTVX")

; char->integer personalizzato
;    in input il carattere e la stringa che codifica l'alfabeto di riferimento
(define univ-char->integer
  (lambda (char alphabet)
    (univ-char->integer-supp char alphabet 0)
    ))

; procedura di supporto ^
(define univ-char->integer-supp
  (lambda (char alphabet value)
    (if (>= value (string-length alphabet))
        error
        (if (char=? char (string-ref alphabet value))
            value
            (univ-char->integer-supp char alphabet (+ 1 value))))
    ))

; versione alternativa della regula di crittazione (usa
; la procedura univ-char->integer).
(define alt-regula
  (lambda (fattore)
    (lambda (carattere alphabeto indice)
      (if (<= indice (- (string-length alphabeto) 1))
          (if (char=? carattere (string-ref alphabeto indice))
              ; T: ending case
              (if (>
                   (+ (univ-char->integer carattere alphabeto) fattore)
                   (- (string-length alphabeto) 1))
                  ; T
                  (string-ref alphabeto
                              (- (+ (univ-char->integer carattere alphabeto) fattore) (string-length alphabeto)))
                  ; F
                  (string-ref alphabeto (+ indice fattore)))
              ; F: recursive call
              ((alt-regula fattore) carattere alphabeto (+ 1 indice)))
          error)
      )))
; Cifratore
(define cipher ; val: stringa cifrata
  (lambda (nuntius regula alphabeto) ; nuntius: stringa da cifrare, regula: procedura di crittazione
    (if (= (string-length nuntius) 0)
        ""
        (string-append
         (string (regula (string-ref nuntius 0) alphabeto 0))
         (cipher (substring nuntius 1) regula alphabeto)))
    ))

(define alt-antiquum-codicem-Caesar (alt-regula 3))

; Funziona
(cipher "ALEAIACTAESTIVLIVSCAESARDIXIT" alt-antiquum-codicem-Caesar alfabeto-latino)

; ------------------------------------------------------------------------

; Parte II: RISOLTA

; Spunti:
; add(m, 0) = m
; add(m, n) = succ(add(m, n-1))
;
; mul(m, 0) = 0
; mul(m, n) = add(m, mul(m, n-1))
;
; pow(m, 0) = 1
; pow(m, n) = nul(m, pow(m, n-1))

; Definisci H, che ha argomenti e valore procedurali, e poi
; definisci add, mul e pow applicando H.
; Esempio: (define mul (H (lambda (x) 0) add))

; procedura identità i
(define i
  (lambda (x)
    x
    ))

; procedura costante 0
(define z
  (lambda (x)
    0
    ))

; procedura costante 1
(define u
  (lambda (x)
    1
    ))

; procedura successore a due parametri (dipendente solo da uno)
(define s2
  (lambda (u v)
    (+ v 1)
    ))

; procedura H alto ordine
(define H
  (lambda (f g)
    (lambda (m n)
      (if (= n 0)
          (f m)
          (g m ((H f g) m (- n 1))))
    )))


(define add (H i s2))
(define mul (H z add))
(define pow (H u mul))
; ------------------------------------------------------------------------