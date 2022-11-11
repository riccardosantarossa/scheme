;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#lang racket
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname frasi) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define articolo ;val restituito : stringa
  (lambda (s) ; controllo l'ultima lettera del soggetto e oggetto e aggiungo gli articoli di conseguenza
    (cond
      [(string=? (substring s (- (string-length s) 1)) "a") (string-append "la " s) ] ;fem singolare
      [(string=? (substring s (- (string-length s) 1)) "e") (string-append "le " s) ] ;fem plurale
      [(string=? (substring s (- (string-length s) 1)) "i") (string-append "i " s) ]  ;masch plurale
      [(string=? (substring s (- (string-length s) 1)) "o") (string-append "il " s) ] ;masch singolare
    )
  )
)

(define singolare ;val restituito: booleano
  (lambda (s)
      (if (or (string=? (substring s (- (string-length s) 1)) "a") (string=? (substring s (- (string-length s) 1)) "o"))
          true ;se il soggetto è singolare ritorno TRUE
          false ; se il soggetto è plurale ritorno FALSE
          )
      )
    )

(define verbo   ;val restituito: stringa
  (lambda (s v)
    (cond ((string=? (substring v (- (string-length v) 3)) "are")
           (if (equal? (singolare s) true)
        (string-append (substring v 0 (- (string-length v) 3)) "a ") ;prima coniugazione singolare
        (string-append (substring v 0 (- (string-length v) 3)) "ano ") ;prima coniugazione plurale
        ))
       ((string=? (substring v (- (string-length v) 3)) "ere")
           (if (equal? (singolare s) true)
        (string-append (substring v 0 (- (string-length v) 3)) "e ") ;prima coniugazione singolare
        (string-append (substring v 0 (- (string-length v) 3)) "ono ") ;prima coniugazione plurale
        ))
       ((string=? (substring v (- (string-length v) 3)) "ire")
           (if (equal? (singolare s) true)
        (string-append (substring v 0 (- (string-length v) 3)) "e ") ;prima coniugazione singolare
        (string-append (substring v 0 (- (string-length v) 3)) "ono ") ;prima coniugazione plurale
        ))
     )
    )
 )

(define frase ;val restituito: stringa
  (lambda (sog ver ogg)
     (string-append (articolo sog) " " (verbo sog ver) (articolo ogg)) ;costruisce la frase con i risultati delle altre funzioni
   )
)