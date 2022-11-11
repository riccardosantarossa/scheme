;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#lang racket
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname convertitore) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))

(define intera ;val: numero intero
  (lambda (n) ;calcola il valore della parte intera con la notazione posizionale
   (if (>= (string-length n) 1) 
    (+ (* (string->number(substring n 0 1)) (expt 2 (-(string-length n) 1))) (intera(substring n 1 (string-length n) )) )
    0)
    )
)


(define decimale ;val: numero decimale
  (lambda (s) ;calcola il valore della parte decimale con la notazione posizionale
  (if (>= (string-length s) 1)    
    (+ (* (string->number(substring s (-(string-length s) 1))) (expt 2 (- 0 (string-length s)))) (decimale (substring s 0 (- (string-length s) 1) )) )
    0)
    )
 )

(define calcIntera 
 (lambda (n) ;trova il punto
   (if (>= (string-length n) 1)
    (if (not (string=? (substring n 0 1) ".")) (string-append (substring n 0 1) (calcIntera(substring n 1 (string-length n) ))) "")
    "")
    ))

(define calcDecimale ;gestisce la parte decimale del numero in ingresso
 (lambda (n)
    (substring n (+ (string-length ( calcIntera n)) 1) (string-length n)) 
  )
)


(define bin-rep->number ;val: numero reale
  (lambda (n) ;riceve un numero binario, con segno e virgola, in unput
    (if (string=? (substring n 0 1) "-") (string-append "-" (number->string (+ (intera (calcIntera (substring n 1 (string-length n) ))) (decimale (calcDecimale (substring n 1 (string-length n) ))))) )
       (+ (intera (calcIntera n)) (decimale (calcDecimale n)))
    )
  )
)