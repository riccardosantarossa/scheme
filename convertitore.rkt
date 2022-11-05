;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname convertitore) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define bin-rep->number 
  (lambda (n)
    (if (string=? (substring n 0 1) "-") (string-append "-" (number->string (+ (intera (calcIntera (substring n 1 (string-length n) ))) (decimale (calcIntera (substring n 1 (string-length n) ))))) )
       (+ (intera (calcIntera n)) (decimale (calcIntera n)))
    )
   )
)

(define intera ;val : numero intero
  (lambda (n)  ;numero binario in input
   (if (>= (string-length n) 1)  
    (+ (* (string->number(substring n 0 1)) (expt 2 (-(string-length n) 1))) (intera (substring n 1 (string-length n) )))
    0
   )
  )
)

(define decimale  ;val : numero decimale
  (lambda (s)  ;sottostringa presa dal . del numero a dx
    (if (>= (string-length s) 1) ;calcola usando le potenze negative di 2
        (+ (* (string->number(substring s (-(string-length s) 1))) (expt 2 (- 0 (string-length s)))) (decimale (substring s 0 (- (string-length s) 1)))) 
        0
        )
    )
 )

(define calcIntera ;val : numero intero
  (lambda (n) 
    (if (>= (string-length n) 1) ;trova la parte intera
        (if (not (string=? (substring n 0 1) "."))
          (string-append (substring n 0 1) (calcIntera (substring n 1 (string-length n) ))) "")
      "")
    )
  )
 
(define calcDecimale ;val : numero decimale
  (lambda (n) ;trova la parte decimale
    (substring n (+ (string-length ( calcIntera n)) 1) (string-length n))
  )
)


