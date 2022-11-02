;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname convertitore) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define bin-rep->number  ;val : numero intero o decimale
  (lambda (n)  ;numero binario in input
   ("") )
  )


(define converti ;val : numero intero o decimale
  (lambda (n)
    (if (> (string-length n )1 )
        (+
         (string->number (expt 2 (string-length n)))
         (converti (substring n 1))
         )
        (if (string=? n "1") ;sequenza di un solo numero
            (string-append "1")
            (string-append "0")
        )
     )
  )
)