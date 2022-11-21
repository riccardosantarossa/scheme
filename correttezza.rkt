;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname correttezza) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))

;CORRETTEZZA DI UN PROGRAMMA RICORSIVO

(define unknown ;val: intero   (x^2)
  (lambda (x)   ;x : intero non negativo
    (if (= x 0)
        0
        (+ unknown (- x 1) (odd x))
     )
  )
)

(define odd   ;val: intero  (2i-1)
  (lambda (i) ;i : intero positivo
    (if (= i 1)
        1
        (+ (odd (- i 1) 2))
    )
  )
)