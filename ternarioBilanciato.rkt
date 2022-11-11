;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#lang racket 
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ternarioBilanciato) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))

;RAPPRESENTAZIONE NEL SISTEMA TERNARIO BILANCIATO (esempio)

(define btd-val ;val: -1, 0, +1
  (lambda (btd) ;btd: "-", ".", "+"
     (cond ((string=? btd "-") -1)
           ((string=? btd ".") 0)
           ((string=? btd "+") +1)
      ) 
    )
)

(define btr-val ;val: intero
  (lambda (btr) ;btr: stringa di - . + 
     (let (
           (k (- (string-length btr) 1))
           )
      (if (= k 0)
          (btd-val btr)
          (+ (* 3 (btr-val (substring btr 0 k)) )
             (btd-val (substring btr k)))
       )
      )
    )
)


;CONVERTE UN NUMERO INTERO NEL SISTEMA TERNARIO BILANCIATO

(define btr-rep ; val: stringa di +/-/.
  (lambda (n)   ; n: intero
    (let(
          (q (quotient n 3))
          (r (remainder n 3))
        )
   (cond ((<= (abs n) 1)
          (btd-rep n)) 
         ((= r -2)  ; n= 3q + r = 3q + -2 -3 +3 = 3(q-1) + +1
          (string-append
           (btr-rep (- q 1)) (btd-rep +1)
           ))      
          ((= r +2 ) ; n= 3q + r = 3q + +2 +3 -3 = 3(q+1) + -1
           (string-append
           (btr-rep (+ q 1)) (btd-rep -1)
           ))
         (else       ; n= 3q + r, dove r in {-1, 0, +1}
          (string-append
           (btr-rep q) (btd-rep r)
           ))      
   ) 
 )
)
)

(define btd-rep ;val: "-", "+", "."
  (lambda (n)  ;n: +1, 0, -1
    (cond  ((= n +1) (string-append "+"))
           ((= n 0) (string-append "."))
           ((= n -1) (string-append "-"))
    )
   )
)








  