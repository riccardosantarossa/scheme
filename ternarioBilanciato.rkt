;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ternarioBilanciato) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
(define btr-rep ; val: stringa di +/-/.
  (lambda (n)   ; n: intero
    (let (
          (q (quotient n 3))
          (r (remainder n 3))
        )
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
 ))