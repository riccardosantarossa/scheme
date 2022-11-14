;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname manhattan3d) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
;PERCORSI DI MANHATTAN IN RETICOLO 3D

(define manhattan-3d ;val: intero
  (lambda (i j k) ;i, j, k interi non negativi

    ;se 2/3 delle dimensioni sono 0 allora il risultato è 1    
    (if (or (and (= i 0) (= j 0)) (and (= i 0) (= k 0)) (and (= j 0) (= k 0)))
        1
        
         ;se una delle tre dimensioni è 0 allora diventa un manhattan 2D
         (cond ((= i 0)
           (percorsi2D j k)
           )
          ((= j 0)
           (percorsi2D i k)
           )
          ((= k 0)
           (percorsi2D i j)
           )
          (else
           ;se nessuna vale 0
           (+ (manhattan-3d (- i 1) j k) (manhattan-3d i (- j 1) k) (manhattan-3d i j (- k 1)))
           )
     )
    )))
  
(define percorsi2D  ;val: intero
  (lambda (i j)   ;i e j interi non negativi
      (if
          (or (= i 0) (= j 0))
          1
          (+ (percorsi2D i (- j 1)) (percorsi2D (- i 1) j)) 
       )
   )
)