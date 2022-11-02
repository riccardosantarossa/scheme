;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ricorsive) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Complemento a 1 mediante RICORSIONE

(define bitComplement
  (lambda (bit)       ; 0 oppure 1
    (if (string=? bit "0")
       "1"
       "0"
     )
   )
)

(define Compl1      ; stringa di 0 e 1 
  (lambda (sq)      ; sq = sequenza di 0 e 1 (stringa)
    (if (> (string-length sq )1 )
        (string-append 
         (bitComplement (substring sq 0 1))
         (Compl1 (substring sq 1))
         )
        (bitComplement sq) ;sequenza di un solo bit 
        )
    )
)


;Creare i vari formati di foglio Ak

(define s0 (expt 2 1/4)  ) ; radice quarta di 2 
(define s1 (expt 2 -1/4) ) ; inversa della radice quarta di 2

(define LatoFoglio      ;val: reale (lato più lungo del foglio)
  (lambda (k)           ;k intero non negativo 
    (if (>= k 2)
      (/ (LatoFoglio (- k 2)) 2)
      (if (= k 0) s0 s1)
    )
   )
)


;RICORSIVE CON TASSELLI

(define tass-qr  ;val: intero positivo
  (lambda (n)    ;n: intero positivo
     (cond 
        ((= n 1) 1)
        ((= n 2) 2)
        (else
          (+ (tass-qr (- n 2)) (tass-qr (- n 1)))
        )
     )
  )
)

(define tass-rb  ;val: intero positivo
  (lambda (n)    ;n: intero positivo
     (cond 
        ((= n 1) 2)
        ((= n 2) 3)
        (else
          (+ (tass-rb (- n 2)) (tass-rb (- n 1)))
        )
     )
  )
)

;RICORSIVA PERCORSI DI MANHATTAN

(define percorsi  ;val: intero
  (lambda (i j)   ;i e j interi non negativi
      (if
          (or (= i 0) (= j 0))
          1
          (+ (percorsi i (- j 1)) (percorsi (- 1 i) j)) 
       )
   )
)