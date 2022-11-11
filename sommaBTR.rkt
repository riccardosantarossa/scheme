;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sommaBTR) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
;SOMMA CIFRE DEL SISTEMA TERNARIO BILANCIATO
(define btr-digit-sum                    ; val:     carattere +/./-
  (lambda (u v c)                        ; u, v, c: caratteri +/./-
    (cond ((char=? u #\-)                ; u v c
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; - - -
                         #\.)
                        ((char=? c #\.)  ; - - .
                         #\+)
                        ((char=? c #\+)  ; - - +
                         #\-)))
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; - . -
                         #\+)
                        ((char=? c #\.)  ; - . .
                         #\-)
                        ((char=? c #\+)  ; - . +
                         #\.)))
                 ((char=? v #\+)         ; - + c
                  c)))
          ((char=? u #\.)
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  ; . - -
                         #\+)
                        ((char=? c #\.)  ; . - .
                         #\-)
                        ((char=? c #\+)  ; . - +
                         #\.)))
                 ((char=? v #\.)         ; . . c
                  c)
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; . + -
                         #\.)
                        ((char=? c #\.)  ; . + .
                         #\+)
                        ((char=? c #\+)  ; . + +
                         #\-)))))
          ((char=? u #\+)
           (cond ((char=? v #\-)         ; + - c
                  c)
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  ; + . -
                         #\.)
                        ((char=? c #\.)  ; + . .
                         #\+)
                        ((char=? c #\+)  ; + . +
                         #\-)))
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ; + + -
                         #\+)
                        ((char=? c #\.)  ; + + .
                         #\-)
                        ((char=? c #\+)  ; + + +
                         #\.)))))
          )))


(define normalized-btr
  (lambda (s)
    (if (>= (string-length s) 1)
        (if (char=? (string-ref s 0) #\.) (normalized-btr (substring s 1 (string-length s))) s )
     ""
    )
  )
)

;Restituisce il cartattere corrispondente alla cifra meno significativa
(define lsd ;val: carattere
  (lambda (btr) ;stringa di . , +, -
    (let ((k (- (string-length btr) 1)) )
      (if (string=? btr "")
          #\.
          (string-ref btr k)
       )
    )     
  )
)



;Restituisce la parte che precede l'ultima cifra
(define head ;val: strings
  (lambda (btr) ;stringa di +, -, .
    (if (string=? btr "")
        ""
        (if (= (string-length btr) 1)
           btr
           (substring btr 0 (- (string-length btr) 1 ))
     )
  )
))

;Restituisce il riporto della somma tra due caratteri e il CARRYin
(define btr-carry                    ; val:     carattere +/./-
  (lambda (u v c)                        ; u, v, c: caratteri +/./-
    (cond ((char=? u #\-)                ; u v c
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  
                         #\-)
                        ((char=? c #\.)  
                         #\-)
                        ((char=? c #\+)  
                         #\+)))
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  
                         #\-)
                        ((char=? c #\.)  
                         #\.)
                        ((char=? c #\+)  
                         #\.)))
                 ((char=? v #\+)         
                    (cond ((char=? c #\-)  
                         #\+)
                        ((char=? c #\.)  
                         #\.)
                        ((char=? c #\+)  
                         #\.)) )))
          ((char=? u #\.)
           (cond ((char=? v #\-)
                  (cond ((char=? c #\-)  
                         #\-)
                        ((char=? c #\.)  
                         #\.)
                        ((char=? c #\+)  
                         #\.)))
                 ((char=? v #\.)         
                  #\.)
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  
                         #\.)
                        ((char=? c #\.)  
                         #\.)
                        ((char=? c #\+)  
                         #\+)))))
          ((char=? u #\+)
           (cond ((char=? v #\-)         
                  (cond ((char=? c #\-)  
                         #\.)
                        ((char=? c #\.) 
                         #\.)
                        ((char=? c #\+)  
                         #\.)))
                 ((char=? v #\.)
                  (cond ((char=? c #\-)  
                         #\.)
                        ((char=? c #\.)  
                         #\.)
                        ((char=? c #\+)  
                         #\+)))
                 ((char=? v #\+) ;u+v = - rip +
                  #\+)
           )
))))

;Funzione che raggruppa i risultati di digit-sum e btr-carry
;"xxx|x" "yyy|y" --> (digit-sum x y) (carry x y) -----> (digit-sum xxx) (carry yyy)

(define btr-carry-sum
  (lambda (u v c)
    (let (( un (normalized-btr u))
          ( vn (normalized-btr v))
          )
      (if (> (string-length vn) 1)
          (string-append 
           (string (btr-digit-sum (lsd un) (lsd vn) c))
           (string (btr-carry-sum (head un) (head vn) (btr-carry (lsd un) (lsd vn) (btr-carry (lsd un) (lsd vn) c))))
          )
          "")
      )
    )
   )

;Funzione principale del programma che fa la somma di due interi in notazione
;ternaria bilanciata
(define btr-sum
  (lambda (u v)
     (btr-carry-sum (normalized-btr u) (normalized-btr v) #\.)
    )
  )

;TEST
;(btr-sum "-" "+")
(btr-sum "-+--" "+")
(btr-sum "-+--" "-")
(btr-sum "+-.+" "-+.-")
(btr-sum "-+--+" "-.--")
(btr-sum "-+-+." "-.-+")
(btr-sum "+-+-." "+.+-")