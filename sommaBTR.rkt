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

;Toglie gli zeri in testa
;(define normalized-btr ;val: stringa normalizzata (senza zeri quindi snza ".")
 ;(lambda (d) ;stringa di . , +, -        
  ;    (if (string=? (substring d 0 1) ".")
   ;       (normalized-btr (substring d 1) )
    ;      (string-append d)
     ; )
    ;😉
 ;😉

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
        (substring btr 0 (- (string-length btr) 1 ))
     )
  )
)

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
                 ((char=? v #\+)
                  (cond ((char=? c #\-)  ;
                         #\-)
                        ((char=? c #\.)  
                         #\+)
                        ((char=? c #\+)  
                         #\+)))))
          )))

;Funzione che raggruppa i risultati di digit-sum e btr-carry
(define btr-carry-sum
  (lambda (u v c)
      (if (or (>= (string-length u) 1 ) (>= (string-length v) 1 ))
          (string-append (btr-carry-sum (head u) (head v) (btr-carry (lsd u) (lsd v) (btr-carry (lsd u) (lsd v) c))) (string (btr-digit-sum (lsd u) (lsd v) c)))
          
         ""
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