;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#lang racket
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ricorsive) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Complemento a 1 mediante RICORSIONE
;di una sequenza in ingresso creo il suo equivalente
;mediante il complemento a 1 ovvero invertendo gli 0 con gli 1

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
;posizionamento di n tasselli in una sequenza di lunghezza variabile
;i tasselli possono essere di dimensioni diverse

;tasselli di due diverse dimensioni, quadrata e rettangolare
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

;tasselli di uguale dimensione ma diverso colore
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
;l'obiettivo è andare da un punto A ad un punto B su un reticolo n*n
;le svolte permesse sono soltanto due, in basso e a destra in quanto non è conveniente tornare indietro

(define percorsi  ;val: intero
  (lambda (i j)   ;i e j interi non negativi
      (if
          (or (= i 0) (= j 0))
          1
          (+ (percorsi i (- j 1)) (percorsi (- 1 i) j)) 
       )
   )
)

;RICORSIVA CON PIATTI E DOLCETTI (numeri di stirling del secondo tipo)
;posizionare n dolcetti in k piatti, ma mettendo almeno un dolcetto per piatto

(define st ;val: intero 
  (lambda (n k) ;n e k numeri interi con 1 <= k <= n
    (if (or (= k 1) (= k n))
        1
        (+ (st (- n 1) (- k 1)) (* k (st (- n 1) k)))
    )
  )
)


;ALLINEAMENTO DI DUE SEQUENZE
;trovare la più lunga sottosequenza comune (LCS) alle due macro sequenze
;sottostringa di stesse lettere nello stesso ordine

;ESEMPI per IDEA
;llcs( Ax, By ) --> k (lunghezza della LCS)
;llcs( Ax, Ay ) = 1 + llcs( x, y ) [1]
;llcs( Ax, By ) = max( llcs( Ax, y ) llcs( x, By ) ) se  a != b [3]
;Casi base : llcs( "", y ) llcs( x, "" ) llcs = 0 [2]

;calcola la LUNGHEZZA della sottosequenza comune più lunga, non come è formata
(define llcs   ; val: intero non negativo
 (lambda (u v) ; u,v: stringhe
     (cond ((or (string=? u "") (string=? v "")) ;[2]
            0)
           ((char=? (string-ref u 0) (string-ref v 0)) ;[1]
            (+ 1 (llcs (substring u 1) (substring v 1) )))
           (else                            ;[3]
            (max (llcs u (substring v 1))
                 (llcs (substring u 1) v)
            ))
     )
  )
 )

;calcola la LCS vera e propria, non la lunghezza
;uso la funzione precedente ma sostituisco gli interi con le stringhe

(define lcs   ; val: stringa
 (lambda (u v) ; u,v: stringhe
     (cond ((or (string=? u "") (string=? v "")) ;[2]
            "")
           ((char=? (string-ref u 0) (string-ref v 0)) ;[1]
            (string-append
             (substring u 0 1)
             (lcs (substring u 1) (substring v 1))
             ))
           (else ;[3]
            (longest ;calcola la stringa più lunga delle due
             (lcs u (substring v 1))
             (lcs (substring u 1) v)
            ))
     )
  )
 )

;calcola la stringa più lunga tra le due in input

(define longest ;val: stringa
  (lambda (u v) ;u,v: stringhe
    (let (( m (string-length u))
          ( n (string-length v))
         )
     (cond ((< m n)
            v)
           ((> m n)
            u)
           ((= (random 2) 0)
            v)
           (else
            u)
      )
   )
 )
)

