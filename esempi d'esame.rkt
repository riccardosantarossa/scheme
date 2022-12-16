;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |esempi d'esame|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))

;PROVA D'ESAME 21.01.2022: temi A e B misti

;ES1A  stringhe cicliche. restituisce la stringa p di lunghezza k se s è
;costituita dalla ripetizione ciclica di p una o più volte, altrimenti la stringa vuota

(define cyclic-pattern ;val: stringa
  (lambda (s k)        ;s: stringa, k: intero positivo
    (let ((n (string-length s)))
      (cond ((< n k)
             "")
            ((= n k) ;se la stringa è lunga come k allora è lei stessa
             s) 
            (else
             (let ((p (cyclic-pattern (substring s k) k))) ;estrago il possibile pattern
               (if (string=? (substring s 0 k) p)
                   p
                   ""
               )
             )))
)))


;ES2B  tassellazione di un cordolo di lunghezza n>=0 con piastrelle di lunghezza 1 e 2
;ma con il vincolo che due piastrelle di lunghezza 2 non possono essere mai vicine

(define tess-1-2x   ;val: intero
  (lambda (n)       ;n: intero non negativo
    (cond ((= n 0)
           1)
          ((= n 1)
           1)
          ((= n 2)
           2)
         (else
          (+ (tess-1-2x (- n 1)) (tess-1-2x (- n 3)))
         ))
  ))



;ES3A  restituire la lista di possibili percorsi di manhattan con spostamenti orizzontali
;limitati  (il terzo parametro è il numero di spostamenti orizzontali consecutivi consentiti)
;nelle stringhe, 0 rappresenta lo spostamento in basso e 1 quello a destra


(define paths        ; val: lista di stringhe
 (lambda (i j k)     ; i, j, k: interi non negativi
 (paths-rec i j k k)
 ))

(define paths-rec       ;val: lista di stringhe
  (lambda (i j k v)     ;i, j, k, v: interi non negativi
    (cond ((= i 0)
            (if (> j v)
                '()
                (list (make-string j #\1)))
           )
          ((= j 0)
           (list (make-string i #\0))
           )
          ((= v 0)
           (map (lambda (x) (string-append "0" x)) ;aggiungo il primo spostamento vertcale
            (paths-rec (- i 1) j k k))
           )
          (else
           (append
            (map (lambda (x) (string-append "0" x)) ;aggiungo il primo spostamento vertcale
             (paths-rec (- i 1) j k k))
            (map (lambda (x) (string-append "1" x)) ;aggiungo il primo spostamento orizzontale
             (paths-rec i (- j 1) k (- v 1))))
          )
 )
 ))

;ES4B Dimostrazione di correttezza per induzione 

;caso base:

;(f "011") --> 3*2^(3-2) - 1 = 5

;ipotesi induttiva: considero k >= 3 intero e prendo t = "11...1011" di lunghezza k

;(f t) --> 3*2^(k-2) - 1

;passo induttivo: per k e t considerati sopra

;(f t') --> 3*2^((k+1)-2) - 1 ;dove t' è t preceduto da "1"


;--------------------------------------------------------------------------------------------------------

;PROVA D'ESAME 29.01.2021: temi A e B misti

;ES1B dati due caratteri la procedura pair riceve la coppia ordinata dei due caratteri (lista)
;date due liste, la procedura pair-list applica pair e restituisce la lista di coppie ordinate 

(define pair    ;val: lista
  (lambda (x y) ;x, y: carattere
   (if (char<? y x)
       (list y x)
       (list x y)
   )
))

(define pair-list  ;val: lista di coppie
  (lambda (lx ly)  ;lx, ly: liste di caratteri
    (if (null? lx)
        null
       (cons (pair (car lx) (car ly)) (pair-list (cdr lx) (cdr ly))) 
    )   
  ))



;ES2A completare il codice che restituisce i caratteri da rimuovere per ottenere la LCS tra
;le due stringe fornite in inèut

(define lcs-align ; val: coppia di liste di caratteri
 (lambda (u v) ; u, v: stringhe
   (let ((m (string-length u)) (n (string-length v)))
     
     (cond ((or (= m 0) (= n 0))
            (list (string->list u) (string->list v)))
           ((char=? (string-ref u 0) (string-ref v 0))
              (lcs-align (substring u 1) (substring v 1)) 
             )
           (else
            (let ((du (lcs-align (substring u 1) v))
                  (dv (lcs-align (substring v 1) u))
                  )
 (if (> (+ (length (car du)) (length (cadr du)))
        (+ (length (car dv)) (length (cadr dv))))
     (list (car dv) (cons (string-ref v 0) (cadr dv)))
     (list (cons (string-ref u 0) (cadr du)) (cadr du))
 )))
 ))))

;--------------------------------------------------------------------------------------------------------

;PROVA D'ESAME 04.02.2020: temi A e B misti

;ES2B rendere minuscola la prima lettera di ogni parola della lista, ma SOLO la prima lettera

(define lower-case ;val: stringa
  (lambda (s)      ;s: stringa
    (string-append 
     (string (char-downcase (string-ref s 0)))
     (substring s 1))
  )
)

(define lower-first ;val: lista di stringhe 
  (lambda (u) ;u: lista di stringhe 
    (map lower-case u)
  )
)


;ES3A  determinare il valore intero di un numero in ternario bilanciato BTR

(define btr-val-tr ; val: intero
   (lambda (btr 0)   ; btr: stringa di – / . / +
     (btr-val-rec btr )
 ))

(define btr-val-rec ; val: intero
  (lambda (btr n)    ; btr: stringa di – / . / +, n: valore numerico parziale (accumulatore) 
    (let ((k (string-length btr))
          )
      (if (= k 0)
          n
          (let ((q (substring btr 1 ))
                (t (string-ref  btr 0))
                )
           (btr-val-rec q (+ (* 3 n) (btd-val t)))
 )))
 ))

(define btd-val
 (lambda (t)
   (cond ((char=? t #\-) -1)
         ((char=? t #\.) 0)
         ((char=? t #\+) +1)
   )
 ))


;ES4B dim. per induzione sull'esercizio 1

;VALORE SU CUI IMPOSTARE LA DIMOSTRAZIONE
;n

;CASO BASE

;pongo n = 0
;Per ogni s nell'intervallo [0,0] (f 0 1 0 1) --> 0-0 = 0


;IPOTESI INDUTTIVA

;Considero n>=0 : Per ogni s nell'intervallo [0,n] (f s 1 n 1) --> n-s


;PASSO INDUTTIVO

;Per n considerato nell'ipotesi : Per ogni s nell'intervallo [0,n+1] (f s 1 n+1 1) --> n+1-s


;DIMOSTRAZIONE DEL PASSO INDUTTIVO

;(f s 1 n+1 1) --> 

;a. s = n+1 : (f n+1 1 n+1 1) --> 0 = n+1-n-1

;b. s in [0,n] : (f s 1 n+1 1) 

;                          --> (+ (f s 1 (- n+1 1) 1) (f s 1 n+1 (- 1 1)))

;                          --> (+ (f s 1 n 1) (f s 1 n+1 (- 1 1)))

;Applico l'ip. induttiva:  --> (+ n-s (f s 1 n+1 (- 1 1)))

;                          --> (+ n-s (f s 1 n+1 0))

;                          --> (+ n-s 1) = n+1-s CVD


;ES5A procedura che restituisce le possibili stringhe di 0 e 1 contenenti al massimo k volte 1

(define combinations ; val: lista di stringhe
 (lambda (k n)       ; k, n: interi non negativi
   (if (= n 0)
       (list "")
       (let ((u (if (= k 0)
              null
              (combinations (- k 1) (- n 1))
            ))
         (v (combinations k (- n 1)))
         )
         (append
          (map (lambda (s) (string-append "1" s)) u)
          (map (lambda (s) (string-append "0" s)) v)
 )))
 ))

;--------------------------------------------------------------------------------------------------------







