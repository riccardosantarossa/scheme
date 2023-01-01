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
             (let ((p (cyclic-pattern (substring s k) k))) ;estraggo il possibile pattern
               (if (string=? (substring s 0 k) p)
                   p
                   ""
               )
             )))
)))

;ES2A tassellazione con piastrelle lunghe 1 e 2 ma le piastrelle singole non possono stare vicine

(define tess-1x-2
  (lambda (n)
    (cond ((= n 0)
           1)
          ((= n 2)
           1)
          (else
           (+ (tess-1x-2 (- n 3)) (tess-1x-2 (- n 2)))
          ))
  )
)

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

;ES3B  restituire la lista di possibili percorsi di manhattan con spostamenti verticali
;limitati  (il terzo parametro è il numero di spostamenti verticali consecutivi consentiti)
;nelle stringhe, 0 rappresenta lo spostamento in basso e 1 quello a destra

(define pathsB ; val: lista di stringhe
 (lambda (i j k) ; i, j, k: interi non negativi
   (paths-rec i j k k)
 ))

(define paths-recB
 (lambda (i j k u)
   (cond ((= i 0)
          ((make-string j #\1)))
         ((= j 0)
           (make-string k #\0))
         ((= u 0)
          (map (lambda (x) (string-append "1" x))
           (paths-recB i (- j 1) k k)))
         (else
          (append
           (map (lambda (x) (string-append "1" x))
                (paths-recB (- i 1) j k (- u 1)))
           (map (lambda (x) (string-append "0" x))
            (paths-recB i (- j 1) k k))))
 )
 ))

;ES4A Dimostrazione di correttezza per induzione

;caso base: (f "111") --> 3*2^(3-2) + 1 = 7

;ipotesi induttiva: considero k >= 3 intero e considero la stringa di lunghezza k t = "00...0111" (dove gli 1 sono (k-3))

;(f t) --> 3*2^(k-3) + 1

;passo induttivo : per t e k considerati -> (f t) --> 3*2^((k+1)- 3) + 1 

;(f t) --> 3*2^((k+1)- 3) + 1

;      --> 3*2^(k+2) + 1

;      --> 3*[ 2^k + 4 + 1]

;      --> 3*2^k + 12 + 3 = 3*2^k + 15

;ES4B Dimostrazione di correttezza per induzione 

;caso base:

;(f "011") --> 3*2^(3-2) - 1 = 5

;ipotesi induttiva: considero k >= 3 intero e prendo t = "11...1011" di lunghezza k

;(f t) --> 3*2^(k-2) - 1

;passo induttivo: per k e t considerati sopra

;(f t') --> 3*2^((k+1)-2) - 1 ;dove t' è t preceduto da "1"


;--------------------------------------------------------------------------------------------------------

;PROVA D'ESAME 29.01.2021: temi A e B misti

;ES1A data una lista di due numeri, restituire la lista contenente il valor medio e la semidifferenza dei due
;date due liste usare pair sugli elementi della stessa posizione

(define pair
  (lambda (x y)
    (list (/ (+ x y) 2) (/ (abs (- x y)) 2))
   )
)

(define pair-list
  (lambda (l1 l2)
    (if (null? l1)
        null
        (cons (pair (car l1) (car l2)) (pair-list (cdr l1) (cdr l2)))
    )
  )
)

;ES1B dati due caratteri la procedura pair riceve la coppia ordinata dei due caratteri (lista)
;date due liste, la procedura pair-list applica pair e restituisce la lista di coppie ordinate 

(define pairB    ;val: lista
  (lambda (x y) ;x, y: carattere
   (if (char<? y x)
       (list y x)
       (list x y)
   )
))

(define pair-listB  ;val: lista di coppie
  (lambda (lx ly)  ;lx, ly: liste di caratteri
    (if (null? lx)
        null
       (cons (pairB (car lx) (car ly)) (pair-listB (cdr lx) (cdr ly))) 
    )   
  ))



;ES2A completare il codice che restituisce i caratteri da rimuovere per ottenere la LCS tra
;le due stringe fornite in input

(define lcs-align1 ; val: coppia di liste di caratteri
 (lambda (u v) ; u, v: stringhe
   (let ((m (string-length u)) (n (string-length v)))
     
     (cond ((or (= m 0) (= n 0))
            (list (string->list u) (string->list v)))
           ((char=? (string-ref u 0) (string-ref v 0))
              (lcs-align1 (substring u 1) (substring v 1)) 
             )
           (else
            (let ((du (lcs-align1 (substring u 1) v))
                  (dv (lcs-align1 (substring v 1) u))
                  )
 (if (> (+ (length (car du)) (length (cadr du)))
        (+ (length (car dv)) (length (cadr dv))))
     (list (car dv) (cons (string-ref v 0) (cadr dv)))
     (list (cons (string-ref u 0) (cadr du)) (cadr du))
 )))
 ))))

;ES2B completare il codice che restituisce la lunghezza di LCS tra u e v e
;la rielaborazione di v in cui i caratteri che non fanno parte della LCS diventano "_"

(define lcs-align ; val: coppia intero/stringa
  (lambda (u v) ; u, v: stringhe
    (let ((m (string-length u)) (n (string-length v))
                                )
      (cond ((= n 0) (list 0 ""))
            ((= m 0)
             (let ((w (lcs-align u (substring v 1))))
               (list 0 (string-append "_" (cadr w)))
               ))
            ((char=? (string-ref u 0) (string-ref v 0))
             (let ((dx '(0 (string-ref u 0)))
                   )
               (list (+ (car dx) 1) (string-append (substring v 0 1) (cadr dx))
                     )))
            (else
             (let ((du (lcs-align (substring u 1) v))
                   (dv (lcs-align u (substring v 1))
                   ))
               (if (< (car du) (car dv))
                   (list (car dv) (string-append "_" (cadr dv)))
                   (lcs-align (substring u 1) (substring v 1))
                   )))
            ))))

;ES3A dimostrazione per induzione

;(f m n) --> n^2 – (m–1)^2

;caso base: m = n = 1  [n-m = 0]

;(f 1 1 ) --> 1 [1^2 - 0]

;passo induttivo:  
  

;--------------------------------------------------------------------------------------------------------

;PROVA D'ESAME 04.02.2020: temi A e B misti

;ES2A rendere maiuscola la prima lettera di ogni parola della lista, ma SOLO la prima lettera
(define upper-case  ;val: stringa
  (lambda (w)       ;w: stringa
    (string-append
     (string (char-upcase (string-ref w 0)))
     (substring w 1)
     )
  )
)

(define standard-form ;val: lista di stringhe
  (lambda (l)         ;l: lista di stringhe
    (map upper-case l)
  )
)


;ES2B rendere minuscola la prima lettera di ogni parola della lista, ma SOLO la prima lettera

(define lower-case ;val: stringa
  (lambda (s)      ;s: stringa
    (string-append 
     (string (char-downcase (string-ref s 0)))
     (substring s 1))
  )
)

(define lower-first ;val: lista di stringhe 
  (lambda (u)       ;u: lista di stringhe 
    (map lower-case u)
  )
)


;ES3A  determinare il valore intero di un numero in ternario bilanciato BTR

(define btr-val-tr ; val: intero
   (lambda (btr)   ; btr: stringa di – / . / +
     (btr-val-rec btr 0)
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

;ES3B convertire un numero dalla base 3 alla base 10
 (define ter-val-tr ; val: intero
   (lambda (ter) ; ter: stringa di 0 / 1 / 2
     (ter-val-rec ter 0)
 ))
 (define ter-val-rec
   (lambda (ter acc)
     (let ((k (string-length ter))
           )
       (if (= k 0)
           acc
           (let ((q (substring ter 1))
                 (t (string-ref ter 0))
                 )
             (ter-val-rec q (+ (* 3 acc) (ted-val t)))
 )))
 ))


(define ted-val
 (lambda (t)
   (cond ((char=? t #\0) 0)
         ((char=? t #\1) 1)
         ((char=? t #\2) 2)
   ) ))



;ES4A dim. per induzione dell'esercizio 1

;Dimostrare che per ogni coppia di interi n e k : (f 1 n 0 k) --> k

;VALORE SU CUI IMPOSTARE LA DIMOSTRAZIONE
;n


;CASO BASE  

;n=0 quindi k=0 perchè compreso tra 0 e n
;(f 1 0 0 0) --> 0 OK


;IPOTESI INDUTTIVA

;considero n>=0  (f 1 n 0 k) --> k




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

;PROVA D'ESAME 03.02.2017: temi A e B misti

;ES1
(define f ; intero
 (lambda (s) ; lista di interi positivi
   (g s 0)
 ))

(define g
 (lambda (s b)
   (if (null? s)
       0
       (if (<= (car s) b)
           (g (cdr s) b)
           (max (g (cdr s) b)
                (+ (g (cdr s) (car s)) 1))
 ))))


;ES2

(define mh ; val: intero
 (lambda (i j) ; i, j: interi non negativi
   (if (or (= i 0) (= j 0))
       1
       (+ (md (- i 1) j) (mr i (- j 1)))
 )))

(define md ; spostamento precedente “in giù”
   (lambda (i j)
     (if (or (= i 0) (< j 2) )
         1
         (+ (md (- i 1) j ) (mr i (- j 2)))
 )))

(define mr ; spostamento precedente “a destra”
  (lambda (i j)
    (if (or (< i 2) (= j 0))
        1
        (+ (md (- i 2) j) (mr i (- j 1)) )
 )))


;ES3

(define is-identifier?  ;val: booleano
  (lambda (s)           ;s: stringa
    (cond
         ((string? "" s) false)
         ((letter? (string-ref s 0))
          (alfanumeric? (substring s 1)))
         (else
          false)
     )
    )
  )


(define letter?   ;val: booleano
  (lambda (l)     ;l: carattere
    (or  (and  (char>=? l #\a) (char<=? l #\z))
         ((and  (char>=? l #\A) (char<=? l #\Z))
    ))
  )
)

(define alfanumeric?
  (lambda (s)
    (if (string=? s "")
        true
        (let ((c (string-ref s 0)))
          (if 
           (or
            (letter? c)
            (and  (char>=? s #\0) (char<=? s #\9))
            (char=? c #\_)
            )
      (alfanumeric? (substring s 1))
      false
  )))
))


;--------------------------------------------------------------------------------------------------------
  
;INDUZIONE

; (g x y) --> xy + |x-y|

;PARAMETRO SCELTO : Ogni n in N, ogni k in N --> nk + |n-k|

;CASI BASE : Ogni k in n (g 0 k) --> 0k + |0-k| --> k (dimostrabile seguendo il programma)

;IPOTESI INDUTTIVA : considero un certo n in N e assumo che valga: ogni k in N --> nk + |n-k|

;PASSO INDUTTIVO : per n considerato (g n+1 k) --> (n+1)k + |n+1 - k|

;DIM
  
;(g n+1 k) --> ? dipende da come è k, 0 oppure >0

;supponiamo k = 0 : (g n+1 0) --> (+ 0 + n+1 0) --> n+1 (va bene perchè (n+1)*0 + |n+1 -0| = n+1)

;supponiamo k > 0 : (g n+1 k) --> (+ (- (g (- n+1 1) (- k 1)) n+1 k))

;                             --> (+ (- (g n k-1) n+1 k))
  
;                             --> (+ (- n(k-1) + |n - k-1| 1) n+1 k)  per ipotesi induttiva

;                             --> n(k-1) + |n - (k-1)| -1 + n+1 + k

;                             --> nk - n + |n - (k-1)| -1 + n+1 + k

;                             --> (n+1)k + |n+1 - k|   CVD






  


