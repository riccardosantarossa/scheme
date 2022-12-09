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

(f "011") --> 3*2^(3-2) - 1 = 5

;ipotesi induttiva: considero k >= 3 intero e prendo t = "11...1011" di lunghezza k

(f t) --> 3*2^(k-2) - 1

;passo induttivo: per k e t considerati sopra

(f t') --> 3*2^((k+1)-2) - 1 ;dove t' è t preceduto da "1"







