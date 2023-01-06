;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |esercitazioni 06-12|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
;ES1 MATCHING STRINGS
(define match
 (lambda (u v)
   (if (or (string=? u "") (string=? v ""))
       ""
       (let ( (uh (string-ref u 0)) (vh (string-ref v 0))
              (s (match (substring u 1) (substring v 1)))
            )
         (if (char=? uh vh)
             (string-append (string uh) s)
             (string-append "*" s)
 ))
 )))


;ES 2 INCREMENTO DI UN NUMERO RAPPRESENTATO SOTTO FORMA DI STRINGHE
(define offset (char->integer #\0))

(define last-digit
  (lambda (base) (integer->char (+ (- base 1) offset)) ))

(define next-digit
  (lambda (dgt) (string (integer->char (+ (char->integer dgt) 1))) ))

(define increment
  (lambda (num base) ; 2 <= base <= 10
    (let ((digits (string-length num)))
      (if (= digits 0)
          "1"
          (let ((dgt (string-ref num (- digits 1))))
            (if (char=? dgt (last-digit base))
                (string-append (increment (substring num 0 (- digits 1)) base)
                 "0")
                (string-append (substring num 0 (- digits 1)) (next-digit dgt) )
             ))
))))


;ES 3 LCS
(define lcs     ; valore: lista di terne
  (lambda (u v) ; u, v: stringhe
    (lcs-rec 1 u 1 v)
))

(define lcs-rec
  (lambda (i u j v) ;i,j: interi non negativi
    (cond ((or (string=? u "") (string=? v ""))
           null)
          ((char=? (string-ref u 0) (string-ref v 0))
           (cons (list i j (substring u 0 1))
                (lcs-rec (+ i 1) (substring u 1) (+ j 1) (substring v 1)) ))
          (else
           (better (lcs-rec (+ i 1) (substring u 1) j v)
                   (lcs-rec i u (+ j 1) (substring v 1)))
          )
)))

(define better  ;val: stringa
  (lambda (x y) ;x,y stringhe
    (if (< (length x) (length y)) y x)
))


;ES 4 STRINGHE CICLICHE

(define cyclic-string      ;val: stringa
  (lambda (pattern length) ;pattern: stringa, length: intero non negativo
    (cyclic-string-rec pattern length 0)
  )
)

(define cyclic-string-rec       ;val: stringa
  (lambda (pattern length cont) ;pattern: stringa, length: intero non negativo, cont: intero non negativo
    (cond
      ((= length 0)
        "")
      ((= (string-length pattern) length)
        pattern)
      ((< length (string-length pattern))
        (substring pattern 0 length))
      (else
       (let ((u (substring pattern cont (+ cont 1))))
        (cyclic-string-rec (string-append pattern u) length (+ cont 1))
       )
      )
    )
  )
)


;ES5 LISTA DI 1 0 E -1

(define av      ;val: lista di 0, 1, -1
  (lambda (lst) ;lst: lista di 0, 1, -1
    (cond
      ((null? (cdr lst))
       null)
      ((< (+ (car lst) (car (cdr lst))) 0)
       (cons -1 (av (cdr lst))))
      ((= (+ (car lst) (car (cdr lst))) 0)
       (cons 0 (av (cdr lst))))
      (else
       (cons 1 (av (cdr lst))))
    )
  )
)


;ES6 VALORI DECIMALI
(define r-val  ;val: numero decimale
  (lambda (s)  ;s: stringa del tipo "0.xxx"
    (r-val-rec s 0)
  )
)

(define r-val-rec  ;val: numero decimale
  (lambda (s cont) ;s: stringa del tipo "0.xxx", cont: intero non negativo
    (if (string=? s "")
        0
         (if (char=? (string-ref s 0) #\.)
            (r-val-rec (substring s 1) (+ cont 1))
            (+ (/ (string->number (substring s 0 1)) (expt 2 cont))  (r-val-rec (substring s 1) (+ cont 1)))
         )
        )
    )
  )  

  
;ES 7 NUMERI COMUNI A DUE LISTE

(define shared  ;val: lista 
  (lambda (u v) ;u, v: liste di interi ordinati
    (cond ((or (null? v) (null? u))
           null)
          ((= (car u) (car v))
            (cons (car u) (shared (cdr u) (cdr v))))  
          ((< (car u) (car v))
           (shared (cdr u) v))
          (else
           (shared u (cdr v)))
)))


;ES8 PARITY CHECK FAILS

(define parity-check-failures ;val: lista di interi
  (lambda (lst)               ;lst: lista di stringhe binarie
    (parity-check lst 0)
  )
)

;Controlla se ogni elemento di lst rispetta o meno la parità 
(define parity-check   ;val: lista di interi
  (lambda (lst cont)   ;lst: lista di stringhe binarie, cont: intero non negativo
    (if (null? lst)
        null       
        (let ((v (sum (car lst) 0)))
          (if (= (remainder v 2) 0)
             (parity-check (cdr lst) (+ cont 1))
             (cons cont (parity-check (cdr lst) (+ cont 1)))
          )
))))

;Fa la somma delle singole parole binarie dentro la lista
(define sum         ;val: intero non negativo
  (lambda (s acc)   ;s: stringa binaria, acc: intero non negativo
    (if (string=? s "")
        acc
        (if (char=? (string-ref s 0) #\1)
            (sum (substring s 1) (+ acc 1))
            (sum (substring s 1) acc)
        ))
  )
)


;ES9 CLOSEST PAIR
(define closest-pair
   (lambda (lst)
     (let  ((a (car lst)) (b (car (cdr lst)))
            (s  (sub a b)))

         
         
     )
   )
)


(define sub
  (lambda (x y)
    (- y x)
  )
)

