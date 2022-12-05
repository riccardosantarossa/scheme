;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname argProcedurali) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))

;PROCEDURE CON ARGOMENTI E VALORI PROCEDURALI
;CIFRARIO DI CESARE 

;ARGOMENTI PROCEDURALI

(define enc   ;val: stringa
  (lambda (msg rule) ;msg: stringa [A,B....,Z], rule : procedura [char -> char]
    (if (string=? msg "")
        ""
        (string-append
         (string (rule (string-ref msg 0)))
         (enc (substring msg 1) rule)  ;rule è una regola che posso scegliere e usare nella finestra dei test
        )                              ;nel nostro caso sarà la procedura rot-3
     )
   ))

;cifra con il cifrario di cesare, shift di 3

(define rot-3 ;val: lettera maiuscola
  (lambda (c) ;c: lettera maiuscola 
    (let (
          (k ( + (char->integer c) 3))
         )
      (if (<= k  pos-Z)
          (integer->char k)
          (integer->char (- k 26))
      )
    )))

(define pos-Z (char->integer #\Z))
(define pos-A (char->integer #\A))

;cifra con il cifrario di ottaviano, shift di 1

(define rot-1 ;val: lettera maiuscola
  (lambda (c) ;c: lettera maiuscola 
    (let (
          (k ( + (char->integer c) 1))
         )
      (if (<= k  pos-Z)
          (integer->char k)
          (integer->char (- k 26))
      )
    )   
  ))

;VALORI PROCEDURALI

;cifrario GENERICO
(define rot   ;val: lettera maiuscola
  (lambda (R) ;R: intero

    (lambda (c) ;c: lettera maiuscola 
       (let (
             (k ( + (char->integer c) R))
            )
          (if (<= k  pos-Z)
             (integer->char k)
             (integer->char (- k 26))
         )
      )   
    )))

;PROCEDURA CON ARGOMENTI E VALORI PROCEDURALI ASSIEME
 
(define dec       ;val:  procedura [char -> char] decrittazione
  (lambda (rule)  ;rule: procedura [char -> char] crittazione
    (let (
          (R (- (char->integer(rule #\A)) pos-A))
         )
      (rot (- 26 R))
    )
  )
)

;inverte la cifratura per decrittare

(define inv       ;val:  procedura [char -> char] decrittazione
  (lambda (rule)  ;rule: procedura [char -> char] crittazione (permutazione)
    (lambda (c)
      (find pos-A c rule)
    )
  )
)

(define find         ;val: carattere
  (lambda (x c rule) ;x: intero (posizione), c: carattere, rule: [char->char]
    (if (char=? (rule (integer->char x)) c)
        (integer->char x)
        (find (+ x 1) c rule)
    )
  )
)


;FUNZIONE MAP
;applica la funzione passata come parametro a tutti gli elementi della lista passata come
;secondo parametro (map (lambda (...) ...) '(...))

;funzone map artigianale

(define mappa   ;val: lista di elementi in D
  (lambda (f s) ;f: procedura [D -> I], s: lista di elementi in D
    (if (null? s)
       null
       (cons (f (car s)) (mappa f (cdr s)))
    )
   )
)


;;FUNZIONI DI ORDINE SUPERIORE

;operatore di composizione tra funzioni cp = f°g = g(f(x))

(define comp    ;val; D->F
  (lambda (g f) ;f: D->E, g: E->F
    (lambda (x)
      (g (f x))
    )
  )
)


;iteratore funzionale: modifica il valore di x iterando K volte una funzione
;passata come parametro

(define iter    ;val: procedura D->D
  (lambda (f k) ; f: D->D, k: intero >= 0
    (lambda (x)
      (if (= k 0)
        x
        (f ((iter f (- k 1)) x))
      )
    )
))

;seconda definizione di iterata usando la composizione

(define iter2   ;val: procedura D->D
  (lambda (f k)
    (if (= k 0)
       (lambda (x) x)   ;invece che restituire direttamente x applico la funz. identica
       (comp f (iter2 (- k 1))) ;uso la composizione definita in precedenza
     )
  ))





