;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname liste) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
;ESEMPI DI LISTE IN SCHEME

;OPERAZIONI SULLE LISTE
;le liste si indicano con '()
;cons -> inserisce un elemento nella lista
;null? -> verifica se la lista è vuota
;car -> restituisce il primo elemento della lista
;crd -> restituisce tutti gli elementi TRANNE il primo


;length -> lunghezza della lista

;list-ref -> restituisce l'elemento in posizione data
(define lista-ref ;val: elemento in posizione k
  (lambda (lst k) ;lst: lista
    (if (= k 0)
        (car lst)
        (lista-ref (cdr lst) (- k 1))
    )
  )
)

;append -> come lo string append, unisce due liste
(define giustapposizione ;val: lista
  (lambda (lst1 lst2) ;lst1, lst2: liste 
    (cond ((null? lst1)
           lst2)
          ((null? lst2)
           lst1)
          (else
            (cons (car lst1) (giustapposizione (cdr lst1) lst2))
          )
    )
          
  )
)


;reverse -> inverte la lista
(define rovescio-x    ;val: lista
  (lambda (lst)     ;lst: lista 
    (if (null? lst)
        null        ;lst dato che è vuota
        (giustapposizione (rovescio (cdr lst)) (cons (car lst) null))
    )    
  )
)

(define rovescio
  (lambda (lst)
    (rovescio-rec lst null)
   )
)

(define rovescio-rec ;val: lista
  (lambda (lst rv)   ;lst, rv: liste
     (if (null? lst)
         rv
         (rovescio-rec (cdr lst) (cons (car lst) rv)) 
     ) 
  )
)

(rovescio '(1 2 3 4))

; (1 2 3 4 5) -> in scheme è (1. (2 3 4 5)) va interpretato in ricorsiva
;(list 5 (+ 2 1) 12 (*3 3)) restituisce una lista di 4 elementi di cui 2 risultati di operazioni
;'(5 (+ 2 1) 12 (*3 3))  restituisce una lista ma gli elementi dell'operazione sono liste differenti