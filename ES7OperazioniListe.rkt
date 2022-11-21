;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ES7OperazioniListe) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))

;Verifica se un elemento appartiene o meno alla lista data in input
(define belong? ;val: true/false
  (lambda (x S) ;x: intero, S:lista di interi
     (cond
       ((null? S)
        false)
       ((= (car S) x)
        true)
       (else
        (belong? x (cdr S))
       )
      )
   )
)


;Inserisce un numero in una lista ordinata e senza ripetizioni
(define sorted-ins ;val: lista
  (lambda (x S)   ;x: intero, S:lista ordinata di interi
    (cond
      ((null? S)
        (cons x S)
       )
      ((= x (car S))
       S)
      ((< x (car S))
        (cons x S)
       )
      (else
       (sorted-ins x (cdr S))
       )
    )
    ))
