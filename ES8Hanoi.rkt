;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ES8Hanoi) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))

;DEFINISCE LA LISTA DI MOSSE NECESSARIA ALLA RISOLUZIONE DEL PROBLEMA
(define hanoi-moves ; val: lista di coppie
 (lambda (n) ; n > 0 intero
 (hanoi-rec n 1 2 3)
 ))

;PROCEDURA CHE OPERA SULLE LISTE PER CREARE LE COPPIE DISCO/ASTICELLA
(define hanoi-rec ; val: lista di coppie
 (lambda (n s d t) ; n intero, s, d, t: posizioni
   (if (= n 1)
       (list (list s d))
       (let ((m1 (hanoi-rec (- n 1) s t d))
             (m2 (hanoi-rec (- n 1) t d s))
             )
         (append m1 (cons (list s d) m2))
       )
    )
 ))

;PROCEDURA CHE DETERMINA LA CONFIGURAZIONE DEI DISCHI ALLA K-ESIMA MOSSA 

(define hanoi-disks  ;val: lista di coppie
  (lambda (n k)      ;n, k: interi (n=num dischi, k=num mosse)
    (config-mosse n k 1 2 3)
  )
)

(define config-mosse ;val: lista di coppie
  (lambda (n k s d t) ;n, k: interi (s, d, t posizione delle asticelle) 
    (if (= k 0)
        (append (list (list s n) (list d 0) (list t 0)))
        ""
    )
  )
)