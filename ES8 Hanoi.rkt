;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |ES8 Hanoi|) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))
(define hanoi-Disks
  (lambda (n k)
     (hanoi-DisksRec n k (list 1 0) (list 2 0) (list 3 0)))
  )

(define hanoi-DisksRec
  (lambda (n k s d t)
    (let ((meta (expt 2 (- n 1))))
        (cond ((= k 0)
           (list (list (car s) n)(list d)(list t))
           )
          ((< k meta)
            (hanoi-DisksRec (- n 1) k (inc s) t d)
           )
          (else
            (hanoi-DisksRec (- n 1) (- k meta) t (inc d) s)
           )
     )))  
)

(define inc                 ; val: lista di due interi
    (lambda (p)             ; p  : lista di due interi
        (list (car p) (+ (list-ref p 1) 1))
     )
 )


;(above (disk-image 5 5 2 2) (towers-background 5)) --> Grandezza DimAsta Asta Altezza

(define hanoi-Immagine
    (lambda (n k)
        (hanoi-picture-rec n k (list 1 0) (list 2 0) (list 3 0) (towers-background n) n)
     )
 )


(define hanoi-ImmagineRec
    (lambda (n k s d t image start_n)
        (let (
              (half_moves (expt 2 (- n 1)))
              )
            (cond
                ((and (= n 1) (= k 0))      ; Penso ci sia un modo più elegante di porre questi casi base, ma amen
                    
                    (above
                        (disk-image n start_n (car s) (list-ref s 1))
                        image
                     ))
                ((and (= n 1) (= k 1))
                    
                    (above
                        (disk-image n start_n (car d) (list-ref d 1))
                        image
                     ))
                ((< k half_moves)
                    (hanoi-ImmagineRec
                        (- n 1) k (inc s) t d
                        (above
                            (disk-image n start_n (car s) (list-ref s 1))
                            image
                         )
                        start_n
                     ))
                (else
                    (hanoi-ImmagineRec
                        (- n 1) (- k half_moves) t (inc d) s
                        (above
                            (disk-image n start_n (car d) (list-ref d 1))
                            image
                         )
                        start_n
                     ))
             )
         )
     )
 )
