;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ES8Hanoi) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks") (lib "hanoi.ss" "installed-teachpacks")) #f)))

(define hanoi-cont ; val: lista di coppie
  (lambda (n s d t k h v a) ; n intero, s, d, t: posizioni
    (if (= n 1)
        (if (> v k)
            0
            (cond ((equal? s h) -1)
                  ((equal? d h) +1)
                  (else 0))
            )
        (let (
              (m1 (hanoi-cont (- n 1) s t d k h (- v a) (quotient a 2) ))
              (m2 (hanoi-cont (- n 1) t d s k h (+ v a) (quotient a 2) ))
              )
          (+ m1 (+

            (if (> v k)
            0
            (cond ((equal? s h) -1)
                  ((equal? d h) +1)
                  (else 0))
            )
            
            m2))
          
          )
        )
    )
  )


(define (hanoi-list n k h)
  (let ((m (expt 2 (- n 1))))
    (if (equal? h 1)
      (+ (hanoi-cont n 1 2 3 k h m (quotient m 2)) n)
      (hanoi-cont n 1 2 3 k h m (quotient m 2)))
    ))


(define (hanoi-disks n k)
  (list
   (list 1 (hanoi-list n k 1))
   (list 2 (hanoi-list n k 2))
   (list 3 (hanoi-list n k 3))
   )
  )