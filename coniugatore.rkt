;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname coniugatore) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define Desinenza
  (lambda (s)
    (substring s (- (string-length s ) 3))
    )
 )

(define ParticipioAre     ;stringa
  (lambda (s)
      (string-append (substring s 0 (- (string-length s ) 3)) "ato")  ;sostituisce la coniugazione con il participio
    )
)

(define ParticipioEre     ;stringa
  (lambda (s)
      (string-append (substring s 0 (- (string-length s ) 3)) "uto")  ;sostituisce la coniugazione con il participio
    )
)

(define ParticipioIre     ;stringa
  (lambda (s)
      (string-append (substring s 0 (- (string-length s ) 3)) "ito")  ;sostituisce la coniugazione con il participio
    )
)

(define Coniuga           ;stringa
  (lambda (s)
    (cond
      [(equal? (Desinenza s)  "are") (ParticipioAre s)]
      [(equal? (Desinenza s)  "ere") (ParticipioEre s)]
      [(equal? (Desinenza s)  "ire") (ParticipioIre s)]
     ) 
   )
)