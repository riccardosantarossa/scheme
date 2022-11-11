;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#lang racket
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname rapprBinario) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
;CONVERTE UN NUMERO BINARIO IN UN NUMERO INTERO IN BASE 10

(define bit-val ;val: 0/1
  (lambda (bit) ;bit: stringa che vale 0 oppure 1
    (if (string=? bit "0")
        0
        1
    )
  )
)

(define bin-val ;val: intero non negativo
  (lambda (bin) ;bin: stringa di 0 e 1
    (let ((n (string-length bin))
         )
      (if (= n 1)
          (bit-val bin)
          (+ (* 2 (bin-val (substring bin 0 (- n 1)))) (bit-val (substring bin (- n 1))))
       )
     )
  )
)
