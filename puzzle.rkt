;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname puzzle) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))

; PROCEDURA PER DISEGNARE UNA CROCE REGOLARE
(glue-tiles (glue-tiles (shift-right (shift-down (half-turn larger-tile) 0.8) 1.6) (shift-down (shift-right (half-turn smaller-tile) 1.6) 4)) (glue-tiles larger-tile (shift-right smaller-tile 1.6)))

;PROCEDURA PER DISEGNARE UN QUADRATO INCLINATO
(glue-tiles (glue-tiles (shift-right  (half-turn larger-tile) 1.4) (shift-right(half-turn smaller-tile) 3)) (glue-tiles (shift-right(shift-down smaller-tile 4) 3) (shift-down  (shift-right  larger-tile 3) 0.8)))
 


 

