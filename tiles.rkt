;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname tiles) (read-case-sensitive #t) (teachpacks ((lib "drawings.ss" "installed-teachpacks"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "drawings.ss" "installed-teachpacks")) #f)))
;TASSELLLAZIONE CON SEMPLICE PIASTRELLA A L

;Funzione per la tassellazione generale
(define L-tessellation ;val: grafico
  (lambda (n) ;n: intero
    (disegno n 2 pattern)
   )
)

;Figura base
(define pattern
  (glue-tiles
   L-tile (glue-tiles
           (glue-tiles
            (shift-down (quarter-turn-left L-tile) 1)
            (shift-right (quarter-turn-right L-tile) 1)
            )
            (shift-right (shift-down L-tile 0.5) 0.5)
           )
   )
)

;Funzione che tratta i casi base e anche la ricorsione 
(define disegno      ;val: grafico
  (lambda (n k figura) ;n, k: interi, pattern: figura da scalare
        (cond ((= n 1)
           L-tile)
          ((= n 2)
            pattern)
          (else
           (if (= (/ n k) 2)
             (glue-tiles
              figura (glue-tiles
                      (glue-tiles
                       (shift-down (quarter-turn-left figura) k)
                       (shift-right (quarter-turn-right figura) k)
                       )
                      (shift-right (shift-down figura (/ k 2) ) (/ k 2))
                      )
              )
             (disegno n (* k 2) (glue-tiles
              figura (glue-tiles
                      (glue-tiles
                       (shift-down (quarter-turn-left figura) k)
                       (shift-right (quarter-turn-right figura) k)
                       )
                      (shift-right (shift-down figura (/ k 2) ) (/ k 2))
                      )))
            )
          )
    )

  )
  
)



