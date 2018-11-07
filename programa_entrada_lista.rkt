#lang racket
(define programa (list 1 ";" 2 "*" ";" "(" 1 ";" 4 ")"))
;(display (first programa))
;(display (rest programa))
(define relacoes (list ))
(display relacoes)
(display "\n")
;(set! programa(append programa(list ";" 3)))

;(display programa)

(define (le_programa programa_entrada anterior anteriorS relacoes)
  (cond[(empty? programa_entrada)
        (display (rest relacoes))
        (display "\n")]
       [(number? (first programa_entrada))
        (set! relacoes(append relacoes(list (list anterior (first programa_entrada)))))
        (set! anterior (first programa_entrada))
        ;(le_programa (rest programa_entrada) anterior anteriorS relacoes)
       ]
       [else
        (cond[(equal? (first programa_entrada) "U") "deu ruim uniao"])
        (cond[(equal? (first programa_entrada) "*")
             (cond[(equal? anteriorS ")")
                   ;(le_programa (rest programa_entrada) anterior "*" relacoes)
                   (set! anteriorS (first programa_entrada))
             ]
             [else (set! relacoes(append relacoes(list(list anterior anterior))))
                   ;(le_programa (rest programa_entrada) anterior anteriorS relacoes)])
            ]
       )
        (set! anteriorS (first programa_entrada))
       
      ]
      
   )
    ]
       )

  (cond[(empty? (rest programa_entrada)) (rest relacoes)][else
  (le_programa (rest programa_entrada) anterior anteriorS relacoes)])
)

(le_programa programa ";" "" relacoes)
