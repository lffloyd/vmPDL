#lang racket
;recebe o programa
(define programa (list 1 ";" 2 "*" ";" "(" 1 ";" 4 ")"))
;cria a lista de relacoes
(define relacoes (list ))


;le um programa e retorna todos os pares ordenados com as possiveis transições do programa
;anteriorS é o simbolo anterior
;anterior é o numero anterior
;relacoes eh a lista com as possiveis transicoes do programa
(define (le_programa programa_entrada anterior anteriorS relacoes)
  ;verifica se o item atual da lista é um numero, se for faz um par ordenado dele com o anterior e salva ele no anterior
  (cond[(number? (first programa_entrada))
        (set! relacoes(append relacoes(list (list anterior (first programa_entrada)))))
        (set! anterior (first programa_entrada))
       ]
       ;se nao for um numero
       [else
        ;verifica se eh o caracter uniao
        (cond[(equal? (first programa_entrada) "U") "deu ruim uniao"])
        ;verifica se eh o caracter *
        (cond[(equal? (first programa_entrada) "*")
              ;verifica se o caracter * vem depois de um ), se nao, adiciona uma transicao do anterior pra o anterior
             (cond[(equal? anteriorS ")") 
                   (set! anteriorS (first programa_entrada))
             ]
             [else (set! relacoes(append relacoes(list(list anterior anterior))))    
             ]
             )
             (set! anteriorS (first programa_entrada))
             ]
      
         )
       ]
   )
  ;verifica se o programa esta vazio, ou seja, foi todo percorrrido, se sim entao retorna, se nao chama a proxima instancia para a tail
  (cond[(empty? (rest programa_entrada))
        (rest relacoes)]
       [else
        (le_programa (rest programa_entrada) anterior anteriorS relacoes)])
)

;chamada do programa
(le_programa programa ";" "" relacoes)
