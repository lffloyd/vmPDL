#lang racket
;Recebe o programa.
(define programa (list "(" 1 ";" 2 ")" "*" ";" "(" 3 ";" 4 ";" 5 ")"))
;Cria a lista de relacoes.
(define relacoes (list ))

#| Le um programa e retorna todos os pares ordenados com as possiveis transições do programa. Onde:
* 'anteriorS' é o simbolo anterior;
* 'anterior' é o numero anterior;
* 'relacoes' eh a lista com as possiveis transicoes do programa. |#
(define (le_programa programa_entrada anterior anteriorS relacoes anteriorU fimU1 fimU2)
  ;verifica se o item atual da lista é um numero, se for faz um par ordenado dele com o anterior e salva ele no anterior
  (cond[(number? (first programa_entrada))
        (cond[(equal? anteriorS "||")
              (set! relacoes(append relacoes(list (list anteriorU (first programa_entrada)))))
              (set! anterior (first programa_entrada))]
             [(equal? anteriorS "B")
              (set! relacoes(append relacoes(list (list fimU1 (first programa_entrada)))))
              (set! relacoes(append relacoes(list (list fimU2 (first programa_entrada)))))
              (set! anterior (first programa_entrada))
              ]
        [else
        (set! relacoes(append relacoes(list (list anterior (first programa_entrada)))))
        (set! anterior (first programa_entrada))])
       ]
       ;se nao for um numero
       [else
        ;verifica se eh o caracter uniao
        (cond[(equal? (first programa_entrada) "|")
              (set! anteriorU anterior)])
        ;
        (cond[(equal? (first programa_entrada) "U")
              (set! fimU1 anterior)])
        ;
        (cond[(equal? (first programa_entrada) "E")
              (set! fimU2 anterior)])
        ;verifica se eh o caracter *
        (cond[(equal? (first programa_entrada) "*")
              ;verifica se o caracter * vem depois de um ), se nao, adiciona uma transicao do anterior pra o anterior
             (cond [(equal? anteriorS ")") 
                   (set! anteriorS (first programa_entrada))]
             [else (set! relacoes(append relacoes(list(list anterior anterior))))])
             ])
        (set! anteriorS (first programa_entrada))])
  ;verifica se o programa esta vazio, ou seja, foi todo percorrrido, se sim entao retorna, se nao chama a proxima instancia para a tail
  (cond[(empty? (rest programa_entrada))
        relacoes]
       [else (le_programa (rest programa_entrada) anterior anteriorS relacoes anteriorU fimU1 fimU2)]))

(set! relacoes(le_programa programa "I" "" relacoes "I" "" ""))

(define grafo (list 1 "(" "I" "a" ")" 3 "(" "b" "c" ")"  4 "(" "c" "d" ")" 5 "(" "d" "e" ")" 2 "(" "a" "b" ")" "X" "X" "X"))
(define gt (list ))
(define result (list ))

(define (f grafo gt grafof result)
  (define vf 0)
  (define final "")
  (set! vf (first grafo))
  (set! final (first(rest(rest(rest grafo)))))
  ;(display vf)
  ;(display final)
  ;(display "\n")
  (set! result(append result(le_grafo grafof gt vf 0 final "")))
  (cond[(equal? (first(rest(rest(rest(rest(rest grafo)))))) "X")
        result]
       [else (f (rest(rest(rest(rest(rest grafo))))) gt grafof result)]
  ))

;(first(rest(rest(rest(rest(rest grafo)))))) first(rest(rest(rest(rest(rest(rest(rest(rest grafo)))))))))

(define (le_grafo grafo gt vf vi final inicial)
  (cond[(empty? grafo) gt]
       [else
        (cond[(number? (first grafo))
                      (set! vi (first grafo))
                      (set! inicial (first (rest(rest grafo))))
                      ;(display "\n")
                      ;(display "vi")
                      ;(display vi)
                      ;(display inicial)
                      ;(display "\n")
                      (cond[(equal? final inicial)
                            (set! gt(append gt(list(list vf vi))))
                            ;(display gt)
                            ])
                      

                      ])
        (le_grafo (rest grafo) gt vf vi final inicial)
        ;(le_grafo (rest grafo) gt 0 vi "" inicial)
        ]))
                     
        
(define (eliminateI relacoes resp)
  (cond[(empty? relacoes) resp]
       [else
        (cond[(equal? (first(first relacoes)) "I")]
             [else (set! resp(append resp(list (first relacoes))))]

  )
        (eliminateI (rest relacoes) resp)]))


(define (eliminateEqual result resp)
(cond[(empty? result) resp]
       [else
        (cond[(equal? (busca (first result) resp 1) 0)]
             [else (set! resp(append resp(list (first result))))]

  )
        (eliminateEqual (rest result) resp)])
  )


(define (ehValido1 relacoes result)
  (cond[(equal? (length relacoes) (length result))
        (ehValido relacoes result)]
       [else "false"]))

(define (busca origem destino resp)
(cond[(empty? destino) resp]
     [else
      ;(display "\n")
      ;(display (first origem))
      ;(display (first(first destino)))
      ;(display "\n")
      (cond[(equal? (first origem) (first(first destino)))
            ;(display "\n")
      ;(display (first(rest origem)))
      ;(display (first(rest(first destino))))
      ;(display "\n")
            (cond[(equal? (first(rest origem)) (first(rest(first destino))))(set! resp 0)]
            )])
      (busca origem (rest destino) resp)])
  )

(define (ehValido relacoes result)
  (cond[(empty? relacoes)
              "true"]
             
       
       [else
        ;(display "\n")
        ;(display (busca (first relacoes) result 1))
         ;     (display result)
         ;(display "\n")
        (cond[(equal? (busca (first relacoes) result 1) 0)
              
              (ehValido (rest relacoes) result)
              ][else "false"])

        ]))
        

(set! result (f grafo gt grafo result))
(set! relacoes (eliminateI relacoes (list )))
(set! result (eliminateEqual result (list )))
;(display relacoes)
;(display "\n")
;(display result)
(display (ehValido1 relacoes result))