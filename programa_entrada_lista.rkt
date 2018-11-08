#lang racket
;Recebe o programa.
(define programa (list "(" 1 ";" 2 ")" "*" ";" "(" 3 ";" 4 ";" 5 ")"))
;Recebe o grafo.
(define grafo (list 1 "(" "I" "a" ")" 3 "(" "b" "c" ")"  4 "(" "c" "d" ")" 5 "(" "d" "e" ")" 2 "(" "a" "b" ")" "X" "X" "X"))
;Cria a lista de relacoes.
(define relacoes (list ))
(define gt (list ))
(define result (list ))

#| Le um programa e retorna todos os pares ordenados com as possiveis transições do programa. Onde:
* 'anteriorS' eh o simbolo anterior;
* 'anterior' eh o numero (programa) anterior;
* 'relacoes' eh a lista com as possiveis transicoes do programa;
* 'anteriorU' eh o simbolo ou programa anterior dentro da uniao atual;
* 'fimU1'/'fimU2' representam os finais de respectivamente primeiro e segundo termos da uniao atual;
* 'pares-uniao' representa uma lista com todos os pares associados por uma uniao (U). |#
(define (processar-programa entrada anterior anteriorS relacoes anteriorU fimU1 fimU2 pares-uniao)
  ;(printf "Ant: ~a - AntS: ~a - Rel.: ~a - AntU: ~a - FimU1: ~a - FimU2: ~a - Uniao: ~a\n" anterior anteriorS relacoes anteriorU fimU1 fimU2 pares-uniao)
  (define item (first entrada))
  ;Verifica se o item atual da lista é um numero, se for faz um par ordenado dele com o anterior e salva ele no anterior
  (cond [(number? item)
        (cond [(equal? anteriorS "||") (set! relacoes (append relacoes(list (list anteriorU item))))
                                       (set! anterior item)]
              [(equal? anteriorS "B") (set! relacoes (append relacoes(list (list fimU1 item))))
                                      (set! relacoes (append relacoes(list (list fimU2 item))))   
                                      ;(set! pares-uniao (append pares-uniao(list (list fimU1 item))))
                                      ;(set! pares-uniao (append pares-uniao(list (list fimU2 item))))
                                      (set! anterior item)]
              [else (set! relacoes (append relacoes(list (list anterior item))))
                    (set! anterior item)])]
       ;Se nao for um numero:
       [else
        ;verifica se eh o caracter uniao
        (cond [(equal? item "|") (set! anteriorU anterior)])
        (cond [(equal? item "U") (set! fimU1 anterior)])
        (cond [(equal? item "E") (set! fimU2 anterior)])
        ;verifica se eh o caracter *
        (cond [(equal? item "*")
              ;verifica se o caracter * vem depois de um ), se nao, adiciona uma transicao do anterior pra o anterior
             (cond [(equal? anteriorS ")") (set! anteriorS item)]
             [else (set! relacoes(append relacoes(list(list anterior anterior))))])])
        (set! anteriorS item)])
  ;verifica se o programa esta vazio, ou seja, foi todo percorrrido, se sim entao retorna, se nao chama a proxima instancia para a tail
  (cond[(empty? (rest entrada)) relacoes]
       [else (processar-programa (rest entrada) anterior anteriorS relacoes anteriorU fimU1 fimU2 pares-uniao)]))

(define (criar-matriz-grafo grafo gt grafof result)
  (define vf 0)
  (define final "")
  (set! vf (first grafo))
  (set! final (first(rest(rest(rest grafo)))))
  ;(display vf)
  ;(display final)
  ;(display "\n")
  (set! result (append result(processar-grafo grafof gt vf 0 final "")))
  (cond [(equal? (first(rest(rest(rest(rest(rest grafo)))))) "X") result]
        [else (criar-matriz-grafo (rest(rest(rest(rest(rest grafo))))) gt grafof result)]))

(define (processar-grafo grafo gt vf vi final inicial)
  (cond[(empty? grafo) gt]
       [else (cond [(number? (first grafo)) (set! vi (first grafo))
                                      (set! inicial (first (rest(rest grafo))))
                      ;(display "\n")
                      ;(display "vi")
                      ;(display vi)
                      ;(display inicial)
                      ;(display "\n")
                      (cond[(equal? final inicial)
                            (set! gt (append gt (list(list vf vi))))
                            ;(display gt)
                            ])])
        (processar-grafo (rest grafo) gt vf vi final inicial)]))        

(define (eliminar-I mtz-prog resp)
  (cond[(empty? mtz-prog) resp]
       [else
        (cond[(equal? (first(first mtz-prog)) "I")]
             [else (set! resp(append resp(list (first mtz-prog))))])
        (eliminar-I (rest mtz-prog) resp)]))

(define (eliminar-iguais mtz-graf resp)
  (cond[(empty? mtz-graf) resp]
       [else
        (cond[(equal? (busca (first mtz-graf) resp 1) 0)]
             [else (set! resp(append resp(list (first mtz-graf))))])
        (eliminar-iguais (rest mtz-graf) resp)]))

(define (eh-modelo? mtz-prog mtz-graf)
  (cond[(equal? (length mtz-prog) (length mtz-graf))
        (eh-modelo-aux? mtz-prog mtz-graf)]
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
      (busca origem (rest destino) resp)]))

(define (eh-modelo-aux? mtz-prog mtz-graf)
  (cond[(empty? mtz-prog)
              "true"]
       [else
        ;(display "\n")
        ;(display (busca (first mtz-prog) mtz-graf 1))
         ;     (display mtz-graf)
         ;(display "\n")
        (cond[(equal? (busca (first mtz-prog) mtz-graf 1) 0)              
              (eh-modelo-aux? (rest mtz-prog) mtz-graf)
              ][else "false"])]))


#|-----------------------------------------------------------------------------------------------------------------------------|#


(set! relacoes(processar-programa programa "I" "" relacoes "I" "" "" '()))
;(printf "Relacoes:-> ~a\n\n" relacoes)
(set! result (criar-matriz-grafo grafo gt grafo result))
(define mtz-programa (eliminar-I relacoes (list )))
(define mtz-grafo (eliminar-iguais result (list )))
(printf "Programa -> ~a\n" programa)
(printf "Grafo -> ~a\n" grafo)
(printf "Grafo eh modelo do programa? ~a\n" (eh-modelo? mtz-programa mtz-grafo))
