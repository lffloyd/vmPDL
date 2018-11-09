#lang racket


#| -----------------------------------------------------Programa e grafo------------------------------------------------------ |#


;Recebe o programa.
(define programa (list"(" "|" 1 ";" 2 ")" "U" "(" "||" 3 ";" 4 ";" "E" ")"))
;Recebe o grafo.
(define grafo (list 1 "(" "I" "a" ")" 2 "(" "a" "b" ")"  3 "(" "I" "c" ")" 4 "(" "c" "d" ")" "X" "X" "X"))


#| ----------------------------------------------------------Funcoes---------------------------------------------------------- |#


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

; Cria a matriz de incidencia do grafo. 
(define (criar-matriz-grafo grafo gt grafof result)
  (define vf 0)
  (define final "")
  (set! vf (first grafo))
  (set! final (first(rest(rest(rest grafo)))))
  (set! result (append result(processar-grafo grafof gt vf 0 final "")))
  (cond [(equal? (first(rest(rest(rest(rest(rest grafo)))))) "X") result]
        [else (criar-matriz-grafo (rest(rest(rest(rest(rest grafo))))) gt grafof result)]))

; Inicia o processamento do grafo de entrada dado. 
(define (processar-grafo grafo gt vf vi final inicial)
  (cond[(empty? grafo) gt]
       [else (cond [(number? (first grafo)) (set! vi (first grafo))
                                      (set! inicial (first (rest(rest grafo))))
                      (cond[(equal? final inicial)
                            (set! gt (append gt (list(list vf vi))))])])
        (processar-grafo (rest grafo) gt vf vi final inicial)]))        

; Elimina da matriz de incidencia do grafo as ocorrencias do simbolo 'I'.
(define (eliminar-I mtz-prog resp)
  (cond[(empty? mtz-prog) resp]
       [else
        (cond[(equal? (first(first mtz-prog)) "I")]
             [else (set! resp(append resp(list (first mtz-prog))))])
        (eliminar-I (rest mtz-prog) resp)]))

; Elimina os pares iguais existentes na matriz do grafo. Recebe a matriz de incidencia do grafo.
(define (eliminar-iguais mtz-graf resp)
  (cond[(empty? mtz-graf) resp]
       [else
        (cond[(equal? (busca (first mtz-graf) resp 1) 0)]
             [else (set! resp(append resp(list (first mtz-graf))))])
        (eliminar-iguais (rest mtz-graf) resp)]))

; Verifica se um grafo eh modelo de um programa. Recebe as matrizes de incidencia correspondentes a ambos e as compara utilizando funcoes auxiliares.
(define (eh-modelo? mtz-prog mtz-graf)
  (cond[(equal? (length mtz-prog) (length mtz-graf))
        (eh-modelo-aux? mtz-prog mtz-graf)]
       [else "false"]))

; Busca todos os pares da matriz de incidencia do programa na matriz de incidencia do grafo. Usada para verificar se o grafo
; eh modelo do programa (usada pelas funcoes 'eh-modelo?').
(define (busca origem destino resp)
  (cond[(empty? destino) resp]
     [else
      (cond[(equal? (first origem) (first(first destino)))
            (cond[(equal? (first(rest origem)) (first(rest(first destino))))(set! resp 0)]
            )])
      (busca origem (rest destino) resp)]))

; Funcao auxiliar de eh-'modelo'?. Executa a funcao de busca para cada par da matriz do programa.
(define (eh-modelo-aux? mtz-prog mtz-graf)
  (cond[(empty? mtz-prog)
              "true"]
       [else (cond [(equal? (busca (first mtz-prog) mtz-graf 1) 0)              
                    (eh-modelo-aux? (rest mtz-prog) mtz-graf)]
                   [else "false"])]))


#| ---------------------------------------------------------Execucao---------------------------------------------------------- |#


; Inicializacao de variaveis do relacionada as relacoes do programa, grafo e suas matrizes correspondentes.
;Cria a lista de relacoes.
(define relacoes (list ))
(define gt (list ))
(define result (list ))
(set! relacoes(processar-programa programa "I" "" relacoes "I" "" "" '()))
(set! result (criar-matriz-grafo grafo gt grafo result))
(define mtz-programa (eliminar-I relacoes (list )))
(define mtz-grafo (eliminar-iguais result (list )))
(printf "Programa -> ~a\n" programa)
(printf "Grafo -> ~a\n" grafo)
(printf "Grafo eh modelo do programa? ~a\n" (eh-modelo? mtz-programa mtz-grafo))
