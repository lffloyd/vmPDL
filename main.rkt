#lang racket
(require graph)
(define eh-valido true)
(define no-atual 'x1)
(define entrada-programa(list 1 2 3 2 2))
(define transicao-possivel '())
(define matriz-grafo '())
;(set! entrada-programa (append entrada-programa (list (list 5 6))))
(define matriz-programa '())

;Recebe o programa.
(define programa (list 0 ";" "(" "|"1 ";" 2 ")" "U" "(" "||" 3 ";" 4 "E" ")" ";" "B" 5  ))
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


;Dado um grafo e a lista de programas, nós executamos esses programas sequencialmente.
(define (executa-programas grafo entrada-programa)
  (cond [(empty? entrada-programa) null]
        [else
         (define programa-atual (first entrada-programa))
         (cond [(number? programa-atual) (executa-programa-simples grafo programa-atual)])
         (executa-programas grafo (rest entrada-programa))]
  )
)
;Para uma transição simples, a partir do no atual e um programa, a função leva o nó atual para o primeiro caminho possivel com base no programa dado.
(define(executa-programa-simples grafo programa)
  (teste-vizinhos grafo programa (get-neighbors grafo no-atual)))

;Percorre todos os vizinhos do no para ver qual deles é o que tem o 'peso' do programa dado, e retorna uma lista com o par ordenado da transição.
(define (teste-vizinhos grafo programa vizinhos)
  (cond [(empty? vizinhos) transicao-possivel]
        [else
         (cond[(equal? programa (edge-weight grafo no-atual (first vizinhos)))
               (define no-aux no-atual)
               (set! no-atual (first vizinhos))
               (set! transicao-possivel (append transicao-possivel (list(list no-aux (first vizinhos)))))
               transicao-possivel]
              [else
               (teste-vizinhos grafo programa (rest vizinhos))])]))

; Verifica se um elemento x eh um programa PDL.
(define (programa? x)
  (cond [(eq? false (regexp-match #rx"[a-z0-9]+" x)) false] [else true]))

; Verifica se um elemento x eh uma proposicao em PDL.
(define (proposicao? x)
  (cond [(eq? false (regexp-match #rx"[A-Z]+" x)) false] [else true]))

; Verifica se um objeto pertence a uma lista de elementos.
(define (pertence-a? obj elems)(
  (cond [(empty? elems) false]
        [(empty? (filter (lambda (x) (equal? x obj)) elems)) false]
        [else true]))
)

#| Recebe como entrada uma 'string' contendo todo o conteudo de um programa PDL lido.
   Separa cada elemento do programa PDL em suas categorias. Funcionalidade ainda nao esta pronta.|#
(define (parser nome-arq)
  (display "Arquivo: ")
  (writeln nome-arq)
  (define in (open-input-file nome-arq))
  (define prog (read-line in))
  (close-input-port in)
  (display "Conteudo: ")
  (writeln prog)
  (define elems (string-split prog " "))
  ;(define programas (filter (lambda (x) (programa? x)) elems))
  (display "Elementos: ")
  (writeln elems)
  ;(define prog-simb (simbolizar-elems elems))
  ;(display "Entrada simbolizada: ")
  ;(display prog-simb)
  ;(processar-programa prog-simb)
  ;(define trechos (pegar-trechos prog-simb (cons (string->symbol "(") '())))
  ;(display "Trechos: ")
  ;(writeln trechos)
  )

; Funcao que remove trechos inuteis numa string s que representa os mundos/estados do grafo.
(define (limpar-string s)
  (cond [(equal? s "") ""]
        [else (map (lambda (x) (string-split (string-trim (string-trim x ",") ")") ", "))
                   (string-split (string-replace (string-trim s "}") " = {" " ") " ("))]))

; Funcao que transforma em simbolos os elementos string contidos numa determinada lista l.
(define (simbolizar-elems l)
  (cond [(empty? l) l]
        [(list? (first l)) (cons (simbolizar-elems (first l)) (simbolizar-elems (rest l)))]
        [else (cons (string->symbol (first l)) (simbolizar-elems (rest l)))]))

; Funcao que junta duas listas.
(define (juntar l1 l2)
  (cond [(empty? l1) l2]
        [else (cons (first l1) (juntar (rest l1) l2))]))

; A partir de uma lista l, retorna as tuplas internas a mesma que representam relacoes binarias associado programas a estados.
(define (pegar-tuplas l)
  (define l2 (limpar-string l))
  (cond [(equal? l "") ""]
        [else (rest (map (lambda (x) (cons (string->symbol (first (first l2))) (simbolizar-elems x))) l2))]))

; Funcao que cria uma lista de relacoes entre programas e estados a partir de um dado arquivo aberto in.
(define (criar-relacoes in rels)
  (define l (read-line in))
  (cond [(eof-object? l) cons l rels]
        [else (juntar (pegar-tuplas l) (criar-relacoes in rels))]))

; Funcao que a partir de uma string m descobre os mundos existentes na mesma. Usada pela funcao 'criar-mundos' a seguir.
(define (pegar-mundos m)
  (cond [(equal? m "") false] [else (regexp-match* #rx"[a-z0-9]+" (string-trim (string-trim m "}") "W = {"))]))

; Funcao que cria o conjunto de mundos/estados possiveis de um grafo a partir de uma string l contendo-os. Retorna uma lista com
; a simbolizacao de cada mundo.
(define (criar-mundos l)
  (cond [(equal? l "") ""]
        [else (map (lambda (x) (string->symbol x)) (pegar-mundos l))]))

; Conerte um simbolo em numero.
(define (symbol->number sym)
  (string->number (symbol->string sym)))

; Gera um grafo a partir de uma dada lista.
(define (gerar-grafo lista grafo mundo)
  (cond [(empty? lista) null]
        [else
             (cond [(member (first(rest(first lista))) mundo)
                   (cond [(member (first(rest(rest(first lista)))) mundo)
                         (add-directed-edge! grafo (first(rest(first lista))) (first(rest(rest(first lista)))) (symbol->number (first(first lista))))
                         (gerar-grafo (rest lista) grafo mundo)
                   ][else
                     (set! eh-valido false)
                     (display "Grafo invalido!")])
             ][else
               (set! eh-valido false)
               (display "Grafo invalido!")])
         ])
)

(define (testa-matriz mundo lista-transicoes)
  (cond[(empty? mundo) null]
       [else
        (cond[(member (first mundo) (first lista-transicoes))
              testa-matriz (rest mundo) (lista-transicoes)]
             [else
              null
              ]
)])

        
)


; Funcao que cria um grafo a partir do nome de arq. de grafo passado por parametro. Cria mundos/estados e relacoes entre os mesmos.
(define (criar-grafo nome-arq)
  (display "Arquivo de grafo: ")
  (writeln nome-arq)
  (define in (open-input-file nome-arq))
  (define mun (criar-mundos (read-line in)))
  (define relac (criar-relacoes in '()))
  (define grafo1 (weighted-graph/directed '()))
  (close-input-port in)
  (display "Mundos: ")
  (writeln mun)
  (display "Relacoes em alpha: ")
  (writeln relac)
  (gerar-grafo relac grafo1 mun)
  (executa-programas grafo1 entrada-programa)
  ;(testa-matriz mundo transicao-possivel)
  (cond [eh-valido
         (display (graphviz grafo1))])
  (display transicao-possivel)
)

; Pede ao usuario os nomes dos arquivos e chama as funcoes correspondentes para o processamento dos mesmos.
(define (main mensagem)
  (cond [(not (equal? "" mensagem)) (writeln mensagem)])
  (display "Insira o nome do arq. do programa PDL: ")
  (define nome-pro (read-line (current-input-port) 'any))
  (cond [(file-exists? nome-pro) (parser nome-pro)]
        [else (main "Arq. de prog. PDL inexistente!")])
  (display "Insira o nome do arq. do grafo PDL: ")
  (define nome-graf (read-line (current-input-port) 'any))
  (cond [(file-exists? nome-graf) (criar-grafo nome-graf)]
        [else (main "Arq. de graf. PDL inexistente!")]))


#|--------------------------------------------------------MAIN---------------------------------------------------------------|#


;chamada do programa
(le_programa programa "I" "" relacoes "I" "" "")
; Chamada a funcao 'main' que recebe do usuario os nomes dos arquivos (de programa e grafo PDL) a serem processados.
(main "")
