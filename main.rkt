#lang racket
(require graph)
(define eh-valido true)

; Verifica se um elemento x eh um programa PDL.
(define (programa? x)
  (cond [(eq? false (regexp-match #rx"[a-z]+" x)) false] [else true]))

; Verifica se um elemento x eh uma proposicao em PDL.
(define (proposicao? x)
  (cond [(eq? false (regexp-match #rx"[A-Z]+" x)) false] [else true]))

; Verifica se um objeto pertence a uma lista de elementos.
(define (pertence-a? obj elems)
  (cond [(empty? elems) false]
        [(empty? (filter (lambda (x) (equal? x obj)) elems)) false]
        [else true]))

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
  (define programas (filter (lambda (x) (programa? x)) elems))
  (define simbolos (filter (lambda (x) (not (pertence-a? x programas))) elems))
  (display "Simbolos: ")
  (writeln simbolos)
  (display "Programas: ")
  (writeln programas))

; Funcao que remove trechos inuteis numa string s que representa os mundos/estados do grafo.
(define (limpar-string s)
  (cond [(equal? s "") ""]
        [else (map (lambda (x) (string-split (string-trim (string-trim x ",") ")") ", "))
                   (string-split (string-replace (string-trim s "}") " = {" " ") " ("))]))

; Funcao que transforma em simbolos os elementos string contidos numa determinada lista l.
(define (simbolizar-elems l)
  (cond [(empty? l) l]
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
                     (display "Grafo Invalido")])
             ][else
               (set! eh-valido false)
               (display "Grafo Invalido")])
         ])
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
  (cond [eh-valido
         (display (graphviz grafo1))])
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

; Chamada a funcao 'main' que recebe do usuario os nomes dos arquivos (de programa e grafo PDL) a serem processados.
(main "")
