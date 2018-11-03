#lang racket

; Verifica se um elemento x eh um programa PDL.
(define (eh-programa x)
  (cond [(eq? false (regexp-match #rx"[a-z]+" x)) false] [else true]))

; Verifica se um elemento x eh uma proposicao em PDL.
(define (eh-proposicao x)
  (cond [(eq? false (regexp-match #rx"[A-Z]+" x)) false] [else true]))

; Verifica se um objeto pertence a uma lista de elementos.
(define (pertence obj elems)
  (cond [(empty? elems) false]
        [(empty? (filter (lambda (x) (equal? x obj)) elems)) false]
        [else true]))

#| Recebe como entrada uma 'string' contendo todo o conteudo de um programa PDL lido.
   Separa cada elemento do programa PDL em suas categorias. |#
(define (parser nome-arq)
  (display "Arquivo: ")
  (writeln nome-arq)
  (define in (open-input-file nome-arq))
  (define prog (read-line in))
  (close-input-port in)
  (display "Conteudo: ")
  (writeln prog)
  (define elems (string-split prog " "))
  (define programas (filter (lambda (x) (eq? true (eh-programa x))) elems))
  (define simbolos (filter (lambda (x) (eq? false (pertence x programas))) elems))
  (display "Simbolos: ")
  (writeln simbolos)
  (display "Programas: ")
  (writeln programas))

; Pede ao usuario os nomes dos arquivos e chama as funcoes correspondentes para o processamento dos mesmos.
(define (main mensagem)
  (cond [(not (equal? "" mensagem)) (writeln mensagem)])
  (display "Insira o nome do arq. do programa PDL: ")
  (define nome-arq (read-line (current-input-port) 'any))
  (cond [(file-exists? nome-arq) (parser nome-arq)]
        [else (main "Arq. de prog. PDL inexistente!")]))

; Chamada a funcao 'main' que recebe do usuario os nomes dos arquivos (de programa e grafo PDL) a serem processados.
(main "")