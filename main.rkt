#lang racket
(require graph)

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
  (define programas (filter (lambda (x) (eq? true (programa? x))) elems))
  (define simbolos (filter (lambda (x) (eq? false (pertence-a? x programas))) elems))
  (display "Simbolos: ")
  (writeln simbolos)
  (display "Programas: ")
  (writeln programas))

(define (pegar-elementos l)
  (cond [(not (equal? l "")) (map (lambda (x) (string-trim x ","))
                                  (string-split (string-replace (string-trim l "}") " = {" " ") " "))]
        [else ""]))

(define (limpar-string s)
  (cond [(equal? s "") ""]
        [else (map (lambda (x) (string-split (string-trim (string-trim x ",") ")") ", "))
                   (string-split (string-replace (string-trim s "}") " = {" " ") " ("))]))

(define (simbolizar-elems l)
  (cond [(empty? l) l]
        [else (cons (string->symbol (first l)) (simbolizar-elems (rest l)))]))

(define (pegar-tuplas l)
  (define l2 (limpar-string l))
  (cond [(equal? l "") ""]
        [else (rest (map (lambda (x) (cons (string->symbol (first (first l2))) (simbolizar-elems x))) l2))]))

(define (criar-relacoes in rels)
  (define l (read-line in))
  (cond [(eof-object? l) cons l rels]
        [else (cons (pegar-tuplas l) (criar-relacoes in rels))]))

(define (pegar-mundos m)
  (cond [(equal? m "") false] [else (regexp-match* #rx"[a-z0-9]+" (string-trim (string-trim m "}") "W = {"))]))

(define (criar-mundos l)
  (cond [(equal? l "") ""]
        [else (map (lambda (x) (string->symbol x)) (pegar-mundos l))]))

(define (criar-grafo nome-arq)
  (display "Arquivo de grafo: ")
  (writeln nome-arq)
  (define in (open-input-file nome-arq))
  (define mun (criar-mundos (read-line in)))
  (define relac (criar-relacoes in '()))
  (close-input-port in)
  (display "Mundos: ")
  (writeln mun)
  (display "Relacoes em alpha: ")
  (writeln relac))

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
