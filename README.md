# vmPDL
Verificador de modelos para programas em "Propositional Dynamic Logic" (PDL).

## Procedimentos

Os procedimentos a seguir descrevem os passos necessários para execução do programa "vmPDL".

### Pré-requisitos

O programa foi testado nos sistemas operacionais Windows e Ubuntu 16.04 sem diferenças de execução entre ambos.
Para executar o programa é necessário possuir uma versão da linguagem Racket instalada. Recomenda-se:

* [Racket >= 7.0](https://racket-lang.org/download/) - O programa foi implementado e testado na versão [7.0](https://download.racket-lang.org/all-versions.html).

### Executando

O arquivo a ser executado para iniciar o programa encontra-se na raiz do projeto e chama-se 'main.rkt'.

Dentro do código-fonte deste arquivo, a linha a seguir em seu início define a entrada do programa PDL a ser avaliado:

```
(define programa (list 1 ";" 2 "*" ";" "(" 1 ";" 4 ")"))
```

Pode-se alterar esta lista para testar o programa com entradas de programa distintas. Exemplos de programas e de grafos serão tratados na seção seguinte.
De forma análoga à definição de um programa, um grafo também será definido a partir da alteração de uma linha do arquivo:

```
(define grafo (list 1 "(" "I" "a" ")" 3 "(" "b" "c" ")"  4 "(" "c" "d" ")" 5 "(" "d" "e" ")" 2 "(" "a" "b" ")" "X" "X" "X"))
```

Após efetuadas as alterações nas duas entradas, pode-se iniciar a execução do arquivo 'main.rkt'. O programa irá avaliar se o grafo descrito é modelo do programa PDL dado. Para executar o programa, a IDE Dr. Racket pode ser utilizada. A IDE acompanha a instalação da linguagem no computador. Considerando o sistema Ubuntu 16.04 e que a linguagem tenha sido instalada na pasta '/home', para executar o Dr. Racket basta digitar no Terminal:

```
~$ ./racket/bin/drracket
```
No menu "Ficheiro" do programa, seleciona-se a opção "Abrir". Uma janela pedindo pela escolha do arquivo a ser aberto será exibida. Deve-se escolher o arquivo 'main.rkt'. O arquivo será carregado, e, após feitas as alterações desejadas, pode-se executar o programa apertando o botão "Executar" no canto superior direito da janela da IDE Dr. Racket.

Informações como 'prints' serão exibidos na janela do console interatico do Racket, que está localizada na parte inferior da tela da IDE, abaixo do campo de edição de código.

Para entrada e grafo exemplififcados acima, o programa dar como saída o seguinte:

```
Programa -> (( 1 U 2 ) * ; ( 3 ; 4 ; 5 ))
Grafo -> (1 ( I a ) 3 ( b c ) 4 ( c d ) 5 ( d e ) 2 ( a b ) X X X)
Grafo eh modelo do programa? true
```

Indicando que o grafo é modelo do programa de entrada especificado.

### Exemplos

Nesta seção, exemplos de programas e grafos a serem dados como entrada serão dados e explicados.

* Exemplo 1: Bláblá

* Exemplo 2: Bláblá


## Autores

* **Luiz Felipe de Melo** - *Implementação, documentação.* - [lffloyd](https://github.com/lffloyd)
* **Vítor Costa** - *Implementação, documentação.* - [vitorhardoim](https://github.com/vitorhardoim)
* **Mariana Werneck** - *Implementação, documentação.* - [marianawerneck](https://github.com/marianawerneck)

Veja a lista de [contribuidores](https://github.com/lffloyd/vmPDL/contributors) participantes no projeto.

## Licença

Projeto licenciado sob a licença MIT - leia [LICENSE.md](https://github.com/lffloyd/vmPDL/blob/master/LICENSE) para maiores detalhes.
