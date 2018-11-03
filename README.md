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

Ao ser iniciado, o programa exibirá um 'prompt' para inserção do nome do arquivo de programa PDL a ser usado:

```
Insira o nome do arq. do programa PDL:
```

Para o qual deve-se digitar o nome do arquivo a ser usado como tal. Em seguida, será requisitado o arquivo do grafo ou 'frame' PDL que será analisado em relação ao programa PDL especificado logo acima. Novamente, um 'prompt' aguarda a digitação do nome do arquivo. Reparar que a inserção de nomes de arquivo inválidos exibirá um erro e os nomes corretos serão pedidos.

## Autores

* **Luiz Felipe de Melo** - *Implementação, documentação.* - [lffloyd](https://github.com/lffloyd)
* **Vítor Costa** - *Implementação, documentação.* - [vitorhardoim](https://github.com/vitorhardoim)
* **Mariana Werneck** - *Implementação, documentação.* - [marianawerneck](https://github.com/marianawerneck)

Veja a lista de [contribuidores](https://github.com/lffloyd/vmPDL/contributors) participantes no projeto.

## Licença

Projeto licenciado sob a licença MIT - leia [LICENSE.md](https://github.com/lffloyd/vmPDL/blob/master/LICENSE) para maiores detalhes.
