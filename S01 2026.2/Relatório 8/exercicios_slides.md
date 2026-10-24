# Relatório 8 — POO com Python — Exercícios (2026.2)

## EXERCÍCIO 1 — A Fogueira de Firelink

**Cenário:** Em Dark Souls, todo morto-vivo carrega almas e Frascos de Estus, que se recarregam nas fogueiras e nunca passam de 10. Os clérigos, além disso, conhecem um milagre próprio.

**Tarefa:**
1. Crie a classe `MortoVivo`, cujo `__init__` recebe e inicializa `nome` (público), `_almas` (protegido) e `__estus` (privado).
2. Crie os métodos `get_estus()` e `set_estus(quantidade)`. O setter só aceita valores **de 0 a 10**. Fora dessa faixa, ele exibe `"Quantidade de Estus inválida!"` e mantém o valor anterior.
3. Crie o método `mostrar_status()`, que **retorna** (não imprime) um texto com o nome, as almas e o Estus.
4. Crie a classe filha `Clerigo`, com o atributo exclusivo `milagre`. Inicialize a parte herdada com `super().__init__(...)` e sobrescreva `mostrar_status()`, reaproveitando o texto do pai com `super().mostrar_status()`.
5. No main, crie um `Clerigo` à sua escolha e exiba o status. Tente definir o Estus como `15` e depois como `10` e exiba o status de novo.
6. Tente imprimir o `__estus` do clérigo diretamente no main e observe o erro. Depois, transforme essa linha em comentário (`#`) para o programa voltar a rodar.

**Requisitos de Implementação (POO):**
- `__init__`, `self` e encapsulamento por convenção (`_` e `__`)
- Getter e setter com validação
- Herança com `super().__init__()` e sobrescrita com `super().metodo()`

---

## EXERCÍCIO 2 — Suprema em Overwatch

**Cenário:** Em Overwatch, todo herói pode usar a sua suprema, mas o efeito depende da função. Os suportes, além disso, conseguem curar a equipe.

**Tarefa:**
1. Crie a classe base `HeroiOverwatch`, cujo `__init__` recebe `codinome` e `funcao`, com o método `usar_suprema()`, que exibe uma suprema genérica.
2. Crie as classes filhas `HeroiTanque` e `HeroiSuporte`, **sem** `__init__` próprio, e sobrescreva `usar_suprema()` nas duas.
3. Em `HeroiSuporte`, crie também o método exclusivo `curar_equipe()`.
4. No main, crie uma lista `list[HeroiOverwatch]` com um herói de cada uma das três classes, incluindo um herói da classe base.
5. Percorra a lista com `for` chamando `usar_suprema()` e, usando `isinstance`, chame `curar_equipe()` apenas nos suportes.

**Requisitos de Implementação (POO):**
- Herança (com o `__init__` herdado do pai) e sobrescrita
- Polimorfismo com `list` e `for`
- `isinstance` para acessar um método exclusivo da filha

---

## EXERCÍCIO 3 — Infiltração no Palácio

**Cenário:** Em Persona 5, um grupo de estudantes chamado Phantom Thieves invade **Palácios**: mundos criados a partir do coração de pessoas corruptas, cada um conhecido pelo nome do seu dono. O líder do grupo, de codinome Joker, luta com a sua **Persona**, uma criatura que nasce de dentro dele e só existe enquanto ele existir (composição). Ao longo da história, ele recruta **aliados**: colegas que já existiam antes de entrar no grupo e continuam existindo fora dele (agregação).

**Glossário:**
- **Arcano:** categoria da Persona, inspirada nas cartas de tarô. No código, é só um texto.
- **Codinome:** apelido que cada membro usa durante as invasões, diferente do seu nome real.

**Tarefa:**
1. Crie a classe `Persona`, com `nome` e `arcano` e o método `invocar()`, que exibe os dois dados.
2. Crie a classe `Aliado`, com `nome` (nome real) e `codinome`.
3. Crie a classe `Lider`, que representa o Joker, com:
   - `codinome`;
   - uma `Persona` criada **dentro** do `__init__`, com nome e arcano à sua escolha;
   - uma lista `_equipe` que começa vazia.
4. Em `Lider`, crie os métodos:
   - `recrutar(aliado: Aliado)`, que adiciona o aliado à `_equipe`;
   - `infiltrar(palacio)`, que recebe o nome do dono do Palácio, exibe o Palácio invadido, invoca a Persona e lista a equipe.
5. No main, crie dois aliados **antes** do líder. Depois, crie o líder, recrute os dois e chame `infiltrar(...)`.
6. Explique, em um comentário no código, o que é composição e o que é agregação no seu programa.

**Requisitos de Implementação (POO):**
- Composição (objeto criado no `__init__`) e Agregação (objeto recebido por parâmetro)
- Lista com `append` e `for`

---

## EXERCÍCIO 4 — Ataque em Night City

**Cenário:** Em Cyberpunk, Night City é uma metrópole futurista controlada por grandes corporações. Para atacá-las, hackers se organizam em **células**, grupos que coordenam diferentes **unidades de rede** em um mesmo ataque. Toda unidade segue o mesmo protocolo: precisa saber executar uma invasão. Mas cada tipo de unidade faz isso do seu jeito. O **netrunner** usa o seu **cyberdeck** para quebrar o **gelo (ICE)** de um servidor. Já o **drone de vigilância** intercepta o sinal da rede.

**Glossário:**
- **Netrunner:** hacker que se conecta diretamente à rede para invadir sistemas.
- **Cyberdeck:** equipamento que o netrunner usa para hackear. Ele é implantado no corpo do netrunner, por isso não existe sem ele (composição).
- **Gelo (ICE):** barreira de segurança de um servidor, parecida com um firewall.
- **Célula hacker:** grupo que reúne unidades que já existiam antes de entrar nele (agregação).

**Tarefa:**
1. **Interface:** crie a classe abstrata `IUnidadeDeRede` (herdando de `ABC`), com o método abstrato `executar_invasao()`.
2. **Parte composta:** crie a classe `Cyberdeck`, com o atributo `modelo`.
3. **Implementação com composição:** crie a classe `OperadorNetrunner`, que implementa `IUnidadeDeRede`:
   - o `__init__` recebe `nome` e o modelo do cyberdeck, e cria o `Cyberdeck` **dentro** dele;
   - `executar_invasao()` exibe o nome do netrunner, o modelo do cyberdeck e que ele está quebrando o gelo (ICE) de um servidor.
4. **Outra implementação:** crie a classe `DroneDeVigilancia`, que também implementa `IUnidadeDeRede`:
   - o `__init__` recebe `codigo`, a identificação do drone;
   - `executar_invasao()` exibe o código do drone e que ele está interceptando o sinal da rede.
5. **Agregação:** crie a classe `CelulaHacker`:
   - o `__init__` recebe `nome` e uma lista de membros já criados, anotada como `list[IUnidadeDeRede]`;
   - `iniciar_ataque()` exibe o nome da célula e chama `executar_invasao()` de cada membro (polimorfismo).
6. No main, crie um netrunner e um drone, junte os dois em uma célula e chame `iniciar_ataque()`.
7. Tente criar um objeto `IUnidadeDeRede` diretamente e observe o erro. Depois, transforme essa linha em comentário (`#`) para o programa voltar a rodar.

**Requisitos de Implementação (POO) — revisão geral:**
- Interface com `ABC` e `@abstractmethod`
- Composição, Agregação e polimorfismo com `for`
