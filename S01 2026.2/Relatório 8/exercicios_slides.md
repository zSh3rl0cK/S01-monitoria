# Relatório 8 — POO com Python — Exercícios (2026.2)

## EXERCÍCIO 1 — A Chama de Izalith

**Cenário:** Em Dark Souls, todo morto-vivo carrega almas e um pouco de humanidade, que nunca passa de 99. Os piromantes, além disso, dominam uma chama própria.

**Tarefa:**
1. Crie a classe `MortoVivo` com o método `__init__`, que recebe e inicializa:
   - `nome` (público);
   - `_almas` (protegido, inteiro);
   - `__humanidade` (privado, inteiro).
2. Use o decorador `@property` para criar o getter e o setter de `humanidade`. O setter só aceita valores **de 0 a 99**. Fora dessa faixa, ele imprime `"Valor de humanidade inválido!"` e mantém o valor anterior.
3. Implemente o método `__str__` em `MortoVivo` (ex.: `"Morto-vivo Laurentius | Almas: 4500 | Humanidade: 3"`).
4. Crie a classe filha `Piromante`, que herda de `MortoVivo` e tem o atributo exclusivo `chama` (ex.: `"Bola de Fogo"`). Inicialize a parte herdada com `super().__init__(...)`.
5. Sobrescreva `__str__` em `Piromante`, reaproveitando o texto do pai com `super().__str__()` e acrescentando a chama.
6. No main:
   - crie um `Piromante` à sua escolha e imprima-o com `print`;
   - tente definir a humanidade como `150` e depois como `10`;
   - imprima o objeto de novo.

**Dica:** dentro de `Piromante`, `self.__humanidade` não funciona (o Python "embaralha" o nome). Use a property `self.humanidade`.

**Requisitos de Implementação (POO):**
- `__init__` e `self`
- Encapsulamento por convenção (`_` e `__`) e `@property` / `@<nome>.setter` com validação
- Herança com `super().__init__()` e sobrescrita de `__str__` com `super().__str__()`

---

## EXERCÍCIO 2 — Hora da Suprema

**Cenário:** Em Overwatch, cada herói carrega a sua suprema durante a partida. Ela só pode ser usada com 100% de carga, e o efeito muda conforme a função do herói.

**Tarefa:**
1. Crie a classe base `Heroi`, cujo `__init__` recebe `nome`, `funcao` e `carga_suprema` (inteiro de 0 a 100).
2. Crie na classe base o método `usar_suprema()`, que apenas executa `raise NotImplementedError`.
3. Crie as classes filhas `HeroiSuporte` e `HeroiDano`:
   - o `__init__` de cada uma recebe só `nome` e `carga_suprema`;
   - a `funcao` é passada fixa ao pai com `super().__init__(...)` (`"Suporte"` ou `"Dano"`).
4. Sobrescreva `usar_suprema()` nas duas filhas, imprimindo uma frase com o nome do herói e o efeito da sua função (ex.: `"Kiriko (Suporte): equipe curada e protegida!"`).
5. No main:
   - crie uma `list` com pelo menos três heróis das duas classes, sendo que **um** deles deve ter carga menor que 100;
   - percorra a lista com `for`. Se a carga for 100, chame `usar_suprema()`. Caso contrário, imprima `"<nome> ainda está carregando a suprema (<carga>%)"`.
6. Crie um objeto `Heroi` diretamente, chame `usar_suprema()`, observe o erro e comente a linha.

**Requisitos de Implementação (POO):**
- Herança com `super().__init__()`
- Método "abstrato" por convenção com `raise NotImplementedError`
- Sobrescrita e polimorfismo com `list` e `for`

---

## EXERCÍCIO 3 — Os Laços de Joker

**Cenário:** Em Persona 5, a Persona de Joker desperta de dentro dele e não existe sem o seu portador (composição). Já os confidentes são pessoas de Tóquio que existiam antes dele e continuam existindo sem ele (agregação).

**Tarefa:**
1. Crie a classe `Persona`, com `nome` e `arcano` e o método `invocar()`, que exibe os dois dados.
2. Crie a classe `Confidente`, com `nome`, `arcano` e `rank` (inteiro de 1 a 10).
3. Crie a classe `LadraoFantasma`:
   - o `__init__` recebe `codinome`, o nome e o arcano da Persona (como texto) e uma lista de confidentes com a anotação `list[Confidente]`;
   - **Composição:** a `Persona` deve ser criada **dentro** do `__init__`, a partir do nome e do arcano recebidos;
   - **Agregação:** a lista de confidentes recebida deve ser guardada em um atributo.
4. Em `LadraoFantasma`, crie os métodos:
   - `invocar_persona()`, que usa o método `invocar()` da Persona;
   - `exibir_confidentes()`, que percorre a lista e exibe cada confidente. Se o rank for 10, mostre `"MAX"` no lugar do número.
5. No main:
   - crie pelo menos dois confidentes **antes** do ladrão (ex.: Sojiro, Hierofante, rank 10; Takemi, Morte, rank 4);
   - crie Joker passando a lista e chame `invocar_persona()` e `exibir_confidentes()`;
   - apague Joker com `del` e imprima o nome de um confidente, mostrando que ele continua existindo.
6. Explique, em um comentário no código, o que é composição e o que é agregação no seu programa.

**Requisitos de Implementação (POO):**
- Composição (objeto criado no `__init__`) e Agregação (lista recebida por parâmetro)
- Anotação de tipo `list[Classe]`, laço `for` e `if`

---

## EXERCÍCIO 4 — Trabalho em Night City

**Cenário:** Em Night City, um fixer reúne edgerunners para um trabalho contra a Arasaka. Todo edgerunner segue o mesmo protocolo (executar o trabalho), mas cada um age do seu jeito. Os solos dependem de um cyberware instalado no próprio corpo.

**Tarefa:**
1. **A Interface Base:** crie a classe abstrata `Edgerunner` (herdando de `ABC`), com um `__init__` que recebe `nome` e o método abstrato `executar_trabalho()` (`@abstractmethod`, sem implementação).
2. **O Implante:** crie a classe `Cyberware`, com `nome` e `custo_humanidade` (inteiro).
3. **Herança e Composição:** crie a classe `Solo`, que herda de `Edgerunner`:
   - o `__init__` recebe `nome`, o nome do implante e o custo de humanidade;
   - o `Cyberware` deve ser criado **dentro** do `__init__` (composição);
   - `executar_trabalho()` exibe o nome do solo, o implante ativado e o custo de humanidade.
4. **Herança:** crie a classe `Netrunner`, que herda de `Edgerunner`. O `executar_trabalho()` dela exibe que o netrunner está invadindo a rede e quebrando o gelo (ICE) de um servidor.
5. **Agregação:** crie a classe `Fixer`, com:
   - `nome` e uma lista de `Edgerunner` recebida no `__init__`;
   - o método `contratar(mercenario)`, que adiciona alguém à lista;
   - o método `iniciar_trabalho()`, que percorre a lista chamando `executar_trabalho()` de cada membro.
6. No main:
   - crie um `Solo` e um `Netrunner` (ex.: David com Sandevistan; Lucy);
   - crie um `Fixer` (ex.: Rogue), passando os dois na lista;
   - contrate mais um edgerunner com `contratar()` e chame `iniciar_trabalho()`.
7. Tente criar um objeto `Edgerunner` diretamente, observe o erro e comente a linha. Em um comentário, compare com o erro do Exercício 2: quando cada um aparece?

**Requisitos de Implementação (POO) — revisão geral:**
- Interface com `ABC` e `@abstractmethod`
- Herança com `super().__init__()` e sobrescrita
- Composição, Agregação e polimorfismo com `for`
