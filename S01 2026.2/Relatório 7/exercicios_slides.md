# Relatório 7 — POO com C# — Exercícios (2026.2)

## EXERCÍCIO 1 — Defesa de Minas Tirith

**Cenário:** Em O Senhor dos Anéis, durante o cerco a Minas Tirith, cada combatente convocado precisa ser registrado com seu nome, seu povo, seu posto e o número do Círculo da cidade (de 1 a 7) que irá defender. Alguns chegam equipados, outros ainda aguardam armamento.

**Tarefa:**
1. Crie a classe `CombatenteDeGondor`.
2. Defina as propriedades `Nome`, `Povo` e `Posto` (string) e `Circulo` (int). Todas podem ser lidas de fora da classe, mas só podem ser alteradas pela própria classe (`private set`).
3. Crie a propriedade `Armamento`, também com `private set`, com valor inicial `"Desarmado"`.
4. Crie um Construtor que obrigue a inicialização de nome, povo, posto e círculo e que, ao final, imprima: `[Convocação] {Nome} foi convocado para o Círculo {Circulo}.`
5. Crie o método `Equipar(string arma)`, que é a única forma de alterar o `Armamento`.
6. Crie o método `ApresentarUnidade()`, que imprime nome, povo, posto e círculo. A linha do armamento só deve ser impressa se o `Armamento` for diferente de `"Desarmado"`.
7. Na Main, crie: Legolas (Elfo, Arqueiro, Círculo 1), equipado com o "Arco dos Galadhrim"; Peregrin Took (Hobbit, Guarda da Cidadela, Círculo 7), sem armamento; e mais um combatente à sua escolha.
8. Chame `ApresentarUnidade()` em todos os objetos.
9. Tente alterar o `Posto` de um combatente diretamente na Main (ex.: `legolas.Posto = "Rei";`), observe o erro de compilação e depois comente essa linha.

**Requisitos de Implementação (POO):**
- Classe e objetos (instanciação com `new`)
- Encapsulamento com propriedades `{ get; private set; }`
- Valor inicial de propriedade (`= "Desarmado"`)
- Construtor com `this`
- Método com parâmetro alterando o estado interno do objeto
- Condicional `if` com `!=` e interpolação de strings (`$"..."`)

---

## EXERCÍCIO 2 — Batalha de Exibição Pokémon

**Cenário:** No mundo Pokémon, durante uma batalha de exibição, cada Pokémon entra em campo, anuncia sua espécie e seu nível e executa um golpe. Pokémon do Tipo Planta usam um golpe especial próprio; Pokémon do Tipo Elétrico primeiro atacam normalmente e depois liberam uma descarga elétrica. Pokémon sem tipo definido usam apenas um ataque genérico.

**Tarefa:**
1. Crie a classe base `Pokemon` com as propriedades `Especie` (string, `{ get; set; }`) e `Nivel` (int, `private set`), ambas recebidas no construtor.
2. Crie o método virtual `EntrarEmCampo()`, que imprime `--- {Especie} (Nv. {Nivel}) entra em campo! ---` e, em seguida, `Ataca com Investida.`
3. Crie a classe `TipoPlanta`, que herda de `Pokemon`, com a propriedade `GolpeEspecial` (string). O construtor recebe espécie, nível e golpe e repassa espécie e nível com `: base(...)`.
4. Em `TipoPlanta`, sobrescreva `EntrarEmCampo()` **sem** chamar o método do pai: imprima o cabeçalho de entrada e a mensagem do golpe especial (ex.: `"Sceptile lança Folha Navalha! Folhas cortantes cruzam o campo."`).
5. Crie a classe `TipoEletrico`, que herda de `Pokemon`, com a propriedade `Voltagem` (int, `private set`).
6. Em `TipoEletrico`, sobrescreva `EntrarEmCampo()` chamando **primeiro** `base.EntrarEmCampo()` e depois imprimindo a descarga (ex.: `"E libera uma descarga de 10000 volts!"`).
7. Na Main, crie uma `List<Pokemon>` e adicione: um Sceptile (TipoPlanta, nível 50, "Folha Navalha"), um Jolteon (TipoEletrico, nível 45, 10000 volts) e um Eevee (`Pokemon` genérico, nível 15).
8. Imprima a quantidade de Pokémon em campo usando `Count` e percorra a lista com `foreach`, chamando `EntrarEmCampo()` de cada um, demonstrando o comportamento polimórfico.

**Requisitos de Implementação (POO):**
- Herança com repasse ao construtor do pai (`: base(...)`)
- Método `virtual` na classe base e `override` nas classes filhas
- Reaproveitamento do comportamento do pai com `base.Metodo()`
- Encapsulamento com `private set`
- Polimorfismo com `List<ClasseMae>`, `foreach` e `Count`

---

## EXERCÍCIO 3 — O Grimório de Frieren

**Cenário:** A elfa Frieren carrega um Grimório onde registra cada feitiço que aprende. O grimório nasce com ela e não faz sentido existir sem sua dona (composição). Ao longo da jornada, ela viaja com companheiros, como Fern e Stark, que já existiam antes de se juntar ao grupo e continuam existindo por conta própria (agregação).

**Tarefa:**
1. Crie a classe `Feitico` com a propriedade `Nome` (recebida no construtor) e o método `Conjurar()`, que imprime `Conjurando {Nome}...`
2. Crie a classe `Grimorio` com uma lista privada `List<Feitico>`, criada no construtor. Ela deve ter:
   - o método `Registrar(string nomeFeitico)`, que cria o `Feitico` **dentro** do próprio método e o adiciona à lista (o feitiço nasce dentro do grimório);
   - o método `ListarFeiticos()`, que imprime a quantidade de feitiços (`Count`) e chama `Conjurar()` em cada um.
3. Crie a classe `Companheiro` com as propriedades `Nome` e `Funcao` (recebidas no construtor) e o método `Apresentar()`.
4. Crie a classe `Maga`:
   - Deve possuir `Nome` e a propriedade `Grimorio` (`private set`), instanciada dentro do construtor (Composição).
   - Deve possuir uma lista privada de `Companheiro`, criada no construtor, e o método `RecrutarCompanheiro(Companheiro c)`, que recebe um companheiro criado fora da classe (Agregação).
   - Deve possuir o método `MostrarGrupo()`, que imprime quantos companheiros ela possui e chama `Apresentar()` de cada um.
5. Na Main:
   - Instancie Fern ("Maga Aprendiz") e Stark ("Guerreiro") **antes** de criar a Maga.
   - Instancie a Maga Frieren e recrute os dois companheiros.
   - Registre três feitiços pelo grimório de Frieren (ex.: "Zoltraak", "Magia para fazer flores brotarem", "Magia para limpar estátuas de bronze").
   - Chame `MostrarGrupo()` e `Grimorio.ListarFeiticos()`.
   - Por fim, chame `Apresentar()` diretamente no objeto de Stark, mostrando que ele existe fora da Maga.
6. Responda em um comentário no código: se a Maga deixasse de existir, o que aconteceria com o Grimório? E com Fern e Stark?

**Requisitos de Implementação (POO):**
- Composição: objeto criado dentro do construtor do dono (`Grimorio` em `Maga`) e objetos criados dentro de um método do dono (`Feitico` em `Grimorio`)
- Agregação: objeto criado fora e recebido por parâmetro (`Companheiro`)
- Campos privados com `List<T>`, `Add`, `Count` e `foreach`
- Construtores e encapsulamento com `private set`

---

## EXERCÍCIO 4 — Arquivos Proibidos da Miskatonic

**Cenário:** Nas profundezas da Biblioteca da Universidade Miskatonic, o bibliotecário Henry Armitage organiza um catálogo de relatos sobre entidades cósmicas. Cada entidade é registrada com um nome e, quando conhecida, sua origem. Algumas, como os Profundos, emergem dos oceanos; outras, como os Mi-Go, cruzam o espaço vindas de Yuggoth; e há relatos tão vagos que só podem ser catalogados de forma genérica.

**Tarefa:**
1. Crie a classe base (**não** abstrata) `EntidadeCosmica`:
   - Propriedades `Nome` (recebida no construtor) e `Origem`, com valor inicial `"Desconhecida"`.
   - O construtor deve imprimir `[Registro] Entidade '{Nome}' adicionada aos arquivos.`
   - Método virtual `Manifestar()`, que imprime o nome, a mensagem `"Uma presença indescritível é sentida."` e, **somente se** a `Origem` for diferente de `"Desconhecida"`, imprime a origem.
2. Crie a classe `Profundo`, que herda de `EntidadeCosmica`, com a propriedade `Profundidade` (int, `private set`). Sobrescreva `Manifestar()` **sem** chamar o pai (ex.: `"Emerge das águas a 300 metros de profundidade, próximo a Innsmouth."`).
3. Crie a classe `MiGo`, que herda de `EntidadeCosmica`, com a propriedade `Artefato` (string). Sobrescreva `Manifestar()` chamando **primeiro** `base.Manifestar()` e depois descrevendo a criatura (ex.: `"Asas membranosas zumbem enquanto carrega um cilindro metálico."`).
4. Crie a classe `Pesquisador`:
   - Propriedade `Nome` e uma lista privada `List<EntidadeCosmica>`, criada no construtor (Composição).
   - Método `Catalogar(EntidadeCosmica e)`, que recebe entidades criadas fora da classe (Agregação).
   - Método `LerCatalogo()`, que imprime quantos registros existem e chama `Manifestar()` de cada entidade.
5. No método Main:
   - Instancie um `Profundo` ("Profundo de Innsmouth", 300 metros).
   - Instancie um `MiGo` e altere a sua `Origem` para `"Yuggoth"`.
   - Instancie uma `EntidadeCosmica` genérica ("A Cor que Caiu do Espaço"), mantendo a origem desconhecida.
   - Crie o pesquisador Henry Armitage, catalogue as três entidades e chame `LerCatalogo()`.

**Requisitos de Implementação (POO) — revisão geral:**
- Herança com `: base(...)`, `virtual`, `override` e `base.Metodo()`
- Encapsulamento com `private set` e valor inicial de propriedade
- Composição (lista criada no construtor) e Agregação (objetos recebidos por parâmetro)
- Polimorfismo percorrendo `List<ClasseMae>` com `foreach`
- Condicional `if` com `!=`
