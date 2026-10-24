// CLASSES COMPONENTES E ABSTRATAS (Interfaces)

// simulação de interface/contrato — JavaScript não tem interface nativa
// toda classe que herdar de Instrumento deve implementar tocar()
// o JavaScript não confere isso antes: se a classe não implementar, o
// erro só aparece em tempo de execução, quando tocar() for chamado
class Instrumento {
    constructor(nome) {
        this.nome = nome;
    }

    // VIRTUAL (equivalente)
    // método que as classes filhas vão sobrescrever com o comportamento
    // específico delas
    tocar() {
        throw new Error(
            `O instrumento ${this.nome} deve implementar o método 'tocar()'`
        );
    }
}

// AGREGAÇÃO (Musico -> Instrumento)
// a Guitarra é criada fora (no main) e só depois entregue ao Musico
// pela Banda, por isso o Musico apenas guarda uma referência a ela
// herda de Instrumento e sobrescreve tocar() com seu comportamento
// específico
class Guitarra extends Instrumento {
    constructor(modelo) {
        // super() chama o constructor do pai (Instrumento)
        // equivale ao : base(...) do C#, ao super().__init__() do
        // Python e ao super(...) do Ruby
        super('Guitarra');
        this.modelo = modelo;
    }

    // SOBRESCRITA (override)
    tocar() {
        return `Tocando um riff na ${this.modelo}!`;
    }
}

class Bateria extends Instrumento {
    constructor(marca) {
        super('Bateria');
        this.marca = marca;
    }

    tocar() {
        return `Batendo um ritmo pesado na ${this.marca}!`;
    }
}


// HERANÇA, POLIMORFISMO E ENCAPSULAMENTO

class Musico {
    // ENCAPSULAMENTO
    // # é a sintaxe do JavaScript para atributo privado
    // equivale ao private do C# e do Java: diferente do __ do Python,
    // que pode ser contornado, o JavaScript bloqueia de verdade o acesso
    // fora da classe (escrever musico.#experiencia lá fora é um erro)
    #experiencia = 0;

    constructor(nome, funcao) {
        this.nome = nome;
        this.funcao = funcao;
    }

    // GETTER
    // get é a sintaxe do JavaScript para criar um getter
    // permite ler this.#experiencia de fora com musico.experiencia
    // (sem parênteses)
    get experiencia() {
        return this.#experiencia;
    }

    // VIRTUAL (equivalente)
    executarAcao() {
        return `${this.nome} (${this.funcao}) está se preparando...`;
    }
}

// HERANÇA
// Vocalista herda de Musico com extends
// a relação é: "Vocalista É UM Musico"
class Vocalista extends Musico {
    constructor(nome, genero) {
        super(nome, 'Vocalista');
        this.generoMusical = genero;
    }

    // SOBRESCRITA (override)
    executarAcao() {
        return `${this.nome} canta ${this.generoMusical}!`;
    }
}


// AGREGAÇÃO E COLLECTIONS

class Banda {
    // # tornando os atributos privados
    // membros e repertório não devem ser alterados diretamente de fora
    #membros;
    #repertorio;

    // AGREGAÇÃO
    // membrosIniciais = [] é um parâmetro com valor padrão
    // se nenhum array for passado, começa vazio
    // os Musicos já existiam antes da Banda e continuam existindo se
    // ela sumir
    constructor(nome, membrosIniciais = []) {
        this.nome = nome;
        this.#membros = membrosIniciais;

        // Map é uma estrutura de dados do JavaScript
        // funciona como um dicionário: guarda pares chave -> valor
        // aqui vai guardar título -> duração das músicas
        // COMPOSIÇÃO: o Map é criado aqui dentro e só existe na Banda
        this.#repertorio = new Map();
    }

    adicionarInstrumento(musicoNome, instrumento) {
        // .find() percorre o array e retorna o primeiro elemento que
        // passa na condição
        // m => m.nome === musicoNome é uma arrow function — sintaxe
        // curta do JavaScript
        // equivale a escrever function(m) { return m.nome === musicoNome }
        const musico = this.#membros.find(m => m.nome === musicoNome);
        if (musico) {
            musico.instrumento = instrumento;
            console.log(
                `[SETUP] ${musicoNome} agora tem um(a) ${instrumento.nome}.`
            );
        }
    }

    adicionarMusica(titulo, duracao) {
        // .set() adiciona um par chave -> valor no Map
        this.#repertorio.set(titulo, duracao);
    }

    listarGuitarristas() {
        // .filter() percorre o array e retorna só os elementos que passam
        // na condição
        // instanceof verifica se o objeto é uma instância de determinada
        // classe
        // equivale ao is_a? do Ruby ou isinstance() do Python
        return this.#membros.filter(m => m.instrumento instanceof Guitarra);
    }

    iniciarShow() {
        console.log(`\n--- ${this.nome} INICIA O SHOW! ---`);

        // POLIMORFISMO EM AÇÃO
        // .forEach() percorre o array — equivale ao .each do Ruby ou ao
        // for x in lista do Python
        // o mesmo executarAcao() para todos, mas cada um executa o seu
        // próprio
        this.#membros.forEach(m => {
            let acao = m.executarAcao();

            // typeof verifica o tipo de uma variável em tempo de execução
            // aqui garante que instrumento.tocar realmente é uma função
            // antes de chamar
            if (m.instrumento && typeof m.instrumento.tocar === 'function') {
                acao += ` | ${m.instrumento.tocar()}`;
            }
            console.log(acao);
        });
    }
}


// --- MAIN ---

// criação dos instrumentos: eles existem antes de serem entregues aos
// músicos (agregação)
const guitarraRiff = new Guitarra('PRS Custom');
const guitarraSolo = new Guitarra('Ibanez');
const bateria      = new Bateria('Pearl');

// HERANÇA em ação — cada um é instanciado do seu tipo específico
const chester = new Vocalista('Chester Bennington', 'Nu Metal');
const mike    = new Vocalista('Mike Shinoda',       'Hip Hop');
const brad    = new Musico('Brad Delson', 'Guitarrista');
const rob     = new Musico('Rob Bourdon', 'Baterista');
const phoenix = new Musico('Phoenix',     'Baixista');

// testando getter — acessa #experiencia sem parênteses por causa do get
console.log(`Experiência de Chester: ${chester.experiencia}`);

// AGREGAÇÃO: músicos já existiam antes da banda ser criada
const linkinPark = new Banda('Linkin Park',
                             [chester, mike, brad, rob, phoenix]);

linkinPark.adicionarInstrumento('Brad Delson', guitarraRiff);
linkinPark.adicionarInstrumento('Rob Bourdon', bateria);

// objeto literal passado direto como instrumento — JavaScript permite
// isso
// funciona porque tem o método tocar(), que é tudo que a Banda precisa
// (duck typing)
linkinPark.adicionarInstrumento('Mike Shinoda', {
    nome: 'Teclado',
    tocar: () => 'Adiciona synths marcantes.'
});

linkinPark.adicionarMusica('Somewhere I Belong', 3.5);
linkinPark.adicionarMusica('Fighting Myself',       3.1);

linkinPark.iniciarShow();

console.log('\nGuitarristas Ativos:');
linkinPark.listarGuitarristas().forEach(g => console.log(`- ${g.nome}`));
