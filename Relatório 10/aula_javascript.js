// CLASSES COMPONENTES E ABSTRATAS (Interfaces)

// simulação de interface/contrato — JavaScript nao tem interface nativa
// qualquer classe que herdar de Instrumento é obrigada a implementar tocar()
// se nao implementar, o erro vai aparecer em tempo de execução
class Instrumento {
    constructor(nome) {
        this.nome = nome;
    }

    // VIRTUAL (equivalente)
    // método que as classes filhas vao sobrescrever com o comportamento específico delas
    tocar() {
        throw new Error(`O instrumento ${this.nome} deve implementar o método 'tocar()'`);
    }
}

// COMPOSIÇÃO
// Guitarra é uma parte do Musico — nasce e morre junto com ele
// herda de Instrumento e sobrescreve tocar() com seu comportamento especifico
class Guitarra extends Instrumento {
    constructor(modelo) {
        // super() chama o constructor do pai (Instrumento)
        // equivale ao base() do C# ou super() do Python
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
    // equivale ao __ do Python ou ao private do C# e Java
    // nao pode ser acessado diretamente de fora da classe
    #experiencia = 0;

    constructor(nome, funcao) {
        this.nome = nome;
        this.funcao = funcao;
    }
    
    // GETTER
    // get é a sintaxe do JavaScript para criar um getter
    // permite ler this.#experiencia de fora com musico.experiencia (sem parenteses)
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
// a relacao é: "Vocalista É UM Musico"
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
    // membros e repertorio nao devem ser alterados diretamente de fora
    #membros;
    #repertorio;
    
    // AGREGAÇÃO
    // membrosIniciais = [] é um parametro com valor padrão
    // se nenhum array for passado, começa vazio
    // os Musicos já existiam antes da Banda e continuam existindo se ela sumir
    constructor(nome, membrosIniciais = []) {
        this.nome = nome;
        this.#membros = membrosIniciais;
        
        // Map é uma estrutura de dados do JavaScript
        // funciona como um dicionario: guarda pares de chave -> valor --> {chave: valor, chave2: valor2}
        // aqui vai guardar titulo -> duracao das musicas
        this.#repertorio = new Map();
    }

    adicionarInstrumento(musicoNome, instrumento) {
        // .find() percorre o array e retorna o primeiro elemento que passa na condição
        // m => m.nome === musicoNome é uma arrow function — sintaxe curta do JavaScript
        // equivale a escrever function(m) { return m.nome === musicoNome }
        const musico = this.#membros.find(m => m.nome === musicoNome);
        if (musico) {
            musico.instrumento = instrumento;
            console.log(`[SETUP] ${musicoNome} agora tem um(a) ${instrumento.nome}.`);
        }
    }
    
    adicionarMusica(titulo, duracao) {
        // .set() adiciona um par chave -> valor no Map
        this.#repertorio.set(titulo, duracao);
    }
    
    listarGuitarristas() {
        // .filter() percorre o array e retorna só os elementos que passam na condição
        // instanceof verifica se o objeto é uma instancia de determinada classe
        // equivale ao is_a? do Ruby ou isinstance() do Python
        return this.#membros.filter(m => m.instrumento instanceof Guitarra);
    }
    
    iniciarShow() {
        console.log(`\n--- ${this.nome} INICIA O SHOW! ---`);
        
        // POLIMORFISMO EM AÇÃO
        // .forEach() percorre o array — equivale ao .each do Ruby ou for..in do Python
        // o mesmo executarAcao() para todos, mas cada um executa o seu proprio
        this.#membros.forEach(m => {
            let acao = m.executarAcao();
            
            // typeof verifica o tipo de uma variavel em tempo de execução
            // aqui garante que instrumento.tocar realmente é uma função antes de chamar
            if (m.instrumento && typeof m.instrumento.tocar === 'function') {
                acao += ` | ${m.instrumento.tocar()}`;
            }
            console.log(acao);
        });
    }
}


// --- MAIN ---

// criação dos instrumentos (componentes de composição)
const guitarraRiff = new Guitarra('PRS Custom');
const guitarraSolo = new Guitarra('Ibanez');
const bateria      = new Bateria('Pearl');

// HERANÇA em ação — cada um é instanciado do seu tipo especifico
const chester = new Vocalista('Chester Bennington', 'Nu Metal');
const mike    = new Vocalista('Mike Shinoda',       'Hip Hop');
const brad    = new Musico('Brad Delson', 'Guitarrista');
const rob     = new Musico('Rob Bourdon', 'Baterista');
const phoenix = new Musico('Phoenix',     'Baixista');

// testando getter — acessa #experiencia sem parenteses por causa do get
console.log(`Experiência de Chester: ${chester.experiencia}`);

// AGREGAÇÃO: musicos já existiam antes da banda ser criada
const linkinPark = new Banda('Linkin Park', [chester, mike, brad, rob, phoenix]);

linkinPark.adicionarInstrumento('Brad Delson', guitarraRiff);
linkinPark.adicionarInstrumento('Rob Bourdon', bateria);

// objeto literal passado direto como instrumento — JavaScript permite isso
// funciona porque tem o método tocar(), que é tudo que a Banda precisa (duck typing)
linkinPark.adicionarInstrumento('Mike Shinoda', {
    nome: 'Teclado',
    tocar: () => 'Adiciona synths marcantes.'
});

linkinPark.adicionarMusica('Somewhere I Belong', 3.5);
linkinPark.adicionarMusica('Fighting Myself',       3.1);

linkinPark.iniciarShow();

console.log('\nGuitarristas Ativos:');
linkinPark.listarGuitarristas().forEach(g => console.log(`- ${g.nome}`));



