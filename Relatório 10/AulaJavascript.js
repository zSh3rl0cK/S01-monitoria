// ===============================================
// 1. CLASSES COMPONENTES E ABSTRATAS (Interfaces)
// ===============================================

// Simulação de Interface/Contrato Abstrato
class Instrumento {
    constructor(nome) {
        this.nome = nome;
    }
    // Abstração: Força a implementação nas classes filhas ou gera erro
    tocar() {
        throw new Error(`O instrumento ${this.nome} deve implementar o método 'tocar()'`);
    }
}

// Composição: Instrumento específico (Polimorfismo 1)
class Guitarra extends Instrumento {
    constructor(modelo) {
        super('Guitarra');
        this.modelo = modelo;
    }
    tocar() {
        // Polimorfismo
        return `🎸 Tocando um riff na ${this.modelo}!`;
    }
}

// Composição: Novo Instrumento específico (Polimorfismo 2)
class Bateria extends Instrumento {
    constructor(marca) {
        super('Bateria');
        this.marca = marca;
    }
    tocar() {
        // Polimorfismo
        return `🥁 Batendo um ritmo pesado na ${this.marca}!`;
    }
}


// ===============================================
// 2. HERANÇA, POLIMORFISMO E ENCAPSULAMENTO
// ===============================================

class Musico {
    // Encapsulamento: Atributo privado
    #experiencia = 0;

    constructor(nome, funcao) {
        this.nome = nome;
        this.funcao = funcao;
    }

    // Getter para o atributo privado
    get experiencia() {
        return this.#experiencia;
    }

    // Método Polimórfico base
    executarAcao() {
        return `${this.nome} (${this.funcao}) está se preparando...`;
    }
}

class Vocalista extends Musico {
    constructor(nome, genero) {
        super(nome, 'Vocalista');
        this.generoMusical = genero;
    }

    // Polimorfismo: Sobrescrita do método
    executarAcao() {
        return `🎤 ${this.nome} canta ${this.generoMusical}!`;
    }
}

// ===============================================
// 3. AGREGAÇÃO E COLLECTIONS (MAP)
// ===============================================

class Banda {
    // Agregação: Array de objetos Musico
    #membros;
    // Collections: Map para organizar repertório (Título -> Duração)
    #repertorio;

    constructor(nome, membrosIniciais = []) {
        this.nome = nome;
        this.#membros = membrosIniciais;
        this.#repertorio = new Map();
    }

    // Adiciona um Instrumento ao Musico
    adicionarInstrumento(musicoNome, instrumento) {
        // Uso de Collections (Array.find)
        const musico = this.#membros.find(m => m.nome === musicoNome);
        if (musico) {
            musico.instrumento = instrumento;
            console.log(`[SETUP] ${musicoNome} agora tem um(a) ${instrumento.nome}.`);
        }
    }

    adicionarMusica(titulo, duracao) {
        // Uso do Map
        this.#repertorio.set(titulo, duracao);
    }

    // Uso de Collections (Array.filter)
    listarGuitarristas() {
        // Filtra membros que possuem um instrumento que é uma Guitarra
        return this.#membros.filter(m => m.instrumento instanceof Guitarra);
    }

    // Uso de Collections (Array.forEach) e Polimorfismo
    iniciarShow() {
        console.log(`\n--- ${this.nome} INICIA O SHOW! ---`);
        this.#membros.forEach(m => {
            let acao = m.executarAcao(); // Polimorfismo (Vocalista vs Musico)
            
            // Verifica se o objeto Instrumento possui o método tocar() (Duck Typing)
            if (m.instrumento && m.instrumento.tocar && typeof m.instrumento.tocar === 'function') {
                 // Composição/Agregação: Acesso ao método do componente
                acao += ` | Ação com Instrumento: ${m.instrumento.tocar()}`;
            }
            console.log(acao);
        });
    }
}

// ===============================================
// EXECUÇÃO DA DEMONSTRAÇÃO (LINKIN PARK)
// ===============================================

// 1. Criação de Instrumentos (Componentes)
const guitarraRiff = new Guitarra('PRS Custom');
const guitarraSolo = new Guitarra('Ibanez');
const bateria = new Bateria('Pearl');

// 2. Herança, Encapsulamento e Instanciação de Membros
// Vocalistas
const chester = new Vocalista('Chester Bennington', 'Rock Alternativo');
const mike = new Vocalista('Mike Shinoda', 'Hip Hop/Teclado');
// Músicos
const brad = new Musico('Brad Delson', 'Guitarrista');
const rob = new Musico('Rob Bourdon', 'Baterista');
const phoenix = new Musico('Phoenix', 'Baixista');

// Teste de Encapsulamento
chester.ganharExperiencia(80);
console.log(`Experiência de Chester: ${chester.experiencia}`); // Uso do Getter

// 3. Agregação e Adição de Membros
const linkinPark = new Banda('Linkin Park', [chester, mike, brad, rob, phoenix]);

// Adição de Instrumentos aos Músicos
linkinPark.adicionarInstrumento('Brad Delson', guitarraRiff);
linkinPark.adicionarInstrumento('Rob Bourdon', bateria); 
linkinPark.adicionarInstrumento('Mike Shinoda', { nome: 'Teclado', tocar: () => '🎹 Adiciona synths marcantes.' }); // Objeto simples com Polimorfismo manual

// Adição de músicas (Uso do Map)
linkinPark.adicionarMusica('In The End', 3.5);
linkinPark.adicionarMusica('Numb', 3.1);

// 4. Polimorfismo e Collections
linkinPark.iniciarShow();

console.log('\nGuitarristas Ativos:');
linkinPark.listarGuitarristas().forEach(g => console.log(`- ${g.nome}`)); // Array.filter
