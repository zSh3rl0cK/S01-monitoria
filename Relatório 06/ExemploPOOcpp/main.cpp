#include <iostream>
#include <string>
#include <vector>
using namespace std;

// todo: CLASSE BASE
// molde principal, todo ser vivo tem um nome
// as classes filhas vão herdar daqui
class SerVivo {

// todo: ENCAPSULAMENTO
// protected = só esta classe e as filhas podem tocar aqui
protected:
    string nome;

public:
    // todo: CONSTRUTOR
    // roda automaticamente quando o objeto é criadp
    // o ": nome(n)" só inicializa o atributo direto
    SerVivo(string n) : nome(n) {}

    // todo: VIRTUAL
    // avisa que as classes filhas podem reescrever esse méto.do
    // sem isso o C++ ia sempre chamar essa versão aqui, ignorando as filhas
    virtual void apresentar() {
        cout << "Eu sou um ser vivo chamado " << nome << "." << endl;
    }

    // todo: DESTRUTOR VIRTUAL
    // garante que a memória seja liberada corretamente
    // pode ignorar por agora, mas tem que estar aqui
    virtual ~SerVivo() {}
};

// todo: HERANÇA
// Humano herda de SerVivo, ou seja, já nasce com o atributo nome
// a relação é: "Humano É UM SerVivo"
class Humano : public SerVivo {
public:
    // repassa o nome pro construtor do pai
    Humano(string n) : SerVivo(n) {}

    // todo: SOBRESCRITA (override)
    // reescreve o apresentar() com o comportamento específico do Humano
    // o override avisa o compilador que isso é intencional
    void apresentar() override {
        cout << "Olá, eu sou o humano " << nome << ", mestre das civilizações." << endl;
    }
};

// mesma ideia do Humano, só muda o texto do apresentar()
class Elfo : public SerVivo {
public:
    Elfo(string n) : SerVivo(n) {}

    void apresentar() override {
        cout << "Saudações, eu sou o elfo " << nome << ", guardião das florestas." << endl;
    }
};

class Fada : public SerVivo {
public:
    Fada(string n) : SerVivo(n) {}

    void apresentar() override {
        cout << "Encantado! Eu sou a fada " << nome << ", portadora da magia." << endl;
    }
};

int main() {
    // todo: POLIMORFISMO
    // vetor de SerVivo* mas vai guardar Humano, Elfo e Fada
    // tratamos todos como SerVivo, mas cada um age do seu jeito
    vector<SerVivo*> seres;

    // new cria o objeto na memória e devolve um ponteiro
    seres.push_back(new Humano("Arthur"));
    seres.push_back(new Elfo("Legolas"));
    seres.push_back(new Fada("Luna"));
    seres.push_back(new SerVivo("Pedro"));

    cout << "=== Apresentações no mundo de fantasia ===" << endl;

    // todo: POLIMORFISMO EM AÇÃO
    // mesma chamada para todos, mas cada um executa o seu próprio apresentar()
    for (SerVivo* s : seres) {
        s->apresentar();
    }

    // todo: MEMÓRIA
    // tudo que criou com new, tem que deletar no final
    for (SerVivo* s : seres) {
        delete s;
    }

    return 0;
}