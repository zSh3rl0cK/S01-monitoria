#include <iostream>
#include <locale.h>
#include <list>
using namespace std;

// Classe Pessoa com atributos nome e idade
class Pessoa {
public:
    string nome;
    int idade;

    Pessoa(string nome, int idade) {
        this->nome = nome;
        this->idade = idade;
    }

    // Método para apresentar a pessoa
    void apresentar() {
        cout << "Olá, meu nome é " << nome << " e eu tenho " << idade << " anos." << endl;
    }
};

// Classe base Veiculo
class Veiculo {
protected:
    string tipo; // atributo protegido (usado pelas filhas)
public:
    // Construtor
    Veiculo(string tipo) {
        this->tipo = tipo;
    }

    // Método comum a todos os veículos
    void ligarMotor() {
        cout << "Motor ligado!" << endl;
    }

    // Método virtual (pode ser sobrescrito pelas classes filhas)
    virtual void mover() {
        cout << "Veículo se movendo..." << endl;
    }
};

// Classe Carro herda de Veiculo
class Carro : public Veiculo {
private:
    string marca;
    string modelo;
public:
    // Construtor chama o construtor da classe base
    Carro(string marca, string modelo) : Veiculo("Carro") {
        this->marca = marca;
        this->modelo = modelo;
    }

    // Imprime informações do carro
    void imprimirInfo() {
        cout << "Carro: " << marca << " " << modelo << endl;
    }

    // Método específico de carro
    void dirigir() {
        cout << "Dirigindo o carro..." << endl;
    }

    // Getters e Setters
    void setMarca(string marca) {
        this->marca = marca;
    }

    string getMarca() {
        return this->marca;
    }

    void setModelo(string modelo) {
        this->modelo = modelo;
    }

    string getModelo() {
        return this->modelo;
    }
};

// Classe Bicicleta herda de Veiculo
class Bicicleta : public Veiculo {
public:
    string marca;

    // Construtor chama o da base
    Bicicleta(string marca) : Veiculo("Bicicleta") {
        this->marca = marca;
    }

    // Sobrescreve o método mover
    void mover() override {
        cout << "A bicicleta pedala na ciclovia." << endl;
    }
};

int main() {
    setlocale(LC_ALL, ""); // Permite acentos no console

    Pessoa pessoa1("Pedro", 20);
    pessoa1.apresentar();

    Veiculo veiculo1("Carro");
    veiculo1.ligarMotor();

    Carro carro1("Fiat", "Mobi");
    Bicicleta bicleta1("Caloi");

    carro1.setMarca("Honda");
    carro1.setModelo("Civic");

    Veiculo* veiculo2 = new Carro("Hyundai", "Hb20");
    Veiculo* veiculo3 = new Bicicleta("Caloi");

    list<Veiculo*> veiculos;
    veiculos.push_back(veiculo2);
    veiculos.push_back(veiculo3);

    for(Veiculo* veiculo : veiculos) {
        veiculo->mover();
    }

    delete veiculo2;
    delete veiculo3;
    return 0;
}
