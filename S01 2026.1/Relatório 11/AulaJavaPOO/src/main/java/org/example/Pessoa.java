package org.example;

// CLASSE ABSTRATA
// Pessoa é o molde principal — nao pode ser instanciada diretamente
// só existe para ser herdada por Cliente, Bibliotecario, Gerente etc
// abstract em Java equivale ao ABC do Python
public abstract class Pessoa {

    // ENCAPSULAMENTO
    // protected significa que so a propria classe e as filhas podem acessar diretamente
    // equivale ao _ (underline) do Python e ao protected do C#
    protected String nome;
    protected int id;

    public Pessoa(String nome, int id) {
        this.nome = nome;
        this.id = id;
    }

    // GETTER e SETTER
    // como os atributos sao protected, criamos métodos públicos
    // pra ler e alterar de forma controlada de fora da hierarquia
    public String getNome() {
        return nome;
    }

    public int getId() {
        return id;
    }

    public void setNome(String nome) {
        this.nome = nome;
    }
}