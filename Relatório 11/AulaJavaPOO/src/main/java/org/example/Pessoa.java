package org.example;

public abstract class Pessoa {
    // Encapsulamento: Atributos privados
    protected String nome;
    protected int id;

    // Construtor
    public Pessoa(String nome, int id) {
        this.nome = nome;
        this.id = id;
    }

    // Getters
    public String getNome() {
        return nome;
    }

    public int getId() {
        return id;
    }

    // Setter (Exemplo de controle de acesso)
    public void setNome(String nome) {
        this.nome = nome;
    }
}