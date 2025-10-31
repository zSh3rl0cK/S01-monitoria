package org.example;

import java.util.LinkedHashSet;

public class Cliente extends Pessoa {
    // Collections: LinkedHashSet - Garante Unicidade (Set) e Ordem de Inserção (Linked)
    private LinkedHashSet<Livros> livrosEmprestados;

    public Cliente(String nome, int id) {
        // Herança: Chamada obrigatória ao construtor da classe pai
        super(nome, id);
        this.livrosEmprestados = new LinkedHashSet<>();
    }

    // Uso do LinkedHashSet
    public void emprestarLivro(Livros livro) {
        if (livrosEmprestados.add(livro)) {
            System.out.println("[Empréstimo] Cliente " + getNome() + " pegou: " + livro.getTitulo() + " emprestado");
        } else {
            System.out.println("[Empréstimo] Aviso: O cliente já possui o livro: " + livro.getTitulo());
        }
    }

    public void listarEmprestados() {
        System.out.println("\n--- Livros de " + getNome() + " (Ordem de Empréstimo) ---");
        // Iteração sobre o LinkedHashSet para mostrar a ordem
        for (Livros livro : livrosEmprestados) {
            System.out.println("- " + livro.getTitulo());
        }
    }
}