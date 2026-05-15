package org.example;

import java.util.ArrayList;
import java.util.List;

// HERANÇA
// Cliente herda de Pessoa com extends
// a relação é: "Cliente É UMA Pessoa"
public class Cliente extends Pessoa {

    // AGREGAÇÃO
    // livrosEmprestados guarda referências pra Livros que já existiam fora do Cliente
    // se o Cliente sumir, os Livros continuam existindo
    private List<Livros> livrosEmprestados;

    public Cliente(String nome, int id) {
        // super() chama o construtor da classe mãe (Pessoa)
        // obrigatório em Java quando a classe pai tem construtor com parâmetros
        super(nome, id);
        this.livrosEmprestados = new ArrayList<>();
    }

    public void emprestarLivro(Livros livro) {
        if (!livrosEmprestados.contains(livro)) {
            livrosEmprestados.add(livro);
            System.out.println("[Empréstimo] Cliente " + getNome() + " pegou: " + livro.getTitulo() + " emprestado.");
        } else {
            System.out.println("[Empréstimo] Aviso: O cliente já possui o livro: " + livro.getTitulo());
        }
    }

    public void listarEmprestados() {
        System.out.println("\n--- Livros de " + getNome() + " ---");

        // POLIMORFISMO EM AÇÃO
        // o mesmo for para todos, mas cada Livro pode ter seu próprio comportamento
        // for(Tipo variavel : colecao) é o foreach do Java
        // equivale ao for..in do Python ou ao .each do Ruby
        for (Livros livro : livrosEmprestados) {
            System.out.println("- " + livro.getTitulo());
        }
    }
}