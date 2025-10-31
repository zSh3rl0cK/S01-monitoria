package org.example;

public class Bibliotecario extends Pessoa implements GerenciaInventario {

    public Bibliotecario(String nome, int id) {
        super(nome, id);
    }

    // Polimorfismo: Implementação do método da Interface
    @Override
    public Livros localizarLivro(String titulo, Estante estante) {
        System.out.println("\n[Busca] Bibliotecário " + getNome() + " localizando '" + titulo + "'...");

        // Iteração sobre os valores do HashMap
        for (Livros livro : estante.getInventario().values()) {
            if (livro.getTitulo().equalsIgnoreCase(titulo)) {
                return livro;
            }
        }
        return null;
    }
}