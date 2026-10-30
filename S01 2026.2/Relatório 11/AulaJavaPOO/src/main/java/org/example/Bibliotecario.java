package org.example;

// HERANÇA + INTERFACE
// Bibliotecario herda de Pessoa e implementa GerenciaInventario
// por isso é obrigado a ter o método localizarLivro
public class Bibliotecario extends Pessoa implements GerenciaInventario {

    public Bibliotecario(String nome, int id) {
        super(nome, id);
    }

    // SOBRESCRITA (override)
    // @Override avisa o compilador que estamos implementando um método da interface
    // equivale ao override do C# ou ao def com mesmo nome no Ruby
    @Override
    public Livros localizarLivro(String titulo, Estante estante) {
        System.out.println("\n[Busca] Bibliotecário " + getNome() + " localizando '" + titulo + "'...");

        // .values() retorna só os valores do HashMap, ignorando as chaves
        // aqui percorremos todos os Livros guardados na estante
        for (Livros livro : estante.getInventario().values()) {
            // equalsIgnoreCase compara strings ignorando maiúsculas e minúsculas
            // equivale ao .downcase == do Ruby ou ao .lower() == do Python
            if (livro.getTitulo().equalsIgnoreCase(titulo)) {
                return livro;
            }
        }
        return null;
    }
}