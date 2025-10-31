package org.example;


import java.util.HashMap;

public class Estante {
    private String codigoEstante;
    // Collections: HashMap - Armazena Livros mapeados pelo ISBN (Chave -> Valor)
    private HashMap<String, Livros> inventario;

    public Estante(String codigo) {
        this.codigoEstante = codigo;
        this.inventario = new HashMap<>();
    }

    public void adicionarLivro(Livros livro) {
        // Uso do HashMap: put(Chave, Valor)
        this.inventario.put(livro.getIsbn(), livro);
        System.out.println("[Inventário] Livro adicionado à estante " + codigoEstante + ": " + livro.getTitulo());
    }

    public Livros buscarPorIsbn(String isbn) {
        // Uso do HashMap: get(Chave)
        return this.inventario.get(isbn);
    }

    public HashMap<String, Livros> getInventario() {
        return inventario;
    }

    public String getCodigoEstante() {
        return codigoEstante;
    }
}