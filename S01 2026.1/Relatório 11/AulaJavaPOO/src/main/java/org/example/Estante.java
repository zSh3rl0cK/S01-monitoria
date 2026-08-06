package org.example;

import java.util.HashMap;

// COMPOSIÇÃO
// Estante é criada dentro da Biblioteca e nao faz sentido existir sem ela
// ela guarda os Livros mapeados pelo ISBN usando um HashMap
public class Estante {

    private String codigoEstante;

    // HashMap é uma estrutura de chave -> valor
    // aqui a chave é o ISBN (String) e o valor é o objeto Livros
    // equivale ao dicionário do Python ou ao Hash do Ruby
    private HashMap<String, Livros> inventario;

    public Estante(String codigo) {
        this.codigoEstante = codigo;
        this.inventario = new HashMap<>();
    }

    public void adicionarLivro(Livros livro) {
        // put(chave, valor) adiciona um par no HashMap
        this.inventario.put(livro.getIsbn(), livro);
        System.out.println("[Inventário] Livro adicionado à estante " + codigoEstante + ": " + livro.getTitulo());
    }

    public Livros buscarPorIsbn(String isbn) {
        // get(chave) busca um valor pelo ISBN no HashMap
        return this.inventario.get(isbn);
    }

    public HashMap<String, Livros> getInventario() {
        return inventario;
    }

    public String getCodigoEstante() {
        return codigoEstante;
    }
}