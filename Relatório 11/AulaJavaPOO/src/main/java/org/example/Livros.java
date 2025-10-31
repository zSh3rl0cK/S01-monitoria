package org.example;

import java.util.ArrayList;

public class Livros {
    private String isbn; // codigo de barras
    private String titulo; // titulo do livro
    // Composição: Livro tem uma referência a um Autor (o Autor pode existir sem o Livro)
    private Autor autor;

    // Collections: ArrayList - Mantém a ordem de inserção das palavras-chave
    private ArrayList<String> palavrasChave;

    public Livros(String isbn, String titulo, Autor autor) {
        this.isbn = isbn;
        this.titulo = titulo;
        this.autor = autor;
        this.palavrasChave = new ArrayList<>();
        autor.adicionarTitulo(titulo); // Notifica o Autor (exemplo de acoplamento fraco)
    }

    public String getIsbn() {
        return isbn;
    }

    public String getTitulo() {
        return titulo;
    }

    // Uso do ArrayList
    public void adicionarPalavraChave(String palavra) {
        this.palavrasChave.add(palavra);
    }

    public String getDetalhes() {
        return "'" + titulo + "' (ISBN: " + isbn + ") - Autor: " + autor.getNome();
    }
}