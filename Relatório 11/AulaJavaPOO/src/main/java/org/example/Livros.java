package org.example;

import java.util.ArrayList;

// AGREGAÇÃO
// Livros guarda uma referência a um Autor que já existia antes
// se o Livro sumir, o Autor continua existindo
public class Livros {

    private String isbn;
    private String titulo;
    private Autor autor;
    private ArrayList<String> palavrasChave;

    public Livros(String isbn, String titulo, Autor autor) {
        this.isbn = isbn;
        this.titulo = titulo;
        this.autor = autor;
        this.palavrasChave = new ArrayList<>();

        // ao criar o Livro, já notificamos o Autor que ele escreveu esse título
        // isso é acoplamento fraco — Livro e Autor se conhecem mas nao dependem um do outro
        autor.adicionarTitulo(titulo);
    }

    public String getIsbn() {
        return isbn;
    }

    public String getTitulo() {
        return titulo;
    }

    public void adicionarPalavraChave(String palavra) {
        this.palavrasChave.add(palavra);
    }

    public String getDetalhes() {
        return "'" + titulo + "' (ISBN: " + isbn + ") - Autor: " + autor.getNome();
    }
}