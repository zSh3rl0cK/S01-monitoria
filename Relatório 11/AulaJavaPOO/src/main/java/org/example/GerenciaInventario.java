package org.example;

public interface GerenciaInventario {
    /*
      Localiza um livro pelo título no inventário da estante.
      titulo é O título do livro a ser procurado.
      retorna O objeto Livro se encontrado, ou null.
     */
    Livros localizarLivro(String titulo, Estante estante);
}