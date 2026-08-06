package org.example;

// INTERFACE
// define um contrato — qualquer classe que implementar GerenciaInventario
// É OBRIGADA a ter o mét.do localizarLivro
// equivale ao module com raise NotImplementedError do Ruby ou ao ABC do Python

public interface GerenciaInventario {
    // o méto.do é declarado mas nao implementado aqui
    // quem implementar a interface decide como ele vai funcionar
    Livros localizarLivro(String titulo, Estante estante);
}