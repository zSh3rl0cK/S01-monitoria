package org.example;

import java.util.HashSet;
import java.util.Set;

public class Autor {
    private String nome;
    // Collections: HashSet - Garante que cada título seja único (sem duplicatas)
    private HashSet<String> titulosEscritos;

    public Autor(String nome) {
        this.nome = nome;
        this.titulosEscritos = new HashSet<>();
    }

    public String getNome() {
        return nome;
    }

    // Uso do HashSet
    public void adicionarTitulo(String titulo) {
        if (titulosEscritos.add(titulo)) {
            System.out.println("[Autor] Título '" + titulo + "' adicionado com sucesso.");
        } else {
            System.out.println("[Autor] Aviso: Título '" + titulo + "' já existe e não foi duplicado.");
        }
    }

    public HashSet<String> getTitulosEscritos() {
        return titulosEscritos;
    }
}