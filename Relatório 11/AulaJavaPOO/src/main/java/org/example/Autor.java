package org.example;

import java.util.ArrayList;
import java.util.List;

// CLASSE SIMPLES
// Autor nao herda de nada, é só um molde com nome e títulos escritos
// vai ser referenciado dentro de Livros (isso é Agregação)
public class Autor {

    // ENCAPSULAMENTO
    // private significa que so a propria classe pode acessar diretamente
    // o acesso de fora é feito pelos getters e setters
    private String nome;
    private List<String> titulosEscritos;

    public Autor(String nome) {
        this.nome = nome;
        // ArrayList é a lista mais comum do Java
        // equivale ao List do C# ou ao array do Ruby
        this.titulosEscritos = new ArrayList<>();
    }

    // GETTER
    // como nome é private, criamos um métod.o público pra lê-lo de fora
    public String getNome() {
        return nome;
    }

    public void adicionarTitulo(String titulo) {
        if (!titulosEscritos.contains(titulo)) {
            titulosEscritos.add(titulo);
            System.out.println("[Autor] Título '" + titulo + "' adicionado com sucesso.");
        } else {
            System.out.println("[Autor] Aviso: Título '" + titulo + "' já existe.");
        }
    }

    public List<String> getTitulosEscritos() {
        return titulosEscritos;
    }
}