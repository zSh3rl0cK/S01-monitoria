package org.example;

public class Main {
    public static void main(String[] args) {
        // 1. Instanciação e Herança
        Autor dostoievski = new Autor("Dostoievski");
        Autor frank = new Autor("Frank Hebert");
        Cliente gerard = new Cliente("Gerard Way", 101); // Herda de Pessoa
        Bibliotecario olisykes = new Bibliotecario("Oliver Sykes", 201); // Herda de Pessoa e Implementa Interface
        Estante ficcao = new Estante("FIC-A1");

        // 2. Composição e Agregação
        // Livro 1 (Agrega Autor)
        Livros noitesBrancas = new Livros("978-8573262848", "Noites Brancas", dostoievski);
        noitesBrancas.adicionarPalavraChave("Romance"); // Uso do ArrayList (mantém ordem)
        noitesBrancas.adicionarPalavraChave("Clássico");

        // Livro 2 (Agrega Autor)
        Livros duna = new Livros("978-8501015391", "Duna",frank);
        duna.adicionarPalavraChave("Ficção Ciêntífica");

        // 3. Collections: HashMap (Inventário)
        ficcao.adicionarLivro(noitesBrancas);
        ficcao.adicionarLivro(duna);

        // Tentativa de adicionar título duplicado (Uso do HashSet no Autor)
        frank.adicionarTitulo("Duna");

        // 4. Collections: LinkedHashSet (Empréstimo - Ordem e Unicidade)
        gerard.emprestarLivro(noitesBrancas);
        gerard.emprestarLivro(duna);
        gerard.emprestarLivro(noitesBrancas); // Tentativa de duplicata (LinkedHashSet impede)
        gerard.listarEmprestados(); // Demonstra a ordem de inserção

        // 5. Polimorfismo e Interface
        String tituloBuscado = "Noites Brancas";
        Livros encontrado = olisykes.localizarLivro(tituloBuscado, ficcao); // Chamada da Interface

        if (encontrado != null) {
            System.out.println("-> Localizado com sucesso! Detalhes: " + encontrado.getDetalhes());
        } else {
            System.out.println("-> Livro não encontrado.");
        }
    }
}