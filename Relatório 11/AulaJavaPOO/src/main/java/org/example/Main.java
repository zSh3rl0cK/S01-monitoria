package org.example;

public class Main {
    public static void main(String[] args) {

        // INSTANCIAÇÃO
        // criando os objetos que vao ser usados ao longo da demonstração
        Autor billCipher    = new Autor("Bill Cipher");
        Autor kazuo         = new Autor("Kazuo Ishiguro");
        Autor williamGibson = new Autor("William Gibson");

        // HERANÇA em ação — Cliente e Bibliotecario sao instanciados como Pessoa
        Cliente gerard    = new Cliente("Gerard Way", 101);
        Bibliotecario oli = new Bibliotecario("Oliver Sykes", 201);

        // COMPOSIÇÃO — Estante faz parte da estrutura da Biblioteca
        Estante ficcao = new Estante("FIC-A1");


        // AGREGAÇÃO — Livro agrega um Autor que já existia antes
        Livros billsBook     = new Livros("978-0000000001", "Bill's Book",                billCipher);
        Livros naoMeAbandone = new Livros("978-8535911732", "Não Me Abandone Jamais",     kazuo);
        Livros neuromancer   = new Livros("978-8576572022", "Neuromancer",                williamGibson);

        // ArrayList — palavras-chave mantém a ordem de inserção
        billsBook.adicionarPalavraChave("Misterio");
        billsBook.adicionarPalavraChave("Sobrenatural");
        naoMeAbandone.adicionarPalavraChave("Distopia");
        neuromancer.adicionarPalavraChave("Ficção Científica");
        neuromancer.adicionarPalavraChave("Cyberpunk");


        // HashMap — adicionando livros à estante mapeados pelo ISBN
        ficcao.adicionarLivro(billsBook);
        ficcao.adicionarLivro(naoMeAbandone);
        ficcao.adicionarLivro(neuromancer);

        // tentativa de título duplicado — o ArrayList com .contains() impede
        williamGibson.adicionarTitulo("Neuromancer");


        // AGREGAÇÃO — Cliente guarda referências pra Livros que já existiam
        gerard.emprestarLivro(billsBook);
        gerard.emprestarLivro(neuromancer);
        gerard.emprestarLivro(billsBook); // tentativa de duplicata — o .contains() impede
        gerard.listarEmprestados();


        // INTERFACE + POLIMORFISMO em ação
        // Bibliotecario implementa GerenciaInventario, entao pode chamar localizarLivro()
        String tituloBuscado = "Neuromancer";
        Livros encontrado = oli.localizarLivro(tituloBuscado, ficcao);

        if (encontrado != null) {
            System.out.println("-> Localizado com sucesso! Detalhes: " + encontrado.getDetalhes());
        } else {
            System.out.println("-> Livro não encontrado.");
        }
    }
}