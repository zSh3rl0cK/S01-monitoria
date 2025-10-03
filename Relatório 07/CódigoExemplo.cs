using System;
using System.Collections.Generic; // Necessário para usar List<T>

// ===============================================
// CLASSE BASE: Item
// Herança e Polimorfismo
// ===============================================
public class Item
{
    // Encapsulamento com Propriedade Automática
    public string Nome { get; set; }

    // Construtor base
    public Item(string nome)
    {
        this.Nome = nome;
        Console.WriteLine($"[Item] {Nome} foi criado.");
    }

    // Método Virtual: Permite que classes filhas alterem este comportamento (Polimorfismo).
    public virtual void Usar()
    {
        Console.WriteLine($"\n--- {Nome} ---");
        Console.WriteLine("Usando um item genérico.");
    }
}

// ===============================================
// CLASSE COMPONENTE (PARTE)
// Usada na COMPOSIÇÃO
// ===============================================
public class Magia
{
    public string Efeito { get; set; } = "Nenhum";

    public void Ativar()
    {
        Console.WriteLine($"Mágica ativada: {Efeito}!");
    }
}

// ===============================================
// CLASSE DERIVADA (Arma)
// HERANÇA + COMPOSIÇÃO
// ===============================================
public class Arma : Item
{
    // Encapsulamento: DanoBase é público para leitura, mas privado para escrita externa.
    public int DanoBase { get; private set; } 
    
    // Composição: A Arma COMPÕE um objeto Magia. A Magia não existiria fora desta Arma.
    public Magia MagiaAssociada { get; set; } 

    // Construtor: Inicializa a classe base (Item) e o componente (MagiaAssociada).
    public Arma(string nome, int dano) : base(nome)
    {
        this.DanoBase = dano;
        // Inicialização da Composição: Cria a Magia junto com a Arma.
        this.MagiaAssociada = new Magia(); 
    }
    
    // Polimorfismo: Sobrescreve o método Usar() da classe Item.
    public override void Usar()
    {
        base.Usar(); // Chama o método Usar() da classe pai.
        Console.WriteLine($"É uma Arma de Dano: {DanoBase}");

        // Aplicação da Composição: Usa a funcionalidade do objeto Magia.
        if (MagiaAssociada.Efeito != "Nenhum")
        {
            MagiaAssociada.Ativar();
        }
    }
}

// ===============================================
// CLASSE DERIVADA (Escudo)
// HERANÇA Simples
// ===============================================
public class Escudo : Item
{
    public int Defesa { get; private set; }

    public Escudo(string nome, int defesa) : base(nome)
    {
        this.Defesa = defesa;
    }

    // Polimorfismo: Sobrescreve Usar() para ter um comportamento diferente.
    public override void Usar()
    {
        base.Usar();
        Console.WriteLine($"É um Escudo de Defesa: {Defesa}. Link se defende!");
    }
}

// ===============================================
// CLASSE AGREGADORA (TODO)
// AGGREGAÇÃO
// ===============================================
public class Link
{
    public string Nome { get; set; } = "Link";

    // Agregação: Link armazena uma lista de itens. O inventário (a lista)
    // pode existir antes e depois de Link.
    private List<Item> _inventario; 

    // Construtor de Link que recebe o inventário (o objeto agregado).
    public Link(List<Item> inventarioInicial)
    {
        _inventario = inventarioInicial;
        Console.WriteLine($"\n[Link] O herói {Nome} foi criado.");
    }

    public void MostrarInventario()
    {
        Console.WriteLine($"\n{Nome} tem {_inventario.Count} itens no seu inventário:");
        // Demonstra Coleções Polimórficas (Item, Arma, Escudo)
        foreach (var item in _inventario)
        {
            item.Usar(); 
        }
    }
}


// ===============================================
// EXECUÇÃO (MAIN)
// Demonstra Coleções e Relações
// ===============================================
public class Program
{
    public static void Main(string[] args)
    {
        Console.WriteLine("=== Iniciando Aventura em Hyrule ===");

        // 1. Criação de Partes (Arma e Escudo)
        Arma masterSword = new Arma("Master Sword", 30);

        // Aplicação da Composição: Acessa e modifica o objeto Magia, que só existe aqui.
        masterSword.MagiaAssociada.Efeito = "Corte de Energia Sagrada";

        Escudo hylianShield = new Escudo("Hylian Shield", 50);

        // 2. CRIAÇÃO DA PARTE AGREGADA (O Inventário)
        // A lista de itens existe independentemente de Link.
        List<Item> inventarioGeral = new List<Item>();
        inventarioGeral.Add(masterSword);
        inventarioGeral.Add(hylianShield);
        inventarioGeral.Add(new Item("Rupee Azul"));

        // 3. CRIAÇÃO DO TODO (Link) RECEBENDO A PARTE (Agregação)
        // Link é criado e passa a utilizar a lista 'inventarioGeral'.
        Link linkHeroi = new Link(inventarioGeral);

        // Demonstra Agregação e Polimorfismo em Coleções
        linkHeroi.MostrarInventario();

        Console.WriteLine("\n=== Fim da Demonstração ===");
    }
}

/*
SAÍDA ESPERADA (Demonstração dos Conceitos):
=== Iniciando Aventura em Hyrule ===
[Item] Master Sword foi criado.
[Item] Hylian Shield foi criado.

=== Explorando o Inventário (Polimorfismo) ===

--- Master Sword ---
Usando um item genérico. (Método base chamado via 'base.Usar()')
É uma Arma de Dano: 30
Mágica ativada: Corte de Energia Sagrada! (Funcionalidade da Composição)

--- Hylian Shield ---
Usando um item genérico.
É um Escudo de Defesa: 50. Link se defende!

--- Rupee Verde ---
Usando um item genérico.
=== Fim da Demonstração ===
*/	
