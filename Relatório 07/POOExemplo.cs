using System;
using System.Collections.Generic;

// todo: CLASSE BASE
// molde principal, todo item do jogo tem um nome
// as classes filhas vão herdar daqui
public class Item
{
    // todo: ENCAPSULAMENTO
    // { get; set; } é a forma do C# de expor o atributo de forma controlada
    // get = pode ler, set = pode alterar
    public string Nome { get; set; }
    
    // todo: CONSTRUTOR
    // roda automaticamente quando o objeto é criado
    public Item(string nome)
    {
        this.Nome = nome;
        Console.WriteLine($"[Item] {Nome} foi criado.");
    }

    // todo: VIRTUAL
    // avisa que as classes filhas podem reescrever esse método
    // sem isso o C# ia sempre chamar essa versão aqui, ignorando as filhas
    public virtual void Usar()
    {
        Console.WriteLine($"\n--- {Nome} ---");
        Console.WriteLine("Usando um item genérico.");
    }
}

// todo: CLASSE SEPARADA (sem herança)
// Magia não é um Item, então não herda — é só uma classe independente
// vai ser usada dentro de Arma mais pra frente (isso é Composição)
public class Magia
{
    public string Efeito { get; set; } = "Nenhum";

    public void Ativar()
    {
        Console.WriteLine($"Mágica ativada: {Efeito}!");
    }
}

// todo: HERANÇA
// Arma herda de Item, já nasce com o atributo Nome
// a relação é: "Arma É UM Item"
public class Arma : Item
{
    // private set = só a própria classe pode alterar o DanoBase
    public int DanoBase { get; private set; }

    // todo: COMPOSIÇÃO
    // Arma tem uma Magia dentro dela
    // quando a Arma é criada, a Magia nasce junto com ela
    public Magia MagiaAssociada { get; set; }

    // repassa o nome pro construtor do pai com : base(nome)
    public Arma(string nome, int dano) : base(nome)
    {
        this.DanoBase = dano;
        this.MagiaAssociada = new Magia();
    }

    // todo: SOBRESCRITA (override)
    // reescreve o Usar() com o comportamento específico da Arma
    // o override avisa o compilador que isso é intencional
    public override void Usar()
    {
        Console.WriteLine($"\n--- {Nome} ---");
        Console.WriteLine($"É uma Arma de Dano: {DanoBase}");

        if (MagiaAssociada.Efeito != "Nenhum")
        {
            MagiaAssociada.Ativar();
        }
    }
}

// todo: HERANÇA
// mesma ideia da Arma, só muda o atributo e o comportamento do Usar()
public class Escudo : Item
{
    public int Defesa { get; private set; }

    public Escudo(string nome, int defesa) : base(nome)
    {
        this.Defesa = defesa;
    }

    public override void Usar()
    {
        // todo: base.Usar()
        // chama o Usar() do pai antes de adicionar o comportamento do Escudo
        // útil quando você quer reaproveitar parte da lógica da classe pai
        base.Usar();
        Console.WriteLine($"É um Escudo de Defesa: {Defesa}. Link se defende!");
    }
}

public class Link
{
    public string Nome { get; set; } = "Link";

    // todo: COMPOSIÇÃO
    // a lista nasce junto com Link e morre junto com ele
    // não faz sentido existir inventário sem um herói dono dele
    private List<Item> _inventario;
    private Arma arma;
    
    public Link()
    {
        this._inventario = new List<Item>();
        Console.WriteLine($"\n[Link] O herói {Nome} foi criado.");
    }

    // todo: AGREGAÇÃO
    // os itens existem fora de Link, ele só guarda uma referência pra eles
    // masterSword e hylianShield foram criados antes e vivem independente do herói
    public void AdicionarItem(Item item)
    {
        this._inventario.Add(item);
    }

    public void MostrarInventario()
    {
        Console.WriteLine($"\n{Nome} tem {_inventario.Count} itens no seu inventário:");

        // todo: POLIMORFISMO EM AÇÃO
        // o foreach trata tudo como Item, mas cada objeto roda o seu próprio Usar()
        foreach (var item in _inventario)
        {
            item.Usar();
        }
    }
}

public class Program
{
    public static void Main(string[] args)
    {
        Console.WriteLine("=== Iniciando Aventura em Hyrule ===");

        Arma masterSword = new Arma("Master Sword", 30);
        masterSword.MagiaAssociada.Efeito = "Corte de Energia Sagrada";

        Escudo hylianShield = new Escudo("Hylian Shield", 50);

        Link linkHeroi = new Link();

        linkHeroi.AdicionarItem(masterSword);
        linkHeroi.AdicionarItem(hylianShield);
        linkHeroi.AdicionarItem(new Item("Rupee Azul"));

        linkHeroi.MostrarInventario();

        Console.WriteLine("\n=== Fim da Demonstração ===");
    }
}