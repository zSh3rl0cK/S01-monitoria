from abc import ABC, abstractmethod


# INTERFACE (contrato via classe abstrata)
# Python não tem interface nativa como C# ou Java
# ABC é a forma do Python simular isso
# toda classe que herdar de IAcao É OBRIGADA a ter atacar() e defender()
class IAcao(ABC):

    @abstractmethod
    def atacar(self, alvo):
        pass

    @abstractmethod
    def defender(self):
        pass


# CLASSE SIMPLES
# Arma não herda de nada, é só um molde com nome e dano
# é criada fora e entregue ao Personagem (isso é Agregação)
class Arma:

    def __init__(self, nome: str, dano: int):
        # ENCAPSULAMENTO - níveis de acesso em Python
        # Python não bloqueia o acesso de verdade, é uma convenção
        # _nome  = um underline = "protegido", evitar mexer fora da classe
        # __dano = dois underlines = "privado", o Python embaralha o nome
        self._nome = nome
        self.__dano = dano

    # self indica que o método recebe o próprio objeto desta classe
    def usar(self):
        return f"{self._nome} (Dano: {self.__dano})"

    # GETTER e SETTER
    # como __dano é privado, criamos métodos pra ler e alterar ele
    # isso é encapsulamento na prática: o objeto controla os seus dados
    def get_dano(self):
        return self.__dano

    def set_dano(self, novo_dano):
        if novo_dano > 0:
            self.__dano = novo_dano
        else:
            print("O dano deve ser positivo!")


# AGREGAÇÃO
# Item existe por conta própria, independente de qualquer Inventario
# se o Inventario sumir, o Item continua existindo
class Item:

    # __init__ é o construtor que usamos nas outras linguagens
    def __init__(self, nome: str):
        self.nome = nome


# AGREGAÇÃO (cont.)
# Inventario guarda referências pros Itens, mas não é dono deles
class Inventario:

    def __init__(self):
        self._itens = []

    def adicionar_item(self, item: Item):
        self._itens.append(item)
        print(f"-> {item.nome} adicionado ao inventário.")

    def listar_itens(self):
        if not self._itens:
            return "Inventário vazio."
        # junta o nome de todos os itens em um texto, separados por vírgula
        return ", ".join([item.nome for item in self._itens])


# CLASSE BASE
# molde principal de todo personagem do jogo
# demonstra Encapsulamento e vai ser herdada por Guerreiro e Mago
class Personagem:

    def __init__(self, nome: str, vida: int, arma: Arma):
        self.nome = nome        # público - qualquer um pode acessar
        self._vida = vida       # protegido - evitar mexer fora da classe
        self.__nivel = 1        # privado - só a classe acessa diretamente
        # agregação: a Arma é criada fora e apenas recebida
        self.arma = arma
        # composição: o Inventario nasce e morre junto com o Personagem
        self.inventario = Inventario()

    def mostrar_status(self):
        return (f"Status: {self.nome} | Vida: {self._vida} | "
                f"Nível: {self.__nivel} | Arma: {self.arma._nome} | "
                f"Itens: {self.inventario.listar_itens()}")

    def receber_dano(self, dano):
        self._vida -= dano
        if self._vida < 0:
            self._vida = 0
        print(f"<{self.nome}> recebeu {dano} de dano. "
              f"Vida restante: {self._vida}")

    def subir_nivel(self):
        self.__nivel += 1
        print(f"{self.nome} subiu para o nível {self.__nivel}!")

    def get_nivel(self):
        return self.__nivel

    def set_vida(self, nova_vida):
        if nova_vida >= 0:
            self._vida = nova_vida
        else:
            print("A vida não pode ser negativa!")


# HERANÇA + INTERFACE
# Guerreiro herda de Personagem E implementa IAcao
# por isso é obrigado a ter atacar() e defender()
class Guerreiro(Personagem, IAcao):

    # SOBRESCRITA
    # implementa atacar() com o comportamento específico do Guerreiro
    def atacar(self, alvo):
        dano_total = self.arma.get_dano() * 1.5
        print(f"<{self.nome}> ATACA com força de GUERREIRO usando "
              f"{self.arma.usar()} em {alvo.nome}!")
        alvo.receber_dano(dano_total)

    def defender(self):
        print(f"<{self.nome}> se defende com o escudo, "
              f"reduzindo o próximo dano.")


# POLIMORFISMO
# Mago também implementa IAcao, mas o atacar() é completamente diferente
# mesma chamada, comportamento diferente: isso é polimorfismo
class Mago(Personagem, IAcao):

    def atacar(self, alvo):
        dano_total = self.arma.get_dano() * 0.8 + 10
        print(f"<{self.nome}> lança uma BOLA DE FOGO (Mago) "
              f"em {alvo.nome}!")
        alvo.receber_dano(dano_total)

    def defender(self):
        print(f"<{self.nome}> usa uma barreira arcana para defesa.")


if __name__ == "__main__":
    print("--- Inicializando o Mini-RPG ---")

    # agregação: as armas são criadas fora e entregues aos personagens
    espada = Arma("Espada Longa", 15)
    cajado = Arma("Cajado Élfico", 12)

    # testando getter e setter antes de criar os personagens
    cajado.set_dano(espada.get_dano())

    arthur = Guerreiro("Arthur", 100, espada)
    merlin = Mago("Merlin", 80, cajado)

    # agregação: itens existem fora e são só referenciados pelo inventário
    pocao = Item("Poção de Vida")
    mapa = Item("Mapa Antigo")

    arthur.inventario.adicionar_item(pocao)
    merlin.inventario.adicionar_item(mapa)

    print("\n--- Status Inicial ---")
    print(arthur.mostrar_status())
    print(merlin.mostrar_status())

    # POLIMORFISMO EM AÇÃO
    # a lista do tipo IAcao guarda Guerreiro e Mago
    # o mesmo .atacar() chama métodos diferentes dependendo do objeto
    time_da_luta: list[IAcao] = [arthur, merlin]

    print("\n--- Simulação da Luta ---")
    for combatente in time_da_luta:
        if isinstance(combatente, Guerreiro):
            combatente.atacar(merlin)
        elif isinstance(combatente, Mago):
            combatente.atacar(arthur)

    # ENCAPSULAMENTO NA PRÁTICA
    # forma correta: usando getter e setter
    print(f"\nNível atual (via getter): {arthur.get_nivel()}")
    arthur.arma.set_dano(25)
    print(f"Dano atualizado via setter: {arthur.arma.get_dano()}")
    arthur.subir_nivel()

    print("\n--- Status Final ---")
    print(arthur.mostrar_status())
    print(merlin.mostrar_status())
