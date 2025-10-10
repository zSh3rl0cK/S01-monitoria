from abc import ABC, abstractmethod

# --- 1. Interfaces em Python (via ABC) ---

class IAcao(ABC):
    """
    Interface (Contrato) que define as ações básicas que qualquer entidade
    capaz de lutar deve implementar. Garante o Polimorfismo.
    """

    @abstractmethod
    def atacar(self, alvo):
        """Define a lógica de ataque da entidade."""
        pass

    @abstractmethod 
    def defender(self):
        """Define a lógica de defesa da entidade."""
        pass


# --- 2. Classes para Relações (Composição e Agregação) ---

class Arma:
    """
    Classe simples para o objeto Arma.
    Será usada em Composição, pois um Personagem "possui" uma Arma.
    Demonstra encapsulamento com atributos protegidos e privados.
    """

    def __init__(self, nome: str, dano: int):
        self._nome = nome  # protegido — pode ser acessado, mas não é recomendado modificar fora da classe
        self.__dano = dano  # privado — acesso direto é bloqueado

    def usar(self):
        return f"{self._nome} (Dano: {self.__dano})"

    # Getter e Setter controlados (encapsulamento total)
    def get_dano(self):
        return self.__dano

    def set_dano(self, novo_dano):
        if novo_dano > 0:
            self.__dano = novo_dano
        else:
            print("O dano deve ser positivo!")


class Item:
    """
    Classe simples para itens diversos.
    Será usada em Agregação no Inventario.
    """

    def __init__(self, nome: str):
        self.nome = nome


class Inventario:
    """
    Classe para Agregação: 'Inventario tem um' conjunto de Itens.
    Os Itens podem existir fora do Inventario (Agregação).
    Uso de Estrutura de Dados (listas).
    """

    def __init__(self):
        self._itens = []  # atributo protegido — acessível, mas não recomendado

    def adicionar_item(self, item: Item):
        self._itens.append(item)
        print(f"-> {item.nome} adicionado ao inventário.")

    def listar_itens(self):
        if not self._itens:
            return "Inventário vazio."
        return ", ".join([item.nome for item in self._itens])


# --- 3. Classes Base e Subclasses (Herança e Implementação da Interface) ---

class Personagem:
    """
    Classe base para todos os personagens.
    Demonstra Abstração e Encapsulamento.
    """

    def __init__(self, nome: str, vida: int, arma: Arma):
        self.nome = nome  # público
        self._vida = vida  # protegido
        self.__nivel = 1  # privado
        self.arma = arma  # composição: Personagem é dono da Arma
        self.inventario = Inventario()  # agregação: Personagem tem um Inventario

    def mostrar_status(self):
        return f"Status: {self.nome} | Vida: {self._vida} | Nível: {self.__nivel} | Arma: {self.arma._nome} | Itens: {self.inventario.listar_itens()}"

    def receber_dano(self, dano):
        self._vida -= dano
        if self._vida < 0:
            self._vida = 0
        print(f"<{self.nome}> recebeu {dano} de dano. Vida restante: {self._vida}")

    def subir_nivel(self):
        """Aumenta o nível (atributo privado)."""
        self.__nivel += 1
        print(f"{self.nome} subiu para o nível {self.__nivel}!")

    # Getters e Setters
    def get_nivel(self):
        return self.__nivel

    def set_vida(self, nova_vida):
        if nova_vida >= 0:
            self._vida = nova_vida
        else:
            print("A vida não pode ser negativa!")


class Guerreiro(Personagem, IAcao):
    """
    Subclasse que herda de Personagem e implementa a interface IAcao.
    Demonstra Herança e Polimorfismo (implementação específica de 'atacar').
    """

    def atacar(self, alvo: Personagem):
        dano_total = self.arma.get_dano() * 1.5  # usa getter (encapsulamento)
        print(f"<{self.nome}> ATACA com força de GUERREIRO usando {self.arma.usar()} em {alvo.nome}!")
        alvo.receber_dano(dano_total)

    def defender(self):
        print(f"<{self.nome}> se defende com o escudo, reduzindo o próximo dano.")


class Mago(Personagem, IAcao):
    """
    Outra subclasse que implementa IAcao, mostrando Polimorfismo.
    """

    def atacar(self, alvo: Personagem):
        dano_total = self.arma.get_dano() * 0.8 + 10
        print(f"<{self.nome}> lança uma BOLA DE FOGO (Mago) em {alvo.nome}!")
        alvo.receber_dano(dano_total)

    def defender(self):
        print(f"<{self.nome}> usa uma barreira arcana para defesa.")


# --- 4. Demonstração de Uso e Polimorfismo ---

if __name__ == "__main__":
    print("--- Inicializando o Mini-RPG ---")

    # Composição: Armas que são "partes" dos personagens
    espada = Arma("Espada Longa", 15)
    cajado = Arma("Cajado Élfico", 12)

    # Teste de encapsulamento via setter
    cajado.set_dano(espada.get_dano())

    # Criação dos Personagens (Herança + Composição da Arma)
    arthur = Guerreiro("Arthur", 100, espada)
    merlin = Mago("Merlin", 80, cajado)

    # Agregação: Itens que podem existir fora do inventário
    pocao = Item("Poção de Vida")
    mapa = Item("Mapa Antigo")

    # Gerenciamento de Itens no Inventario (Agregação + Estrutura de Dados)
    arthur.inventario.adicionar_item(pocao)
    merlin.inventario.adicionar_item(mapa)

    print("\n--- Status Inicial dos Personagens ---")
    print(arthur.mostrar_status())
    print(merlin.mostrar_status())

    # Estrutura de Dados (Lista) para Polimorfismo
    # Todos são do tipo IAcao e podem ser gerenciados na mesma lista
    time_da_luta: list[IAcao] = [arthur, merlin]

    print("\n--- Simulação da Luta (Polimorfismo e Interface IAcao) ---")

    # Iteração polimórfica: chamamos o mesmo método 'atacar', mas a ação é diferente
    for combatente in time_da_luta:
        if isinstance(combatente, Guerreiro):
            combatente.atacar(merlin)  # Guerreiro ataca Mago
        elif isinstance(combatente, Mago):
            combatente.atacar(arthur)  # Mago ataca Guerreiro

    print("\n--- Testando Encapsulamento ---")
    # 🔹 Acesso protegido (permitido, mas não recomendado)
    arthur._vida = 999
    print(f"Vida alterada externamente (protegido): {arthur._vida}")

    # 🔹 Tentativa de acessar atributo privado (não acessível diretamente)
    try:
        print(arthur.__nivel)
    except AttributeError:
        print("Não é possível acessar __nivel diretamente (privado)")

    # 🔹 Acesso correto via getter
    print(f"Nível atual (via getter): {arthur.get_nivel()}")

    # 🔹 Modificando atributo privado da arma via método (encapsulamento)
    arthur.arma.set_dano(25)
    print(f"Dano da arma atualizado via método: {arthur.arma.get_dano()}")

    # 🔹 Subir nível corretamente
    arthur.subir_nivel()

    print("\n--- Status Final ---")
    print(arthur.mostrar_status())
    print(merlin.mostrar_status())