# ===============================================
# MÓDULOS (Interfaces / Contratos de Comportamento)
# ===============================================

# Módulo que define que um objeto pode queimar o Cosmo (Interface)
module CapacidadeCosmo
  # Método que DEVE ser implementado pela classe que incluir o módulo
  def queimar_cosmo
    raise NotImplementedError, "#{self.class} deve implementar o método 'queimar_cosmo'"
  end
end

# Módulo para habilidades de defesa
module CapacidadeDefensiva
  def defender
    puts "🛡️ Defesa cósmica ativada!"
  end
end

# ===============================================
# CLASSE COMPONENTE (PARTE da Composição)
# ===============================================

# Representa a Armadura (ou "Kamei") que é parte essencial do Cavaleiro
class Armadura
  attr_reader :nome, :material

  # Construtor do Componente
  def initialize(nome, material)
    @nome = nome
    @material = material
  end

  def mostrar_detalhes
    "Armadura de #{@nome}, feita de #{@material}."
  end
end

# ===============================================
# CLASSE BASE: Guerreiro (Herança e Encapsulamento)
# ===============================================
class Guerreiro
  # Encapsulamento: attr_reader cria apenas o getter (somente leitura)
  attr_reader :nome, :constelacao

  # Encapsulamento: Atributo privado/protegido por convenção
  def initialize(nome, constelacao, forca_inicial)
    @nome = nome
    @constelacao = constelacao
    @_forca = forca_inicial # Convenção de atributo para uso interno
  end

  # Getter explícito para o atributo de uso interno
  def forca
    @_forca
  end
  
  # Setter explícito com lógica de validação
  def forca=(novo_valor)
    @_forca = novo_valor if novo_valor.is_a?(Numeric) && novo_valor.positive?
  end

  # Método para ser sobrescrito (Polimorfismo)
  def atacar
    puts "💥 #{nome} desfere um golpe básico!"
  end
end

# ===============================================
# CLASSE DERIVADA: Cavaleiro (Herança, Módulos e Composição)
# ===============================================
class Cavaleiro < Guerreiro
  # Inclui os Módulos (Interfaces)
  include CapacidadeCosmo
  include CapacidadeDefensiva

  # Atributo que armazena o objeto Armadura (Composição)
  attr_accessor :armadura

  # Construtor do Cavaleiro (Chama o initialize do pai com 'super')
  def initialize(nome, constelacao, forca_inicial, nome_armadura, material_armadura)
    super(nome, constelacao, forca_inicial) # Chama initialize de Guerreiro

    # Composição: A armadura é criada junto com o Cavaleiro.
    @armadura = Armadura.new(nome_armadura, material_armadura)
    puts "✨ #{nome} surge com a #{@armadura.nome}."
  end

  # Polimorfismo: Sobrescrita do método atacar do Guerreiro
  def atacar
    puts "☄️ #{nome} usa o poder da sua constelação #{constelacao}!"
    puts "  [Bônus da Armadura: #{@armadura.material}]" # Uso do componente
  end

  # Implementação do método abstrato do Módulo CapacidadeCosmo
  def queimar_cosmo
    puts "🔥 O Cosmo de #{nome} queima! Poder atual: #{forca * 5}"
  end
end

# ===============================================
# CLASSE DE AGREGAÇÃO: ExercitoDeAtena (Collections)
# ===============================================
class ExercitoDeAtena
  # Agregação: Recebe uma coleção de Cavaleiros (Array)
  attr_accessor :cavaleiros

  def initialize(cavaleiros = [])
    @cavaleiros = cavaleiros # Agregação: Os Cavaleiros já existiam fora do Exército.
  end

  # Uso de Collections (.each)
  def iniciar_batalha(tecnica)
    puts "\n--- Exército de Atena Inicia a Batalha com: #{tecnica} ---"
    @cavaleiros.each do |c|
      c.atacar # Polimorfismo: Chama o 'atacar' específico de cada Cavaleiro
    end
  end

  # Uso de Collections (.select) para filtrar objetos com certo módulo
  def listar_com_defesa
    puts "\n--- Cavaleiros com Capacidade Defensiva ---"
    
    # Filtra (select) apenas os objetos que respondem ao método 'defender'
    defensores = @cavaleiros.select { |c| c.respond_to?(:defender) } 
    
    defensores.each(&:defender) # Uso do & para chamar o método
  end
end

# ===============================================
# EXECUÇÃO DA DEMONSTRAÇÃO
# ===============================================

# 1. Criação e Encapsulamento
seiya = Cavaleiro.new("Seiya", "Pégaso", 1000, "Pégaso", "Bronze")
shiryu = Cavaleiro.new("Shiryu", "Dragão", 1200, "Dragão", "Bronze")
saga = Guerreiro.new("Saga", "Gêmeos", 5000) # Um guerreiro base sem armadura específica

# Testando Encapsulamento/Setter (A forca só pode ser alterada via setter)
seiya.forca = 1100
puts "\nSeiya agora tem #{seiya.forca} de força. (Leitura via Getter)"

# 2. Polimorfismo e Módulos (Interfaces)
puts "\n--- Demonstração de Habilidades ---"
seiya.queimar_cosmo # Método implementado do Módulo CapacidadeCosmo
seiya.defender      # Método do Módulo CapacidadeDefensiva
# saga.queimar_cosmo # <-- Isso daria erro, pois Guerreiro não inclui o módulo!

# 3. Composição
puts "\nDetalhes da Composição:"
puts seiya.armadura.mostrar_detalhes # Acesso à Armadura (Componente)

# 4. Agregação e Collections
exercito = ExercitoDeAtena.new([seiya, shiryu]) # Agregação de Cavaleiros

# Demonstração de Polimorfismo em Coleções (.each)
exercito.iniciar_batalha("Técnicas Secretas")

# Demonstração de Collections avançadas (.select) com Módulos
exercito.listar_com_defesa
