# ------- Contexto: Gachiakuta ---------
# No mundo de Gachiakuta, a sociedade é dividida entre quem vive nas alturas
# e quem é jogado no Abismo — um lugar repleto de lixo e criaturas chamadas Manchas.
# Os Catadores são habitantes do Abismo que aprenderam a dar vida a objetos descartados,
# chamados Utensilios, usando o poder do espirito. Organizados em grupos de Zeladores,
# eles partem em missoes para sobreviver e recuperar o que foi perdido nas profundezas.


# MÓDULOS (funciona como interface em outras linguagens)
# define um comportamento que qualquer classe pode "incluir"
# se a classe incluir o módulo, ela é obrigada a ter esse comportamento
module CapacidadeEspirito
  def invocar_espirito
    raise NotImplementedError, "#{self.class} deve implementar invocar_espirito"
  end
end

# MÓDULO DE COMBATE
# outro módulo, mas esse já traz a implementação pronta
# qualquer classe que incluir CapacidadeCombate ganha o entrar_em_combate() de graça
module CapacidadeCombate
  def entrar_em_combate
    puts "#{self.class} entra em postura de combate!"
  end
end


# COMPOSIÇÃO
# Utensilio nao existe sozinho fora do Catador
# ele nasce e morre junto com ele
class Utensilio
  attr_reader :nome, :tipo

  # Construtor
  def initialize(nome, tipo)
    @nome = nome
    @tipo = tipo
  end

  def mostrar_detalhes
    "Utensilio #{@nome} do tipo #{@tipo}."
  end
end


# CLASSE BASE
# molde principal de todo habitante do Abismo
# as classes filhas vao herdar daqui
class Habitante

  # ENCAPSULAMENTO
  # attr_reader cria apenas o getter para nome e origem — so leitura, nao da pra alterar de fora
  # eles nao tem _ porque sao atributos normais e publicos, só bloqueamos a escrita com attr_reader
  # @_forca usa _ por convencao para indicar que é um atributo interno (privado ou protected),
  # ou seja, nao deve ser acessado diretamente de fora — equivale ao "protegido" de outras linguagens
  # como precisamos de validacao na forca (nao pode ser negativa), criamos getter e setter manuais
  attr_reader :nome, :origem

  def initialize(nome, origem, forca_inicial)
    @nome = nome
    @origem = origem
    @_forca = forca_inicial
  end

  # GETTER e SETTER manual
  # como @_forca é interno, criamos métodos pra ler e alterar ele de forma controlada
  def forca
    @_forca
  end

  def forca=(novo_valor)
    # o if aqui funciona igual ao de outras linguagens:
    # só atualiza @_forca se novo_valor for um numero positivo
    if novo_valor.is_a?(Numeric) && novo_valor.positive?
      @_forca = novo_valor
    end
  end

  # VIRTUAL (equivalente)
  # método que as classes filhas vao sobrescrever com o comportamento específico delas
  def atacar
    puts "#{nome} desfere um golpe basico!"
  end
end


# HERANÇA + MÓDULOS
# Catador herda de Habitante e inclui os dois módulos
# por isso é obrigado a implementar invocar_espirito
class Catador < Habitante
  include CapacidadeEspirito
  include CapacidadeCombate

  # attr_accessor é como ter attr_reader e attr_writer juntos
  # ou seja, pode ler E alterar o utensilio de fora da classe
  # usamos accessor aqui porque o Catador pode trocar de Utensilio
  attr_accessor :utensilio

  # SUPER
  # super chama o initialize do pai (Habitante) passando os dados que ele precisa
  # depois inicializamos o que é especifico do Catador
  def initialize(nome, origem, forca_inicial, nome_utensilio, tipo_utensilio)
    super(nome, origem, forca_inicial)

    # COMPOSIÇÃO em ação
    # Utensilio é criado aqui dentro, ele pertence ao Catador
    @utensilio = Utensilio.new(nome_utensilio, tipo_utensilio)
    puts "#{nome} surge com o #{@utensilio.nome}."
  end

  # SOBRESCRITA (override)
  # reescreve o atacar() com o comportamento especifico do Catador
  def atacar
    puts "#{nome} ataca usando o utensilio #{@utensilio.nome}!"
    puts "  [Tipo: #{@utensilio.tipo}]"
  end

  # IMPLEMENTAÇÃO DO MÓDULO
  # aqui a classe cumpre o contrato do CapacidadeEspirito
  # sem isso o Ruby lanca um NotImplementedError
  def invocar_espirito
    puts "#{nome} invoca o espirito do utensilio! Poder: #{forca * 5}"
  end
end


# AGREGAÇÃO
# GrupoDeZeladores guarda referências pra Catadores que ja existiam fora dele
# se o grupo sumir, os catadores continuam existindo
class GrupoDeZeladores
  attr_accessor :catadores

  def initialize(catadores = [])
    @catadores = catadores
  end

  def iniciar_missao(missao)
    puts "\n--- Grupo iniciando missao: #{missao} ---"

    # POLIMORFISMO EM AÇÃO
    # o mesmo .atacar() para todos, mas cada um executa o seu proprio
    @catadores.each do |c|
      c.atacar
    end
  end

  def listar_combatentes
    puts "\n--- Zeladores prontos para combate ---"

    # INTROSPECÇÃO com .select
    # .select percorre o array e retorna só os elementos que passam na condição do bloco
    # o bloco é o trecho entre {} — é como um mini-método anonimo executado pra cada elemento
    # |c| é a variavel temporaria que representa o elemento atual da iteracao
    # as barras || sao a sintaxe do Ruby pra declarar esse parametro do bloco, igual um (c) em outras linguagens
    # respond_to?(:entrar_em_combate) verifica se o objeto c possui aquele método disponivel
    combatentes = @catadores.select { |c| c.respond_to?(:entrar_em_combate) }
    combatentes.each(&:entrar_em_combate)
  end
end


# --- MAIN ---

rudo   = Catador.new("Rudo",   "Aoiro",      1000, "Luvas",           "Ofensiva")
enjin  = Catador.new("Enjin",  "Abismo",     1200, "Guarda-chuva",   "Velocidade")
barado = Habitante.new("Barado", "Superficie", 800)

rudo.forca = 1100
puts "\nRudo agora tem #{rudo.forca} de forca."

puts "\n--- Habilidades ---"
rudo.invocar_espirito
rudo.entrar_em_combate
# barado.invocar_espirito <- daria erro, Habitante nao inclui o modulo

puts "\nComposicao:"
puts rudo.utensilio.mostrar_detalhes

# agregação: catadores ja existiam antes do grupo ser criado
grupo = GrupoDeZeladores.new([rudo, enjin])

grupo.iniciar_missao("Recuperar utensilios perdidos no Abismo")
grupo.listar_combatentes