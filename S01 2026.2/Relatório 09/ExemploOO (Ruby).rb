# ------- Contexto: Gachiakuta ---------
# No mundo de Gachiakuta, a sociedade é dividida entre quem vive nas
# alturas e quem é jogado no Abismo — um lugar repleto de lixo e
# criaturas chamadas Manchas.
# Os Catadores são habitantes do Abismo que aprenderam a dar vida a
# objetos descartados, chamados Utensílios, usando o poder do espírito.
# Organizados em grupos de Zeladores, eles partem em missões para
# sobreviver e recuperar o que foi perdido nas profundezas.


# MÓDULOS (funcionam como interfaces em outras linguagens)
# definem um comportamento que qualquer classe pode "incluir"
# a classe que incluir este módulo deve implementar invocar_espirito;
# o Ruby não confere isso antes: o erro só aparece quando o método
# for chamado em um objeto que não o implementou
module CapacidadeEspirito
  def invocar_espirito
    raise NotImplementedError,
          "#{self.class} deve implementar invocar_espirito"
  end
end

# MÓDULO DE COMBATE
# outro módulo, mas esse já traz a implementação pronta
# qualquer classe que incluir CapacidadeCombate ganha o
# entrar_em_combate() de graça
module CapacidadeCombate
  def entrar_em_combate
    puts "#{self.class} entra em postura de combate!"
  end
end


# COMPOSIÇÃO
# Utensilio não existe sozinho fora do Catador
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
# as classes filhas vão herdar daqui
class Habitante

  # ENCAPSULAMENTO
  # em Ruby, toda variável de instância (as que começam com @) já é
  # inacessível de fora da classe: só dá para lê-la ou alterá-la por
  # meio de métodos
  # attr_reader cria apenas o getter para nome e origem: de fora, eles
  # podem ser lidos, mas não alterados
  # o _ em @_forca não muda nada para o Ruby: é só uma convenção para
  # indicar um atributo interno, que não ganhou attr_reader
  # como precisamos de validação na força (precisa ser um número
  # positivo), criamos getter e setter manuais
  attr_reader :nome, :origem

  def initialize(nome, origem, forca_inicial)
    @nome = nome
    @origem = origem
    @_forca = forca_inicial
  end

  # GETTER e SETTER manual
  # como @_forca é interno, criamos métodos pra ler e alterar ele de
  # forma controlada
  def forca
    @_forca
  end

  def forca=(novo_valor)
    # o if aqui funciona igual ao de outras linguagens:
    # só atualiza @_forca se novo_valor for um número positivo
    if novo_valor.is_a?(Numeric) && novo_valor.positive?
      @_forca = novo_valor
    end
  end

  # VIRTUAL (equivalente)
  # método que as classes filhas vão sobrescrever com o comportamento
  # específico delas
  def atacar
    puts "#{nome} desfere um golpe basico!"
  end
end


# HERANÇA + MÓDULOS
# Catador herda de Habitante e inclui os dois módulos
# por isso deve implementar invocar_espirito (contrato do módulo)
class Catador < Habitante
  include CapacidadeEspirito
  include CapacidadeCombate

  # attr_accessor é como ter attr_reader e attr_writer juntos
  # ou seja, pode ler E alterar o utensílio de fora da classe
  # usamos accessor aqui porque o Catador pode trocar de Utensílio
  attr_accessor :utensilio

  # SUPER
  # super chama o initialize do pai (Habitante) passando os dados que
  # ele precisa
  # depois inicializamos o que é específico do Catador
  def initialize(nome, origem, forca_inicial, nome_utensilio, tipo_utensilio)
    super(nome, origem, forca_inicial)

    # COMPOSIÇÃO em ação
    # Utensilio é criado aqui dentro, ele pertence ao Catador
    @utensilio = Utensilio.new(nome_utensilio, tipo_utensilio)
    puts "#{nome} surge com o #{@utensilio.nome}."
  end

  # SOBRESCRITA (override)
  # reescreve o atacar() com o comportamento específico do Catador
  def atacar
    puts "#{nome} ataca usando o utensilio #{@utensilio.nome}!"
    puts "  [Tipo: #{@utensilio.tipo}]"
  end

  # IMPLEMENTAÇÃO DO MÓDULO
  # aqui a classe cumpre o contrato do CapacidadeEspirito
  # sem isso, chamar invocar_espirito lançaria o NotImplementedError
  # definido no módulo
  def invocar_espirito
    puts "#{nome} invoca o espirito do utensilio! Poder: #{forca * 5}"
  end
end


# AGREGAÇÃO
# GrupoDeZeladores guarda referências pra Catadores que já existiam
# fora dele
# se o grupo sumir, os catadores continuam existindo
class GrupoDeZeladores
  attr_accessor :catadores

  def initialize(catadores = [])
    @catadores = catadores
  end

  def iniciar_missao(missao)
    puts "\n--- Grupo iniciando missao: #{missao} ---"

    # POLIMORFISMO EM AÇÃO
    # o mesmo .atacar() para todos, mas cada um executa o seu próprio
    @catadores.each do |c|
      c.atacar
    end
  end

  def listar_combatentes
    puts "\n--- Zeladores prontos para combate ---"

    # INTROSPECÇÃO com .select
    # .select percorre o array e retorna só os elementos que passam na
    # condição do bloco
    # o bloco é o trecho entre {} — é como um mini-método anônimo
    # executado pra cada elemento
    # |c| é a variável temporária que representa o elemento atual da
    # iteração
    # as barras || são a sintaxe do Ruby pra declarar esse parâmetro do
    # bloco, igual um (c) em outras linguagens
    # respond_to?(:entrar_em_combate) verifica se o objeto c possui
    # aquele método disponível
    combatentes = @catadores.select { |c| c.respond_to?(:entrar_em_combate) }
    combatentes.each(&:entrar_em_combate)
  end
end


# --- MAIN ---

rudo   = Catador.new("Rudo",   "Aoiro",  1000, "Luvas",        "Ofensiva")
enjin  = Catador.new("Enjin",  "Abismo", 1200, "Guarda-chuva", "Velocidade")
barado = Habitante.new("Barado", "Superficie", 800)

rudo.forca = 1100
puts "\nRudo agora tem #{rudo.forca} de forca."

puts "\n--- Habilidades ---"
rudo.invocar_espirito
rudo.entrar_em_combate
# barado.invocar_espirito <- daria NoMethodError: Habitante não inclui
# o módulo CapacidadeEspirito

puts "\nComposicao:"
puts rudo.utensilio.mostrar_detalhes

# agregação: catadores já existiam antes do grupo ser criado
grupo = GrupoDeZeladores.new([rudo, enjin])

grupo.iniciar_missao("Recuperar utensilios perdidos no Abismo")
grupo.listar_combatentes
