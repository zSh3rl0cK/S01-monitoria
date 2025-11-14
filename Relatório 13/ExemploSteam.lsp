;; ===========================================================
;; PROGRAMA EM COMMON LISP: Catalogo de Jogos (Estilo Steam)
;; ===========================================================
;; Demonstracao de:
;; - Estruturas de dados (defstruct)
;; - Funcoes puras e recursivas
;; - Funcoes de alta ordem (mapcar, lambda)
;; - Processamento de listas e filtros funcionais
;; ===========================================================


;; -----------------------------------------------------------
;; 1. ESTRUTURA DE DADOS E LISTA INICIAL
;; -----------------------------------------------------------

;; Define uma nova estrutura (tipo de dado) chamada 'jogo'.
;; Isso e equivalente a uma classe simples com atributos:
;; titulo, preco, tags e avaliacao.
(defstruct jogo
  titulo      ;; titulo do jogo (string)
  preco       ;; preco em BRL (numero)
  tags        ;; lista de strings (ex: '("RPG" "Aventura"))
  avaliacao)  ;; nota de avaliacao (numero entre 0 e 1)


;; Cria uma variavel global *catalogo-steam* contendo
;; uma lista de objetos 'jogo'. O prefixo *nome* e convencao
;; para variaveis globais em Common Lisp.
(defvar *catalogo-steam*
  (list
   (make-jogo :titulo "Cyberpunk 2077" :preco 199.99 :tags '("RPG" "Ficcao Cientifica") :avaliacao 0.82)
   (make-jogo :titulo "Elden Ring" :preco 249.99 :tags '("RPG" "Acao") :avaliacao 0.95)
   (make-jogo :titulo "Portal 2" :preco 36.99 :tags '("Puzzle" "Aventura") :avaliacao 0.89)
   (make-jogo :titulo "Hades" :preco 73.99 :tags '("Roguelike" "Acao") :avaliacao 0.94)
   (make-jogo :titulo "Stardew Valley" :preco 19.99 :tags '("RPG" "Simulacao") :avaliacao 0.97)
   (make-jogo :titulo "Persona 3 Reload" :preco 229.99 :tags '("RPG" "Strategy") :avaliacao 0.99)))


;; -----------------------------------------------------------
;; 2. FUNCOES PURAS (sem efeitos colaterais)
;; -----------------------------------------------------------

;; Funcao que aplica um desconto percentual sobre um preco.
;; O 'let' cria uma variavel local (como 'var' ou 'let' em outras linguagens).
(defun aplicar-desconto (preco percentual)
  (let ((desconto (* preco percentual)))  ;; calcula o valor do desconto
    (- preco desconto)))                  ;; retorna o preco final

;; Formata um objeto 'jogo' em string, exibindo titulo e preco.
;; 'format' e como 'printf' em C ou 'f-string' em Python.
(defun formatar-titulo (jogo)
  (format nil "~a (~,2f BRL)"
          (jogo-titulo jogo)   ;; acessa o campo 'titulo'
          (jogo-preco jogo)))  ;; acessa o campo 'preco'



;; -----------------------------------------------------------
;; 3. FUNCOES DE ALTA ORDEM (HOF)
;; -----------------------------------------------------------
;; Funcoes que retornam ou recebem outras funcoes.

;; Cria uma funcao anonima (lambda) que verifica se o jogo
;; custa menos ou igual ao preco limite.
;; resultado sera uma funcao que pode ser passada para outros comandos
;; como remove-if-not - remove os elementos que NAO satisfazem a condicao
(defun filtro-preco-maximo (preco-limite)
  (lambda (jogo)
    (<= (jogo-preco jogo) preco-limite)))

;; Aplica um desconto percentual a todos os jogos do catalogo.
;; 'mapcar' aplica uma funcao a cada elemento da lista e retorna
;; uma nova lista transformada (como 'map' em Python).
(defun promocao-global (catalogo percentual)
  (mapcar
   (lambda (jogo)
     ;; Copiamos todos os campos do jogo original
     ;; Mas aplicamos o desconto ao preco com aplicar-desconto:
     (make-jogo :titulo (jogo-titulo jogo)
                :preco (aplicar-desconto (jogo-preco jogo) percentual)
                :tags (jogo-tags jogo)
                :avaliacao (jogo-avaliacao jogo)))
   catalogo))
;; No fim, mapcar devolve uma nova lista de jogos, todos com o preco atualizado
;; sem modificar o catalogo original (imutabilidade funcional).



;; -----------------------------------------------------------
;; 4. PROCESSAMENTO DE DADOS
;; -----------------------------------------------------------

;; Busca jogos por genero e preco maximo.
(defun buscar-e-processar (catalogo genero max-preco)
  ;; Print: mostrar catalogo de entrada (apenas titulos) - visao geral
  (format t "~%[DEBUG] Executando buscar-e-processar para genero='~a' max-preco=~,2f~%" genero max-preco)
  (format t "~%[DEBUG] Catalogo (titulos): ~a~%" (mapcar #'jogo-titulo catalogo))

  (let ((jogos-filtrados-por-genero
          ;; 'remove-if-not' mantem apenas os jogos que possuem a tag desejada.
          ;; 'member' verifica se um elemento esta presente em uma lista.
          (remove-if-not
           (lambda (jogo)
            ;; verificar se o genero procurado esta entre as tags do jogo
            ;; :test diz que a comparacao deve ser feita entre strings, e nao por referencia.
             (member genero (jogo-tags jogo) :test #'string-equal))
           catalogo)))
    ;; Print: apos filtro por genero
    (format t "~%[DEBUG] Apos filtro por genero '~a' (titulos): ~a~%"
            genero (mapcar #'jogo-titulo jogos-filtrados-por-genero))

    ;; resultado - uma lista com apenas os jogos do genero desejado.
    (let ((jogos-dentro-do-orcamento
            ;; filtra novamente usando a funcao retornada por filtro-preco-maximo
            ;; devolve uma funcao lambda criada com o valor max-preco.
            (remove-if-not (filtro-preco-maximo max-preco) jogos-filtrados-por-genero)))
      ;; Print: apos filtro por preco
      (format t "~%[DEBUG] Apos filtro por preco <= ~,2f (titulos): ~a~%"
              max-preco (mapcar #'jogo-titulo jogos-dentro-do-orcamento))

      ;; Resultado: agora temos so os jogos do genero certo e dentro do orcamento.
      ;; aplica essa funcao a cada jogo, formata o resultado final em strings 
      (let ((resultado-formatado (mapcar #'formatar-titulo jogos-dentro-do-orcamento)))
        ;; Print: mostrar cada item formatado (um por linha) - ideal para aula
        (format t "~%[DEBUG] Resultado formatado final:~%")
        (dolist (linha resultado-formatado)
          (format t "  ~a~%" linha))
        ;; Retornar a lista de strings formatadas (comportamento original)
        resultado-formatado))))



;; -----------------------------------------------------------
;; 5. RECURSAO (em vez de loops)
;; -----------------------------------------------------------

;; Soma todas as avaliacoes dos jogos.
(defun somar-avaliacoes (catalogo)
  (cond
    ;; Caso base: lista vazia -> soma 0
    ((null catalogo) 0)
    ;; Caso recursivo: soma a avaliacao do primeiro jogo (car)
    ;; com a soma das avaliacoes do resto (cdr)
    (t (+ (jogo-avaliacao (car catalogo))
          (somar-avaliacoes (cdr catalogo))))))


;; lambda = funcao "de bolso" (criada na hora);
;; mapcar = percorre listas aplicando transformacoes;
;; remove-if-not = faz filtragem;
;; Tudo funciona encadeado sem precisar de for ou if.

;; -----------------------------------------------------------
;; 6. DEMONSTRACAO (Saidas de exemplo)
;; -----------------------------------------------------------

(format t "~%--- CATALOGO ORIGINAL ---~%")
(mapcar #'jogo-titulo *catalogo-steam*)  ;; lista apenas os titulos

;; Mostrar catalogo original com titulos e precos (mais detalhado)
(format t "~%--- CATALOGO ORIGINAL (DETALHADO) ---~%")
(dolist (j *catalogo-steam*)
  (format t "  ~a - ~,2f BRL - tags: ~a - aval: ~,2f~%"
          (jogo-titulo j) (jogo-preco j) (jogo-tags j) (jogo-avaliacao j)))

(format t "~%--- DEMO 1: APLICANDO PROMOCAO (10%% OFF) ---~%")
(defvar *catalogo-promocional* (promocao-global *catalogo-steam* 0.10))

;; Mostrar comparacao antes/depois (titulos e precos)
(format t "~%[DEMO] Antes (titulos e preco):~%")
(dolist (j *catalogo-steam*)
  (format t "  ~a - ~,2f BRL~%" (jogo-titulo j) (jogo-preco j)))
(format t "~%[DEMO] Depois da promocao (titulos e preco):~%")
(dolist (j *catalogo-promocional*)
  (format t "  ~a - ~,2f BRL~%" (jogo-titulo j) (jogo-preco j)))

(format t "~%--- DEMO 2: BUSCANDO JOGOS DE 'RPG' ABAIXO DE 150 BRL ---~%")
;; Chama a funcao que ja imprime passo-a-passo
(buscar-e-processar *catalogo-steam* "RPG" 150.00)

(format t "~%--- DEMO 3: SOMANDO AVALIACOES USANDO RECURSAO ---~%")
(defvar soma-total (somar-avaliacoes *catalogo-steam*))
(format t "Soma Total das Avaliacoes: ~,2f~%" soma-total)
(defvar media-avaliacao (/ soma-total (length *catalogo-steam*)))
(format t "Media das Avaliacoes: ~,2f~%" media-avaliacao)

;; -----------------------------------------------------------
;; FUNÇÃO: filtro-preco-maximo
;;
;; Esta função é um exemplo clássico da programação funcional:
;; ela NÃO devolve um número ou uma informação simples.
;; Em vez disso, ela devolve OUTRA FUNÇÃO.
;;
;; O parâmetro 'preco-limite' é usado para montar uma função
;; anônima (lambda) que testa se um jogo é barato o suficiente.
;;
;; Ou seja, chamar (filtro-preco-maximo 100) retorna uma função
;; equivalente a:
;;    (lambda (jogo) (<= (jogo-preco jogo) 100))
;;
;; Essa função retornada será usada depois como um "filtro",
;; passada para comandos como 'remove-if-not', que percorrem a
;; lista e mantêm somente os jogos que satisfazem a condição.
;;
;; Pense nisso como criar uma "ferramenta de filtragem".
;; Ela só funciona quando combinada com outra função que processa
;; listas.
;;
;; 'lambda' em Lisp é como as lambdas do Python — funções criadas
;; na hora, sem nome, retornadas como valores.
;;
;; O resultado desta função é um teste booleano:
;;    T (verdadeiro) se o jogo é <= preco-limite
;;    NIL (falso) caso contrário.
;; -----------------------------------------------------------

;; -----------------------------------------------------------
;; FUNÇÃO: promocao-global
;;
;; O objetivo desta função é aplicar um desconto a TODOS os jogos
;; de um catálogo. Ela segue o estilo funcional: não altera a
;; lista original, mas cria uma NOVA LISTA transformada.
;;
;; Aqui entra a função 'mapcar', que funciona como o 'map' do Python:
;; ela percorre cada elemento da lista e aplica uma função sobre ele.
;;
;; A função aplicada em cada item é um 'lambda' (função anônima)
;; que recebe um único parâmetro: 'jogo'.
;;
;; Dentro desse lambda, criamos uma NOVA estrutura 'jogo' usando
;; (make-jogo ...). Isso garante que o catálogo original permaneça
;; imutável — um conceito importante em programação funcional.
;;
;; Cada novo jogo recebe:
;; - mesmo título
;; - PREÇO DESCONTADO (via aplicar-desconto)
;; - mesmas tags
;; - mesma avaliação
;;
;; Resultado final:
;;   Uma lista NOVA, com todos os jogos atualizados com o desconto.
;;
;; Metáfora para alunos:
;;   Pense em uma ESTEIRA DE PRODUÇÃO:
;;   cada jogo entra, passa pelo processador de desconto,
;;   e sai com um NOVO preço — sem alterar o original.
;; -----------------------------------------------------------


;; -----------------------------------------------------------
;; FUNÇÃO: buscar-e-processar
;;
;; Esta função demonstra um PIPELINE de transformação funcional:
;; ela filtra um catálogo em estágios, encadeando filtros e
;; transformações sem loops, sem variáveis mutáveis e sem ifs
;; espalhados.
;;
;; São três etapas principais:
;;
;; 1. FILTRAR por gênero:
;;    Usamos 'remove-if-not', que mantém apenas os elementos para
;;    os quais o predicado (função de teste) retorna verdadeiro.
;;
;;    O predicado é um lambda que verifica:
;;       (member genero (jogo-tags jogo))
;;    Usamos :test #'string-equal para comparar strings de forma
;;    case-insensitive.
;;
;; 2. FILTRAR por preço:
;;    Aqui entra a função 'filtro-preco-maximo'.
;;    Ela retorna um lambda especial que compara preços.
;;
;;    'remove-if-not' é novamente aplicado, agora usando este
;;    predicado retornado pela outra função.
;;
;;    Ou seja:
;;      primeiro filtramos por gênero,
;;      depois filtramos o resultado pelo preço.
;;
;;    Isso cria um verdadeiro "funil" de filtragem.
;;
;; 3. FORMATAÇÃO para a saída:
;;    Após encontrar os jogos adequados, usamos:
;;       (mapcar #'formatar-titulo lista)
;;
;;    Aqui NÃO usamos lambda — apenas passamos a função já existente.
;;    O resultado final é uma lista de STRINGS prontas para
;;    exibição, como:
;;       "Hades (73.99 BRL)"
;;
;; Metáfora para ensino:
;;   Imagine um FUNIL de processamento:
;;
;;   Catálogo completo
;;        ↓ (filtro de gênero)
;;   Jogos do gênero desejado
;;        ↓ (filtro de preço)
;;   Jogos dentro do orçamento
;;        ↓ (formatação)
;;   Lista de strings final para o usuário.
;;
;; Esta função é perfeita para ensinar o estilo funcional:
;; dados entram, passam por várias transformações puras,
;; e o resultado final sai sem mutações no caminho.
;; -----------------------------------------------------------
