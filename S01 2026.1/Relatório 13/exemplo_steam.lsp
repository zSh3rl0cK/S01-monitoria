; Em LISP, ponto e vírgula inicia um comentário — equivale ao # do Python ou ao // do JavaScript
; diferente de outras linguagens, LISP é totalmente baseado em listas entre parênteses
; a estrutura básica é sempre: (operador argumento1 argumento2 ...)


; 1. ESTRUTURA DE DADOS com defstruct
; defstruct cria um tipo com campos nomeados — equivale ao data do Haskell ou class em OO
; automaticamente gera funções de acesso no formato: nome-do-tipo-campo
; ex: (jogo-titulo j) acessa o campo titulo de um jogo j
(defstruct jogo
  titulo
  tipo        ; "Acao", "RPG", "Simulacao", "Survival", "Luta"
  preco
  forca-magica)


; 2. VARIÁVEL GLOBAL com defparameter
; defparameter define uma variável global
; equivale ao let do JavaScript no escopo global ou a uma variável de módulo no Python
; a convenção *asteriscos* indica que é uma variável global em LISP
(defparameter *catalogo*
  ; list cria uma lista — equivale ao [] do Python ou ao [] do Ruby
  (list
    (make-jogo :titulo "Persona 3 Reload"   :tipo "RPG"       :preco 299.90 :forca-magica 95)
    (make-jogo :titulo "Cyberpunk 2077"     :tipo "Acao"      :preco 149.90 :forca-magica 70)
    (make-jogo :titulo "Danganronpa"        :tipo "Luta"      :preco 89.90  :forca-magica 85)
    (make-jogo :titulo "Stardew Valley"     :tipo "Simulacao" :preco 37.90  :forca-magica 40)
    (make-jogo :titulo "Until Then"         :tipo "RPG"       :preco 49.90  :forca-magica 60)
    (make-jogo :titulo "Overwatch"          :tipo "Acao"      :preco 0.00   :forca-magica 75)
    (make-jogo :titulo "Hollow Knight"      :tipo "Survival"  :preco 29.90  :forca-magica 88)
    (make-jogo :titulo "Dark Souls 3"       :tipo "Survival"  :preco 119.90 :forca-magica 99)))


; 3. FUNÇÃO PURA com defun
; defun define uma função — equivale ao def do Python ou ao def do Ruby
; a estrutura é: (defun nome-da-funcao (parametros) corpo)
; função pura — depende só dos argumentos, sem acessar nada de fora
(defun adiciona-imposto (preco)
  ; * é a multiplicação em LISP — operadores sempre vêm antes dos operandos
  ; isso se chama notação prefixa: (operador a b) ao invés de a operador b
  (* preco 1.15))


; 4. CONDICIONAL com cond
; cond é o equivalente ao if/else if/else de outras linguagens
; cada cláusula é uma lista: (condição resultado)
; T no final equivale ao else — sempre verdadeiro, é o caso padrão
(defun bonus-maldicao (forca)
  (cond
    ; > é maior que - também em notação prefixa: (> forca 80) equivale a forca > 80
    ((> forca 80) (* forca 1.5))
    ; T é o else — se nenhuma condição anterior for verdadeira, cai aqui
    (T forca)))


; 5. FILTRAGEM com remove-if-not
; remove-if-not mantém só os elementos que passam na condição — equivale ao filter do Haskell
; recebe uma função (ou lambda) e uma lista
; string= compara strings em LISP — equivale ao == para strings
(defun filtra-jogos-acao (catalogo)
  (remove-if-not
    ; lambda define uma função anônima — equivale ao \ do Haskell ou lambda do Python
    ; a estrutura é: (lambda (parametro) corpo)
    (lambda (j) (string= (jogo-tipo j) "Acao"))
    catalogo))


; 6. TRANSFORMAÇÃO com mapcar
; mapcar aplica uma função em cada elemento de uma lista — equivale ao map do Haskell
; devolve uma nova lista com os resultados, sem modificar a original
(defun aplica-imposto-lista (catalogo)
  (mapcar
    (lambda (j)
      ; make-jogo cria um novo jogo com os campos atualizados
      ; :campo valor é a sintaxe de keyword arguments — equivale ao nome: valor do Ruby
      (make-jogo
        :titulo       (jogo-titulo j)
        :tipo         (jogo-tipo j)
        :preco        (adiciona-imposto (jogo-preco j))
        :forca-magica (jogo-forca-magica j)))
    catalogo))


; 7. COMPOSIÇÃO DE FUNÇÕES com let*
; processa-venda encadeia filter e map em sequência
; primeiro filtra, depois transforma — sem modificar o catálogo original
(defun processa-venda (catalogo)
  (let*
    ; let* define variáveis locais em sequência — cada uma pode usar as anteriores
    ; equivale ao let do JavaScript dentro de uma função
    ((jogos-acao  (filtra-jogos-acao catalogo))
     (com-imposto (aplica-imposto-lista jogos-acao))
     ; mapcar aqui aplica bonus-maldicao na forca-magica de cada jogo filtrado
     (com-bonus   (mapcar
                    (lambda (j)
                      (make-jogo
                        :titulo       (jogo-titulo j)
                        :tipo         (jogo-tipo j)
                        :preco        (jogo-preco j)
                        :forca-magica (bonus-maldicao (jogo-forca-magica j))))
                    com-imposto)))
    ; o último valor de um let* é o que a função devolve
    com-bonus))


; 8. FORMATAÇÃO com format
; format é o printf do LISP
; t significa que a saída vai pro terminal (equivale ao stdout)
; ~a insere um valor na string — equivale ao {} do Python ou ao #{} do Ruby
; ~,2f formata um número decimal com 2 casas — equivale ao %.2f do Python
; ~% é uma quebra de linha
(defun exibe-jogo (j)
  (format NIL "~a | Preco: R$~,2f | Forca Magica: ~,1f"
    (jogo-titulo j)
    (jogo-preco j)
    (jogo-forca-magica j)))


; 9. FUNÇÃO PRINCIPAL e DOLIST
; defun main é a convenção — em LISP nao existe um main obrigatório como em outras linguagens
; chamamos ela explicitamente no final do arquivo
(defun main ()

  (format t "~%--- Catalogo Completo ---~%")
  ; dolist percorre uma lista — equivale ao for..in do Python ou ao .each do Ruby
  ; a estrutura é: (dolist (variavel lista) corpo)
  (dolist (j *catalogo*)
    (format t "~a~%" (exibe-jogo j)))

  (format t "~%--- Jogos de Acao com Imposto e Bonus ---~%")
  (let ((resultado (processa-venda *catalogo*)))
    (dolist (j resultado)
      (format t "~a~%" (exibe-jogo j)))))


; chamada explícita da função principal
; em LISP o arquivo é executado de cima pra baixo,
; então a main precisa ser chamada depois de todas as definições
(main)