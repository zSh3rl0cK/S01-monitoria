# Capítulos da apostila (LaTeX)

Fontes LaTeX dos capítulos da `Apostila_S01__OO_PF.pdf`. A apostila é compilada no Overleaf (pdfLaTeX, com `babel` em português), onde ficam o arquivo principal, o preâmbulo e o capítulo do Relatório 6.

| Arquivo | Capítulo | Linguagem |
|---------|----------|-----------|
| `relatorio6-exercicios.tex` | 1 (só a seção de exercícios) | C++ |
| `relatorio7.tex` | 2 | C# |
| `relatorio8.tex` | 3 | Python |
| `relatorio9.tex` | 4 | Ruby |
| `relatorio10.tex` | 5 | JavaScript |
| `relatorio11.tex` | 6 | Java |

Cada arquivo entra no arquivo principal com `\input{relatorioN}`, nessa ordem. O `relatorio7.tex` cria o ambiente `exercicio` e a cor `azulRelatorio`. Os capítulos seguintes só os recriam quando são compilados sozinhos.

**Atenção:** a apostila compilada até 24/10/2026 ainda tinha, nos capítulos 1 e 2, os exercícios anteriores aos slides que os alunos receberam em 2026.2.

- **Capítulo 1 (Rel. 6):** no Overleaf, substitua a seção "Exercícios Práticos do Módulo" do capítulo pelo conteúdo de `relatorio6-exercicios.tex`, que segue `Relatório 06/poo_cpp.pdf`. As caixas usam o ambiente `exercicioCpp`, definido no próprio arquivo, porque o ambiente `exercicio` só é criado no capítulo 2.
- **Capítulo 2 (Rel. 7):** o `relatorio7.tex` traz os exercícios iguais aos de `Relatório 07/poo_csharp.pdf`. Troque o capítulo inteiro, ou só a seção de exercícios, se o resto tiver sido editado no Overleaf.

Quando um capítulo for alterado no Overleaf, atualize também o arquivo desta pasta.
