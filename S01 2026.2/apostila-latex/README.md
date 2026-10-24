# Capítulos da apostila (LaTeX)

Fontes LaTeX dos capítulos da `Apostila_S01__OO_PF.pdf`. A apostila é compilada no Overleaf (pdfLaTeX, com `babel` em português), onde ficam o arquivo principal, o preâmbulo e o capítulo do Relatório 6.

| Arquivo | Capítulo | Linguagem |
|---------|----------|-----------|
| `relatorio7.tex` | 2 | C# |
| `relatorio8.tex` | 3 | Python |
| `relatorio9.tex` | 4 | Ruby |
| `relatorio10.tex` | 5 | JavaScript |
| `relatorio11.tex` | 6 | Java |

Cada arquivo entra no arquivo principal com `\input{relatorioN}`, nessa ordem. O `relatorio7.tex` cria o ambiente `exercicio` e a cor `azulRelatorio`. Os capítulos seguintes só os recriam quando são compilados sozinhos.

**Atenção:** o `relatorio7.tex` desta pasta traz os exercícios iguais aos dos slides de 2026.2 (`Relatório 07/poo_csharp.pdf`). A apostila compilada até 24/10/2026 ainda tinha a versão anterior desses exercícios. Troque o capítulo no Overleaf, ou só a seção "Exercícios Práticos do Módulo", se o resto do capítulo tiver sido editado lá.

Quando um capítulo for alterado no Overleaf, atualize também o arquivo desta pasta.
