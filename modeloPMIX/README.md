# Aplicação de algoritmo de otimização em ferramenta computacional de modelos estocásticos de vazões
**Projeto de Iniciação Científica 2018/2 - 2019/1**

Algoritmos de otimização são utilizados em diversas áreas do conhecimento, com finalidade de alcançar soluções ótimas para determinado problema, por meio de iterações em códigos computacionais. O uso desse tipo de técnica para estimar parâmetros de modelos estocásticos de séries de vazões pode contribuir significativamente para gestão e planejamento de recursos hídricos, visto que possibilita uma melhor reprodutibilidade de propriedades estatísticas de séries temporais estudadas. Nesse sentido, busca-se identificar e implementar algoritmo de otimização para estimativa dos parâmetros de modelos PARMA multiplicativos (PMIX), que serão usados para geração de séries sintéticas de vazões mensais, com finalidade de estimativa de volumes de reservatórios de regularização de vazões. Os modelos serão implementados no software gratuito R e os resultados serão comparados, em função de tempos de execução dos modelos e de erros médios padrão e de médias absolutas de erros percentuais.

#### Algoritmos utilizados

  - Método de Powell;
  - Algoritmo Evolucionário Multiobjetivo baseado na técnica _fast Nondominated Sorting Genetic Algorithm_ (NSGA-II), em conjunto com o Método de Powell;

> Para utilização, é necessária a obtenção da série sintética a ser analisada e um mínimo domínio da linguagem R.
