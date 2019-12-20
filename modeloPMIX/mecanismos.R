# MECANISMOS DO ALGORITMO GENETICO
#
# populacao: Lista de saida da InicializaPopulacao, contendo os individuos, uma serie sintetica gerada e suas avaliacoes

# CLASSIFICACAO DA POPULACAO POR NIVEIS DE DOMINANCIA DE PARETO
#**UTILIZA CLUSTERIZACAO**
FNS = function (populacao) {
  npIndividual = parLapply (cl, populacao, function (x)
                                           unlist (dominanciaCompleta (x, populacao)))
  
  np = unlist (lapply (npIndividual, sum))
  np = np - 1
  
  return (np)
}

# ANALISA SE UM TERMO DOMINA EM PELO MENOS 4 AVALIACOES SOBRE OUTRO
dominanciaCompleta = function (termo, populacao) {
  lapply (populacao, function (x)
                     if (
			 ((termo$avaliacao$media >= x$avaliacao$media) && (termo$avaliacao$dp >= x$avaliacao$dp) &&
                          (termo$avaliacao$facAnual >= x$avaliacao$facAnual) && (termo$avaliacao$facMensal >= x$avaliacao$facMensal)) ||
                         ((termo$avaliacao$media >= x$avaliacao$media) && (termo$avaliacao$dp >= x$avaliacao$dp) &&
                          (termo$avaliacao$facAnual >= x$avaliacao$facAnual) && (termo$avaliacao$somRes >= x$avaliacao$somRes)) ||
                         ((termo$avaliacao$media >= x$avaliacao$media) && (termo$avaliacao$dp >= x$avaliacao$dp) && (termo$avaliacao$facMensal >= x$avaliacao$facMensal) &&
                          (termo$avaliacao$somRes >= x$avaliacao$somRes)) ||
                         ((termo$avaliacao$media >= x$avaliacao$media) && (termo$avaliacao$facAnual >= x$avaliacao$facAnual) &&
			  (termo$avaliacao$facMensal >= x$avaliacao$facMensal) && (termo$avaliacao$somRes >= x$avaliacao$somRes)) ||
                         ((termo$avaliacao$dp >= x$avaliacao$dp) && (termo$avaliacao$facAnual >= x$avaliacao$facAnual) &&
			  (termo$avaliacao$facMensal >= x$avaliacao$facMensal) && (termo$avaliacao$somRes >= x$avaliacao$somRes))
			)
                     return (1)
    
                     else
                     return (0))
}

# ORDENA OS INDIVIDUOS DENTRO DAS FRONTEIRAS
CDA = function (populacao) {
  nINDIVIDUO = length (populacao[[1]]$individuo)
  n = length (populacao)
  
  if (n > 2) {
    avaliacoes = sapply (populacao, function (x) x$avaliacao)
    avaliacoes = matrix (as.numeric (avaliacoes), ncol = n)
    p = 1:((length (avaliacoes)) / n)
    distancias = t (sapply (p, function (x)
                               distancia (avaliacoes[x, ], n)))
    
    diversidade = apply (distancias, 2, sum)
    diversidade = order (diversidade, decreasing = T)
  }
  else
    diversidade = 1:n
  
  return (diversidade)
}

# CALCULO DA DISTANCIA ENTRE SOLUCOES
distancia = function (avaliacoes, n) {
  
  dist = numeric (n)
  ord = order (avaliacoes)
  avMax = max (avaliacoes)
  avMin = min (avaliacoes)
  
  for (i in (2:(n-1))) {
    dist[ord[i]] = dist[ord[i-1]] + ((avaliacoes[ord[i+1]] - avaliacoes[ord[i-1]]) / (avMax - avMin))
  }
  
  dist[ord[1]] = dist[ord[n]] = Inf
  return (dist)
}

# ORDENAMENTO DA POPULACAO
#**UTILIZA CLUSTERIZACAO**
CCO = function (populacao) {
  np = FNS (populacao)
  p = sort (unique (np))
  fronteiras = lapply (p, function (x)
                          which (np %in% x))
  populacaoFronteiras = parLapply (cl, fronteiras, function (x)
                                                   populacao[x])
  diversidadeFronteiras = parLapply (cl, populacaoFronteiras, CDA)
  p = 1:length (p)
  diversidade = sapply (p, function (x)
                           fronteiras[[x]][diversidadeFronteiras[[x]]])
  diversidade = unlist (diversidade)
  
  populacao = populacao[diversidade]
  return  (populacao)
}
