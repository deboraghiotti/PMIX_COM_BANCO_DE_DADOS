source ('avaliacao.R')

# INICIALIZACAO DA POPULACAO E GERACAO DE INDIVIDUOS
# entrada: Saida da funcao entrada
# nSINTETICA: Variavel global com o tamanho da serie sintetica

# GERACAO DE UM INIDIVIDUO ALEATORIAMENTE
geraIndividuo = function (entrada, lags) {
  nINDIVIDUO = sum (lags) * 12
  individuoMIN = -1
  individuoMAX = 1
  individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
  momentos = momentos (entrada, individuo, lags, nSINTETICA)
  
  # ENQUANTO AS AVALICOES ESTOURAREM, E PROCURADO OYUTRO INDIVIDUO
  while (estouro (momentos)) {
    individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
    momentos = momentos (entrada, individuo, lags, nSINTETICA)
  }
  avaliacao = avaliacao (entrada, momentos)
  
  final = list (individuo = individuo, serie = momentos$serie, avaliacao = avaliacao)
  return (final)
}

# AVALIACAO DE UM INDIVIDUO JA OBTIDO
avaliaIndividuo = function (entrada, lags, individuo) {
  momentos = momentos (entrada, individuo, lags, nSINTETICA)
  if (estouro (momentos))
    return (NULL)
  
  avaliacao = avaliacao (entrada, momentos)
  
  final = list (individuo = individuo, serie = momentos$serie, avaliacao = avaliacao)
  return (final)
}

# GERACAO DA POPULACAO
#**UTILIZA CLUSTERIZACAO**
geraPopulacao = function (entrada, lags, parametrosIniciais, nP) {
  populacao = list ()
  
  # CASO NAO HAJA PARAMETROS INICIAIS, SAO GERADOS ALEATORIAMENTE
  if (is.null (parametrosIniciais)) {
    p = 1:nP
    populacao = lapply (p, function (x)
                           geraIndividuo (entrada, lags))
  }
  
  # CASO CONTRARIO, SAO OBTIDAS AS AVALIACOES
  else {
    p = 1:((length(parametrosIniciais)) / (12*(sum (lags))))
    populacao = parLapply (cl, p, function (x)
                                  avaliaIndividuo (entrada, lags, parametrosIniciais[x, ]))
    # SAO RETIRADOS OS QUE HOUVERAM ESTOURO
    populacao = populacao[lengths(populacao) != 0]
    
    # CASO NAO SEJAM O BASTANTE, OS DEMAIS SAO GERADOS POR CROSSOVER
    if ((length (populacao)) < nP) {
      print ("gerando crossover entre os individuos...")
      n = nP - (length (populacao))
      populacao = completaPopulacao (entrada, lags, populacao, n)
    }
  }
  
  print ("populacao completa!")
  return (populacao)
}

completaPopulacao = function (entrada, lags, populacao, n) {
  p = 1:n
  
  populacaoRestante = list ()
  populacaoRestante = lapply (p, function (x)
                                 cruzamentoBLX (entrada, lags, populacao, 1, -1))
  
  populacaoFinal = c (populacao, populacaoRestante)
  return (populacaoFinal)
}

# CRUZAMENTO BLX ENTRE OS INDIVIDUOS
#**UTILIZA CLUSTERIZACAO**
geraCruzamento = function (entrada, lags, populacao, Pc, Pm, nP) {
  p = 1:nP
  
  # E GERADA UMA NOVA POPULACAO ATRAVES DOS OPERADORES DE CRUZAMENTO
  novaPopulacao = list ()
  novaPopulacao = parLapply (cl, p, function (x)
                                    cruzamentoBLX (entrada, lags, populacao, Pc, Pm))
  
  return (novaPopulacao)
}

# CRUZAMENTO COM UMA PROBABILIDADE Pc DE CRUZAMENTO E Pm DE MUTACAO
cruzamentoBLX = function (entrada, lags, populacao, Pc, Pm) {
  nINDIVIDUO = sum (lags) * 12
  
  if ((runif (1, 0, 1)) < Pm) {
    individuo = geraIndividuo (entrada, lags)
    return (individuo)
  }
    
  ALFA = 0.5
  beta = runif (nINDIVIDUO, -ALFA, 1 + ALFA)
  homog = runif (nINDIVIDUO, 0, 1) <= Pc
  
  pais = torneio (3, length (populacao))
  pai1 = populacao[[pais[1]]]$individuo
  pai2 = populacao[[pais[2]]]$individuo
  
  filho = pai1 + homog*beta*(pai2 - pai1)
  momentos = momentos (entrada, filho, lags, nSINTETICA)
  
  while (estouro (momentos)) {
    pais = torneio (3, length (populacao))
    pai1 = populacao[[pais[1]]]$individuo
    pai2 = populacao[[pais[2]]]$individuo
    
    filho = pai1 + homog*beta*(pai2 - pai1)
    momentos = momentos (entrada, filho, lags, nSINTETICA)
  }
  avaliacao = avaliacao (entrada, momentos)
  
  final = list (individuo = filho, serie = momentos$serie, avaliacao = avaliacao)
  return (final)
}

# ESCOLHA DOS INDIVIDUOS POR TORNEIO
torneio = function (nPossibilidades, n) {
  nPossibilidades = min (nPossibilidades, n)
  possiveis = sample (n, nPossibilidades, replace = F)
  possiveis = sort (possiveis)
  possiveis = possiveis[1:2]
  
  return (possiveis)
}