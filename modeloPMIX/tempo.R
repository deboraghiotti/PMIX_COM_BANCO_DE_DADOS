source ('powell.R')
source ('inicializaPop.R')
source ('mecanismos.R')

# FUNCAO GERAL DO ALGORITMO GENETICO
#**UTILIZA CLUSTERIZACAO**
# nP: Tamanho da populacao
# Pc: Probabilidade de cruzamento
# Pm: Probabilidade de mutacao
NSGA = function (lags, nP, Pc, Pm, cicloMAX, MAXDiferenca) {
  if (AUTO == F) {
    if (sistema == "Linux")
      dados = tk_choose.files ( )
    else if (sistema == "Windows")
      dados = choose.files ( )
  }
  inicio = format (Sys.time (), "%F %Hh%M")
  
  entrada = entrada (dados)
  print ("Formando populacao inicial...")
  
  # OBTENCAO DOS PONTOS INICIAIS
  saidasPMIX = PMIXs (entrada$serieHN, lags, nP)
  parametrosIniciais = sapply (saidasPMIX, function (x) x$parametros)
  parametrosIniciais = t (parametrosIniciais)

  populacao = geraPopulacao (entrada, lags, parametrosIniciais, nP)
  populacao = CCO (populacao)
  ciclo = 0
  
  # CICLO DO AG
  while ((ciclo < cicloMAX) && (MAPEdiferenca (populacao) > MAXDiferenca)) {
    ciclo = ciclo + 1
    print (ciclo)
    novaPopulalacao = geraCruzamento (entrada, lags, populacao, Pc, Pm, nP)
    populacaoTotal = c (populacao, novaPopulalacao)
    populacaoTotal = CCO (populacaoTotal)
    populacao = populacaoTotal[1:nP]
  }
  
  fim = format (Sys.time (), "%F %Hh%M")
  
  # ESCRITA DOS ARQUIVOS
  diretorio = getwd ( )
  
  estacao = substr (dados, start = 1, stop = (nchar (dados)-4))
  if (! (dir.exists (estacao)))
    dir.create (file.path (estacao))
  setwd (estacao)
  data = format (Sys.time (), "%F %Hh%M")
  ordem = paste0 ("PMIX(", lags[1], ",", lags[2], ",", lags[3], ",", lags[4], ") ", data)
  if (! (dir.exists (ordem)))
    dir.create (file.path (ordem))
  setwd (ordem)
  if (! (dir.exists ("Series")))
    dir.create (file.path ("Series"))
  
  inicio = paste ("INICIO:", inicio)
  fim = paste ("FIM:", fim)
  
  sink ("TempoExecucao.txt")
  print (inicio)
  print (fim)
  print (paste ("CICLOS:", ciclo))
  sink ( )
  
  arquivoParametrosIniciais (saidasPMIX, lags)
  arquivoParametros (populacao, lags)
  arquivoAvaliacoes (populacao)
  arquivosSeries (populacao)
  
  setwd (diretorio)
}

# CALCULO DO MAPE DA DIFERENCA ENTRE OS PARAMETROS
MAPEdiferenca = function (populacao) {
  nP = length (populacao)
  a = round (runif (1, 1, nP))
  individuo = populacao[[a]]$individuo
  
  MAPE = lapply (populacao, function (x)
                                   abs ((individuo - x$individuo) / individuo))
  MAPEdif = sum (unlist (MAPE)) / (nP * (length (individuo)))
  
  if (is.finite (MAPEdif))
    return (MAPEdif)
  else
    return (1)
}

arquivoParametrosIniciais = function (saidasPMIX, lags) {
  p = 1:length (saidasPMIX)
  parametros = t (sapply (p, function (x) c (saidasPMIX[[x]]$ciclos, saidasPMIX[[x]]$somRes, saidasPMIX[[x]]$parametros)))
  parametros = data.frame (parametros)
  colnames (parametros) = c ("Ciclos", "SomRes", nomesLags (lags))
  write.csv2 (parametros, "ParametrosIniciais.csv", row.names = F)
}

arquivosSeries = function (populacao) {
  p = 1:length (populacao)
  lapply (p, function (x) {
    nome = paste0 ("Series/Serie_", x, ".csv")
    serie = data.frame (populacao[[x]]$serie)
    colnames (serie) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
    write.csv2 (serie, nome)
  })
}

arquivoParametros = function (populacao, lags) {
  parametros = t (sapply (populacao, function (x)
                                     x$individuo))
  parametros = data.frame (parametros)
  rownames (parametros) = c (1:length (populacao))
  colnames (parametros) = nomesLags (lags)
  write.csv2 (parametros, "ParametrosFinais.csv")
}

arquivoAvaliacoes = function (populacao) {
  avaliacoes = sapply (populacao, function (x)
                                  x$avaliacao)
  avaliacoes = t (matrix (as.numeric (avaliacoes), ncol = length (populacao)))
  avaliacoes = data.frame (avaliacoes)
  rownames (avaliacoes) = c (1:length (populacao))
  colnames (avaliacoes) = c("MAPEmedia", "MAPEdp", "MAPEfacAnual", "MAPEfacMensal", "SomRes")
  write.csv2 (avaliacoes, "Avaliacoes.csv")
}

nomesLags = function (lags) {
  meses = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  phis = NULL
  thts = NULL
  PHIs = NULL
  THTs = NULL
  if (lags[1] > 0)
    phis = paste0 (meses, "/", (paste0 ("phi", 1:lags[1])))
  if (lags[2] > 0)
    thts = paste0 (meses, "/", (paste0 ("tht", 1:lags[2])))
  if (lags[3] > 0)
    PHIs = paste0 (meses, "/", (paste0 ("PHI", 1:lags[3])))
  if (lags[4] > 0)
    THTs = paste0 (meses, "/", (paste0 ("THT", 1:lags[4])))
  
  nomes = c (phis, thts, PHIs, THTs)
  return (nomes)
}
