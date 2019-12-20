source ('tempo.R')

sistema <<- Sys.info ( )['sysname']

nSINTETICA <<- 10000
nPopulacao = 50
ProbCruzamento = 0.8
ProbMutacao = 0.05
MAXCiclos = 10000
MAXDiferenca = 0.2
AUTO <<- F

require ('parallel')
if (sistema == "Linux") {
  require ('tcltk')
}
# DEFINICAO DA QUANTIDADE DE NOS
cores = 1
cl = makeCluster (cores)
clusterExport (cl, list ("powell", "residuos", "tamanhoPasso",
                         "avaliaIndividuo", "momentos", "residuos", "serieSintetica", "autocorrelacaoAnual", "autocorrelacaoMensal", "avaliacao",
                         "cruzamentoBLX", "geraIndividuo", "torneio", "estouro", "nSINTETICA",
                         "dominanciaCompleta", "CDA", "distancia"))
# CASO COMPLETO DO MODELO
modeloPMIX = function (dados) {
  lags = list ( )
  lags[[1]] = c (1,0,0,0)
  lags[[2]] = c (1,0,1,0)
  lags[[3]] = c (1,1,0,0)
  lags[[4]] = c (1,1,1,0)
  lags[[5]] = c (1,1,1,1)
  lags[[6]] = c (2,0,0,0)
  lags[[7]] = c (2,0,1,0)
  lags[[8]] = c (2,1,0,0)
  lags[[9]] = c (2,1,1,0)
  #lags[[10]] = c (2,1,1,1)
  
  AUTO <<- T
  dados <<- choose.files ( )
  
  lapply (lags, function (x) {
    print("Iniciando novo NSGA...")
    NSGA (x, nPopulacao, ProbCruzamento, ProbMutacao, MAXCiclos, MAXDiferenca)
    })
  # PARAR O CLUSTER
  stopCluster (cl)
  print ("Finalizado!")
}