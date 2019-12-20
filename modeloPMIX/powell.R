source ('entrada.R')
source ('correlograma.R')
source ('sumQuadRes.R')

# CALCULO DOS PARAMETROS DO MODELO PELO METODO DE POWELL
PMIX = function (p, q, P, Q) {
  lags = c (p, q, P, Q)
  if (sistema == "Linux")
    dados = tk_choose.files ( )
  else if (sistema == "Windows")
    dados = choose.files ( )
  
  serie = entrada (dados)$serieHN
  
  inicio = format (Sys.time (), "%F %Hh%M")
  # PONTO INICIAL: 1 NOS AUTORREGRESSIVOS, 0 NOS MEDIAS MOVEIS
  P0 = c (rep (1, 12*lags[1]), rep (0, 12*lags[2]), rep (1, 12*lags[3]), rep (0, 12*lags[4]))
  saidaPowell = powell (serie, lags, P0)
  fim = format (Sys.time (), "%F %Hh%M")
  
  
  # ESCRITA DOS ARQUIVOS
  diretorio = getwd ( )
  
  estacao = substr (dados, start = 1, stop = (nchar (dados)-4))
  if (! (dir.exists (estacao)))
    dir.create (file.path (estacao))
  setwd (estacao)
  data = format (Sys.time (), "%F %Hh%M")
  ordem = paste0 ("PMIX(", lags[1], ",", lags[2], ",", lags[3], ",", lags[4], ") ", data)
  
  parametros = saidaPowell$parametros
  phi = matrix (0, ncol = 12)
  tht = matrix (0, ncol = 12)
  PHI = matrix (0, ncol = 12)
  THT = matrix (0, ncol = 12)
  
  limInf = 0
  limSup = 0
  
  if (lags[1] > 0) {
    limInf = 1
    limSup = 12*lags[1]
    phi = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  if (lags[2] > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*lags[2] - 1
    tht = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  if (lags[3] > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*lags[3] - 1
    PHI = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  if (lags[4] > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*lags[4] - 1
    THT = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  
  parametrosPowell = data.frame (t (phi), t (tht), t (PHI), t (THT))
  colnames (parametrosPowell) = c (rep ("phi", max (1, lags[1])), rep ("tht", max (1, lags[2])), rep ("PHI", max (1, lags[3])), rep ("THT", max (1, lags[4])))
  rownames (parametrosPowell) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  write.csv2 (parametrosPowell, paste0 (ordem, ".csv"))
  
  sink (paste0 ("Execucao", ordem, ".txt"))
  print (inicio)
  print (fim)
  print (paste ("CICLOS:", saidaPowell$ciclos))
  print (paste ("SOMA RESIDUAL:", saidaPowell$somRes))
  sink ( )
  
  setwd (diretorio)
}

# METODO DE POWELL
powell = function (serie, lags, P0) {
  # NUMERO MAXIMO DE ITERACOES
  iteracoesMAX = 500
  # TAMANHO DO VETOR
  ordem = sum (lags)
  passo = matrix (0, ncol = (ordem*12), nrow = (ordem*12))
  # PASSO INICIAL
  for (i in 1 : (ordem*12)) passo[i, i] = 1
  
  E0 = residuos (serie, P0, lags)$somRes
  Pi = P0
  Ei = E0
  
  for (ciclos in (1 : iteracoesMAX)) {
    Pdec = 1
    delta = 0
    for (i in (1 : (ordem*12))) {
      Eiant = Ei
      busca = tamanhoPasso (serie, lags, Pi, passo[, i])
      Pi = busca$ponto
      Ei = busca$somRes
      
      # BUSCA PELO MAIOR DECRESCIMO
      if (Eiant - Ei > delta) {
        delta = Eiant - Ei
        Pdec = i
      }
    }
    
    # CRITERIO DE PARADA
    if (E0 - Ei <  10^-5) {
      final = list (parametros = Pi, somRes = Ei, ciclos = ciclos)
      return (final)
    }
    
    Pe = 2*Pi - P0
    Ee = residuos (serie, Pe, lags)$somRes
    if (Ee < E0) {
      if (2*(E0 - 2*Ei + Ee)*((E0 - Ei - delta)^2) < ((E0 - Ee)^2)*delta) {
        # PASSO QUE SUBSTITUIRA O DE MAIOR DECRESCIMO
        S = Pi - P0
        busca = tamanhoPasso (serie, lags, S, Pi)
        Ps = busca$ponto
        Es = busca$somRes
        if (Pdec < ordem*12)
          passo[, (Pdec:((ordem*12)-1))] = passo[, ((Pdec+1):(ordem*12))]
        
        passo[, (ordem*12)] = S
        Pi = Ps
        Ei = Es
      }
    }
    P0 = Pi
    E0 = Ei
  }

  final = list (parametros = Pi, somRes = Ei, ciclos = ciclos)
  return (final)
}

# OTIMIZACAO PARA O TAMANHO DO PASSO
tamanhoPasso = function (serie, lags, ponto, passo) {
  passoMin = -1
  passoMax = 1
  otimizacao = optimize (function (x) residuos (serie, (ponto + x*passo), lags)$somRes,
                         c (passoMin, passoMax), maximum = FALSE, tol = (.Machine$double.eps^0.5))
  
  final = list (ponto = ponto + otimizacao$minimum*passo, somRes = otimizacao$objective)
  return (final)
}

# GERACAO DAS SEMENTES PARA O ALGORITMO GENETICO
#**UTILIZA CLUSTERIZACAO**
PMIXs = function (serie, lags, n) {
  print ("Obtendo parametros pelo metodo de Powell")
  p = 1:n
  Pinicial = lapply (p, function (x)
                        geraPinicial (lags))
  # SAO GERADOS n PMIXs
  saidas = parLapply (cl, Pinicial, function (x)
                                    powell (serie, lags, x))
  
  return (saidas)
}

# GERACAO DOS PONTOS INICIAIS ALEATORIAMENTE, DENTRO DO INTERVALO ESPECIFICADO
# PHIs: +-[0.5, 1.5]
# THTs: +-[0, 0.5]
geraPinicial = function (lags) {
  phi = numeric (0)
  tht = numeric (0)
  PHI = numeric (0)
  THT = numeric (0)
  if (lags[1] > 0)
    phi = (runif (12*lags[1], 0.5, 1.5)) * (sapply ((round (runif (12*lags[1], -1, 0))), function (x) if (x == 0) x = 1 else x = -1))
  if (lags[2] > 0)
    tht = runif (12*lags[2], -0.5, 0.5)
  if (lags[3] > 0)
    PHI = (runif (12*lags[3], 0.5, 1.5)) * (sapply ((round (runif (12*lags[3], -1, 0))), function (x) if (x == 0) x = 1 else x = -1))
  if (lags[4] > 0)
    THT = runif (12*lags[4], -0.5, 0.5)
  
  Pinicial = c (phi, tht, PHI, THT)
  
  return (Pinicial)
}

p = 1:10
p = 5
