# MODELAGEM ANUAL

n = 10000
sistema <<- Sys.info ( )['sysname']

# FUNCAO GENERICA PARA O CENARIO SINTETICO ANUAL
cenarioSinteticoAnual = function (P, Q) {
  lags = c (P, Q)
  # O ARQUIVO E ESCOLHIDO DENTRO DO PROGRAMA
  if (sistema == "Linux")
    dados = tk_choose.files ( )
  else if (sistema == "Windows")
    dados = choose.files ( )
  
  leitura = read.table (dados, header = TRUE, sep = ";", dec = ",")
  leitura = leitura[, -1]
  serieH = matrix (leitura, ncol = 12, byrow = TRUE)
  serieAnualH = apply (serieH, 1, sum)
  serieHL = log (serieAnualH)
  mediaHL = mean (serieHL)
  dpHL = sd (serieHL)
  serieHN = (serieHL - mediaHL) / dpHL
  
  modelo = ARMA (serieHN, lags)
  parametros = modelo$parametros
  dpRes = modelo$dpRes
  c = modelo$constante
  
  serieS = serieSinteticaAnual (parametros, dpRes, c, lags, n)
  serieS = (serieS * dpHL) + mediaHL
  serieS = exp (serieS)
  
  final = list (serieSintetica = serieS, residuos = modelo$residuos, parametros = parametros)
  return (final)
}

# serieAnual: Vetor contendo a serie padronizada e normalizada anual.
# lags: Ordem do modelo ARMA (p, q)
ARMA = function (serieAnual, lags) {
  # METODO DA MINIMIZACAO DA SOMA DOS QUADRADOS DOS RESIDUOS
  modelo = arima0 (ts (serieAnual), order = c (lags[1], 0, lags[2]), seasonal = list (order = c (0, 0, 0), period = NA),
                  xreg = NULL, include.mean = TRUE, delta = 0.01, transform.pars = TRUE, fixed = NULL, init = NULL,
                  method = "CSS")
  parametros = as.vector (modelo$coef)
  constante = parametros[length(parametros)]
  parametros = parametros[-length(parametros)]
  dpRes = sqrt (modelo$sigma2)
  residuos = modelo$residuals
  
  final = list (residuos = residuos, parametros = parametros, dpRes = dpRes, constante = constante)
  return (final)
}

# parametros: Vetor contendo parametros phi e tht. Parametros de ordem zero devem ser omitidos.
# dpRes: Desvio-padrao anual.
# lags: Vetor contendo as ordens dos parametros do modelo.
# n: Tamanho da serie a ser gerada.
serieSinteticaAnual = function (parametros, dpRes, c, lags, nS) {
  p = lags[1]
  q = lags[2]
  
  limInf = 0
  limSup = 0
  if (p > 0) {
    limInf = 1
    limSup = p
    phi = parametros[limInf : limSup]
  }
  if (q > 0) {
    limInf = limSup + 1
    limSup = limInf + q - 1
    tht = parametros[limInf : limSup]
  }
  
  residuoS = rnorm (nS)
  residuoS = (residuoS - mean (residuoS)) / sd (residuoS)
  residuoS = residuoS * dpRes
  
  serieS = rep (0, nS)
  for (v in ((max (p, q) + 1):nS)) {
    auto = 0
    mm = 0
    if (p > 0) {
      for (i in (1:p))
        auto = auto + phi[i]*serieS[v-i]
    }
    if (q > 0) {
      for (j in (1:q))
        mm = mm + tht[j]*residuoS[v-j]
    }
    serieS[v] = c + auto + mm + residuoS[v]
  }
  
  return (serieS)
}