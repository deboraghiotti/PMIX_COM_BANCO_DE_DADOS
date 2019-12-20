volumeUtil = function (Pregularizacao, M) {
  # O ARQUIVO E ESCOLHIDO DENTRO DO PROGRAMA
  if (sistema == "Linux")
    dados = tk_choose.files ( )
  else if (sistema == "Windows")
    dados = choose.files ( )
  
  serie = leituraSerie (dados)
  nS = length (serie)
  
  if (M)
    periodo = rep (c (31,28,31,30,31,30,31,31,30,31,30,31), (nS/12))
  else
    periodo = rep (365, nS)
  
  periodo = periodo*24*60*60
  # tempoAcumulado = sapply (1:nS, function (x) sum (periodo[1:x]))
  volumePadronizado = (serie - (mean (serie)*Pregularizacao))*periodo
  
  volumeAcumulado = numeric (nS+1)
  volumeAcumulado[1] = 0
  for (i in 2:(nS+1)) {
    volumeAcumulado[i] = volumePadronizado[i-1] + volumeAcumulado[i-1]
  }
  
  pico = NULL
  i = 2
  while (i < (nS-1)) {
    if ((volumeAcumulado[i] > volumeAcumulado[i-1]) && (volumeAcumulado[i] > volumeAcumulado[i+1])) {
      j = i+1
      while (j < (nS-1)) {
        if (((volumeAcumulado[j] > volumeAcumulado[j-1]) && (volumeAcumulado[j] > volumeAcumulado[j+1])) && 
            (volumeAcumulado[j] > volumeAcumulado[i])) {
          pico = c (pico, volumeAcumulado[i] - min (volumeAcumulado[i:j]))
          i = j-1
          j = nS
        }
        j = j+1
      }
    }
    i = i+1
  }
  picoMax = max (pico)
  return (picoMax)
}

leituraSerie = function (dados) {
  leitura = read.table (dados, header = TRUE, sep = ";", dec = ",")
  leitura = leitura[, -1]
  serie = as.matrix (leitura)
  
  serie = as.vector (t (serie))
  return (serie)
}
