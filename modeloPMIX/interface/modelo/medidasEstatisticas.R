Hurst = function (serie) {
  n = length (serie)
  media = mean (serie)
  serieNormalizada = serie - media
  somaAcumulada = numeric (n)
  somaAcumulada[1] = serieNormalizada[1]
  for (i in 2:n) {
    somaAcumulada[i] = somaAcumulada[i-1] + serieNormalizada[i]
  }

  R = max (somaAcumulada) - min (somaAcumulada)
  S =  sd (serie)
  Hurst = log (R/S) / (log((n)/2))
  
  return (Hurst)
}