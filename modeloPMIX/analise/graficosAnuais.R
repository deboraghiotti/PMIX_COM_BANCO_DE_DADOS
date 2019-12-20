source ('entrada.R')
source ('correlograma.R')
source ('sumQuadRes.R')
source ('cenarioSintetico.R')

# INTERPRETACAO LINHA POR LINHA

sistema = Sys.info ( )['sysname']
if (sistema == "Linux")
  require ('tcltk')

if (sistema == "Linux")
  fileSerieH = tk_choose.files ( )
if (sistema == "Windows")
  fileSerieH = file.choose ( )

serieH = entrada (fileSerieH)$serieH

if (sistema == "Linux")
  fileSerieAG = tk_choose.files ( )
if (sistema == "Windows")
  fileSerieAG = file.choose ( )

serieAG = read.csv2 (fileSerieAG, header = T, sep = ";", dec = ",")
serieAG = serieAG[, -1]
serieAG = as.matrix (serieAG)

lags = c (1,0,0,0)
lags = c (1,1,0,0)
lags = c (1,1,1,1)
if (sistema == "Linux")
  fileParametrosP = tk_choose.files ( )
if (sistema == "Windows")
  fileParametrosP = file.choose ( )

parametrosP = read.csv2 (fileParametrosP, header = T, sep = ";", dec = ",")
parametrosP = parametrosP[, -1]
parametrosP = as.matrix (parametrosP)
parametrosP = parametrosP[parametrosP != 0]
serieP = cenarioSintetico (serieH, parametrosP, lags, 10000)

graficoFACAnual ( )
graficoFACMensal ( )

graficoFACAnual = function ( ) {
  lagMax = 12
  png (file = paste0 ("FACAnual", lags[1], lags[2], lags[3], lags[4], ".png"), width = 150, height = 100, units = "mm", res = 300)
  par (lwd = 1, col= 'black')
  plot (NA, main = "Autocorrelações anuais", xlim = c (1, 12), ylim = c (-1, 1), xlab = "Lags", ylab = "Valores calculados", axes = F, type = "n")
  axis (1, 1:lagMax, 1:lagMax)
  axis (2, -1:1, c (-1, 0, 1))
  par (lty = 1)
  lines (1:12, rep(0, 12))
  intConfianca = 1.96 / sqrt (length (serieH) / 12)
  par (lty = 2)
  lines (1:12, rep (intConfianca, 12))
  lines (1:12, rep (-intConfianca, 12))
  box ( )
  legend("bottomright", legend = c ("Histórico", "Powell", "Algoritmo Genético"),
         lty = 1, col = c ('gray40', 'deepskyblue4', 'darkblue'), bty="n", pch = 20)
    
  serieHAnual = apply (serieH, 1, sum)
  par (col = 'gray40', lty = 1)
  points (autocorrelacaoAnual (serieHAnual, lagMax)[-1], type = "o", pch = 20)
  
  seriePAnual = apply (serieP, 1, sum)
  par (col = 'deepskyblue4', lty = 1)
  points (autocorrelacaoAnual (seriePAnual, lagMax)[-1], type = "o", pch = 20)
  
  serieAGAnual = apply (serieAG, 1, sum)
  par (col = 'darkblue', lty = 1)
  points (autocorrelacaoAnual (serieAGAnual, lagMax)[-1], type = "o", pch = 20)
  dev.off ( )
}

graficoFACMensal = function ( ) {
  png (file = paste0 ("FACMensal", lags[1], lags[2], lags[3], lags[4], ".png"), width = 600, height = 300, units = "mm", res = 300)
  par (mfrow = c (3, 4))
  lapply (1:12, graficoFACMensalzinho)
  par (mfrow = c (1, 1))
  dev.off ( )
}

graficoFACMensalzinho = function (lag) {
  par (lwd = 1, col= 'black')
  plot (NA, main = paste ("Autocorrelações Mensais - lag", lag), xlim = c (1, 12), ylim = c (-1, 1), xlab = "Meses", ylab = "Valores calculados", axes = F, type = "n")
  axis (1, 1:12, c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
  axis (2, -1:1, c (-1, 0, 1))
  par (lty = 1)
  lines (1:12, rep(0, 12))
  intConfianca = 1.96 / sqrt (length (serieH) / 12)
  par (lty = 2)
  lines (1:12, rep (intConfianca, 12))
  lines (1:12, rep (-intConfianca, 12))
  box ( )
  legend("bottomright", legend = c ("Histórico", "Powell", "Algoritmo Genético"),
         lty = 1, col = c ('gray40', 'deepskyblue4', 'darkblue'), bty="n", pch = 20)
  
  par (col = 'gray40', lty = 1)
  points (autocorrelacaoMensal (serieH, lag)[lag+1,], type = "o", pch = 20)
  
  par (col = 'deepskyblue4', lty = 1)
  points (autocorrelacaoMensal (serieP, lag)[lag+1,], type = "o", pch = 20)
  
  par (col = 'darkblue', lty = 1)
  points (autocorrelacaoMensal (serieAG, lag)[lag+1,], type = "o", pch = 20)
}


