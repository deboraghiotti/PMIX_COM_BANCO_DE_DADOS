source ('entrada.R')

# INTERPRETACAO LINHA POR LINHA

sistema = Sys.info ( )['sysname']
if (sistema == "Linux")
  require ('tcltk')

if (sistema == "Linux")
  fileSerieH = tk_choose.files ( )
if (sistema == "Windows")
  fileSerieH = file.choose ( )

serieH = entrada (fileSerieH)$serieH
serieH = data.frame (serieH)
colnames (serieH) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

if (sistema == "Linux")
  fileSerieAG = tk_choose.files ( )
if (sistema == "Windows")
  fileSerieAG = file.choose ( )

serieAG = read.csv2 (fileSerieAG, header = T, sep = ";", dec = ",")
serieAG = serieAG[, -1]

par (lwd = 1, col= 'black')
plot (NA, main = "Vazões mensais (m^3/s)", xlim = c (1, 12), ylim = c (min (serieAG), max (serieAG)),
      xlab = "", ylab = "", axes = F, type = "n")
axis (1, 1:12, c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
par (lty = 1)
lines (1:12, rep(0, 12))
box ( )
