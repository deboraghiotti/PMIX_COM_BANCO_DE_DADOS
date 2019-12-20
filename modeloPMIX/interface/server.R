function (input, output, session) {
  output$parametros = renderPrint ({
    if (input$tipo == 1) {
      print ("Metodo de Powell")
      print (c (input$p, input$q, input$P, input$Q))
    }
    else {
      print ("Algoritmo Genetico")
      print (c (input$p, input$q, input$P, input$Q))
      print (paste ("Tamanho da Serie Sintetica:", input$nsint))
      print ("Parametros Geneticos:")
      print (paste ("Tamanho da Populacao:", input$nPop))
      print (paste ("Probabilidade de Cruzamento:", input$pC))
      print (paste ("Probabilidade de Mutacao:", input$pM))
      print ("Criterios de Parada:")
      print (paste ("Ciclo Maximo:", input$cicloMax))
      print (paste ("MAPE da Diferenca maxima entre os individuos:", input$MAPEdiferencaMAX))
    }
  })
  
 SHTable <- Estacoes()
 
 observe({
   updateSelectInput(session,"estacao",
                     choices=as.character(SHTable$nome))
 })
 
 output$result <- renderText({
   paste("You chose", input$estacao)
 })
 
 #PAREI AQUI
 selectEstacaoRow <-  reactive({
   s <- findID(SHTable,input$estacao)
   s
   })
 print(selectEstacaoRow)
 
 
  serieHist = reactive ({
    serieH = read.csv2 (input$file$datapath,
                        header = input$header,
                        sep = input$sep,
                        dec = input$dec)
    serieH = serieH[, -1]
    serieH = matrix (serieH, ncol = 12, byrow = T)
  })
  
  serieHistAnual = reactive ({
    apply (serieHist ( ), 1, sum)
  })
  
  funcaoAlgoritmo = reactive({
    if (input$iniciar)
      isolate (algoritmo (input))
  })
  
  serieEscolhida = reactive ({
    if (input$analise == 1) {
      serieS = funcaoAlgoritmo ( )$arqSeries
      if (input$tipo == 2) {
        serieS = serieS[[as.numeric (input$nSerie)]]
        print(serieS)
      }
    }
    else
      serieS = leituraSerie ( ) [[as.numeric (input$nSerieA)]]

    return (serieS)
  })
  
  serieEscolhidaAnual = reactive ({
    apply (serieEscolhida ( ), 1, sum)
  })
  
  leituraSerie = reactive ({
    arqSeries = lapply (input$serieArquivada$datapath, function (x)
                                                       read.csv2 (x, header = input$headerA,
                                                       sep = input$sepA,
                                                       dec = input$decA))
    arqSeries = lapply (arqSeries, function (x)
                                   x[ ,-1])
    serieS = lapply (arqSeries, function (x)
                                as.matrix (x))
    return (serieS)
  })
  
  output$resultadoGeral = renderPrint ({
    if (input$iniciar == 0)
      return ("Aguardando inicio...")
    
    duracao = funcaoAlgoritmo ( )$duracao
    print (paste ("Duracao:", duracao, "seg"))
    
    if (input$tipo == 1) {
      ciclos = funcaoAlgoritmo ( )$algoritmo$ciclos
      somRes = funcaoAlgoritmo ( )$algoritmo$somRes
      
      print ("Metodo de Powell")
      print (paste ("ciclos: ", ciclos))
      print (paste ("Somatorio dos residuos:", somRes))
    }
    else {
      ciclos = funcaoAlgoritmo ( )$algoritmo$ciclos
      
      print("Algoritmo Genetico")
      print (paste ("ciclos: ", ciclos))
    }
  })
  
  output$dados = renderPlot ({
    req (input$file)
    if (is.null (input$file))
      return (NULL)
    
    plotSerie (serieHist ( ))
  })
  
  output$tabelaMedias = renderDataTable ({
    if ((input$iniciar) || ((input$analise == 2) && (length (input$serieArquivada$datapath) > 0))) {
      MediaHist = apply (serieHist ( ), 2, mean)
      MediaSint = apply (serieEscolhida ( ), 2, mean)
      DesvioHist = apply (serieHist ( ), 2, sd)
      DesvioSint = apply (serieEscolhida ( ), 2, sd)
      medidas = data.frame (MediaHist, MediaSint, DesvioHist, DesvioSint)
      rownames (medidas) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      colnames (medidas) = c ("Media Historica", "Media Sintetica", "Desvio-padrao Historico", "Desvio-padrao Sintetico")
      datatable (medidas)
    }
  })
  
  output$volumeUtilHist = renderPrint ({
    if  (! (is.null (input$file))) {
      print ("Volume util")
      print (paste (volumeUtil (serieHist ( ), (input$Pregularizacao/100), TRUE), "m^3"))
    }
  })
  
  output$hurstHist = renderPrint ({
    if  (! (is.null (input$file))) {
      print ("Coeficiente de Hurst")
      print (isolate (Hurst (as.vector (serieHist ( )))))
    }
  })
  
  output$tabelaAnualHist = renderDataTable ({
    if (! (is.null (input$file))) {
      facAnual = data.frame (as.vector (autocorrelacaoAnual (serieHistAnual ( ), 12)[-1]))
      rownames (facAnual) = paste ("lag", 1:12)
      datatable (facAnual, colnames = NULL) # %>% formatStyle (backgroundColor = styleInterval (c (0, 1), c ('gray', 'yellow'))
    }
  })
  
  output$tabelaMensalHist = renderDataTable ({
    if  (! (is.null (input$file))) {
      facMensal = data.frame (autocorrelacaoMensal (serieHist ( ), 12)[-1, ])
      colnames (facMensal) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      rownames (facMensal) = paste ("lag", 1:12)
      datatable (facMensal)
    }
  })
  
  output$tabelaAvaliacao = renderDataTable ({
    input$analise
    if ((input$iniciar) && (input$analise == 1)) {
      if (input$tipo == 1) {
        parametros = funcaoAlgoritmo ( )$arqParametros
        
        phi = matrix (0, ncol = 12)
        tht = matrix (0, ncol = 12)
        PHI = matrix (0, ncol = 12)
        THT = matrix (0, ncol = 12)
        
        limInf = 0
        limSup = 0
        
        if (input$p > 0) {
          limInf = 1
          limSup = 12*input$p
          phi = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        if (input$q > 0) {
          limInf = limSup + 1
          limSup = limInf + 12*input$q - 1
          tht = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        if (input$P > 0) {
          limInf = limSup + 1
          limSup = limInf + 12*input$P - 1
          PHI = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        if (input$Q > 0) {
          limInf = limSup + 1
          limSup = limInf + 12*input$Q - 1
          THT = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
        }
        
        parametrosPowell = data.frame (t (phi), t (tht), t (PHI), t (THT))
        colnames (parametrosPowell) = c (rep ("phi", max (1, input$p)), rep ("tht", max (1, input$q)), rep ("PHI", max (1, input$P)), rep ("THT", max (1, input$Q)))
        rownames (parametrosPowell) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
        return (datatable (parametrosPowell))
      }
      else {
        avaliacoes = data.frame (funcaoAlgoritmo ( )$arqAvaliacoes)
        colnames (avaliacoes) = c ("MAPE media", "MAPE desvio", "MAPE FAC anual", "MAPE FAC mensal", "Soma Residual")
        rownames (avaliacoes) = paste ("Serie", 1:input$nPop)
        return (datatable (avaliacoes))
      }
    }
    
    else if ((input$analise == 2) && (length (input$serieArquivada$datapath) > 0)) {
      mediaH = apply (serieHist ( ), 2, mean)
      dpH = apply (serieHist ( ), 2, sd)
      facAnualH = autocorrelacaoAnual (serieHist ( ), 12)[-1]
      facMensalH = autocorrelacaoMensal (serieHist ( ), 12)[-1, ]
      
      MAPEMedia = NULL
      MAPEDesvio = NULL
      MAPEFacAnual = NULL
      MAPEFacMensal = NULL
      
      avaliacoes = lapply (leituraSerie ( ), function (x) {
        mediaS = apply (x, 2, mean)
        dpS = apply (x, 2, sd)
        facAnualS = autocorrelacaoAnual (x, 12)[-1]
        facMensalS = autocorrelacaoMensal (x, 12)[-1, ]
        
        MAPEMedia = sum (abs ((mediaH - mediaS) / mediaH)) / 12
        MAPEDesvio = sum (abs ((dpH - dpS)) / dpH) / 12
        MAPEFacAnual = sum (abs ((facAnualH - facAnualS) / facAnualH)) / 12
        MAPEFacMensal = sum (abs ((facMensalH - facMensalS) / facMensalH)) / (12*12)
        c (MAPEMedia, MAPEDesvio, MAPEFacAnual, MAPEFacMensal)
      })
      
      avaliacoes = matrix (unlist (avaliacoes), ncol = 4, byrow = T)
      avaliacoes = data.frame (avaliacoes)
      colnames (avaliacoes) = c ("MAPE media", "MAPE desvio", "MAPE FAC anual", "MAPE FAC mensal")
      rownames (avaliacoes) = paste ("Serie", 1:length (input$serieArquivada$datapath))
      return (datatable (avaliacoes))
    }
  })
  
  output$GraficoSerie = renderPlot ({
    if ((input$iniciar) || ((input$analise == 2) && (length (input$serieArquivada$datapath) > 0))) {
      inicializaGraficoSERIE (serieHist ( ))
      graficoSERIE (serieHist ( ), 'cornflowerblue')
      graficoSERIE (serieEscolhida ( ), 'blue')
    }
  })
  
  output$FACAnuais = renderPlot ({
    if ((input$iniciar) || ((input$analise == 2) && (length (input$serieArquivada$datapath) > 0))) {
      inicializaGraficoFACANUAL (serieHistAnual ( ), 12)
      graficoFACANUAL (serieHistAnual ( ), 12, 'cornflowerblue')
      graficoFACANUAL (serieEscolhidaAnual ( ), 12, 'blue')
    }
  })
  
  output$tabelaAnual = renderDataTable ({
    if ((input$iniciar) || ((input$analise == 2) && (length (input$serieArquivada$datapath) > 0))) {
      facAnual = data.frame (as.vector (autocorrelacaoAnual (serieEscolhidaAnual ( ), 12)[-1]))
      rownames (facAnual) = paste ("lag", 1:12)
      datatable (facAnual, colnames = NULL)
    }
  })
  
  output$FACMensais = renderPlot ({
    if ((input$iniciar) || ((input$analise == 2) && (length (input$serieArquivada$datapath) > 0))) {
      inicializaGraficoMENSAL (serieHist ( ), as.numeric (input$lagMensalMAX))
      graficoFACMENSAL (serieHist ( ), as.numeric (input$lagMensalMAX), 'cornflowerblue')
      graficoFACMENSAL (serieEscolhida ( ), as.numeric (input$lagMensalMAX), 'blue')
    }
  })
  
  output$tabelaMensal = renderDataTable ({
    if ((input$iniciar) || ((input$analise == 2) && (length (input$serieArquivada$datapath) > 0))) {
      facMensal = data.frame (autocorrelacaoMensal (serieEscolhida ( ), 12)[-1, ])
      colnames (facMensal) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
      rownames (facMensal) = paste ("lag", 1:12)
      datatable (facMensal)
    }
  })
  
  output$volumeUtil = renderPrint ({
    if  (! (is.null (input$file))) {
      print ("Serie historica")
      print (paste (volumeUtil (serieHist ( ), (input$Pregularizacao/100), TRUE), "m^3"))
    }
    if ((input$iniciar) || ((input$analise == 2) && (length (input$serieArquivada$datapath) > 0))) {
      print ("Serie sintetica")
      print (paste (volumeUtil (serieEscolhida ( ), (input$Pregularizacao/100), TRUE), "m^3"))
    }
  })
  
  output$hurst = renderPrint ({
    if  (! (is.null (input$file))) {
      print ("Serie historica")
      print (isolate (Hurst (as.vector (serieHist ( )))))
    }
    if ((input$iniciar) || ((input$analise == 2) && (length (input$serieArquivada$datapath) > 0))) {
      print ("Serie sintetica")
      print ((Hurst (as.vector (serieEscolhida ( )))))
    }
  })
  
  observe ({
    if (input$tipo == 1) {
      updateSelectInput(session, "nSerie",
                        choices = 1,
                        selected = 1)
    }
    else {
      updateSelectInput (session, "nSerie",
                         choices = 1:input$nPop,
                         selected = input$nPop
      )
    }
  })
  
  observe ({
      updateSelectInput (session, "nSerieA",
                         choices = 1:length (input$serieArquivada$datapath),
                         selected = length (input$serieArquivada$datapath)
      )
  })
  
  output$downloadSerie = downloadHandler (
    filename = function ( ) {
      paste0 ("serie_", input$nSerie, ".csv")
    },
    content = function (file) {
      write.table (data.frame (serieEscolhida ( )), file,
                   col.names = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"),
                   row.names = F,
                   sep = input$sep,
                   dec = input$dec)
    }
  )
  
}