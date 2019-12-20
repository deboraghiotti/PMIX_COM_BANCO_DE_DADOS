navbarPage ("PMIX (p,q,P,Q)",
                  tabPanel ("Dados Historicos",
                            sidebarPanel (
                              fileInput ("file", "Serie Historica hidrologica de vazoes",
                                         multiple = FALSE,
                                         accept = c ("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv")),
                              selectInput("estacao",
                                          "Estacoes:",
                                          c("")),
                              textOutput("result"),
                              
                              tags$hr ( ),
                              checkboxInput ("header", "Header", TRUE),
                              selectInput ("sep", label = "Separador de colunas", 
                                           choices = list ("Ponto" = '.', "Virgula" = ",", "Ponto e virgula" = ";", "Tabulacao" = "\t"), 
                                           selected = ";"),
                              selectInput ("dec", label = "Separador decimal", 
                                           choices = list ("Ponto" = '.', "Virgula" = ","), 
                                           selected = ',')
                            ),
                            mainPanel (plotOutput ("dados"),
                                       hr ( ),
                                       verbatimTextOutput ("volumeUtilHist"),
                                       hr ( ),
                                       verbatimTextOutput ("hurstHist"),
                                       hr ( ),
                                       dataTableOutput("tabelaAnualHist"),
                                       hr ( ),
                                       dataTableOutput("tabelaMensalHist")
                            )
                  ),
                  tabPanel ("Algoritmo",
                            sidebarLayout(
                              sidebarPanel (
                                radioButtons ("tipo", label = "Estimacao de parametros",
                                              choices = list ("Metodo de Powell" = 1,
                                                              "Algoritmo Genetico" = 2), 
                                              selected = 1),
                                tags$hr ( ),
                                verbatimTextOutput ("parametros")
                              ),
                              mainPanel(
                                fluidRow (
                                  column (width = 2,
                                          numericInput ("p", label = "p", value = 1, min = 0, max = 12, width = "70px")
                                  ),
                                  column (width = 2,
                                          numericInput ("q", label = "q", value = 0, min = 0, max = 12, width = "70px")
                                  ),
                                  column (width = 2,
                                          numericInput ("P", label = "P", value = 0, min = 0, max = 12, width = "70px")
                                  ),
                                  column (width = 2,
                                          numericInput ("Q", label = "Q", value = 0, min = 0, max = 12, width = "70px")
                                  )
                                ),
                                tags$hr ( ),
                                fluidRow (
                                  column (width = 8,
                                          sliderInput ("nsint", label = "Tamanho da serie sintetica", min = 0, max = 50000, value = 10000, width = "90%")
                                  ),
                                  column (width = 4,
                                          checkboxInput ("volume", "Gerar Volume", TRUE),
                                          checkboxInput ("hurst", "Gerar Hurst", TRUE)
                                  )
                                )
                              )
                            ),
                            tags$hr ( ),
                            fluidRow (
                              column (width = 4,
                                      sliderInput ("nPop", label = "Tamanho da populacao", min = 10, max = 100, value = 50, width = "100%"),
                                      sliderInput ("cicloMax", label = "Ciclo Maximo", min = 0, max = 50000, value = 10000, width = "100%")
                              ),
                              column (width = 2,
                                      numericInput ("pC", label = "Probabilidade de cruzamento", value = 80, min = 0, max = 100, width = "80%"),
                                      numericInput ("pM", label = "Probabilidade de mutacao", value = 5, min = 0, max = 100, width = "80%")
                              ), 
                              column (width = 2,
                                      numericInput ("MAPEdiferencaMAX", label = "MAPEdiferencaMAX", value = 5, min = 0, max = 100, width = "80%"),
                                      numericInput ("MAPEavaliacao", label = "MAPEavaliacao", value = 20, min = 0, max = 100, width = "80%")
                              ),
                              column (width = 2,
                                      numericInput ("lagAnual", label = "lag Anual", value = 1, min = 1, max = 12, width = "70px"),
                                      numericInput ("lagMensal", label = "lag Mensal", value = 1, min = 1, max = 12, width = "70px"),
                                      checkboxInput ("lagSignificativo", "Lag Significativo", TRUE)
                              )
                            )
                            
                  ),
                  tabPanel ("Resultados",
                            selectInput ("analise", label = "Local de analise", 
                                         choices = list ("Estimados" = 1, "Arquivados" = 2),
                                         selected = "Estimados"),
                            pageWithSidebar (
                              headerPanel (NULL),
                              sidebarPanel (
                                conditionalPanel (condition ="input.analise == 1",
                                                  verbatimTextOutput ("resultadoGeral"),
                                                  actionButton ("iniciar", "Iniciar!"),
                                                  tags$hr ( ),
                                                  selectInput ("nSerie", "Serie a ser analisada:", choices = 1:50, selected = 50),
                                                  tags$hr ( ),
                                                  downloadButton ("downloadSerie", "Download", icon ("save"))
                                                  ),
                                conditionalPanel (condition = "input.analise == 2",
                                                  fileInput ("serieArquivada", "Series a serem analisadas",
                                                             multiple = TRUE,
                                                             accept = c ("text/csv",
                                                                         "text/comma-separated-values,text/plain",
                                                                         ".csv")),
                                                  tags$hr ( ),
                                                  checkboxInput ("headerA", "Header", TRUE),
                                                  selectInput ("sepA", label = "Separador de colunas", 
                                                               choices = list ("Ponto" = '.', "Virgula" = ",", "Ponto e virgula" = ";", "Tabulacao" = "\t"), 
                                                               selected = ";"),
                                                  selectInput ("decA", label = "Separador decimal", 
                                                               choices = list ("Ponto" = '.', "Virgula" = ","), 
                                                               selected = ','),
                                                  tags$hr ( ),
                                                  selectInput ("nSerieA", "Serie a ser analisada:", choices = 1:50, selected = 50)
                                                  )
                                ),
                              
                            mainPanel (
                              tabsetPanel (
                                tabPanel("Tabela avaliacoes",
                                         br ( ),
                                         dataTableOutput ("tabelaAvaliacao")
                                ),
                                tabPanel("Graficos series",
                                         br ( ),
                                         plotOutput("GraficoSerie"),
                                         dataTableOutput("tabelaMedias")
                                ),
                                tabPanel("Graficos FAC anuais",
                                         br ( ),
                                         plotOutput("FACAnuais"),
                                         dataTableOutput("tabelaAnual")
                                ),
                                tabPanel("Graficos FAC mensais",
                                         br ( ),
                                         selectInput ("lagMensalMAX", "lag mensal analisado:", choices = 1:12, selected = 1),
                                         plotOutput ("FACMensais"),
                                         dataTableOutput ("tabelaMensal")
                                ),
                                tabPanel("Medidas",
                                         br ( ),
                                         p (strong ("Calculo do volume util")),
                                         fluidRow (
                                           column (width = 6,
                                                   sliderInput ("Pregularizacao", "Porcentagem de regularizacao", min = 0, max = 100, value = 50, width = "100%")
                                           ),
                                           column (width = 6,
                                                   verbatimTextOutput ("volumeUtil")
                                           )
                                         ),
                                         hr ( ),
                                         p (strong ("Coeficiente de Hurst")),
                                         verbatimTextOutput ("hurst")
                                )
                              )
                            )
                        )    
                  )

)