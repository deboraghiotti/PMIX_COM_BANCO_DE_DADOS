source('auxiliar.R')
library(shinyalert)
navbarPage ("PMIX (p,q,P,Q)",
                  tabPanel ("Dados Historicos",
                            sidebarPanel (
                              
                              ###################### ALTEREI PARA O BANCO DE DADOS
                              
                              shinyjs::useShinyjs(),
                              useShinyalert(),
                              shinyjs::inlineCSS(appCSS),
                              titlePanel(h3("Cadastrar Estacao")),
                              div(
                                id="form",
                                
                                textInput("nomeEstacao",labelMandatory("Nome")),
                                textInput("codigoEstacao",labelMandatory("Codigo")),
                                textInput("rioEstacao","Rio"),
                                textInput("anosEstacao","Anos"),
                                textInput("areaEstacao","Area da Bacia"),
                                textInput("latEstacao","Latitude"),
                                textInput("lngEstacao","Longitude"),
                                fileInput("fileSH",labelMandatory("Importar Serie Historica")),
                                actionButton("cadastrar", "Cadastrar", class = "btn-primary"),
                                
                                shinyjs::hidden(
                                  span(id = "cadastrando_msg", "Cadastrando..."),
                                  div(id = "error",
                                      div(br(), tags$b("Error: "), span(id = "error_msg"))
                                  )
                                )
                              ),
                              shinyjs::hidden(
                                div(
                                  id = "cadastro_realizado_msg",
                                  h3("Cadastro da Estacao realizado com sucesso!"),
                                  actionLink("cadastrar_outra", "Cadastrar outra Estacao")
                                )
                              )
                              ####################################################
                            ),
                            mainPanel (
                              titlePanel(h3("Consultar Estacao")),
                              selectizeInput("consultaEstacoes",label="",choices=""),
                              actionButton("ConsultarButton", "Consultar", class = "btn-primary"),
                              
                              shinyjs::hidden( 
                                div(id = "estacao_resultados",
                                    hr(),
                                    DT::dataTableOutput('dadosEstacaoTable'),
                                    br(),
                                    tabsetPanel(
                                      tabPanel(
                                        "Serie Historica",
                                        br(),
                                        plotOutput ("dados")
                                        
                                      ),
                                      tabPanel(
                                        "Volume Util",
                                        br(),
                                        verbatimTextOutput ("volumeUtilHist")
                                        
                                      ),
                                      tabPanel(
                                        "Hurst",
                                        br(),
                                        verbatimTextOutput ("hurstHist")
                                      ),
                                      tabPanel(
                                        "ACF ANUAL",
                                        br(),
                                        dataTableOutput("tabelaAnualHist")
                                        
                                      ),
                                      tabPanel(
                                        "ACF MENSAL",
                                        br(),
                                        dataTableOutput("tabelaMensalHist")
                                      ),
                                      tabPanel(
                                        "Avaliacoes",
                                        br(),
                                        dataTableOutput("tabelaAvaliacaoHist")
                                      )
                                    ),
                                    fluidRow(column(12,
                                                    actionButton("LimparButton", "   Limpar   ", class = "btn-primary"),
                                                    actionButton("DeletarButton", "Delete", class = "btn-primary",style="background-color:#ff0000;border-color: #ff0000")))))
                            )
                  ),
                  tabPanel ("Algoritmo",
                            sidebarLayout(
                              sidebarPanel (
                                selectizeInput("estacoes",label = "Escolha a Estacao",choices=""),
                                hr (),
                                radioButtons ("tipo", label = "Estimacao de parametros",
                                              choices = list ("Metodo de Powell" = 1,
                                                              "Algoritmo Genetico" = 2), 
                                              selected = 1),
                                tags$hr ( ),
                                fluidRow (
                                  column (width = 3,
                                          numericInput ("p", label = "p", value = 1, min = 0, max = 12)
                                  ),
                                  column (width = 3,
                                          numericInput ("q", label = "q", value = 0, min = 0, max = 12, width = "70px")
                                  ),
                                  column (width = 3,
                                          numericInput ("P", label = "P", value = 0, min = 0, max = 12, width = "70px")
                                  ),
                                  column (width = 3,
                                          numericInput ("Q", label = "Q", value = 0, min = 0, max = 12, width = "70px")
                                  )
                                ),
                                hr(),
                                sliderInput ("nsint", label = "Tamanho da serie sintetica", min = 0, max = 50000, value = 10000, width = "90%"),
                                fluidRow (
                                  column(width = 6,checkboxInput ("volume", "Gerar Volume", TRUE)),
                                  column(width = 6, checkboxInput ("hurst", "Gerar Hurst", TRUE))
                                ),
                                hr(),
                                actionButton("goButton", "Escolher", class = "btn-primary")
                              ,width = 5),
                              mainPanel(
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
                                                  downloadButton ("downloadSerie", "Download", icon ("save")),
                                                  tags$hr ( ),
                                                  selectInput ("nSerieBD", "Serie a ser armazenada no banco de dados:", choices = 1:50, selected = 50),
                                                  actionButton("armazenarBD","Armazenar",class = "btn-primary"),
                                                  shinyjs::hidden(
                                                    span(id = "armazenando_msg", "Armazenando..."),
                                                    div(id = "error_armazenar",
                                                        div(br(), tags$b("Error: "), span(id = "error_msg_armazenar"))
                                                    )
                                                  )
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
                                         dataTableOutput("tabelaAnual"),
                                         downloadButton ("downloadTabelaAnual", "Download", icon ("save"))
                                ),
                                tabPanel("Graficos FAC mensais",
                                         br ( ),
                                         selectInput ("lagMensalMAX", "lag mensal analisado:", choices = 1:12, selected = 1),
                                         plotOutput ("FACMensais"),
                                         dataTableOutput ("tabelaMensal"),
                                         downloadButton ("downloadTabelaMensal", "Download", icon ("save"))
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
                  ),
            tabPanel("Series Geradas",
                     shinyjs::useShinyjs(),
                     shinyjs::inlineCSS(appCSS),
                     fluidRow(
                       column(12,titlePanel(h3("Modelagem Estocastica Resultados",align="center"))) ,
                       br(),
                       column(12, 
                              tabsetPanel(
                                tabPanel("Series Sinteticas",
                                         br(),
                                         DT::dataTableOutput("SeriesSinteticas"),
                                         actionButton("selecionar_ss_button","Selecionar",class= "btn-primary"),
                                         br(),hr(),
                                         shinyjs::hidden(
                                           div(id = "ss_resultados",
                                               tabsetPanel(
                                                 tabPanel("Grafico da Serie",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="grafico_ss_panel",
                                                              plotOutput("GraficoSS")
                                                            ) 
                                                          )
                                                 ),
                                                 tabPanel("ACF MENSAL",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="acf_mensal_ss_panel",
                                                              DT::dataTableOutput('AcfMensal_SS_Table')
                                                            ) 
                                                          )
                                                 ),
                                                 tabPanel("ACF ANUAL",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="acf_anual_ss_panel",
                                                              DT::dataTableOutput('AcfAnual_SS_Table')
                                                            ) 
                                                          )
                                                 ),
                                                 tabPanel("Hurst",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="hurst_ss_panel",
                                                              DT::dataTableOutput('Hurst_SS_Table')
                                                            ) 
                                                          )
                                                 ),
                                                 tabPanel("Avaliacoes",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="avaliacao_ss_panel",
                                                              DT::dataTableOutput('Avaliacao_SS_Table')
                                                            ) 
                                                          )
                                                 ),
                                                 tabPanel("Volume de Reservatorio",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="volume_ss_panel",
                                                              DT::dataTableOutput('Volume_SS_Table')
                                                            ) 
                                                          )
                                                 ),
                                                 tabPanel("Soma Residual",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="soma_ss_panel",
                                                              DT::dataTableOutput('Soma_SS_Table')
                                                            ) 
                                                          )
                                                 )              
                                               ),
                                               fluidRow(
                                                 column(12,
                                                 actionButton("limpar_ss_button", "   Limpar   ", class = "btn-primary"),
                                                 actionButton("delete_ss_button", "   Deletar   ", class = "btn-primary",style="background-color:#ff0000;border-color: #ff0000")
                                                 )
                                              )
                                           )
                                         )
                                ),
                                tabPanel("Series Desagregadas",
                                         br(),
                                         DT::dataTableOutput("SeriesDesagregadas"),
                                         actionButton("selecionar_sd_button","Selecionar",class= "btn-primary"),
                                         br(),hr(),
                                         shinyjs::hidden(
                                           div(id = "sd_resultados",
                                               tabsetPanel(
                                                 tabPanel("ACF MENSAL",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="acf_mensal_sd_panel",
                                                              DT::dataTableOutput('AcfMensal_SD_Table')
                                                            ) 
                                                          )
                                                 ),
                                                 tabPanel("ACF ANUAL",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="acf_anual_sd_panel",
                                                              DT::dataTableOutput('AcfAnual_SD_Table')
                                                            ) 
                                                          )
                                                 ),
                                                 tabPanel("Hurst",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="hurst_sd_panel",
                                                              DT::dataTableOutput('Hurst_SD_Table')
                                                            ) 
                                                          )
                                                 ),
                                                 tabPanel("Avaliacoes",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="avaliacao_sd_panel",
                                                              DT::dataTableOutput('Avaliacao_SD_Table')
                                                            ) 
                                                          )
                                                 ),
                                                 tabPanel("Volume de Reservatorio",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="volume_sd_panel",
                                                              DT::dataTableOutput('Volume_SD_Table')
                                                            ) 
                                                          )
                                                 ),
                                                 tabPanel("Soma Residual",
                                                          br(),
                                                          shinyjs::hidden(
                                                            div(
                                                              id="soma_sd_panel",
                                                              DT::dataTableOutput('Soma_SD_Table')
                                                            ) 
                                                          )
                                                 )              
                                               ),
                                               actionButton("limpar_sd_button", "Limpar", class = "btn-primary")
                                           )
                                         )
                                )
                              )
                       )
                     )
                   ),
            tabPanel("Modelo ARMA",
                     sidebarLayout(
                       sidebarPanel(
                         titlePanel(h3("Modelo ARMA (p,q)",align="center")),
                         br(),
                         selectizeInput("estacoes_ARMA",label = "Escolha a Estacao",choices=""),
                         hr(),
                         titlePanel(h5(strong("Escolha os lags:"))),
                         fluidRow(
                          column(6,numericInput ("p_ARMA", label = "p", value = 1, min = 0, max = 12, width = "70px")),
                          column(6,numericInput ("q_ARMA", label = "q", value = 0, min = 0, max = 12, width = "70px"))
                         ),
                         hr(),
                         sliderInput ("nsint_ARMA", label = "Tamanho da serie sintetica", min = 0, max = 50000, value = 10000),
                         hr(),
                         fluidRow( 
                          column(6,actionButton("goButton_ARMA", "Iniciar", class = "btn-primary")),
                          column(6,actionButton("limparButton_ARMA", "Limpar", class = "btn-primary",style="background-color:#ff0000;border-color: #ff0000"))
                         ),
                         hr(),
                         titlePanel(h5(strong("Armazenar Serie:"))),
                         actionButton("armazenarButton_ARMA", "Armazenar", class = "btn-primary"),
                         shinyjs::hidden(
                           span(id = "armazenando_msg_ARMA", "Armazenando..."),
                           div(id = "error_armazenar_ARMA",
                               div(br(), tags$b("Error: "), span(id = "error_msg_armazenar_ARMA"))
                           )
                         )
                         
                       ),
                       mainPanel(
                         shinyjs::hidden(
                           div(id="resultados_ARMA",
                            tabsetPanel (
                             tabPanel("Graficos FAC anuais",
                                      br(),
                                      plotOutput ("GraficoSerie_ARMA"),
                                      dataTableOutput("tabelaAnual_ARMA")
                                      ),
                             
                             tabPanel("Avaliacao",
                                      br ( ),
                                      h4 (strong ("Tabela de Avaliacoes Historica")),
                                      dataTableOutput("tabelaAvaliacaoHist_ARMA"),
                                      hr(),
                                      h4 (strong ("Tabela de Avaliacoes Sintetica")),
                                      dataTableOutput("tabelaAvaliacao_ARMA")
                                      
                             ),
                             tabPanel("Medidas",
                                      br ( ),
                                      p (strong ("Calculo do volume util")),
                                      fluidRow (
                                        column (width = 6,
                                                sliderInput ("Pregularizacao_ARMA", "Porcentagem de regularizacao", min = 0, max = 100, value = 50, width = "100%")
                                        ),
                                        column (width = 6,
                                                verbatimTextOutput ("volumeUtil_ARMA")
                                        )
                                      ),
                                      
                                      hr ( ),
                                      p (strong ("Coeficiente de Hurst")),
                                      verbatimTextOutput ("hurst_ARMA"),
                                      
                                      hr(),
                                      p (strong ("Soma Residual")),
                                      verbatimTextOutput ("somaRes_ARMA")
                             )
                           )
                         )
                        )
                       )
                    )
            )
                      


)