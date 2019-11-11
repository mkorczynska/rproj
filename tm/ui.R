shinyUI(
    fluidPage(
        tags$head(tags$style(
            HTML(
                'body, label, input, button, select { 
                    font-size: 18px;
                }'
                )
            )
        ),
        theme = shinytheme("paper"),
        navbarPage("Grupowanie tekstów",
                   tabPanel("Korpus tekstów",
                            sidebarLayout(
                                sidebarPanel(id="sidebar",
                                    fileInput("corp_file", 
                                              "Wybierz plik korpusu tekstów (z rozszerzeniem .rda)",
                                              multiple=FALSE
                                    )#end fileInput
                                ), #end sidebarPanel
                                
                                mainPanel(
                                    DT::dataTableOutput("texts")
                                )#end mainPanel
                            )#end sidebarLayout
                   ), #end tabPanel
                   
                   tabPanel("Podstawowe informacje",
                            navlistPanel(
                                tabPanel("Liczebność wyrazów",
                                         mainPanel(
                                             DT::dataTableOutput("stats")
                                         )#end mainPanel
                                ), #end tabPanel
                                
                                tabPanel("Liczebność wyrażen kilkuwyrazowych",
                                         sliderInput("number",
                                                     "Liczba słów w wyrażeniu:",
                                                     min = 1,  max = 10, value=5
                                                     ), #end sliderInput
                                         mainPanel(
                                             DT::dataTableOutput("terms")
                                         )#end mainPanel
                                ), #end tabPanel
                                
                                tabPanel("Częstość wystepowania wybranego słowa",
                                         textInput("text", 
                                                   label = h3("Podaj słowo"), 
                                                   value = "..."
                                                   ), #end textInput
                                         
                                         mainPanel(
                                             plotOutput("term_plot", width = "150%", height = "800px")
                                         )#end mainPanel
                                )#end tabPanel
                            )#end navlistPanel
                   ), #end tabPanel
                   
                   tabPanel("LDA",
                            navlistPanel(
                                tabPanel("Metryki",
                                         mainPanel(
                                             plotOutput("ar_dev", width = "150%", height = "800px"),
                                             plotOutput("grif_cao", width = "150%", height = "800px")
                                         )#end mainPanel
                                ), #end tabPanel
                                
                                tabPanel("Prawdopodobieństwa przynależności dla słów",
                                         sliderInput("topics",
                                                     "Liczba tematów:",
                                                     min = 1,  max = 20, value=2
                                         ), #end sliderInput
                                         
                                         mainPanel(
                                             DT::dataTableOutput("lda")
                                         )#end mainPanel
                                ), #end tabPanel
                                
                                tabPanel("Wykres tematów",
                                         
                                         mainPanel(
                                             plotOutput("lda_topics", width = "150%", height = "800px")
                                         )#end mainPanel
                                ), #end tabPanel
                                
                                tabPanel("Przypisanie",
                                         
                                         mainPanel(
                                             DT::dataTableOutput("texts_groups")
                                         )#end mainPanel
                                ), #end tabPanel
                                tabPanel("Słowa",
                                         sliderInput("words",
                                                     "Liczba słów:",
                                                     min = 1,  max = 20, value=2
                                         ), #end sliderInput
                                         
                                         mainPanel(
                                             tableOutput("words_in_topics")
                                         )#end mainPanel
                                )#end tabPanel
                            )#end navlistPanel
                   )#end tabPanel
        )#end navbarPage
    )#end fluidPage
)#end

