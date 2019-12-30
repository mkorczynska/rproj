# ui.R based on RStudio's stockVis app, Shiny lesson 6, here: http://shiny.rstudio.com/tutorial/lesson6/ 

library(shiny)
library(dygraphs)
library(dplyr)
library(shinythemes)

shinyUI(
    fluidPage(
        theme = shinytheme("paper"),
        navbarPage("Modele przełącznikowe",
                   tabPanel("Ładowanie danych",
                            sidebarLayout(
                                sidebarPanel(
                                    # Input: Select a file ----
                                    fileInput("file1", "Wybierz plik CSV",
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    
                                    # Horizontal line ----
                                    tags$hr(),
                                    
                                    # Input: Checkbox if file has header ----
                                    checkboxInput("header", "Header", TRUE),
                                    
                                    # Input: Select separator ----
                                    radioButtons("sep", "Separator",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ","),
                                    
                                    # Horizontal line ----
                                    tags$hr()
                                ),
                                
                                mainPanel(
                                    # Output: Data file ----
                                    DT::dataTableOutput("dane")
                                    
                                )
                            )
                   ),
                   tabPanel("Stopy",
                            sidebarLayout(
                                sidebarPanel(
                                    # Input: Select a file ----
                                    fileInput("file1", "Wybierz plik CSV",
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    
                                    # Horizontal line ----
                                    tags$hr(),
                                    
                                    # Input: Checkbox if file has header ----
                                    checkboxInput("header", "Header", TRUE),
                                    
                                    # Input: Select separator ----
                                    radioButtons("sep", "Separator",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ","),
                                    
                                    # Horizontal line ----
                                    tags$hr()
                                ),
                                mainPanel(
                                
                                # Output: Data file ----
                                DT::dataTableOutput("stopy_zwrotu"),
                                plotOutput("wykres_zamkniecia")
                                
                                )
                            )
                    ),
                   tabPanel("Wyjscie",
                            sidebarLayout(
                                sidebarPanel(
                                    # Input: Select a dataset ----
                                    selectInput("typmodelu", "Wybierz typ modelu:",
                                                choices = c("statyczny", "dynamiczny")),
                                    selectInput("typoptymalizacji", "Wybierz metodę optymalizacji:",
                                                choices = c("Nelder-Mead", "BFGS")),
                                    numericInput("num", label = "Numeric input", value = 0.01),
                                    numericInput("num", label = "Numeric input", value = 0.01)
                                ),
                                mainPanel(
                                    # Output: Tabset w/ plot, summary, and table ----
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Parametry wyjściowe", verbatimTextOutput("parametry")),
                                                tabPanel("Ksi", DT::dataTableOutput("ksi_all")),
                                                tabPanel("Ksi opóźnione", DT::dataTableOutput("ksi_lag")),
                                                tabPanel("Wykres")
                                    )
                                )
                            )
                    )
        )
    )
)