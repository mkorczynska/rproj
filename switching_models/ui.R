library(shiny)
library(dygraphs)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(tidyr)
library(plotly)

shinyUI(
    fluidPage(
        theme = shinytheme("cosmo"),
        navbarPage("Modele przełącznikowe",
                   tabPanel("Dane 1",
                            sidebarLayout(
                                sidebarPanel(
                                    fileInput("file1", "Wybierz plik CSV",
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    tags$hr(),
                                    checkboxInput("header1", "Nagłówek", TRUE),
                                    radioButtons("separator1", "Separator",
                                                 choices = c(Przecinek = ",",
                                                             Średnik = ";",
                                                             Tabulator = "\t"),
                                                 selected = ","),
                                    radioButtons("decimal1", "Separator dziesiętny",
                                                 choices = c(Przecinek = ",",
                                                             Kropka = "."),
                                                 selected = ","),
                                    tags$hr(),
                                    textInput("nazwa_data", label = "Podaj nazwę kolumny, w której znajdują się daty:"),
                                    textInput("nazwa_ceny", label = "Podaj nazwę kolumny, w której znajdują się ceny:"),
                                    tags$hr()
                                ),
                                mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Wczytane dane", DT::dataTableOutput("dane")),
                                                tabPanel("Stopy zwrotu", DT::dataTableOutput("stopy_zwrotu")),
                                                tabPanel("Wykres", plotlyOutput("wykres_ceny"))
                                    )
                                    
                                )
                            )
                   ),
                   tabPanel("Dane 2",
                            sidebarLayout(
                                sidebarPanel(
                                    fileInput("file2", "Wybierz plik CSV",
                                              multiple = FALSE,
                                              accept = c("text/csv",
                                                         "text/comma-separated-values,text/plain",
                                                         ".csv")),
                                    tags$hr(),
                                    checkboxInput("header2", "Nagłółwek", TRUE),
                                    radioButtons("separator2", "Separator",
                                                 choices = c(Przecinek = ",",
                                                             Średnik = ";",
                                                             Tabulator = "\t"),
                                                 selected = ","),
                                    radioButtons("decimal2", "Separator dziesiętny",
                                                 choices = c(Przecinek = ",",
                                                             Kropka = "."),
                                                 selected = ","),
                                    tags$hr(),
                                    textInput("nazwa_czas", label = "Podaj nazwę kolumny, w której znajdują się dane związane z czasem:"),
                                    textInput("nazwa_kolumny", label = "Podaj nazwę kolumny, w której znajdują się dane (docelowy wektor x):")
                                ),
                                mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Wczytane dane", DT::dataTableOutput("dane2")),
                                                tabPanel("Wykres", plotlyOutput("wykres_dane"))
                                                )
                                )
                            )
                    ),
                   tabPanel("Statyczna macierz przejścia",
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput("typoptymalizacji", "Wybierz metodę optymalizacji:",
                                                choices = c("Nelder-Mead", "BFGS")),
                                    h4("Parametry modelu statycznego:"),
                                    numericInput("p11", label = "P11", value = 0.95),
                                    numericInput("p22", label = "P22", value = 0.95),
                                    checkboxInput("wybor_dane2", label = "Dane 2 jako wektor x", value = 1)
                                ),
                                mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Parametry wyjściowe", verbatimTextOutput("parametry_s")),
                                                tabPanel("Ksi", DT::dataTableOutput("ksi_all_s")),
                                                tabPanel("Ksi opóźnione", DT::dataTableOutput("ksi_lag_s")),
                                                tabPanel("Ksi wygładzone", DT::dataTableOutput("ksi_smooth_s")),
                                                tabPanel("Wykres ksi", plotlyOutput("wykres_ksi")),
                                                tabPanel("Wykres ksi wygładzone", plotlyOutput("wykres_ksi_smooth"))
                                    )
                                )
                            )
                    ),
                   tabPanel("Dynamiczna macierz przejścia",
                            sidebarLayout(
                                sidebarPanel(
                                    selectInput("typoptymalizacji", "Wybierz metodę optymalizacji:",
                                                choices = c("Nelder-Mead", "BFGS")),
                                    tags$hr(),
                                    h4("Parametry modelu dynamicznego:"),
                                    numericInput("b0_1", label = "Pierwszy element wektora B0", value = 0.90),
                                    numericInput("b0_2", label = "Drugi element wektora B0", value = 0.90),
                                    numericInput("b1_1", label = "Pierwszy element wektora B1", value = 0.90),
                                    numericInput("b1_2", label = "Pierwszy element wektora B1", value = 0.90),
                                    checkboxInput("wybor_dane2_d", label = "Dane 2 jako wektor x", value = 1)
                                ),
                                mainPanel(
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Parametry wyjściowe", verbatimTextOutput("parametry_d")),
                                                tabPanel("Ksi", DT::dataTableOutput("ksi_all_d")),
                                                tabPanel("Ksi opóźnione", DT::dataTableOutput("ksi_lag_d")),
                                                tabPanel("Wykres", plotOutput("wykres_ksi_d"))
                                    )
                                )
                            )
                   ),
                   tabPanel("Porównanie",
                            mainPanel(
                                verbatimTextOutput("porownanie")
                            )
                            )
        )
    )
)