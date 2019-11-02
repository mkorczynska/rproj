#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(RSelenium)
library(rvest)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tm)
library(corpus)
library(shinydashboard)
library(RColorBrewer)
library(wordcloud)
library(cluster)
library(ape)
library(cluster)
library(topicmodels)
library(ldatuning)


# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        theme = shinytheme("journal"),
        navbarPage("Grupowanie tekstów",
                   tabPanel("Korpus teksow",
                            sidebarLayout(
                                sidebarPanel(
                                    fileInput("corp_file", 
                                              "Wybierz plik korpusu",
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
                                tabPanel("Liczebnosc wyrazow",
                                         mainPanel(
                                             DT::dataTableOutput("stats")
                                         )#end mainPanel
                                ), #end tabPanel
                                
                                tabPanel("Liczebnosc wyrazen kilkuwyrazowych",
                                         sliderInput("number",
                                                     "liczba slow w wyrazeniu:",
                                                     min = 1,  max = 10, value=5
                                                     ), #end sliderInput
                                         mainPanel(
                                             DT::dataTableOutput("terms")
                                         )#end mainPanel
                                ), #end tabPanel
                                
                                tabPanel("Czestosc wystepowania wybranego slowa",
                                         textInput("text", 
                                                   label = h3("Text input"), 
                                                   value = "Wpisz słowo.."
                                                   ), #end textInput
                                         
                                         mainPanel(
                                             plotOutput("term_plot")
                                         )#end mainPanel
                                ), #end tabPanel
                                
                                tabPanel("Chmura słów",
                                         mainPanel(
                                             plotOutput("cloud")
                                         )#end mainPanel
                                )#end tabPanel
                            )#end navlistPanel
                   ), #end tabPanel
                   
                   tabPanel("LDA",
                            navlistPanel(
                                tabPanel("Arun, Deveaud",
                                         mainPanel(
                                             plotOutput("ar_dev"),
                                             plotOutput("grif_cao")
                                         )#end mainPanel
                                ), #end tabPanel
                                
                                tabPanel("prawdopodobienstwa dla slow",
                                         sliderInput("topics",
                                                     "liczba tematow:",
                                                     min = 1,  max = 10, value=2
                                         ), #end sliderInput
                                         
                                         mainPanel(
                                             DT::dataTableOutput("lda")
                                         )#end mainPanel
                                ), #end tabPanel
                                
                                tabPanel("Wykres tematow",
                                         
                                         mainPanel(
                                             plotOutput("lda_topics")
                                         )#end mainPanel
                                ), #end tabPanel
                                
                                tabPanel("Przypisanie",
                                         
                                         mainPanel(
                                             DT::dataTableOutput("przypis")
                                         )#end mainPanel
                                ), #end tabPanel
                                tabPanel("Slowa",
                                         
                                         mainPanel(
                                             DT::dataTableOutput("slowa")
                                         )#end mainPanel
                                )#end tabPanel
                            )#end navlistPanel
                   )#end tabPanel
        )#end navbarPage
    )#end fluidPage
)#end

