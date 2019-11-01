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


# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        theme = shinytheme("flatly"),
        navbarPage("Grupowanie tekstow",
                   tabPanel("Korpus teksow",
                            sidebarLayout(
                                sidebarPanel(
                                    fileInput("corp_file", 
                                              "Wybierz plik korpusu",
                                              multiple=FALSE
                                    )
                                ),
                                mainPanel(
                                    DT::dataTableOutput("texts")
                                )
                            )
                   )
                   
                   
        )
    )
)

