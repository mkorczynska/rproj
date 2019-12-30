# server.R

library(EIAdata)
library(quantmod)
library(dygraphs)
library(DT)
library(dplyr)
library(kableExtra)
source("C://Users/HP ENVY/Documents/R/functions.r")


shinyServer(function(input, output) {
    #wczytywanie pliku .csv z daymi
    output$dane <- DT::renderDataTable(DT::datatable({
        
        req(input$file1)
        
        tryCatch(
            {
                datafile<<-read.csv(input$file1$datapath,
                               header = input$header,
                               sep = input$sep)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        
    }))

    
    output$stopy_zwrotu <- DT::renderDataTable(DT::datatable({ #dodac wybor kolumny do wczytania konkretow (data, zamkniecie)
        rates <- datafile %>% select(Data, Zamkniecie) %>% 
            mutate(Stopa = log(Zamkniecie/lag(Zamkniecie, 1)), Data = as.Date(Data, format="%d.%m.%Y"))
        
        rates <<- rates[-c(1),]   # Bez pierwszej obserwacji, której nie ma dla stóp zwrotu
        r <<- rates$Stopa
        rates
    }))
    
    output$wykres_zamkniecia<-renderPlot({ #plot dla zamkniecia czy stop zwrotu
        plot(as.Date(rates$Data), rates$Zamkniecie, type = "l")
    })
    
    output$parametry<-renderPrint({
        n <<- dim(rates)[1] 
        x <<- rates$Stopa
        one <<- c(1,1)
        
        start = c(m1=mean(r), s1=sd(r), m2=0, s2=sd(r), b0 = c(2.2, 0), b1=c(2.94, 0)) #PARAMETRY
        
        par <- optim(start, lh, method = "Nelder-Mead")    # statyczne
        par$par
    })
    
    output$ksi_all<-DT::renderDataTable(DT::datatable({
        ksi_matrices <<- lh(par$par, return_ksi = T)
        
        ksi_all <- ksi_matrices[[1]]
        ksi_lag_all <- (ksi_matrices[[2]])[,-1]
        ksi_all<-t(ksi_all)
        ksi_all
    }))
    
    output$ksi_lag<-DT::renderDataTable(DT::datatable({
        ksi_lag_all <- (ksi_matrices[[2]])[,-1]
        ksi_lag_all<-t(ksi_lag_all)
        ksi_lag_all
    }))
    
    })