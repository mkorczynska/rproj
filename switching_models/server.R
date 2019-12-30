# server.R

library(EIAdata)
library(quantmod)
library(dygraphs)
library(DT)
library(dplyr)
library(kableExtra)
source("C://Users/HP ENVY/Documents/R/Projects/MP/MP/functions.r")


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
        rates
    }))
    
    output$wykres_zamkniecia<-renderPlot({ #plot dla zamkniecia czy stop zwrotu
        plot(as.Date(rates$Data), rates$Zamkniecie, type = "l")
    })
    
    output$parametry<-renderPrint({
        r <<- rates$Stopa
        n <<- dim(rates)[1] 
        x <<- rates$Stopa
        one <<- c(1,1)
        
        start_s = c(m1=mean(r), s1=sd(r), m2=0, s2=sd(r), b0 = 0.99, b1 = 0.99)
        
        # Create 0-row data frame which will be used to store data
        dat <- data.frame(x = numeric(0), y = numeric(0))
        
        withProgress(message = 'Making calculations', value = 0, {
            # Number of times we'll go through the loop
            n <- 10
            
            for (i in 1:n) {
                # Each time through the loop, add another row of data. This is
                # a stand-in for a long-running computation.
                dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                
                # Increment the progress bar, and update the detail text.
                incProgress(1/n, detail = paste("Doing part", i))
                
                # Pause for 0.1 seconds to simulate a long computation.
                Sys.sleep(0.1)
            }
        })
        par <- optim(start_s, lh, dynamic = F, method = "BFGS")
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