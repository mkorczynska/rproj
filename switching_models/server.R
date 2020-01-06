# server.R

library(EIAdata)
library(quantmod)
library(dygraphs)
library(DT)
library(dplyr)
library(kableExtra)
library(ggplot2)
library(tidyr)
library(plotly)
library(crosstalk)
source("C://Users/HP ENVY/Documents/R/Projects/rproj/switching_models/functions.r")


shinyServer(function(input, output) {
    #wczytywanie pliku .csv z danymi
    output$dane <- DT::renderDataTable(DT::datatable({
        
        req(input$file1)
        
        tryCatch(
            {
                datafile<<-read.csv(input$file1$datapath,
                               header = input$header1,
                               sep = input$separator1,
                               dec = input$decimal1)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        
    }))
    
    #wczytywanie pliku .csv z daymi
    output$dane2 <- DT::renderDataTable(DT::datatable({
        
        req(input$file2)
        
        tryCatch(
            {
                datafile2<<-read.csv(input$file2$datapath,
                                    header = input$header2,
                                    sep = input$separator2,
                                    dec = input$decimal2)
            },
            error = function(e) {
                stop(safeError(e))
            }
        )
        
    }))

    
    output$stopy_zwrotu <- DT::renderDataTable(DT::datatable({ #dodac wybor kolumny do wczytania konkretow (data, zamkniecie)
        rates <- datafile %>% select(czas_1 = input$nazwa_data, ceny = input$nazwa_ceny) %>% 
            mutate(Stopa = log(ceny/lag(ceny, 1)), czas_1 = as.Date(czas_1, format="%d.%m.%Y"))
        
        rates <<- rates[-c(1),]   # Bez pierwszej obserwacji, ktĂłrej nie ma dla stĂłp zwrotu
        rates
    }))
    
    output$wykres_ceny<-renderPlotly({
      plot_ly(rates, x=~rates$czas_1, y=~rates$ceny, type = "scatter", mode = 'lines')
    })
    
    output$wykres_dane<-renderPlotly({
      dw <- datafile2 %>% select(czas_2 = input$nazwa_czas, sens = input$nazwa_kolumny)
      plot_ly(dw, x=~dw$czas_2, y=~dw$sens, type = "scatter", mode = 'lines')
    })
    
    output$parametry_s<-renderPrint({
        r <<- rates$Stopa
        no <<- dim(rates)[1] 
        one <<- c(1,1)
        
        dat <- data.frame(a = numeric(0), b = numeric(0))
        
        withProgress(message = 'Obliczanie..', value = 0, {
            number <- 10
            
            for (i in 1:number) {
                dat <- rbind(dat, data.frame(a = rnorm(1), b = rnorm(1)))
                incProgress(1/number, detail = paste("Krok:", i))
                Sys.sleep(0.1)
            }
        })

        
        if (input$wybor_dane2 != 1){
          x <<- cbind(rep(1, no), rnorm(no, 0, 0.01))
        }else{
          dw <- datafile2 %>% select(czas_2 = input$nazwa_czas, sens = input$nazwa_kolumny)
          x<<- cbind(rep(1, no), dw$sens)
        }
        start_s = c(m1=mean(r), s1=sd(r), m2=0, s2=sd(r), b0 = input$p11, b1 = input$p22)
        params_s <<- optim(start_s, lh, dynamic = F, method = input$typoptymalizacji)
        params_s$par
    })
    
    output$ksi_all_s<-DT::renderDataTable(DT::datatable({
        ksi_matrices_s <<- lh(params_s$par, dynamic = F, return_ksi = T)
        
        ksi_all_s <<- ksi_matrices_s[[1]]
        ksi_all_s <- t(ksi_all_s)
        ksi_all_s
    }))
    
    
    output$ksi_lag_s<-DT::renderDataTable(DT::datatable({
        ksi_lag_all_s <<- (ksi_matrices_s[[2]])[,-1]
        ksi_lag_all_s<-t(ksi_lag_all_s)
        ksi_lag_all_s
    }))
    
    output$ksi_smooth_s<-DT::renderDataTable(DT::datatable({
        ksi_smooth_all_s <- ksi_all_s[,1]
        p = matrix(data = c(params_s$par[5], 1-params_s$par[5], 1-params_s$par[6], params_s$par[6]), nrow = 2, byrow = T)
      
        for(t in (no-1):1){
          ksi_smooth_s <- ksi_all_s[,t]*(p%*%(ksi_lag_all_s[,no-1]/ksi_lag_all_s[,t]))
          ksi_smooth_all_s <- cbind(ksi_smooth_all_s, ksi_smooth_s)
        }
      
        # "odwrócenie" wygładzonego ksi, które było obliczane od końca
        ksi_smooth_all_s <<- rbind(rev(ksi_smooth_all_s[1,]), rev(ksi_smooth_all_s[2,]))
        t(ksi_smooth_all_s)
    }))
    
    output$wykresy_s<-renderPlot({ #plot dla zamkniecia czy stop zwrotu
        plot_data <- cbind(rates, Ksi = ksi_all_s[1,], Smooth_Ksi = ksi_smooth_all_s[1,])
      
        # Połączenie danych na potrzeby 
        gathered <- plot_data %>% select(Date = czas_1, index = ceny, Ksi, Smooth_Ksi) %>%
          gather(key = "Type", value = "Value", -Date, factor_key = T)
      
        gathered$Date <- as.Date(gathered$Date)
      
        ggplot(gathered, aes(Date, Value, group=1)) +
          geom_line() +
          facet_wrap(~Type, ncol = 1, scales = "free_y")
    })
    
    output$wykres_ksi<-renderPlotly({
      plot_data <- cbind(rates, Ksi = ksi_all_s[1,], Smooth_Ksi = ksi_smooth_all_s[1,])
      dw <- plot_data %>% select(Date = czas_1, Ksi)
      plot_ly(dw, x=~dw$Date, y=~dw$Ksi, type = "scatter", mode = 'lines')
    })
    
    output$wykres_ksi_smooth<-renderPlotly({
      plot_data <- cbind(rates, Ksi = ksi_all_s[1,], Smooth_Ksi = ksi_smooth_all_s[1,])
      dw <- plot_data %>% select(Date = czas_1, Smooth_Ksi)
      plot_ly(dw, x=~dw$Date, y=~dw$Smooth_Ksi, type = "scatter", mode = 'lines')
    })
    
    output$parametry_d<-renderPrint({
      r <<- rates$Stopa
      no <<- dim(rates)[1] 
      one <<- c(1,1)
      
      dat <- data.frame(a = numeric(0), b = numeric(0))
      
      withProgress(message = 'Obliczanie..', value = 0, {
        number <- 10
        
        for (i in 1:number) {
          dat <- rbind(dat, data.frame(a = rnorm(1), b = rnorm(1)))
          incProgress(1/number, detail = paste("Krok: ", i))
          Sys.sleep(0.1)
        }
      })
      
      
      if (input$wybor_dane2_d != 1){
        x <<- cbind(rep(1, no), rnorm(no, 0, 0.01))
      }else{
        dw <- datafile2 %>% select(czas_2 = input$nazwa_czas, sens = input$nazwa_kolumny)
        x <<- cbind(rep(1, no), dw$sens)
      }
      start_d = c(m1=mean(r), s1=sd(r), m2=0, s2=sd(r), b0 = c(input$b0_1, input$b0_2), b1=c(input$b1_1, input$b1_2))
      params_d <<- optim(start_d, lh, dynamic = T, method = input$typoptymalizacji)
      params_d$par
    })
    
    output$ksi_all_d<-DT::renderDataTable(DT::datatable({
      ksi_matrices_d <<- lh(params_d$par, dynamic = T, return_ksi = T)
      
      ksi_all_d <<- ksi_matrices_d[[1]]
      ksi_all_d <- t(ksi_all_d)
      ksi_all_d
    }))
    
    
    output$ksi_lag_d<-DT::renderDataTable(DT::datatable({
      ksi_lag_all_d <<- (ksi_matrices_d[[2]])[,-1]
      ksi_lag_all_d<-t(ksi_lag_all_d)
      ksi_lag_all_d
    }))
    
    output$ksi_smooth_d<-DT::renderDataTable(DT::datatable({
      ksi_smooth_all_d <- ksi_all_d[,1]
      #p = matrix(data = c(params_s$par[5], 1-params_s$par[5], 1-params_s$par[6], params_s$par[6]), nrow = 2, byrow = T)
      
      for(t in (no-1):1){
        ksi_smooth_s <- ksi_all_s[,t]*(p%*%(ksi_lag_all_s[,no-1]/ksi_lag_all_s[,t]))
        ksi_smooth_all_s <- cbind(ksi_smooth_all_s, ksi_smooth_s)
      }
      
      # "odwrócenie" wygładzonego ksi, które było obliczane od końca
      ksi_smooth_all_s <<- rbind(rev(ksi_smooth_all_s[1,]), rev(ksi_smooth_all_s[2,]))
      t(ksi_smooth_all_s)
    }))
    
    output$wykresy_d<-renderPlot({ #plot dla zamkniecia czy stop zwrotu
      plot_data_d <- cbind(rates, Ksi = ksi_all_d[1,], x = x[,2])
      
      # Połączenie danych na potrzeby 
      gathered_d <- plot_data_d %>% select(Date = Data, index = Zamkniecie, Ksi, x) %>%
        gather(key = "Type", value = "Value", -Date, factor_key = T)
      
      gathered_d$Date <- as.Date(gathered_d$Date)
      
      ggplot(gathered_d, aes(Date, Value, group=1)) +
        geom_line() +
        facet_wrap(~Type, ncol = 1, scales = "free_y") +
        ggtitle("Model przełącznikowy z dynamiczną macierzą przejść")
    })
    
    output$wykres_ksi_d<-renderPlotly({
      plot_data_d <- cbind(rates, Ksi = ksi_all_d[1,], x = x[,2])
      dw <- plot_data_d %>% select(Date = czas_1, Ksi, x)
      plot_ly(dw, x=~dw$Date, y=~dw$Ksi, type = "scatter", mode = 'lines')
    })
    
    output$porownanie<-renderPrint({
      lm_stat <- 2*(params_s$value - params_d$value)
      lm_stat
    })
    
    })