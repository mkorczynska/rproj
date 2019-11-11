options(shiny.maxRequestSize = 30*1024^2)
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(tm)
library(corpus)
library(topicmodels)
library(tidytext)
library(ldatuning)

shinyServer(function(input, output) {
    output$texts <- DT::renderDataTable(DT::datatable({
        req(input$corp_file)
        
        tryCatch({
            load(file=input$corp_file$datapath)
            corp<<-data.frame(text = sapply(bigcorp, as.character), stringsAsFactors = FALSE)
            b_corp<<-bigcorp
            dtm<<-DocumentTermMatrix(b_corp) #tm
        },
        error = function(e){
            stop(safeError(e))
            }
        )
        return(corp)
        
    }))
    
    #liczebnosc wyrazow
    output$stats <- DT::renderDataTable({
        term_stats<-term_stats(b_corp) #corpus
    })
    
    #wyrazenia wielowyrazowe
    output$terms<-DT::renderDataTable({
        terms<-term_stats(corp, ngrams = input$number, types = TRUE)
    })
    
    #wykres liczebnosci wyrazow w tekstach
    output$term_plot<-renderPlot({
        chunks <- text_split(corp, "tokens", 5000) #corpus
        count <- text_count(chunks, input$text) #corpus
        i <- seq_along(count)
        
        plot(i, count, type = "l", xlab = "Tekst",
             ylab = "Liczba wystąpień",
             main = paste(dQuote(input$text), "- występowanie w tekstach"), col = 2)
        points(i, count, pch = 19, cex = 0.8, col = 2)
    })
    
    #LDA
    output$lda<-DT::renderDataTable({
        num_topics<<-input$topics
        lda<<-LDA(dtm, num_topics) #topicmodels
        text_topics <<- tidy(lda, matrix = "beta") #tidytext
        
        beta_spread <<- text_topics %>%
            mutate(topic = paste0("topic", topic)) %>%
            spread(topic, beta) %>%
            filter(topic1 > .001 | topic2 > .001) %>%
            mutate(log_ratio = log2(topic2 / topic1))
        
        datatable(beta_spread)
    })
    
    #tematy LDA
    output$lda_topics<-renderPlot({
        num_topics<<-input$topics
        lda<<-LDA(dtm, num_topics)
        topics <- tidy(lda, matrix = "beta")
        topics %>%
            group_by(topic) %>%
            top_n(15, beta) %>%
            ungroup() %>%
            arrange(beta) %>%
            mutate(term = factor(term, levels = unique(term))) %>%
            ggplot() +
            geom_col(aes(term, beta, fill = factor(topic)), color = "gray50", show.legend = FALSE) +
            facet_wrap(~topic, scales = "free_y") +
            coord_flip()
    })
    
    #metryki arun, devaud
    output$ar_dev<-renderPlot({
        results <- FindTopicsNumber( #ldatuning
            dtm,
            topics = seq(from = 2, to = 20, by = 2),
            metrics = c("Arun2010", "Deveaud2014"),
            method = "Gibbs",
            control = list(seed = 77),
            mc.cores = 3L,
            verbose = TRUE
        )
        FindTopicsNumber_plot(results) #ldatuning
    })
    
    #metryki griffiths, caojuan
    output$grif_cao<-renderPlot({
        results_2 <- FindTopicsNumber(
            dtm,
            topics = seq(from = 2, to = 20, by = 2),
            metrics = c("Griffiths2004", "CaoJuan2009"),
            method = "Gibbs",
            control = list(seed = 77),
            mc.cores = 3L,
            verbose = TRUE
        )
        FindTopicsNumber_plot(results_2)
    })
    
    #teksty i tematy
    output$texts_groups<-DT::renderDataTable({
        num_topics<<-input$topics
        lda<<-LDA(dtm, num_topics)
        ldaOut.topics <- as.matrix(topics(lda))
        corp_df<-data.frame(text = sapply(corp, as.character), stringsAsFactors = FALSE)
        texts_groups<-cbind(corp_df, ldaOut.topics)
        texts_groups
    })
    
    #zapisanie slow dla poszczegolnych tematow
    output$words_in_topics<-renderTable({
        num_topics<<-input$topics
        lda<<-LDA(dtm, num_topics)
        num_words<<-input$words
        ldaOut.terms <- as.matrix(terms(lda, num_words))
        ldaOut.terms
    })
    
})