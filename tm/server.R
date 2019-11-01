#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.maxRequestSize = 30*1024^2)
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(scales)
library(RSelenium)
library(rvest)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(tidytext)
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



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$texts <- DT::renderDataTable(DT::datatable({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        
        req(input$corp_file)
        
        # when reading semicolon separated files,
        # having a comma separator causes `read.csv` to error
        tryCatch(
            {
                load(file=input$corp_file$datapath)
                corp<<-data.frame(text = sapply(bigcorp, as.character), stringsAsFactors = FALSE)
                b_corp<<-bigcorp
                #df<-readRDS(input$file1$datapath)
                #df<-data.frame(text = sapply(df, as.character), stringsAsFactors = FALSE)
                
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
        
        return(corp)
        
    }))
    
    output$stats <- DT::renderDataTable({
        term_stats<-term_stats(corp)
    })
    
    output$terms_3<-DT::renderDataTable({
        terms_3<-term_stats(corp, ngrams = input$number, types = TRUE)
    })
    
    output$term_plot<-renderPlot({
        chunks <- text_split(corp, "tokens", 5000)
        
        count <- text_count(chunks, input$text)
        
        i <- seq_along(count)
        plot(i, count, type = "l", xlab = "Tekst",
             ylab = "Liczba wystapien",
             main = paste(dQuote(input$text), "- wystepowanie w tekstach"), col = 2)
        points(i, count, pch = 16, cex = 0.5, col = 2)
    })
    
    output$dtm<-DT::renderDataTable({
        dtm = DocumentTermMatrix(b_corp)
        
        freq <- colSums(as.matrix(dtm))   
        length(freq)   
        ord <- order(freq)   
        
        #czestosc slow
        freq <- colSums(as.matrix(dtm))
        freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
        
        #ramka ze slowami i ich frekwencja
        word_freq <- data.frame(freq=freq)   
    })
    
    output$plot_2<-renderPlot({
        dtm = DocumentTermMatrix(b_corp)
        
        freq <- colSums(as.matrix(dtm))   
        length(freq)   
        ord <- order(freq)   
        
        #czestosc slow
        freq <- colSums(as.matrix(dtm))
        freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
        
        #ramka ze slowami i ich frekwencja
        word_freq <- data.frame(freq=freq)   
        
        p <- ggplot(subset(word_freq, freq>300), aes(x = reorder(word, -freq), y = freq)) +
            geom_bar(stat = "identity") + 
            theme(axis.text.x=element_text(angle=45, hjust=1))
        p 
    })
    
    output$plot_wordcloud<-renderPlot({
        dtm = DocumentTermMatrix(b_corp)
        
        freq <- colSums(as.matrix(dtm))   
        length(freq)   
        ord <- order(freq)   
        
        #czestosc slow
        freq <- colSums(as.matrix(dtm))
        freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
        
        set.seed(20)   
        wordcloud(names(freq), freq, min.freq=50, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))  
    })
    
    output$kmeans1<-renderPlot({
        dtm = DocumentTermMatrix(b_corp)
        m<-as.matrix(dtm)
        distance<-dist(m, method = "euclidean")
        kfit <- kmeans(distance, 2, nstart=100)
        kfit <- kmeans(distance, centers = 7)
        clusters_kmeans<-as.data.frame(kfit$cluster)
        clusplot(as.matrix(distance), kfit$cluster, color=T, shade=T, labels=2, lines=0)
    })
    
    output$kmeans2<-renderPlot({
        dtm = DocumentTermMatrix(b_corp)
        m<-as.matrix(dtm)
        distance<-dist(m, method = "euclidean")
        wss <- 2:29
        for (i in 2:29) wss[i] <- sum(kmeans(distance,centers=i,nstart=25)$withinss)
        plot(2:29, wss[2:29], type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
    })
    
    output$lda<-DT::renderDataTable({
        dtm = DocumentTermMatrix(b_corp)
        num_topics<<-input$topics
        lda<<-LDA(dtm, num_topics)
        text_topics <- tidy(lda, matrix = "beta")
        datatable(text_topics)
        text_top_terms <- text_topics %>%
            group_by(topic) %>%
            top_n(10, beta) %>%
            ungroup() %>%
            arrange(topic, -beta)
        
        text_top_terms %>%
            mutate(term = reorder(term, beta)) %>%
            ggplot(aes(term, beta, fill = factor(topic))) +
            geom_col(show.legend = FALSE) +
            facet_wrap(~ topic, scales = "free") +
            coord_flip()
        
        beta_spread <<- text_topics %>%
            mutate(topic = paste0("topic", topic)) %>%
            spread(topic, beta) %>%
            filter(topic1 > .001 | topic2 > .001) %>%
            mutate(log_ratio = log2(topic2 / topic1))
        
        datatable(beta_spread)
    })
    
    output$plot_lda_1<-renderPlot({
        beta_spread %>%
            group_by(direction = log_ratio > 0) %>%
            top_n(15, abs(log_ratio)) %>%
            ungroup() %>%
            mutate(term = reorder(term, log_ratio)) %>%
            ggplot(aes(term, log_ratio)) +
            geom_col() +
            labs(y = "Log2 ratio of beta in topic 2 / topic 1") +
            coord_flip()
    })
    
    output$lda_topics<-renderPlot({
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
    
    output$dendro<-renderPlot({
        dtm = DocumentTermMatrix(b_corp)
        m<-as.matrix(dtm)
        distance<-dist(m, method = "euclidean")
        groups <- hclust(distance, method="ward.D")
        plot(groups, cex=0.9, hang=-1)
        rect.hclust(groups, k=7)
        plot(groups)
        clusters<-as.data.frame(cutree(groups,k=7))
    })
    
    output$clusters<-renderPlot({
        dtm = DocumentTermMatrix(b_corp)
        m<-as.matrix(dtm)
        distance<-dist(m, method = "euclidean")
        groups <- hclust(distance, method="ward.D")
        plot(groups, cex=0.6, hang = -1)
        rect.hclust(groups, k=3, border = "green")
        rect.hclust(groups, k=6, border = "blue")
        rect.hclust(groups, k=9, border = "red")
    })
    
    output$cloud <- renderPlot({
        wordcloud_rep <- repeatable(wordcloud)
        dtm = DocumentTermMatrix(b_corp)
        
        freq <- colSums(as.matrix(dtm))   
        length(freq)   
        ord <- order(freq)   
        
        #czestosc slow
        freq <- colSums(as.matrix(dtm))
        freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
        
        set.seed(20) 
        v <- freq
        wordcloud_rep(names(v), v, scale=c(4,0.5),
                      min.freq = input$freq, max.words=input$max,
                      colors=brewer.pal(8, "Dark2"))
    })
    
    output$ar_dev<-renderPlot({
        dtm = DocumentTermMatrix(b_corp)
        results <- FindTopicsNumber(
            dtm,
            topics = seq(from = 2, to = 10, by = 2),
            metrics = c("Arun2010", "Deveaud2014"),
            method = "Gibbs",
            control = list(seed = 77),
            mc.cores = 3L,
            verbose = TRUE
        )
        FindTopicsNumber_plot(results)
    })
    
    output$grif_cao<-renderPlot({
        dtm = DocumentTermMatrix(b_corp)
        results_2 <- FindTopicsNumber(
            dtm,
            topics = seq(from = 2, to = 30, by = 3),
            metrics = c("Griffiths2004", "CaoJuan2009"),
            method = "Gibbs",
            control = list(seed = 77),
            mc.cores = 3L,
            verbose = TRUE
        )
        FindTopicsNumber_plot(results_2)
    })
    
})

