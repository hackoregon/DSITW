##This worked as of 4-9-15
##Don't fuck it up

library(stringr)
library(tm)
library(dplyr)
library(wordcloud)

##Preprocess the input text
prepInput <- function(input){
        docs <- Corpus(VectorSource(input))
        docs <- tm_map(docs, function(x) iconv(x, "latin1", "ASCII", sub=""))
        docs <- tm_map(docs, removePunctuation)
        docs <- tm_map(docs, removeNumbers)
        docs <- tm_map(docs, tolower)
        docs <- tm_map(docs, stripWhitespace)  
        docs <- tm_map(docs, PlainTextDocument)
        input <- as.character(docs[[1]])
        el <- unlist(str_split(input," "))
        return(el)
}

##BI-GRAM PREDICTION
tbiPred <- function(el) {
        unigram <- el[length(el)]
        outDat <- wf_B[wf_B$start == unigram,]
        return(outDat)
}

##TRI-GRAM PREDICTION
ttriPred <- function(el) {
        bigram <- paste(el[length(el)-1],el[length(el)])
        outDat <- wf_T[wf_T$start == bigram,]
        return(outDat)
}

##QUAD-GRAM PREDICTION
tquadPred <- function(el) {
        trigram <- paste(el[length(el)-2],el[length(el)-1],el[length(el)])
        outDat <- wf_Q[wf_Q$start == trigram,]
        return(outDat)
}

##looks iteratively through the ngram models to find the best fit
tansPred <- function(input) {
        if(nrow(tquadPred(input)) != 0) {return(tquadPred(input))}
        if(nrow(ttriPred(input)) != 0) {return(ttriPred(input))}
        if(nrow(tbiPred(input)) != 0) {return(tbiPred(input))}
}

##BEST GUESS PICKER
bestOf <- function(inFrame) {
        if(is.null(inFrame)) { return("Sorry you have me stumped.  Try again?") }
        ans <- sample(inFrame$end[inFrame$count == max(inFrame$count)],1)
        return(ans)
}

##NEXT BEST THREE GUESSES
nextBest <- function(inFrame,ans) {
        if(is.null(inFrame)) { return("")}
        temp <- arrange(inFrame[inFrame$end != ans,])
        if(nrow(temp) >= 3) {otherAns<-slice(temp,1:3)}
        else {otherAns<-slice(temp,nrow(temp)) }
        return(otherAns$end)
}

##WORDCLOUD GENERATION
wordOut <- function(inFrame){
        if(is.null(inFrame)) { #plots a ? if there is no data to work with
                count <- 1
                end <- "?"
                inFrame <- data.frame(count,end)       
        }
        wordcloud(inFrame$end,inFrame$count, min.freq = 1, max.words = 25 )
}

# ##Read in data of nprocessed n-grams
# #######################
# wf_B<-readRDS("C:/Users/LTM/DataScienceCertificateCapstone/ShinyTest/data/wf_B.rds")
# wf_T<-readRDS("C:/Users/LTM/DataScienceCertificateCapstone/ShinyTest/data/wf_T.rds")
# wf_Q<-readRDS("C:/Users/LTM/DataScienceCertificateCapstone/ShinyTest/data/wf_Q.rds")

wf_B<-readRDS("./data/wf_B.rds")
wf_T<-readRDS("./data/wf_T.rds")
wf_Q<-readRDS("./data/wf_Q.rds")


##Start the shiny server
shinyServer(function(input, output) {
        inDat <- reactive(prepInput(input$sent))
        inFrame <- reactive(tansPred(inDat()))
        ans <- reactive(bestOf(inFrame()))
        nextBests <- reactive(nextBest(inFrame(),ans()))
        output$out <- renderText(ans())
        output$nextBests <- renderText(nextBests())
        output$plot1 <- renderPlot({ wordOut(inFrame()) })
        }
        )
        
        

