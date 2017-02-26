##Code base for Week six Work


##Loads packages
suppressMessages(library(tm))
suppressMessages(library(stringr))
suppressMessages(library(ggplot2))
suppressMessages(library(stringi))
library(R.utils)
library(openNLP)
library(RWeka)
library(rJava)
library(LaF)
library(wordcloud)
library(utils)
library(qdap)
library(ngram)
library(rlist)
library(dplyr)

dataFolder <- c('C:/Users/LTM/DataScienceCertificateCapstone/Data/final/en_US/')
setwd(dataFolder)
#docs <- Corpus(DirSource(dataFolder))
#summary(docs)

#Reads the US Files
USTwit <- readLines("./en_US.twitter.txt")
UsNews<-readLines("./en_US.news.txt")
UsBlogs <- readLines("./en_US.blogs.txt")

##Counts the Number of lines in a file
USTWitLineCount <- countLines("en_US.twitter.txt")
USnewsLineCount <- countLines("./en_US.news.txt")
usblogLinesCount <- countLines("./en_US.blogs.txt")


#Samples 1% of the lines in each
twitSamp <- sample_lines("./en_US.twitter.txt",.01*USTWitLineCount)
newsSamp <- sample_lines("./en_US.news.txt",.01*USnewsLineCount)
blogSamp <- sample_lines("./en_US.blogs.txt",.01*usblogLinesCount)

#Goes to new dir to write files to
setwd("./Samp")

#Writes out text files of the sampled data
fileConn<-file("twit.txt")
writeLines(twitSamp, fileConn)
close(fileConn)

fileConn<-file("news.txt")
writeLines(newsSamp, fileConn)
close(fileConn)
##Blogs
fileConn<-file("blog.txt")
writeLines(blogSamp, fileConn)
close(fileConn)

curDir <-getwd()

##Makes a corpus
docs <- Corpus(DirSource(curDir))
summary(docs)

##Cleans up the data for later processing
#remove non-ascii characters
docs <- tm_map(docs, function(x) iconv(x, "latin1", "ASCII", sub=""))
#Removes punctuation
docs <- tm_map(docs, removePunctuation)
##Removes the numbers
docs <- tm_map(docs, removeNumbers)
##Converts to lowercase
docs <- tm_map(docs, tolower)
#strip the whitespace
docs <- tm_map(docs, stripWhitespace)   
#ensure that it is a plain text doc now 
docs <- tm_map(docs, PlainTextDocument)

#######################
##I need to test this before I decide...
##head(stopwords("en"))
##remove stopwords
#docs <- tm_map(docs, removeWords, stopwords("english"))


#####################################################################
#####################################################################
##TOKENIZATION AND N-GRAM PROCESSING

# function for uni-gram
unigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1)) 
#uni-gram tokens matrix
tdm_uni <- TermDocumentMatrix(docs, control = list(tokenize = unigram_tokenizer)) 

# function for bi-gram
bigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2)) 
#bi-gram tokens matrix
tdm_bi<- TermDocumentMatrix(docs, control = list(tokenize = bigram_tokenizer)) 

#function for tri-gram
trigram_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3)) 
#tri-gram tockens matrix
tdm_tri <- TermDocumentMatrix(docs, control = list(tokenize = trigram_tokenizer)) 

##This one takes a while ~5 min
#function for 4-gram
quad_tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4)) 
#4-gram tokens matrix
tdm_quad <- TermDocumentMatrix(docs, control = list(tokenize = quad_tokenizer)) 


#####################################################################
#####################################################################



####################
##Bigram sub-module
####################

##Makes a dataframe with the start and end of a bigram as columns
freq_B <- rowSums(as.matrix(tdm_bi))  
wf_B <- data.frame(word=names(freq_B),count=freq_B,stringsAsFactors=FALSE)
biL <- str_split(wf_B$word," ")
wf_B$start <- sapply(biL,FUN=function(x) x[1])
wf_B$end <- sapply(biL,FUN=function(x) x[2])

####################
##Trigram sub-module
####################

##Makes a dataframe with the starting two words and end of a trigram as columns
freq_T <- rowSums(as.matrix(tdm_tri))  #we use rowSums as the terms are along the rows
wf_T <- data.frame(word=names(freq_T),count=freq_T,stringsAsFactors=FALSE)
triL <- str_split(wf_T$word," ")
wf_T$start <- sapply(triL,FUN=function(x) paste(x[1],x[2]))
wf_T$end <- sapply(triL,FUN=function(x) x[3])

#####################
##Quad-Gram Submodule
#####################

##Makes a dataframe with the starting three words and end of a quadgram as columns
freq_Q <- rowSums(as.matrix(tdm_quad))  
wf_Q <- data.frame(word=names(freq_Q),count=freq_Q,stringsAsFactors=FALSE)
quadL <- str_split(wf_Q$word," ")
wf_Q$start <- sapply(quadL,FUN=function(x) paste(x[1],x[2],x[3]))
wf_Q$end <- sapply(quadL,FUN=function(x) x[4])


######################################
##SAVES THE OUTPUT OF THE N-GRAM MODEL
######################################

saveRDS(wf_B, "wf_B.rds")
saveRDS(wf_T,"wf_T.rds")
saveRDS(wf_Q, "wf_Q.rds")

##Read it out with this....
wf_B<-readRDS("C:/Users/LTM/DataScienceCertificateCapstone/ShinyTest/data/wf_B.rds")
wf_T<-readRDS("C:/Users/LTM/DataScienceCertificateCapstone/ShinyTest/data/wf_T.rds")
wf_Q<-readRDS("C:/Users/LTM/DataScienceCertificateCapstone/ShinyTest/data/wf_Q.rds")


######################################################################
######################################################################

#############################
##INPUT PROCESSING SUB-MODULE
#############################

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

###############################
##BIGRAM PREDICTION SUB-MODULE
###############################

##splits the input AND returns the next word that is most likely
biPred <- function(input) {
        #print("starting")
        el <- prepInput(input)
        #print(el)
        unigram <- el[length(el)]
        #print(unigram)
        temp_wf_B <- wf_B[wf_B$start == unigram,]
        #print(temp_wf_B)
        if(nrow(temp_wf_B) == 0) { return("empty") }
        ans <- sample(temp_wf_B$end[temp_wf_B$count == max(temp_wf_B$count)],1)
        return(ans)
}

###############################
##TRIGRAM PREDICTION SUB-MODULE
###############################

##splits the input AND returns the next word that is most likely
triPred <- function(input) {
        #print("starting")
        el <- prepInput(input)
        #print(el)
        bigram <- paste(el[length(el)-1],el[length(el)])
        temp_wf_T <- wf_T[wf_T$start == bigram,]
        #print(temp_wf_T)
        if(nrow(temp_wf_T) == 0) { return("empty") }
        ans <- sample(temp_wf_T$end[temp_wf_T$count == max(temp_wf_T$count)],1)
        return(ans)
}

###############################
##QUADGRAM PREDICTION SUB-MODULE
###############################

##splits the input AND returns the next word that is most likely
quadPred <- function(input) {
        #print("starting")
        el <- prepInput(input)
        #print(el)
        trigram <- paste(el[length(el)-2],el[length(el)-1],el[length(el)])
        temp_wf_Q <- wf_Q[wf_Q$start == trigram,]
        #print(temp_wf_Q)
        if(nrow(temp_wf_Q) == 0) { return("empty") }
        ans <- sample(temp_wf_Q$end[temp_wf_Q$count == max(temp_wf_Q$count)],1)
        return(ans)
}


###############################################################################
###############################################################################
#####################################################################
#####################################################################

#######################################
##PREDICTION WRAPPER FOR JUST THE ANSWER
########################################

##looks iteratively through the ngram models to find the best fit
ansPred <- function(input) {
        if(typeof(input) != "character") {return('That is not a proper sentence')}
        if(nchar(input) == 0) {return("You haven't entered anything!")}
        if(quadPred(input) != "empty") {return(quadPred(input))}
        if(triPred(input) != "empty") {return(triPred(input))}
        if(biPred(input) != "empty") {return(biPred(input))}
        return("Sorry you have me stumped.  Maybe try another sentence?") }

#####################################
##PREDICTION WRAPPER TESTING SCRIPT##
#####################################

##TO DO - extract a bunch of possible input strings and feed them in to test
test<- c("I am so excited to get home")
ansPred(test)

#############################################################################
#############################################################################

################################
##UPDATED OUTPUT##
##HAS BEST ANSWER##
##HAS NEXT THREE GOOD ANSWERS IF THEY EXIST##
############################################

##Note that all of the functions should return the 'outDat' df

##splits the input AND returns the next word that is most likely
tbiPred <- function(input) {
        el <- prepInput(input)
        unigram <- el[length(el)]
        outDat <- wf_B[wf_B$start == unigram,]
        if(nrow(outDat) == 0) { return("empty") }
        return(outDat)
}

###############################
##TRIGRAM PREDICTION SUB-MODULE
###############################

##splits the input AND returns the next word that is most likely
ttriPred <- function(input) {
        el <- prepInput(input)
        bigram <- paste(el[length(el)-1],el[length(el)])
        print(bigram)
        outDat <- wf_T[wf_T$start == bigram,]
        if(nrow(outDat) == 0) { return("empty") }
        return(outDat)
}


##############################
##Quad Gram updated function##
##############################

tquadPred <- function(input) {
        el <- prepInput(input)
        trigram <- paste(el[length(el)-2],el[length(el)-1],el[length(el)])
        outDat <- wf_Q[wf_Q$start == trigram,]
        if(nrow(outDat) == 0) { return("empty") }
        return(outDat)
}

######################
##BEST GUESS PICKER##
######################


bestOf <- function(input) {
        if(nrow(input) == 0) { return("oops") }
        ans <- sample(input$end[input$count == max(input$count)],1)
        return(ans)
}

###########################
##NEXT BEST THREE GUESSES##
###########################

nextBest <- function(input,ans) {
        temp <- arrange(input[input$end != ans,])
        if(nrow(temp) >= 3) {otherAns<-slice(temp,1:3)}
        else {otherAns<-slice(temp,nrow(temp)) }
        return(otherAns$end)
}


##looks iteratively through the ngram models to find the best fit
tansPred <- function(input) {
        #if(typeof(input) != "character") {return('That is not a proper sentence')}
        #if(nchar(input) == 0) {return("You haven't entered anything!")}
        if(nrow(tquadPred(input)) != 0) {return(tquadPred(input))}
        if(nrow(ttriPred(input)) != 0) {return(ttriPred(input))}
        if(nrow(tbiPred(input)) != 0) {return(tbiPred(input))}}
        #return("Sorry you have me stumped.  Maybe try another sentence?") }

# rollUpPred <- function(input) {
#         #if(typeof(input) != "character") {return('That is not a proper sentence')}
#         #if(nchar(input) == 0) {return("You haven't entered anything!")}
#         if(nrow(tquadPred(input)) != 0) {
#                 best<- tquadPred(input)
#                 next <- 
#                 }
#         if(nrow(ttriPred(input)) != 0) {return(ttriPred(input))}
#         if(nrow(tbiPred(input)) != 0) {return(tbiPred(input))}
#         
# }

#return("Sorry you have me stumped.  Maybe try another sentence?") }


########################################
##GENERATE A WORD CLOUD OF THE OPTIONS##
########################################

wordcloud(inFrame$end,inFrame$count, min.freq = 1)


###############################
##TESTING CODE THAT DOES WORK##
###############################

test<- c("test this out")
test2 <- prepInput(test)
ttriPred(test2)
inFrame<-tansPred(test2)
ans <- bestOf(inFrame)
ans
nextBest(inFrame,ans)
wordcloud(inFrame$end,inFrame$count, min.freq = 1)

####################################################################


##Exporting a png of a wordcloud
library(wordcloud)
count <- c(1,2,2,4,1,3,1)
end <- c("boy","this","was","fun","wasn't","it", "?")
inFrame <- data.frame(count,end)
#png("wordcloud_packages.png", width=12,height=8, units='in', res=300)
wordcloud(inFrame$end,inFrame$count, min.freq = 1, max.words = 25)
