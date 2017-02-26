__author__ = 'LTM'

##Hack U Machine Learning Course Project
##Predict the next most likely word in a user provided text string

import os
import nltk
import sys
import random

##Setting the working directory
os.chdir('C:/Users/LTM/DataScienceCertificateCapstone/Data/final/en_US/')
print os.getcwd()

##Reads in the data and cleans the non-ascii characters
textdoc = "./en_US.twitter.txt"
rawdoc = open(textdoc, 'r')

##Samples the data so we only read 1% of the file to make it manageable
##It also removes the non-ascii characters
op=open("./asciidoc.txt",'w')
for line in rawdoc:
        if random.uniform(0,1) <= .01 :
            line=line.strip().decode("ascii","ignore").encode("ascii")
            if line=="":continue
            op.write(line)
rawdoc.close()
op.close()

#Read in the data to a string
textdoc = "./asciidoc.txt"
rawdoc = open(textdoc, 'r')
docs = rawdoc.read()
print sys.getsizeof(docs)

##Convert to lower case
docs = docs.lower()

## Tokenize the data
print 'tokenizing....'
##This treats the punctuation as it's own token
tokens = nltk.wordpunct_tokenize(docs)
print tokens[0:10]

##POS tagging
##I don't actually need this but it might be useful later
tags = nltk.pos_tag(tokens)

##Generate the bi and trigrams
from nltk.collocations import *

trigram_measures = nltk.collocations.TrigramAssocMeasures()
finderT = TrigramCollocationFinder.from_words(tokens)

##This computest the frequency of occurrence for the trigrams 
scoredT = finderT.score_ngrams(trigram_measures.raw_freq)

##Here is the test input
sentence = raw_input("Please enter a partial sentence and I will guess the next word for you :" )
print sentence + '....'

##Processes the sentence in the same way as the corpus
sentence = sentence.lower()
tokSent = nltk.wordpunct_tokenize(sentence)
input =  tokSent[-2:]

##This sorts the entire list and removes the frequencies
##I don't actually need this but it might be useful later
tempSort = sorted(trigram for trigram, score in scoredT)

##Matches the input to the first two tokens of the trrigram
##Then sorts the results in order of freqeuncy
matches = [(trigram, score) for (trigram, score) in scoredT if trigram[:2] == tuple(input)]

##Generates a guess for the next word
guess = matches[0][0][2]
print 'Best guess for the next word is: ' + guess








