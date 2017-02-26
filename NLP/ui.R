##This worked as of 4-9-16
##Don't change this
##Use it as a touchstone

library(shiny)

shinyUI(fluidPage(
        titlePanel(h1("Predictive Text for the Coursera Data Science Capstone", align = "center")
                
        ), # closes the title panel
        
        sidebarLayout(
                
                ##SIDEBAR PANEL LAYOUT BELOW##
                sidebarPanel(
                #br(),
                textInput("sent", label = "Please enter a sentence fragment"),
                submitButton("Submit"), #placeholder for submit
                helpText("Please give this a few seconds to load before you click submit.  There is a lot of data to load."),
                h3("How does this work?"),
                helpText("The predictive text algorithm used for this project was built by analyzing data scraped from Twitter, News, and Blog posts. The data was processed to identify sequences of up to four words (i.e. quad-grams to unigrams) and their respective likelihood of occurrence.When you type in a sentence fragment, the algorithm looks for a match to the last three words you have typed and outputs the most likely word to follow. .  If it can't match all three, it then tries the last two, then the last one.   The Wordcloud shown to the right represents all of the possible predictions, with their size proportionate to their likelihood of occurrence."),
                h3("Who made this?"),
                helpText(em("Prepared by Lindsay T Mico CEP, GISP during the Spring of 2016"))
                
                
        ), # closes the sidebar
                
                ##MAIN PANEL LAYOUT BELOW##
                mainPanel(h3("Best guess..."),
                br(),
                textOutput("out"),
                h3("A few other good guesses..."),
                br(),
                textOutput("nextBests"),
                br(),
                h3("Wordcloud!"),
                plotOutput("plot1")
                ) #closes the main panel layout
        )
))
        