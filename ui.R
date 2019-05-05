
library(shiny)
library(shinythemes)
library(ggplot2)
library(DT)
library(psych)
library(doBy)
library(car)
library(e1071)
library(tidyverse)
library(mlbench)
library(psych)
library(skimr)
library(corrplot)
library(e1071)
library(plotly)
library(fmsb)
library(BaylorEdPsych)
library(gmodels)
library(lsr)
# Define UI for application that draws a histogram
shinyUI(navbarPage("Navbar",
                   tabPanel("Tweets about Candidate",
                            fluidPage(theme = shinytheme("yeti"),
                                      sidebarLayout(
                                        sidebarPanel(conditionalPanel( 'input.tabs == "Top 20 Words"',
                                                                       selectInput("a", "Candidate A:",
                                                                                  choices = list("Cory Booker",
                                                                                                 "Amy Klobuchar",
                                                                                                 "Bernie Sanders",
                                                                                                 "Kamala Harris" ,
                                                                                                 "Elizabeth Warren"), selected ="Kamala Harris") ,
                                                                      selectInput("b", "Candidate B:",
                                                                                  choices = list("Cory Booker",
                                                                                                 "Amy Klobuchar",
                                                                                                 "Bernie Sanders",
                                                                                                 "Kamala Harris" ,
                                                                                                 "Elizabeth Warren"), selected ="Bernie Sanders")),
                                                     conditionalPanel( 'input.tabs == "Top 15 Biograms"',
                                                                       selectInput("c", "Candidate A:",
                                                                                   choices = list("Cory Booker",
                                                                                                  "Amy Klobuchar",
                                                                                                  "Bernie Sanders",
                                                                                                  "Kamala Harris" ,
                                                                                                  "Elizabeth Warren"), selected ="Kamala Harris") ,
                                                                       selectInput("d", "Candidate B:",
                                                                                   choices = list("Cory Booker",
                                                                                                  "Amy Klobuchar",
                                                                                                  "Bernie Sanders",
                                                                                                  "Kamala Harris" ,
                                                                                                  "Elizabeth Warren"), selected ="Bernie Sanders")),
                                                     conditionalPanel( 'input.tabs == "Negative & Postive"',
                                                                       selectInput("aa", "Candidate A:",
                                                                                   choices = list("Cory Booker",
                                                                                                  "Amy Klobuchar",
                                                                                                  "Bernie Sanders",
                                                                                                  "Kamala Harris" ,
                                                                                                  "Elizabeth Warren"), selected ="Kamala Harris") ,
                                                                       selectInput("bb", "Candidate B:",
                                                                                   choices = list("Cory Booker",
                                                                                                  "Amy Klobuchar",
                                                                                                  "Bernie Sanders",
                                                                                                  "Kamala Harris" ,
                                                                                                  "Elizabeth Warren"), selected ="Bernie Sanders")),
                                                     conditionalPanel( 'input.tabs == "Sentiment"',
                                                                       selectInput("ab", "Candidate A:",
                                                                                   choices = list("Cory Booker",
                                                                                                  "Amy Klobuchar",
                                                                                                  "Bernie Sanders",
                                                                                                  "Kamala Harris" ,
                                                                                                  "Elizabeth Warren"), selected ="Kamala Harris") ,
                                                                       selectInput("cd", "Candidate B:",
                                                                                   choices = list("Cory Booker",
                                                                                                  "Amy Klobuchar",
                                                                                                  "Bernie Sanders",
                                                                                                  "Kamala Harris" ,
                                                                                                  "Elizabeth Warren"), selected ="Bernie Sanders"))
                                                     ),
                                                                      mainPanel(
                                                                                tabsetPanel(id = "tabs",
                                                                                tabPanel("Top 20 Words",h3(strong("Top Words Candidate A")), plotOutput("plot"),
                                                                                        h3(strong("Top Words Candidate B ")), plotOutput("plot2")),
                                                                                tabPanel("Top 15 Biograms",h3(strong("Top Biograms Candidate A")), plotOutput("plot3"),
                                                                                        h3(strong("Top Biograms Candidate B")), plotOutput("plot4")),
                                                                                tabPanel("Negative & Postive",h3(strong("Top Negative & Postive Words Candidate A")), 
                                                                                         plotOutput("plot5"),
                                                                                         h3(strong("Top Negative & Postive Words Candidate B")), plotOutput("plot6")),
                                                                                tabPanel("Sentiment",h3(strong("Sentiment Candidate A")), plotOutput("plot7"),
                                                                                         h3(strong("Sentiment Candidate B")), plotOutput("plot8"))
                                                                                )))
                                                     )
                                      ),
                   tabPanel("Personal Twitter Account",
                            sidebarLayout(
                              sidebarPanel(conditionalPanel( 'input.tabs2 == "Count of Tweets By Month"',
                                                             selectInput("ww", "Candidate A:",
                                                                         choices = list("Cory Booker",
                                                                                        "Amy Klobuchar",
                                                                                        "Bernie Sanders",
                                                                                        "Kamala Harris" ,
                                                                                        "Elizabeth Warren"), selected ="Amy Klobuchar") ,
                                                             selectInput("rr", "Candidate B:",
                                                                         choices = list("Cory Booker",
                                                                                        "Amy Klobuchar",
                                                                                        "Bernie Sanders",
                                                                                        "Kamala Harris" ,
                                                                                        "Elizabeth Warren"), selected ="Elizabeth Warren")),
                                           conditionalPanel( 'input.tabs2 == "Count of Tweets By Day of Week"',
                                                             selectInput("ff", "Candidate A:",
                                                                         choices = list("Cory Booker",
                                                                                        "Amy Klobuchar",
                                                                                        "Bernie Sanders",
                                                                                        "Kamala Harris" ,
                                                                                        "Elizabeth Warren"), selected ="Amy Klobuchar") ,
                                                             selectInput("gg", "Candidate B:",
                                                                         choices = list("Cory Booker",
                                                                                        "Amy Klobuchar",
                                                                                        "Bernie Sanders",
                                                                                        "Kamala Harris" ,
                                                                                        "Elizabeth Warren"), selected ="Elizabeth Warren")),
                                           conditionalPanel( 'input.tabs2 == "Emotion Sentiment"',
                                                             selectInput("sent", "Candidate A:",
                                                                         choices = list("Cory Booker",
                                                                                        "Amy Klobuchar",
                                                                                        "Bernie Sanders",
                                                                                        "Kamala Harris" ,
                                                                                        "Elizabeth Warren"), selected ="Amy Klobuchar") ,
                                                             selectInput("sent2", "Candidate B:",
                                                                         choices = list("Cory Booker",
                                                                                        "Amy Klobuchar",
                                                                                        "Bernie Sanders",
                                                                                        "Kamala Harris" ,
                                                                                        "Elizabeth Warren"), selected ="Elizabeth Warren")),
                                           conditionalPanel( 'input.tabs2 == "Postive & Negative Sentiment"',
                                                             selectInput("sent3", "Candidate A:",
                                                                         choices = list("Cory Booker",
                                                                                        "Amy Klobuchar",
                                                                                        "Bernie Sanders",
                                                                                        "Kamala Harris" ,
                                                                                        "Elizabeth Warren"), selected ="Amy Klobuchar"),
                                                             selectInput("sent4", "Candidate B:",
                                                                         choices = list("Cory Booker",
                                                                                        "Amy Klobuchar",
                                                                                        "Bernie Sanders",
                                                                                        "Kamala Harris" ,
                                                                                        "Elizabeth Warren"), selected ="Elizabeth Warren")),
                                           conditionalPanel( 'input.tabs2 == "Biograms"',
                                                             selectInput("bio1", "Candidate A:",
                                                                         choices = list("Cory Booker",
                                                                                        "Amy Klobuchar",
                                                                                        "Bernie Sanders",
                                                                                        "Kamala Harris" ,
                                                                                        "Elizabeth Warren"), selected ="Amy Klobuchar"),
                                                             selectInput("bio2", "Candidate B:",
                                                                         choices = list("Cory Booker",
                                                                                        "Amy Klobuchar",
                                                                                        "Bernie Sanders",
                                                                                        "Kamala Harris" ,
                                                                                        "Elizabeth Warren"), selected ="Elizabeth Warren"))
                                           
                                
                              ),
                              mainPanel(tabsetPanel( id = "tabs2",
                                                     tabPanel("Count of Tweets By Month",h3(strong("Candidate A")), plotOutput("plot11"),
                                                              h3(strong("Candidate B")), plotOutput("plot22")),
                                                     tabPanel("Count of Tweets By Day of Week",h3(strong("Candidate A")), plotOutput("plot33"),
                                                              h3(strong("Candidate B")), plotOutput("plot44")),
                                                     tabPanel("Emotion Sentiment",h3(strong("Candidate A")), plotOutput("plot55"),
                                                              h3(strong("Candidate B")), plotOutput("plot66")),
                                                     tabPanel("Postive & Negative Sentiment",h3(strong("Candidate A")), plotOutput("plot77"),
                                                              h3(strong("Candidate B")), plotOutput("plot88")),
                                                     tabPanel("Biograms",h3(strong("Candidate A")), plotOutput("plot99"),
                                                              h3(strong("Candidate B")), plotOutput("plot111"))
                                                     ))
                            ))
                                      )
)
                                    
                           
