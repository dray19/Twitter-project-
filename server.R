
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
   
  output$plot <- renderPlot({
    top_words(input$a)
  })
  output$plot2 <- renderPlot({
    top_words2(input$b)
  })
  output$plot3 <- renderPlot({
    bio_top(input$c)
  })
  output$plot4 <- renderPlot({
    bio_top2(input$d)
  })
  output$plot5 <- renderPlot({
    Bing %>% filter(name==input$aa) %>% count(word, sentiment, sort=TRUE) %>%
      group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
      ggplot(aes(x=reorder(word, n), y=n)) +
      geom_col(aes(fill=sentiment), show.legend=FALSE) +
      coord_flip() +
      facet_wrap(~sentiment, scales="free_y") +
      labs(x="", y="Number of Times Used", title=input$aa) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_fill_manual(values = c("positive"="green", "negative"="red")) +
      theme(text = element_text(size = 20))
  })
  output$plot6 <- renderPlot({
    Bing %>% filter(name==input$bb) %>% count(word, sentiment, sort=TRUE) %>%
      group_by(sentiment) %>% arrange(desc(n)) %>% slice(1:20) %>%
      ggplot(aes(x=reorder(word, n), y=n)) +
      geom_col(aes(fill=sentiment), show.legend=FALSE) +
      coord_flip() +
      facet_wrap(~sentiment, scales="free_y") +
      labs(x="", y="Number of Times Used", title=input$bb) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      scale_fill_manual(values = c("positive"="green", "negative"="red")) +
      theme(text = element_text(size = 20))
  })
  output$plot7 <- renderPlot({
    nrc %>% filter(name == input$ab) %>% count(sentiment) %>%
      ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
      geom_bar(stat="identity") + coord_polar() +
      theme(legend.position = "none", axis.text.x = element_blank()) +
      geom_text(aes(label=sentiment, y=13000)) + 
      scale_y_continuous(limits = c(0,20000), breaks = c(0,4000,8000,12000,16000, 20000)) +
      labs(x="", y="", title= input$ab) + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      theme(text = element_text(size = 20))
  })
  output$plot8 <- renderPlot({
    nrc %>% filter(name == input$cd) %>% count(sentiment) %>%
      ggplot(aes(x=sentiment, y=n, fill=sentiment)) +
      geom_bar(stat="identity") + coord_polar() +
      theme(legend.position = "none", axis.text.x = element_blank()) +
      geom_text(aes(label=sentiment, y=13000)) + 
      scale_y_continuous(limits = c(0,20000), breaks = c(0,4000,8000,12000,16000, 20000)) +
      labs(x="", y="", title= input$cd) + 
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      theme(text = element_text(size = 20))
  })
  output$plot11 <- renderPlot({
   plot_count(input$ww)
  })
  output$plot22 <- renderPlot({
    plot_count(input$rr)
  })
  output$plot33 <- renderPlot({
    plot_wday(input$ff)
  })
  output$plot44 <- renderPlot({
    plot_wday(input$gg)
  })
  output$plot55 <- renderPlot({
    if (input$sent == "Cory Booker"){
      return(tml_sent$CoryBooker$total_sent)
    } else if (input$sent == "Amy Klobuchar"){
      return(tml_sent$amyklobuchar$total_sent)
    } else if(input$sent == "Bernie Sanders"){
      return(tml_sent$SenSanders$total_sent)
    } else if (input$sent == "Kamala Harris"){
      return(tml_sent$KamalaHarris$total_sent)
    } else {
      return(tml_sent$SenWarren$total_sent)
    }
  })
  output$plot66 <- renderPlot({
    if (input$sent2 == "Cory Booker"){
      return(tml_sent$CoryBooker$total_sent)
    } else if (input$sent2 == "Amy Klobuchar"){
      return(tml_sent$amyklobuchar$total_sent)
    } else if(input$sent2 == "Bernie Sanders"){
      return(tml_sent$SenSanders$total_sent)
    } else if (input$sent2 == "Kamala Harris"){
      return(tml_sent$KamalaHarris$total_sent)
    } else {
      return(tml_sent$SenWarren$total_sent)
    }
  })
  output$plot77 <- renderPlot({
    if (input$sent3 == "Cory Booker"){
      return(tml_sent$CoryBooker$PN)
    } else if (input$sent3 == "Amy Klobuchar"){
      return(tml_sent$amyklobuchar$PN)
    } else if(input$sent3 == "Bernie Sanders"){
      return(tml_sent$SenSanders$PN)
    } else if (input$sent3 == "Kamala Harris"){
      return(tml_sent$KamalaHarris$PN)
    } else {
      return(tml_sent$SenWarren$PN)
    }
  })
  output$plot88 <- renderPlot({
      if (input$sent4 == "Cory Booker"){
        return(tml_sent$CoryBooker$PN)
      } else if (input$sent4 == "Amy Klobuchar"){
        return(tml_sent$amyklobuchar$PN)
      } else if(input$sent4 == "Bernie Sanders"){
        return(tml_sent$SenSanders$PN)
      } else if (input$sent4 == "Kamala Harris"){
        return(tml_sent$KamalaHarris$PN)
      } else {
        return(tml_sent$SenWarren$PN)
      }
  })
  output$plot99 <- renderPlot({
   cor_plot(input$bio1)
  })
  output$plot111 <- renderPlot({
    cor_plot2(input$bio2)
  })
})
