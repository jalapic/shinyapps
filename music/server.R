library(shiny)
library(ggplot2)
library(grid)
library(dplyr)



#import data
newdf <- read.csv("newdf.csv", stringsAsFactors=F)



# set colors
mycolors <- c("#E41A1C", "#377EB8", "#4DAF4A",
              "#984EA3", "#FF7F00", "#FFFF33",
              "#A65628", "#F781BF", "#999999",
              "#FFE4C4", "#E0EEEE")





shinyServer(function(input, output) {
  


#### Reading in data and setting up dataframe


    
  
  df1 <- reactive({
    
    mintime <- input$timeper[1]
    maxtime <- input$timeper[2]
    
    df1 <- 
      newdf %>% filter(Time >= mintime & Time <= maxtime)
    
    ranks <- input$checkGroup
    
    
if (input$graphtype == 1) {
      
    
    
    
    if (input$winloser == 1) {
      
    df1 <- df1[((df1$winner %in% ranks) == T),]
    }
    
    
    else if (input$winloser == 2) {
      
    df1 <- df1[((df1$winner %in% ranks) == T | (df1$loser %in% ranks) == T),]
    }
    
    
    else
      
    df1 <- df1[((df1$loser %in% ranks) == T),]
    
    }
    
else   
  
{
  
  if (input$winloser == 1) {
  
  df1 <- df1[(df1$winner %in% ranks ==T) & (df1$loser %in% ranks ==T),]    
  df1 <- df1[((df1$winner %in% ranks) == T),]
  }


  else if (input$winloser == 2) {
  
  
    df1 <- df1[(df1$winner %in% ranks ==T) & (df1$loser %in% ranks ==T),]    
    df1 <- df1[((df1$winner %in% ranks) == T | (df1$loser %in% ranks) == T),]
   }


  else
  
  
    df1 <- df1[(df1$winner %in% ranks ==T) & (df1$loser %in% ranks ==T),]    
    df1 <- df1[((df1$loser %in% ranks) == T),]

      
}    
    
    
    })
  


  
  
  

  output$MyPlot <- renderPlot({
    
  # doing if/else to reactively change line-width / horizontal grid line colors ... must be a better way...
    
    if (input$linewt==1 & input$horizline==1) {
        
      ggplot() +
      geom_segment(aes(x = Time, xend = Time, y = winner, yend = loser, color=factor(winner)),
                   arrow=arrow(length = unit(.3, "cm")), lwd=.75, 
      data=df1())  +
      geom_point(aes(x = Time, y = winner, color = factor(winner)), size=3, data=df1()) + 
      coord_cartesian(ylim=c(0.5,11.5)) +
      scale_y_reverse(breaks=1:11) +
      scale_color_manual(values = mycolors, limits=c(1:11)) +
      theme_bw() +
      theme(legend.position = "none",
            panel.grid.major.y = element_line(color = mycolors, size=1),
            axis.text.y = element_text(color = mycolors)) +
      ylab("Animal rank")
    }

    else
      
    if (input$linewt==1 & input$horizline==2) {
      
      ggplot() +
        geom_segment(aes(x = Time, xend = Time, y = winner, yend = loser, color=factor(winner)),
                     arrow=arrow(length = unit(.3, "cm")), lwd=.75, 
                     data=df1())  +
        geom_point(aes(x = Time, y = winner, color = factor(winner)), size=3, data=df1()) + 
        coord_cartesian(ylim=c(0.5,11.5)) +
        scale_y_reverse(breaks=1:11) +
        scale_color_manual(values = mycolors, limits=c(1:11)) +
        theme_bw() +
        theme(legend.position = "none",
              panel.grid.major.y = element_line(size=1)) +
        ylab("Animal rank")
    }
    
    else

    if (input$linewt==2 & input$horizline==1) {
        
    ggplot() +
      geom_segment(aes(x = Time, xend = Time, y = winner, yend = loser, color=factor(winner)),
                   arrow=arrow(length = unit(.3, "cm")), lwd=.7, 
                   data=df1())  +
      geom_point(aes(x = Time, y = winner, color = factor(winner)), size=3, data=df1()) + 
      coord_cartesian(ylim=c(0.5,11.5)) +
      scale_y_reverse(breaks=1:11)+
      scale_color_manual(values = mycolors, limits=c(1:11)) +
      theme_bw() +
      theme(legend.position = "none",
       panel.grid.major.y = element_line(color = mycolors, size=1),
       axis.text.y = element_text(color = mycolors)) +
  ylab("Animal rank")
    
}

else

  ggplot() +
  geom_segment(aes(x = Time, xend = Time, y = winner, yend = loser, color=factor(winner)),
               arrow=arrow(length = unit(.3, "cm")), lwd=.7, 
               data=df1())  +
  geom_point(aes(x = Time, y = winner, color = factor(winner)), size=3, data=df1()) + 
  coord_cartesian(ylim=c(0.5,11.5)) +
  scale_y_reverse(breaks=1:11)+
  scale_color_manual(values = mycolors, limits=c(1:11)) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(size=1)) +
  ylab("Animal rank")

    
    })
  
  
})