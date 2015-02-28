library(grid)
library(reshape2)
library(ggplot2)
library(compete)
library(shiny)
library(shinydashboard)
library(dplyr)


shinyServer(function(input, output) {
  
  ### plotting theme
  mytheme <-  theme(
    plot.title = element_text(hjust=0,vjust=1, size=rel(2.3)),
    panel.background = element_blank(),
    panel.grid.major.y = element_line(color="gray85"),
    panel.grid.major.x = element_line(color="gray85"),
    panel.grid.minor = element_blank(),
    plot.background  = element_blank(),
    text = element_text(color="gray20", size=10),
    axis.text = element_text(size=rel(1.0)),
    axis.text.x = element_text(color="gray20",size=rel(1.5)),
    axis.text.y = element_text(color="gray20", size=rel(1.5)),
    axis.title.x = element_text(size=rel(1.5), vjust=0),
    axis.title.y = element_text(size=rel(1.5), vjust=1),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),
    legend.position = "none"
  )
  
  
  ### t-test tab
  

  
  mydf <- reactive({
  
  set.seed(as.numeric(input$text))
   
  A <-  rnorm(as.numeric(input$slider1), as.numeric(input$textxA), as.numeric(input$textsdA))
  B <-  rnorm(as.numeric(input$slider2), as.numeric(input$textxB), as.numeric(input$textsdB))
    
  mydf <- data.frame(
      values = c(A,B),
      group =  c(rep("A", as.numeric(input$slider1)), rep("B", as.numeric(input$slider2)))
                        )    
  })
  

  mydf1 <- reactive({
    
    set.seed(as.numeric(input$text))

    A <- rnorm(1000, as.numeric(input$textxA), as.numeric(input$textsdA))  
    B <- rnorm(1000, as.numeric(input$textxB), as.numeric(input$textsdB))
    
    res <- NULL
    for(i in 4:1000){
    res[[i]]  <- t.test(A[1:i], B[1:i])[[3]][[1]]
    }
    
    mydf1 <- data.frame(
      values = res,
      n =  1:1000
    )
    
  })
  
  
  
  output$plot2 <- renderPlot({
    
    ggplot(mydf(), aes(group, values)) + 
      geom_boxplot(aes(x=group, values, color=group, fill=group), alpha=0.1) + 
      geom_point(aes(color=group, size=3)) +
      scale_color_manual(values=c("dodgerblue", "darkorange")) +
      scale_fill_manual(values=c("dodgerblue", "darkorange")) +
      mytheme
    
  })
  

  output$text3 <- renderUI({
    
    mydf <- as.data.frame(mydf())
    x <- split(mydf$values, mydf$group)
    
    Group_As <- x[[1]]
    Group_Bs <- x[[2]]
    
    obj <-  t.test(Group_As, Group_Bs)

    
    HTML(
      paste0("t = ", round(obj[[1]],3), '<br/>', 
             "df = ", round(obj[[2]],3), '<br/>',
             "p-value = ", round(obj[[3]],5), '<br/>',
             '<br/>',
             "95% CI min t = ", round(obj[[4]][[1]],2), '<br/>',
             "95% CI max t = ", round(obj[[4]][[2]],2), '<br/>')
  )
    
    
    
  })
    

  output$plotX <- renderPlot({
    
    ggplot(mydf1(), aes(n, values)) + 
      geom_point(size=1) +
      geom_line() +
      ylim(0,1) +
      xlim(0,1000) +
      geom_hline(yintercept=0.05, color="red", lwd=1, lty=2) +
      ylab("p-value") +
      xlab("sample size (assumes equal by group)") +
      ggtitle("Discuss...") +
      mytheme +
      theme(plot.title = element_text(hjust=0,vjust=1, size=rel(1.4)))
    
    
  })
  
  
  output$plotY <- renderPlot({
    
    
    mydf <- as.data.frame(mydf())

    ax <- split(mydf, mydf$group)[[1]][[1]]
    bx <- split(mydf, mydf$group)[[2]][[1]]
    
    tstatobs <- t.test(ax,bx)[[1]][[1]]

    
    

    
gett <-  function(df) { 

  df1 <- data.frame(values = df$values, group = sample(df$group))

  ax <- split(df1, df1$group)[[1]][[1]]
  bx <- split(df1, df1$group)[[2]][[1]]
  
  tstat <- t.test(ax,bx)[[1]][[1]]
  return(tstat)
  
}

nperms <- as.numeric(input$text1)

myres <-  replicate(nperms, gett(mydf))
myresdf <- data.frame(results = myres)

toright <- round(sum(myres >= tstatobs) / nperms, 3)*100


my_grob = grobTree(textGrob(paste0("t >= observed t  = ", toright, "%"),  x=0.6,  y=0.95, hjust=0, vjust=0,
                            gp=gpar(col="navy", fontsize=12, fontface="italic")))

    
ggplot(myresdf, aes(results)) + 
      geom_bar(color="black", fill="gray89") +
      geom_vline(xintercept=tstatobs, color="red", lwd=1, lty=2) +
      ylab("Frequency") +
      xlab("Simulated values of t") +
      ggtitle("Red line = observed t-statistic") +
      mytheme +
      theme(plot.title = element_text(color="red", hjust=0,vjust=1, size=rel(1.2))) +
      annotation_custom(my_grob)
    
    
  })
  
  
 ### anova tab

andf <- reactive({
  
  set.seed(as.numeric(input$text))
  
  
  
  A <-  rnorm(as.numeric(input$an), as.numeric(input$amean), as.numeric(input$asd))
  B <-  rnorm(as.numeric(input$bn), as.numeric(input$bmean), as.numeric(input$bsd))
  C <-  rnorm(as.numeric(input$cn), as.numeric(input$cmean), as.numeric(input$csd))
  
  andf <- data.frame(
    values = c(A,B,C),
    group =  c(rep("A", as.numeric(input$an)), 
               rep("B", as.numeric(input$bn)),
               rep("C", as.numeric(input$cn))
               )
               )
  
})

  
output$plotA <- renderPlot({
  
  ggplot(andf(), aes(group, values)) + 
    geom_boxplot(aes(x=group, values, color=group, fill=group), alpha=0.1) + 
    geom_point(aes(color=group, size=3)) +
    scale_color_manual(values=c("dodgerblue", "darkorange", "orangered4")) +
    scale_fill_manual(values=c("dodgerblue", "darkorange", "orangered4")) +
    mytheme
  
})


output$text4 <- renderUI({
  
  andf <- as.data.frame(andf())  

  obj <- oneway.test(values ~ group, andf)




HTML(
    paste0("(Applies Welch's Correction)", '<br/>',
           "F = ", round(obj[[1]][[1]],3), '<br/>', 
           "num df = ", round(obj[[2]][[1]],2), '<br/>',
           "denom df = ", round(obj[[2]][[2]],2), '<br/>',
           "p-value = ", round(obj[[3]][[1]],4), '<br/>',
           '<br/>'
           )
  )
  
  
  
})

 

output$text5 <- renderUI({
  
  andf <- as.data.frame(andf())
    
obj <-  pairwise.t.test(andf$values, andf$group, p.adjust.method="holm") 

HTML(
    paste0("(P adjust method = holm's)", '<br/>',
           "A vs B: p= ", round(obj$p.value[[1]],3), '<br/>', 
           "A vs C: p= ", round(obj$p.value[[2]],3), '<br/>', 
           "B vs C: p= ", round(obj$p.value[[4]],3), '<br/>', 
           '<br/>'
    )
  )
  
  
  
})


output$plotB <- renderPlot({
  
  andf <- as.data.frame(andf())
  results = aov(values ~ group, andf)
  obj <- TukeyHSD(results, conf.level = 0.95)
  
  obj1 <- as.data.frame.matrix(obj$group)
  
  dfx <- data.frame(x = rownames(obj1),  y = c(obj1[,2],obj1[,3]))
  
  dfy <- data.frame(x1 = rownames(obj1), y1 = obj1[,1])
  
  ggplot(dfx, aes(x, y)) + 
    geom_point(size = 4) + 
    geom_point(aes(x1, y1), color = "red", fill="red", size=5, shape=17, data=dfy) +
    geom_line(aes(group = x), lwd=1) + 
    mytheme +
    geom_hline(yintercept=0, color="red", lwd=1, lty=2) +
    ylab("Difference") +
    xlab("Comparison") +
    theme(axis.text.x=element_text(angle=90, size=rel(1.5)))
    
})



  
  
}
)