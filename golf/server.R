
#### Load Libraries ----
library(reshape2)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
library(ggvis)
library(formattable)
library(htmlwidgets)
library(purrr)
library(tidyr)
library(grid)
library(psych)

#### Begin Shiny Server ----
 shinyServer(function(input, output, session) {


### Dataframe to convert original variable names to human readable names: ----
   
  titledf <- data.frame(var1 = c("drivepct","driveavg"
                                 ,"girpct","goforgreenpct" 
                                 ,"sandsavepct","scramblingpct"
                                 ,"threeputtpct","oneputtpct"
                                 ,"putts.avg","birdiepct" 
                                 ,"score.avg" ,"birdiebogieratio"
                                 ,"par3.birdiepct","par4.birdiepct" 
                                 ,"par5.birdiepct" ,"top10s"
                                 ,"firsts","seconds" 
                                 ,"thirds","moneypct" ,"totalmoney"),
                        
                        var2 = c("Driving Accuracy",  "Driving Distance", "Greens in Regulation",
                                 "Goes For Green", "Sand Saves", "Scrambling", "Three Putts", "One Putts", "Putts Average",
                                 "Birdie %", "Scoring Average", "Birdie-Bogie Ratio", "Par 3 Birdie %", "Par 4 Birdie %",
                                 "Par 5 Birdie %", "Top 10s", "Firsts", "Seconds", "Thirds", "Money %",
                                 "Total Money") 
  )
  
  
  



#### Scatterplot Tab ----

# For Dynamic Titles
titlex1 <- reactive({  as.character(titledf$var2[match(input$selectx1, titledf$var1)])    })
titlex2 <- reactive({  as.character(titledf$var2[match(input$selectx2, titledf$var1)])    })
titley1 <- reactive({  as.character(titledf$var2[match(input$selecty1, titledf$var1)])    })
titley2 <- reactive({  as.character(titledf$var2[match(input$selecty2, titledf$var1)])    })


### Scatter 1 Get Data
mydf1 <- reactive({
  
  mydf1  <- pga
  
  if (input$yearinput=="All") { 
    mydf1
  }
  
  else
    
    if (input$yearinput!="All"){
      
      mydf1 <- mydf1 %>% filter(year==input$yearinput)
      
    }
  
  mydf1 <-  mydf1 %>% select(x1 = which(colnames(pga)==input$selectx1), y1=which(colnames(pga)==input$selecty1), 
                             label, name, year)
  
})


#Scatter1 Plot
reactive({
  
  if(input$radioscatter==2){
    mydf1() %>% 
    ggvis(~x1, ~y1, key:= ~label,  opacity := input$slider) %>% 
    layer_points(size.hover := 200, fill.hover := "red") %>% 
    set_options(width = "auto", height = "auto") %>%
    add_tooltip(function(data) data$label) %>% 
   add_axis("x", title = titlex1(), 
             properties = axis_props(
      title = list(fontSize = 20, dx=-5),
      labels = list(fontSize = 16)) )  %>% 
    add_axis("y", title = titley1(), 
             properties = axis_props(
               title = list(fontSize = 20, dy=-20),
               labels = list(fontSize = 16)) )  
 
  }
    
  
  else
    
    if(input$radioscatter==1){
      
      mydf1() %>% 
        ggvis(~x1, ~y1) %>% 
        layer_points(size.hover := 200, fill.hover := "red", key:= ~label,  opacity := input$slider) %>% 
        set_options(width = "auto", height = "auto") %>%
        add_tooltip(function(data) data$label) %>% 
        add_axis("x", title = titlex1(), 
                 properties = axis_props(
                   title = list(fontSize = 20, dx=-5),
                   labels = list(fontSize = 16)) )  %>% 
        add_axis("y", title = titley1(), 
                 properties = axis_props(
                   title = list(fontSize = 20, dy=-20),
                   labels = list(fontSize = 16)) )  %>%
        layer_model_predictions(model = "lm",  stroke:="red", fill.hover := "red", se = TRUE, strokeWidth := 5)
      
    }  
  
  })  %>%  bind_shiny("plot1", "plot1_ui")
  


#### Scatter2 Get Data
mydf2 <- reactive({
  
  mydf2  <- pga
  
  if (input$yearinput=="All") { 
    mydf2
  }
  
  else
    
    if (input$yearinput!="All"){
      
      mydf2 <- mydf2 %>% filter(year==input$yearinput)
      
    }
  
  mydf2 <-  mydf2 %>% select(
    x2 = which(colnames(pga)==input$selectx2), y2=which(colnames(pga)==input$selecty2), 
    label, name, year)
  
})

#Scatter2 Plot

  reactive({

    if(input$radioscatter==2){
      
      mydf2() %>% 
    ggvis(~x2, ~y2, key:= ~label,  opacity := input$slider) %>% 
    layer_points(size.hover := 200, fill.hover := "red") %>% 
    set_options(width = "auto", height = "auto") %>%
    add_tooltip(function(data) data$label) %>% 
    add_axis("x", title = titlex2(), 
             properties = axis_props(
               title = list(fontSize = 20, dx=-5),
               labels = list(fontSize = 16)) )  %>% 
    add_axis("y", title = titley2(), 
                 properties = axis_props(
                 title = list(fontSize = 20, dy=-20),
                  labels = list(fontSize = 16)) )  
    }
    else
      if(input$radioscatter==1){
    
              mydf2() %>% 
            ggvis(~x2, ~y2) %>% 
            layer_points(size.hover := 200, fill.hover := "red", key:= ~label,  opacity := input$slider) %>% 
            set_options(width = "auto", height = "auto") %>%
            add_tooltip(function(data) data$label) %>% 
            add_axis("x", title = titlex2(), 
                     properties = axis_props(
                       title = list(fontSize = 20, dx=-5),
                       labels = list(fontSize = 16)) )  %>% 
            add_axis("y", title = titley2(), 
                     properties = axis_props(
                       title = list(fontSize = 20, dy=-20),
                       labels = list(fontSize = 16)) )  %>%
          layer_model_predictions(model = "lm", stroke:="red", fill.hover := "red", se = TRUE, strokeWidth := 4)
        
      } 
    
      })  %>%  bind_shiny("plot2", "plot2_ui")

  
## Scatter1 Text Output
output$textp1 <- renderUI({
  
  x1val <- as.numeric(mydf1()$x1)
  y1val <- as.numeric(mydf1()$y1)
  
  str1 <- paste0("r = ",    round(cor.test(x1val, y1val)$estimate,3))
  str3 <- paste0("p = ",    round(cor.test(x1val, y1val)$p.value,3))
  
  HTML(paste(str1, str3, sep = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'))
  
})

## Scatter2 Text Output
output$textp2 <- renderUI({
  
  x2val <- as.numeric(mydf2()$x2)
  y2val <- as.numeric(mydf2()$y2)
  
  str1 <- paste0("r = ",    round(cor.test(x2val, y2val)$estimate,3))
  str3 <- paste0("p = ",    round(cor.test(x2val, y2val)$p.value,3))
  
  HTML(paste(str1, str3, sep = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;'))
  
})



####  Distribution Tab:----

#titles reactive
titlex0 <- reactive({  as.character(titledf$var2[match(input$selectx0, titledf$var1)])    })
titlex0a <- reactive({  as.character(titledf$var2[match(input$selectx0, titledf$var1)])    }) #found crashes if use same reactive in multiple other reactives, so keeping separate
titlex0b <- reactive({  as.character(titledf$var2[match(input$selectx0, titledf$var1)])    }) #found crashes if use same reactive in multiple other reactives, so keeping separate


### reactive data filtering for histogram viz:
mydf1a <- reactive({
  mydf1a  <- pga
  if (input$yearinput0=="All") { 
    mydf1a
  }
  else
    if (input$yearinput0!="All"){
      mydf1a <- mydf1a %>% filter(year==input$yearinput0)
    }
  mydf1a <-  mydf1a %>% select(
    x1a = which(colnames(pga)==input$selectx0), label, name, year)
})


#histogram
reactive({
  mydf1a()  %>% ggvis(~x1a) %>%
    layer_histograms(width = input$slider0, fill := "#E74C3C") %>% 
   set_options(width = "auto") %>%
    add_axis("x", title = titlex0(), 
             properties = axis_props(
               title = list(fontSize = 20, dx=-5),
               labels = list(fontSize = 16)) )  %>%  
    add_axis("y", title = "Count", 
             properties = axis_props(
               title = list(fontSize = 20, dy=-20),  #dy to offset
               labels = list(fontSize = 16)) )
  })  %>%  bind_shiny("plot1a", "plot1a_ui")




### reactive data filtering for boxplot viz:
mydf2a <- reactive({
  mydf2a  <- pga %>% select(
    x1a = which(colnames(pga)==input$selectx0), label, name, year)
})



# Boxplot
reactive({
  mydf2a()  %>% ggvis(~year, ~x1a) %>% 
    layer_points(key:= ~label, size := 15, fill.hover := "red") %>% 
    layer_boxplots(size := 0)   %>%
    set_options(width = "auto") %>%
    add_axis("y", title = titlex0a(), 
             properties = axis_props(
               title = list(fontSize = 20, dy=-20),
               labels = list(fontSize = 16)) )  %>%  
    add_axis("x", title = "Year", format="####", 
             properties = axis_props(
               title = list(fontSize = 20),
               labels = list(fontSize = 11)) )  %>%
    add_tooltip(function(data) data$label)
  })  %>%  bind_shiny("plot2a", "plot2a_ui")




## Text for Descriptive Stats
output$textp1a <- renderUI({
  
  x1aval <- as.numeric(mydf1a()$x1a)

  str1 <- paste0("mean = ",    round(mean(x1aval),2))
  str2 <- paste0("sd = ",    round(sd(x1aval),2))

  HTML(paste(str1, str2, sep = '&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;  <br><br>'))
  
 })





### Formattable-distributions  
## output - formattable

output$formattable1 <- renderFormattable({
  
  if(input$radio==1){
  pgadf <- mydf1a() %>% arrange(desc(x1a)) %>% filter(row_number()<=input$num) %>% select(name,year,x1a)
  }
  
  if(input$radio==2){
    pgadf <- mydf1a() %>% arrange(x1a) %>% filter(row_number()<=input$num) %>% select(name,year,x1a)
  }
  
  colnames(pgadf)[3] <- "value"
  
  formattable(pgadf, list(
    value = color_bar("pink", 0.8)
  ))
  
})



#### FormatTable Tab: ----


## output table
output$formattable2 <- renderFormattable({
 
  pga1x <- pga %>% select(name, year, drivepct, driveavg, girpct, goforgreenpct,
                          sandsavepct, scramblingpct, threeputtpct, birdiebogieratio,
                          putts.avg, score.avg, top10s, firsts) 
  
  NN <- which(colnames(pga1x)==input$selectx00) %>% as.numeric
  
   
  if(input$yearinput00!="All"){
  
  pga1 <- pga %>% select(Name=name, Year=year, Drive.Acc=drivepct, Drive.Avg=driveavg, GIR=girpct, Go.Green=goforgreenpct,
                         Sand.Save=sandsavepct, Scambling=scramblingpct, Three.Putts=threeputtpct, Birdie.Bogie=birdiebogieratio,
                         Putts=putts.avg, Score.Avg=score.avg, Top10s=top10s, Firsts=firsts)  %>% 
          filter(Year==as.numeric(input$yearinput00))
  
  }
  
  else
    
    if(input$yearinput00=="All"){
      
      pga1 <- pga %>% select(Name=name, Year=year, Drive.Acc=drivepct, Drive.Avg=driveavg, GIR=girpct, Go.Green=goforgreenpct,
                             Sand.Save=sandsavepct, Scambling=scramblingpct, Three.Putts=threeputtpct, Birdie.Bogie=birdiebogieratio,
                             Putts=putts.avg, Score.Avg=score.avg, Top10s=top10s, Firsts=firsts) 
      
      
          }
  

  
  if(input$radio00==1){

    pgadf00 <- pga1[order(rank(pga1[,NN])),]  %>% filter(row_number()<=input$num00)
    
    formattable(pgadf00, list(
      Score.Avg = color_tile("orange", "white")
    ))
    
}
  
  else
  
  if(input$radio00==2){
    
    pgadf00 <- pga1[order(-rank(pga1[,NN])),]  %>% filter(row_number()<=input$num00)
    
    formattable(pgadf00, list(
      Score.Avg = color_tile("orange", "white")
    ))
    
    }
    
}) ## close formattable output




#### Comparisons Tab: ----

#  panel plot  get data

mydf4 <- reactive({
  
  pga_100 <- pga %>% 
    filter(year==as.numeric(input$yearinput4)) %>% 
    select(5,1,6,10,13,16,19,29,33,36,39,42)%>%
    map_if(is.numeric, function(x) cume_dist(x))
  
  ### Need to Adjust Columns so when lower == better, is other way around. 
  pga_100x <- pga %>% 
    filter(year==as.numeric(input$yearinput4)) %>% 
    select(5,22,26,31) %>%
    map_if(is.numeric, function(x) cume_dist(-x))
  
  pga_100y <- pga_100 %>% left_join(pga_100x)
  
  mynames = input$checkGroup
  
  pga_100y1 <- pga_100y %>% 
    filter(name %in% mynames) %>% 
    gather(key,value,2:ncol(.)) %>%
    mutate(value1 = value - 0.5) 
  
  
#reverse levels of names
  mylevs <- c("score.avg", "par5.birdiepct", "par4.birdiepct", "par3.birdiepct",  
              "birdiepct", "birdiebogieratio",  "putts.avg", "threeputtpct", 
              "sandsavepct", "scramblingpct", "girpct", 
              "goforgreenpct", "driveavg", "drivepct")
 
  
   

  pga_100y1$key <- factor(pga_100y1$key, levels = mylevs)
  
  
# Reorder variables:
  if(input$selectx4!="name"){
  mylevsname <- pga_100y1 %>% filter(key == as.character(input$selectx4)) %>% arrange(desc(value1)) %>% .$name
  pga_100y1$name <- factor(pga_100y1$name, levels = mylevsname)
  }
  else
  if(input$selectx4=="name"){
    mylevsname <- pga_100y1 %>% arrange(name)  %>% .$name %>% as.character %>% unique
    pga_100y1$name <- factor(pga_100y1$name, levels = mylevsname)
    
  }
  
#add group for color:  
  mydf4 <- pga_100y1 %>% mutate(grp = ifelse(value1>=0, 1, 2))
  

})


output$plot4 = renderPlot(height = 800, units="px", {
  
  
  Nplayers <- mydf4() %>% .$name %>% as.character %>% unique %>% length
  if(Nplayers>10) stop ("Error: Please pick a maximum of 10 to compare!")
  if(Nplayers<=5) stop ("Error: Please pick at least 6 to compare!")
  
  ## black.white...
  
  if(input$radio4c==2){
  p4 <- mydf4()  %>% 
    ggplot(., aes(value1,key)) + 
    geom_segment(aes(x=value1, xend=0, y=key, yend=key),color='black', lwd=6) + 
    geom_vline(xintercept=0,lty=1, color='black',  lwd=1) + 
    scale_x_continuous(breaks=c(-.5,0,.5), labels=c("worst", "average", "best")) +
    facet_wrap(~name, ncol=5, nrow=2)  +
    xlab("") + ylab("") +
    theme(
      plot.title = element_text(hjust=0,vjust=1, size=rel(2.3)),
      plot.background  = element_blank(),
      strip.background  = element_blank(),
      panel.background = element_blank(),
      panel.grid.major.x = element_line(color="gray70",linetype = c("28")),
      panel.grid.major.y = element_line(color="gray86"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(size=rel(1.8)),
      text = element_text(color="gray20", size=10),
      axis.text = element_text(size=rel(1.0)),
      axis.text.x = element_text(color="gray20",size=rel(1.6)),
      axis.text.y = element_text(color="gray20", size=rel(1.6)),
      axis.title.x = element_text(size=rel(1.8), vjust=0),
      axis.title.y = element_text(size=rel(1.8), vjust=1),
      axis.ticks.y = element_blank(),
      legend.position = "none",
      panel.margin = unit(2, "lines")
    ) 
  
  if(input$radio4==2){
    print(p4)
  }
  else
  if(input$radio4==1){
  p4a <- p4 + theme(panel.border = element_rect(fill = NA, colour = "black"))
  print(p4a)
  }
  
  }
  
  else
  if(input$radio4c==1){
    
    p4c <- mydf4()  %>% 
      ggplot(., aes(value1,key)) + 
      geom_segment(aes(x=value1, xend=0, y=key, yend=key,color=grp), lwd=6) + 
      geom_vline(xintercept=0,lty=1, color='black',  lwd=1) + 
      scale_x_continuous(breaks=c(-.5,0,.5), labels=c("worst", "average", "best")) +
      facet_wrap(~name, ncol=5)  +
      xlab("") + ylab("") +
      theme(
        plot.title = element_text(hjust=0,vjust=1, size=rel(2.3)),
        plot.background  = element_blank(),
        strip.background  = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color="gray70",linetype = c("28")),
        panel.grid.major.y = element_line(color="gray86"),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size=rel(1.8)),
        text = element_text(color="gray20", size=10),
        axis.text = element_text(size=rel(1.0)),
        axis.text.x = element_text(color="gray20",size=rel(1.6)),
        axis.text.y = element_text(color="gray20", size=rel(1.6)),
        axis.title.x = element_text(size=rel(1.8), vjust=0),
        axis.title.y = element_text(size=rel(1.8), vjust=1),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.margin = unit(2, "lines")
      ) 
    
    if(input$radio4==2){
      print(p4c)
    }
    else
      if(input$radio4==1){
        p4ac <- p4c + theme(panel.border = element_rect(fill = NA, colour = "black"))
        print(p4ac)
      }
      
    
  }
    
  
  
  
})  


####  Tab5: Factor Analysis:

#Do Factor Analysis:

fit5 <- reactive({
  
  tmpx <- pga %>% filter(year==as.numeric(input$yearinput5)) 
  tmp <- cbind(name = tmpx$name, tmpx[colnames(tmpx) %in%  input$checkGroup5])
  

  
fit5<-psych::fa(tmp[-1],nfactors=3, rotate="varimax", scores="regression")

})



## Loadings Output
output$formattable5 <- renderFormattable({
  
  
  fac.load.df <- data.frame(
    Factor1 = unlist(fit5()$loadings)[,1],
    Factor2 = unlist(fit5()$loadings)[,2],
    Factor3 = unlist(fit5()$loadings)[,3]
  )
  
 
   if(input$radio5==2) { fac.load.df <- fac.load.df %>% as.data.frame %>% round(.,2) }
 else
   if(input$radio5==1) { fac.load.df <- apply(fac.load.df, 2, 
                                              function(x) ifelse(x< -as.numeric(input$slider5) | x>as.numeric(input$slider5), x, NA)) %>% 
                           as.data.frame.matrix  %>% round(.,2) }
  

 formattable(fac.load.df, list(
  
  Factor1 = formatter("span", style = x ~ ifelse(x > as.numeric(input$slider5) | x < -as.numeric(input$slider5), style(color = "red", font.weight = "bold"), NA)),
  Factor2 = formatter("span", style = x ~ ifelse(x > as.numeric(input$slider5) | x < -as.numeric(input$slider5), style(color = "red", font.weight = "bold"), NA)),
  Factor3 = formatter("span", style = x ~ ifelse(x > as.numeric(input$slider5) | x < -as.numeric(input$slider5), style(color = "red", font.weight = "bold"), NA))
  
  ))
  
})


## Text for Proportion Variance
output$textp5b <- renderUI({
  
  provar <- colSums(fit5()$loadings*fit5()$loadings)/dim(fit5()$loadings)[1]
  
  str1 <- paste0("Factor1 = ", round(100*provar[[1]],1), "%")
  str2 <- paste0("Factor2 = ", round(100*provar[[2]],1), "%")
  str3 <- paste0("Factor3 = ", round(100*provar[[3]],1), "%")
  
  
  HTML(paste(str1, str2, str3, sep = '<br><br>'))
  
})


# Individual Scores Plot - get data
reactive({
  
  tmpx <- pga %>% filter(year==as.numeric(input$yearinput5)) 
  tmp <- cbind(name = tmpx$name, tmpx[colnames(tmpx) %in%  input$checkGroup5])
  
    fit5<-psych::fa(tmp[-1],nfactors=3, rotate="varimax", scores="regression")
  thurA<-psych::factor.scores(tmp[-1],fit5, method = c("Thurstone"))
  ddf5<-data.frame(x=thurA$scores[,1], y=thurA$scores[,2],z=thurA$scores[,2], name=tmp[,1])

    data_line <- data.frame(
    x_rng = range(ddf5[,1]), 
    y_rng = c(0, 0)
  )
  
  data_line1 <- data.frame(
    y_rng = range(ddf5[,2]), 
    x_rng = c(0, 0)
  )
  
  
  if(input$factors5==3){

  if(input$lines5==2){
    ddf5  %>% 
      ggvis(~x, ~y) %>% 
      layer_points( size = ~z, size.hover := 200,
                    fillOpacity := 0.9, fill.hover := "red",
                    fillOpacity.hover := 0.95, key:= ~name) %>%
      set_options(width = "auto") %>%
      layer_paths(x = ~x_rng, y = ~y_rng, stroke := "blue", strokeWidth := 3, data = data_line) %>%
  layer_paths(x = ~x_rng, y = ~y_rng, stroke := "blue", strokeWidth := 3, data = data_line1) %>%
  add_axis("x", title = 'Factor 1', 
           properties = axis_props(
             title = list(fontSize = 20, dx=-15),
             labels = list(fontSize = 16)) )  %>% 
  add_axis("y", title = 'Factor 2', 
           properties = axis_props(
             title = list(fontSize = 20, dy=-20),
             labels = list(fontSize = 16)) )  %>%
  add_legend("size", title = "Factor 3") %>%
  add_tooltip(function(data) data$name) 
  }

  else
    
if(input$lines5==1){
  ddf5  %>% 
    ggvis(~x, ~y) %>% 
    layer_points( size = ~z, size.hover := 200,
                  fillOpacity := 0.9, fill.hover := "red",
                  fillOpacity.hover := 0.95, key:= ~name) %>%
    set_options(width = "auto") %>%
    add_axis("x", title = 'Factor 1', 
             properties = axis_props(
               title = list(fontSize = 20, dx=-15),
               labels = list(fontSize = 16)) )  %>% 
    add_axis("y", title = 'Factor 2', 
             properties = axis_props(
               title = list(fontSize = 20, dy=-20),
               labels = list(fontSize = 16)) )  %>%
    add_legend("size", title = "Factor 3") %>%
    add_tooltip(function(data) data$name) 
}

  }

else
  
  if(input$factors5==2){
 
    if(input$lines5==2){
      ddf5  %>% 
        ggvis(~x, ~y) %>% 
        layer_points(size.hover := 200,
                      fillOpacity := 0.9, fill.hover := "red",
                      fillOpacity.hover := 0.95, key:= ~name) %>%
        set_options(width = "auto") %>%
        layer_paths(x = ~x_rng, y = ~y_rng, stroke := "blue", strokeWidth := 3, data = data_line) %>%
        layer_paths(x = ~x_rng, y = ~y_rng, stroke := "blue", strokeWidth := 3, data = data_line1) %>%
        add_axis("x", title = 'Factor 1', 
                 properties = axis_props(
                   title = list(fontSize = 20, dx=-15),
                   labels = list(fontSize = 16)) )  %>% 
        add_axis("y", title = 'Factor 2', 
                 properties = axis_props(
                   title = list(fontSize = 20, dy=-20),
                   labels = list(fontSize = 16)) )  %>%
        add_tooltip(function(data) data$name) 
    }
    
    else
      
      if(input$lines5==1){
        ddf5  %>% 
          ggvis(~x, ~y) %>% 
          layer_points(size.hover := 200,
                        fillOpacity := 0.9, fill.hover := "red",
                        fillOpacity.hover := 0.95, key:= ~name) %>%
          set_options(width = "auto") %>%
          add_axis("x", title = 'Factor 1', 
                   properties = axis_props(
                     title = list(fontSize = 20, dx=-15),
                     labels = list(fontSize = 16)) )  %>% 
          add_axis("y", title = 'Factor 2', 
                   properties = axis_props(
                     title = list(fontSize = 20, dy=-20),
                     labels = list(fontSize = 16)) )  %>%
          add_tooltip(function(data) data$name) 
      }
    
  }


})  %>%  bind_shiny("plot5a", "plot5a_ui")


## Data for plotting scores vs score-avg
ddf5i <- reactive({
  tmpx <- pga %>% filter(year==as.numeric(input$yearinput5)) 
  tmp <- cbind(name = tmpx$name, tmpx[colnames(tmpx) %in%  input$checkGroup5])
  fit5<-psych::fa(tmp[-1],nfactors=3, rotate="varimax", scores="regression")
  thurA<-psych::factor.scores(tmp[-1],fit5, method = c("Thurstone"))
  ddf5i<-data.frame(x=thurA$scores[,1], y=thurA$scores[,2],z=thurA$scores[,2], name=tmp[,1], score=tmpx$score.avg)
})

#factor1 vs scoring avg
reactive({
ddf5i()  %>% ggvis(~x, ~score) %>% 
  layer_points(size.hover := 200,
               fillOpacity := 0.9, fill.hover := "red",
               fillOpacity.hover := 0.95, key:= ~name) %>%
  set_options(width = "auto") %>%
  add_axis("x", title = 'Factor 1', ticks=6,
           properties = axis_props(
             title = list(fontSize = 20, dx=-15),
             labels = list(fontSize = 16)) )  %>% 
  add_axis("y", title = 'Scoring Average', 
           properties = axis_props(
             title = list(fontSize = 20, dy=-20),
             labels = list(fontSize = 16)) )  %>%
  add_tooltip(function(data) data$name)  %>%
    layer_model_predictions(model = "lm", stroke:="red", fill.hover := "red", se = TRUE, strokeWidth := 4)

  }) %>%  bind_shiny("plot5x", "plot5x_ui")


#factor2 vs scoring avg
reactive({
  ddf5i()  %>% ggvis(~y, ~score) %>% 
    layer_points(size.hover := 200,
                 fillOpacity := 0.9, fill.hover := "red",
                 fillOpacity.hover := 0.95, key:= ~name) %>%
    set_options(width = "auto") %>%
    add_axis("x", title = 'Factor 2', ticks=6, 
             properties = axis_props(
               title = list(fontSize = 20, dx=-15),
               labels = list(fontSize = 16)) )  %>% 
    add_axis("y", title = 'Scoring Average', 
             properties = axis_props(
               title = list(fontSize = 20, dy=-20),
               labels = list(fontSize = 16)) )  %>%
    add_tooltip(function(data) data$name) %>%
    layer_model_predictions(model = "lm",  stroke:="red", fill.hover := "red", se = TRUE, strokeWidth := 4)
  
  
}) %>%  bind_shiny("plot5y", "plot5y_ui")


#factor3 vs scoring avg
reactive({
  ddf5i()  %>% ggvis(~z, ~score) %>% 
    layer_points(size.hover := 200,
                 fillOpacity := 0.9, fill.hover := "red",
                 fillOpacity.hover := 0.95, key:= ~name) %>%
    set_options(width = "auto") %>%
    add_axis("x", title = 'Factor 3',  ticks=6,
             properties = axis_props(
               title = list(fontSize = 20, dx=-15),
               labels = list(fontSize = 16)) )  %>% 
    add_axis("y", title = 'Scoring Average', 
             properties = axis_props(
               title = list(fontSize = 20, dy=-20),
               labels = list(fontSize = 16)) )  %>%
    add_tooltip(function(data) data$name) %>%
    layer_model_predictions(model = "lm",  stroke:="red", fill.hover := "red", se = TRUE, strokeWidth := 4)
  
  
}) %>%  bind_shiny("plot5z", "plot5z_ui")




####  Close ShinyServer----

})  
 
 

#---- End.
 
 

 
