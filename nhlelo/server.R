library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(XML)
library(PlayerRatings)

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
  
  ## df for abbreviations
  
  ddf <- data.frame(team =
               c("Anaheim Ducks","Arizona Coyotes","Boston Bruins","Buffalo Sabres","Calgary Flames"       
             ,"Carolina Hurricanes"   ,"Chicago Blackhawks"    ,"Colorado Avalanche"    ,"Columbus Blue Jackets" ,"Dallas Stars"         
             ,"Detroit Red Wings"     ,"Edmonton Oilers"       ,"Florida Panthers"      ,"Los Angeles Kings"     ,"Minnesota Wild"       
             ,"Montreal Canadiens"    ,"Nashville Predators"   ,"New Jersey Devils"     ,"New York Islanders"    ,"New York Rangers"     
             ,"Ottawa Senators"       ,"Philadelphia Flyers"   ,"Pittsburgh Penguins"   ,"San Jose Sharks"       ,"St. Louis Blues"      
             ,"Tampa Bay Lightning"   ,"Toronto Maple Leafs"   ,"Vancouver Canucks"     ,"Washington Capitals"   ,"Winnipeg Jets"),
             
             abbrv = c("ANA", "ARZ", "BOS", "BUF", "CGY", "CAR", "CHI", "COL", "CBJ", "DAL", "DET", "EDM", "FLA", 
                       "LAK", "MIN", "MON", "NAS", "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", 
                       "TOR", "VAN", "WAS", "WIN")
  )
  
  
  
  #### Get Data

  # On launch, get 2014/15 data
  doc <- readHTMLTable("http://www.hockey-reference.com/leagues/NHL_2015_games.html")
  x <- doc[[1]]
  colnames(x)<-c("date", "visitor", "vgoal" ,"home", "hgoal", "soot", "notes")
  
  x$hgoal <- as.numeric(as.character(x$hgoal))
  x$vgoal <- as.numeric(as.character(x$vgoal))
  
  
  x2014a <- x %>%
    filter(complete.cases(.)==T) %>%
    mutate(soot = gsub("^$|^ $", NA, .$soot)) %>%
    mutate(hpoints = ifelse(is.na(soot)==T, ifelse(hgoal>vgoal, 2, 0), ifelse(soot=="OT" | soot=="SO", ifelse(hgoal>vgoal, 2, 1)))) %>%
    mutate(vpoints = ifelse(is.na(soot)==T, ifelse(vgoal>hgoal, 2, 0), ifelse(soot=="OT" | soot=="SO", ifelse(vgoal>hgoal, 2, 1)))) %>%
    select(date, team=home, opp=visitor, pts=hpoints) %>%
    mutate(score = ifelse(pts==2, 1, 0)) %>%
    arrange(date) %>%
    mutate(event = dense_rank(date)) 
  
  x2014 <- x2014a %>%
    select(event, team, opp, score) %>%
    ungroup()
  
  x2014$team <- as.character(x2014$team)
  x2014$opp <- as.character(x2014$opp)
  
  
  
###  Reactively do ELO ratings.

mydata <- reactive({

  N <- max(x2014[,1])
  
  
  
  obj <- elo(x2014, history=T, kfac=as.numeric(input$slider1), gamma=rep(as.numeric(input$slider2), nrow(x2014)))

  xx<-as.data.frame(obj$history)
  
  mydata <- xx[,1:N] %>% 
    mutate(team = rownames(.)) %>%
    gather(period, value, 1:N) %>%
    mutate(period = gsub("[.][^.]*", "", .$period))
  
  
  
})
  

  
#### Outputs  
  

  output$html1 <- renderUI({

    mydata1 <- as.data.frame(mydata())
    
    mydata1$period <- as.numeric(as.character(mydata1$period))
    mydata1$value <- as.numeric(as.character(mydata1$value))
    
    teams <- mydata1 %>% filter(period == max(period)) %>% arrange(desc(value)) %>% .$team
    ratings <- mydata1 %>% filter(period == max(period)) %>% mutate(ratings = round(value, 0)) %>% arrange(desc(value)) %>% .$ratings
    
    teams1 <- as.character( ddf$abbrv[match(teams, ddf$team)] )
    
    
    
    
        HTML(
    paste0("1 = ", teams1[[1]], " - ", ratings[1], '<br/>', "2 = ", teams1[[2]], " - ", ratings[2], '<br/>',
           "3 = ", teams1[[3]], " - ", ratings[3], '<br/>', "4 = ", teams1[[4]], " - ", ratings[4], '<br/>',
           "5 = ", teams1[[5]], " - ", ratings[5], '<br/>', "6 = ", teams1[[6]], " - ", ratings[6], '<br/>',
           "7 = ", teams1[[7]], " - ", ratings[7], '<br/>', "8 = ", teams1[[8]], " - ", ratings[8], '<br/>',
           "9 = ", teams1[[9]], " - ", ratings[9], '<br/>', "10 = ", teams1[[10]], " - ", ratings[10], '<br/>',
           "11 = ", teams1[[11]], " - ", ratings[11], '<br/>', "12 = ", teams1[[12]], " - ", ratings[12], '<br/>',
           "13 = ", teams1[[13]], " - ", ratings[13], '<br/>', "14 = ", teams1[[14]], " - ", ratings[14], '<br/>',           
           "15 = ", teams1[[15]], " - ", ratings[15], '<br/>', "16 = ", teams1[[16]], " - ", ratings[16], '<br/>',
           "17 = ", teams1[[17]], " - ", ratings[17], '<br/>', "18 = ", teams1[[18]], " - ", ratings[18], '<br/>',
           "19 = ", teams1[[19]], " - ", ratings[19], '<br/>', "20 = ", teams1[[20]], " - ", ratings[20], '<br/>',
           "21 = ", teams1[[21]], " - ", ratings[21], '<br/>', "22 = ", teams1[[22]], " - ", ratings[22], '<br/>',
           "23 = ", teams1[[23]], " - ", ratings[23], '<br/>', "24 = ", teams1[[24]], " - ", ratings[24], '<br/>',
           "25 = ", teams1[[25]], " - ", ratings[25], '<br/>', "26 = ", teams1[[26]], " - ", ratings[26], '<br/>',
           "27 = ", teams1[[27]], " - ", ratings[27], '<br/>', "28 = ", teams1[[28]], " - ", ratings[28], '<br/>',
           "29 = ", teams1[[29]], " - ", ratings[29], '<br/>', "30 = ", teams1[[30]], " - ", ratings[30], '<br/>',           
           '<br/>'
             )
  )
      
  })
    

  output$plot1 <- renderPlot({ 
    
    
    mydata1 <- as.data.frame(mydata())
    
    mydata1$period <- as.numeric(as.character(mydata1$period))
    mydata1$value <- as.numeric(as.character(mydata1$value))
    
    
    mylevels <- mydata1 %>% filter(period == max(period)) %>% arrange(desc(value)) %>% .$team
    
    mydata1$team <- factor(mydata1$team, levels = mylevels)
    
    mycolors <- colorRampPalette(c(input$boxa, input$boxb, input$boxc))(length(mylevels))
    
    mydatateam <- mydata1 %>% filter(team == input$teams)
    
    mydates <- unique(x2014a %>% select(date, period=event))
    mydates$date <- as.Date(mydates$date, format="%Y-%m-%d")
    mydates1 <- mydates %>% filter(period==1 | period==50 | period==100 | period==150)
    
    ggplot(mydata1, aes(period, value)) + 
      geom_line(aes(color=team, group=team)) +
      geom_line(data=mydatateam, aes(period, value, color=team, group=team), lwd=1.5) +
      scale_x_discrete(breaks = c(1,50,100,150), labels=c(mydates1$date[1], mydates1$date[2], mydates1$date[3], mydates1$date[4])) +
      mytheme +
      scale_color_manual(values = mycolors) +
      xlab("Date") +
      ylab("ELO Rating")
    
    
    })
  
  



  
}
)