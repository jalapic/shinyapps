
shinyServer(function(input, output) {
    

  ### on launch, make this function
  getdatafun1 <- function(df, teamname){
    
    dfhome <- df %>% mutate(GF=as.numeric(as.character(hgoal)),
                            GA=as.numeric(as.character(vgoal)),
                            GD = GF-GA,
                            result=ifelse(GD>0, "W", ifelse(GD<0, "L", "D")),
                            venue="home") %>%
      select(date, team=home, opponent=visitor, GF,GA,GD,result,venue)
    
    dfaway <- df %>% mutate(GF=as.numeric(as.character(vgoal)),
                            GA=as.numeric(as.character(hgoal)),
                            GD = GF-GA,
                            result=ifelse(GD>0, "W", ifelse(GD<0, "L", "D")),
                            venue="away") %>%
      
      select(date, team=visitor, opponent=home, GF,GA,GD,result,venue)
    
    dfboth<-rbind(dfhome,dfaway)
    
    dfboth <- dfboth %>% 
      filter(team==teamname) %>%
      mutate(Gameno = dense_rank(date)) %>% 
      arrange(Gameno) %>% 
      mutate(Pts = ifelse(result=="W", 3, ifelse(result=="D", 1, 0))) %>% 
      mutate(Cumpts = cumsum(Pts),
             CumGF = cumsum(GF),
             CumGA = cumsum(GA),
             CumGD = cumsum(GD)) %>%
      select(Gameno, Cumpts, CumGF, CumGA, CumGD)
    return(dfboth)
  }
  
  
  # On launch, get 2014/15 data
  
  doc1 <- readHTMLTable("http://www.espn.co.uk/football/sport/match/index.html?event=3;type=results")

  #Because espn changes it's mind often about how many columns to use in its tables
  if (ncol(doc1[[2]]) <= 8) { temp1 <- doc1[[2]][c(1,3:5)] }
  else { temp1 <- doc1[[2]][c(1,4:6)] }


 colnames(temp1)<-c("date", "home", "FT", "visitor")
  
  tempx1 <- NULL
  tempx1 <- ifelse(is.na(temp1$home)==T, as.character(temp1$date), NA)  #as.character() because 'date' is a factor
  tempx1 <- zoo::na.locf(tempx1)
  temp1$date <- paste(temp1$date, tempx1, sep=" ")
  temp1 <- temp1[complete.cases(temp1),]
  
  ### this hack is being used because it looks like shinyapps.io has issues with Month Names
  #  temp1$date <- as.Date(temp1$date, format="%H:%M %A, %d %B %Y")   #this line works locally.
  
  #this is painful to type
  temp1$date <- sub(".*, ", "", temp1$date)
  temp1$date <-gsub("January", "1", temp1$date)
  temp1$date <-gsub("February", "2", temp1$date)
  temp1$date <-gsub("March", "3", temp1$date)
  temp1$date <-gsub("April", "4", temp1$date)
  temp1$date <-gsub("May", "5", temp1$date)
  temp1$date <-gsub("June", "6", temp1$date)
  temp1$date <-gsub("July", "7", temp1$date)
  temp1$date <-gsub("August", "8", temp1$date)
  temp1$date <-gsub("September", "9", temp1$date)
  temp1$date <-gsub("October", "10", temp1$date)
  temp1$date <-gsub("November", "11", temp1$date)
  temp1$date <-gsub("December", "12", temp1$date)
  
  temp1$date <- as.Date(temp1$date, format="%d %m %Y")   #this line works locally.
  
  
  library(tidyr)
  temp1 <- temp1 %>% separate(FT, c("hgoal", "vgoal"), sep="-", remove=F)
  
  
  #### Reactive for old data
  
  myolddata <- reactive({
    
    minyear <-  input$slider[1]
    maxyear <-  input$slider[2]
    
    df1 <- engsoccerdata2 %>% filter(Season >= minyear  & Season <= maxyear)
    df1$date <- as.Date(df1$Date, format="%Y-%m-%d")
    df1 <- df1 %>% filter(home==input$select | visitor==input$select)  %>% filter(tier==1)
    myseasons1 <- split(df1,df1$Season)
    x1 <- lapply(myseasons1, getdatafun1, teamname=input$select)
    alldata1 <- do.call("rbind", x1)
    alldata1$id <- rep(names(x1), sapply(x1, nrow))
    return(alldata1)
  })
  
  
  #### Reactive for 2015 data
  
  mydata <- reactive({
    
    
    temp2014a <- getdatafun1(temp1, input$select) %>% mutate(id=2014)
    
    # combine data
    alldata1x <- as.data.frame(myolddata())
    mydf1<-rbind(alldata1x,temp2014a)
    
    tmp1 <- data.frame(Gameno=0,Cumpts=0, CumGF=0, CumGA=0, CumGD=0, id=unique(mydf1$id))
    mydf1<-rbind(mydf1,tmp1)
    
    mydf1$grp <- ifelse(mydf1$id ==input$text1, 1, ifelse(mydf1$id == input$text2, 2, 0))
    
    return(mydf1)
    
  })
  
  ### bright color line
  mydataA <- reactive({
    mu2014 <- mydata() %>% filter(id==input$text2)
  })
  
  ### dark colored line
  mydataB <- reactive({
    mu2013 <- mydata() %>% filter(id==input$text1)
  })
  
  
  ## colors schemes
  mycolors <- reactive({
    
    if(input$radio==1) {mycolors <- c("thistle1", "red4", "red")}
    else
      if(input$radio==2) {mycolors <- c("lightcyan2", "mediumblue", "dodgerblue")}
    else
      if(input$radio==3) {mycolors <- c("gray80", "black", "gray35")}
    
  })
  
  
  
  
  output$plot1 <- renderPlot({
    
    myd <- as.data.frame(mydata())
    mydA <- as.data.frame(mydataA())
    mydB <- as.data.frame(mydataB())
    mycols <- as.vector(mycolors())
    
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
    
    if(input$select1=="Cumpts"){
      ggplot(myd, aes(Gameno, Cumpts, group=id, color=grp)) +
        geom_line(aes(group=id, color=factor(grp))) +
        geom_line(data=mydA, aes(Gameno, Cumpts, group=id, color=factor(grp)), lwd=1.1) +  
        geom_line(data=mydB, aes(Gameno, Cumpts, group=id, color=factor(grp)), lwd=1.1) +
        xlab("Game number") + 
        ylab("Cumulative points") +
        scale_color_manual(values=mycols) +
        mytheme
    }  
    
    else
      
      if(input$select1=="CumGF"){
        ggplot(myd, aes(Gameno, CumGF, group=id, color=grp)) +
          geom_line(aes(group=id, color=factor(grp))) +
          geom_line(data=mydA, aes(Gameno, CumGF, group=id, color=factor(grp)), lwd=1.1) +  
          geom_line(data=mydB, aes(Gameno, CumGF, group=id, color=factor(grp)), lwd=1.1) +
          xlab("Game number") + 
          ylab("Cumulative goals scored") +
          scale_color_manual(values=mycols) +
          mytheme
      }  
    
    else
      
      if(input$select1=="CumGA"){
        ggplot(myd, aes(Gameno, CumGA, group=id, color=grp)) +
          geom_line(aes(group=id, color=factor(grp))) +
          geom_line(data=mydA, aes(Gameno, CumGA, group=id, color=factor(grp)), lwd=1.1) +  
          geom_line(data=mydB, aes(Gameno, CumGA, group=id, color=factor(grp)), lwd=1.1) +
          xlab("Game number") + 
          ylab("Cumulative goals against") +
          scale_color_manual(values=mycols) +
          mytheme
      }  
    
    else
      
      if(input$select1=="CumGD"){
        ggplot(myd, aes(Gameno, CumGD, group=id, color=grp)) +
          geom_line(aes(group=id, color=factor(grp))) +
          geom_line(data=mydA, aes(Gameno, CumGD, group=id, color=factor(grp)), lwd=1.1) +  
          geom_line(data=mydB, aes(Gameno, CumGD, group=id, color=factor(grp)), lwd=1.1) +
          xlab("Game number") + 
          ylab("Cumulative goal difference") +
          scale_color_manual(values=mycols) +
          mytheme
      }  
    
  })
}
)