library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(engsoccerdata)
library(tidyr)

#### A function for generating the data for XY plots

draw.data <- function(type){

  if(1 %in% type & !(2 %in% type) & !(3 %in% type) & !(4 %in% type)){
      df1 <- engsoccerdata2 %>% filter(tier==1)}

  if(2 %in% type & !(1 %in% type) & !(3 %in% type) & !(4 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==2)}

  if(3 %in% type & !(2 %in% type) & !(1 %in% type) & !(4 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==3)}

  if(4 %in% type & !(2 %in% type) & !(3 %in% type) & !(1 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==4)}

  if(1 %in% type & 2 %in% type & !(3 %in% type) & !(4 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==1 | tier==2)}

  if(1 %in% type & 3 %in% type & !(2 %in% type) & !(4 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==1 | tier==3)}

  if(1 %in% type & 4 %in% type & !(2 %in% type) & !(3 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==1 | tier==4)}

  if(2 %in% type & 3 %in% type & !(1 %in% type) & !(4 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==2 | tier==3)}

  if(2 %in% type & 4 %in% type & !(3 %in% type) & !(1 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==2 | tier==4)}

  if(3 %in% type & 4 %in% type & !(1 %in% type) & !(2 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==3 | tier==4)}

  if(1 %in% type & 2 %in% type & 3 %in% type & !(4 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==1 | tier==2 | tier==3)}

  if(1 %in% type & 2 %in% type & 4 %in% type & !(3 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==1 | tier==2 | tier==4)}

  if(1 %in% type & 3 %in% type & 4 %in% type & !(2 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==1 | tier==3 | tier==4)}

  if(2 %in% type & 3 %in% type & 4 %in% type & !(1 %in% type)){
    df1 <- engsoccerdata2 %>% filter(tier==4 | tier==2 | tier==3)}

  if(2 %in% type & 3 %in% type & 4 %in% type & 1 %in% type){
    df1 <- engsoccerdata2}


  return(df1)
}



#### Function for drawing Gantt plots

plotGantt1 <- function(data, res.col='resources',
                       start.col='start',
                       end.col='end',
                       res.colors=rainbow(15),
                       plottitle="team",
                       minval=1888,
                       maxval=2014)
{
  #slightly enlarge Y axis margin to make space for labels
  op <- par('mar')
  par(mar = op + c(0,1.2,0,0))

  #minval <- min(data[,start.col],na.rm=T)
  #maxval <- max(data[,end.col],na.rm=T)

  res.colors <- rev(res.colors)
  resources <- sort(unique(data[,res.col]),decreasing=T)


  plot(c(minval,maxval),
       c(0.5,length(resources)+0.5),
       type='n', xlab=NA,ylab=NA,yaxt='n', main = plottitle, cex.main=1.5)
  axis(side=2,at=1:length(resources),labels=resources,las=1)
  for(i in 1:length(resources))
  {
    yTop <- i+0.5
    yBottom <- i-0.5
    subset <- data[data[,res.col] == resources[i],]
    for(r in 1:nrow(subset))
    {
      color <- res.colors[((i-1)%%length(res.colors))+1]
      start <- subset[r,start.col]
      end <- subset[r,end.col]
      rect(start,yBottom,end,yTop,col=color)
    }
  }

  par(mar=op) # reset the plotting margins
  invisible()
}




################## ShinyServer begins here ###########################





shinyServer(function(input, output) {



#### XY scatterplot tabpanel

  df1 <- reactive({


    minyear <- input$year[1]
    maxyear <- input$year[2]




    if (input$Outcomey == "totgoal" & input$Outcomex == "hgoal"){

    draw.data(input$type) %>%
      filter(Season >= minyear) %>%
      filter(Season <= maxyear) %>%
      group_by(Season, tier) %>%
      summarise(meanOutcomey=mean(totgoal),meanOutcomex=mean(hgoal)
                )

    }

    else

      if (input$Outcomey == "totgoal" & input$Outcomex == "Year"){

        df1 <- draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(totgoal)
          )
        z.s <- df1$Season
        df1 <- data.frame(df1,meanOutcomex=z.s)
      }

    else



      if (input$Outcomey == "totgoal" & input$Outcomex == "totgoal"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(totgoal),meanOutcomex=mean(totgoal)
          )

      }

    else

    if (input$Outcomey == "totgoal" & input$Outcomex == "vgoal"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(totgoal),meanOutcomex=mean(vgoal)
          )

      }

    else

    if (input$Outcomey == "totgoal" & input$Outcomex == "goaldif"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(totgoal),meanOutcomex=mean(goaldif)
          )

      }

    else

    if (input$Outcomey == "totgoal" & input$Outcomex == "hwinpct"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(totgoal),totalHW=sum(result=="H"), totalG=length(result), meanOutcomex=totalHW/totalG
          )

      }

    else

      if (input$Outcomey == "totgoal" & input$Outcomex == "vwinpct"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(totgoal),totalHW=sum(result=="A"), totalG=length(result), meanOutcomex=totalHW/totalG
          )

      }

    else

      if (input$Outcomey == "totgoal" & input$Outcomex == "drawpct"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(totgoal),totalHW=sum(result=="D"), totalG=length(result), meanOutcomex=totalHW/totalG
          )

      }

    else


    # take2

      if (input$Outcomey == "hgoal" & input$Outcomex == "Year"){

        df1 <- draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(hgoal)
          )
        z.s <- df1$Season
        df1 <- data.frame(df1,meanOutcomex=z.s)
      }

    else



      if (input$Outcomey == "hgoal" & input$Outcomex == "totgoal"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(hgoal),meanOutcomex=mean(totgoal)
          )

      }

    else

      if (input$Outcomey == "hgoal" & input$Outcomex == "vgoal"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(hgoal),meanOutcomex=mean(vgoal)
          )

      }

    else

      if (input$Outcomey == "hgoal" & input$Outcomex == "goaldif"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(hgoal),meanOutcomex=mean(goaldif)
          )

      }

    else

      if (input$Outcomey == "hgoal" & input$Outcomex == "hwinpct"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(hgoal),totalHW=sum(result=="H"), totalG=length(result), meanOutcomex=totalHW/totalG
          )

      }

    else

      if (input$Outcomey == "hgoal" & input$Outcomex == "vwinpct"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(hgoal),totalHW=sum(result=="A"), totalG=length(result), meanOutcomex=totalHW/totalG
          )

      }

    else

      if (input$Outcomey == "hgoal" & input$Outcomex == "drawpct"){

        draw.data(input$type) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          group_by(Season, tier) %>%
          summarise(meanOutcomey=mean(hgoal),totalHW=sum(result=="D"), totalG=length(result), meanOutcomex=totalHW/totalG
          )

      }

    else

    if (input$Outcomey == "hgoal" & input$Outcomex == "hgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(hgoal),meanOutcomex=mean(hgoal)
        )

    }

  else

    # take 3
    if (input$Outcomey == "vgoal" & input$Outcomex == "Year"){

      df1 <- draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(vgoal)
        )
      z.s <- df1$Season
      df1 <- data.frame(df1,meanOutcomex=z.s)
    }

  else



    if (input$Outcomey == "vgoal" & input$Outcomex == "totgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(vgoal),meanOutcomex=mean(totgoal)
        )

    }

  else

    if (input$Outcomey == "vgoal" & input$Outcomex == "vgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(vgoal),meanOutcomex=mean(vgoal)
        )

    }

  else

    if (input$Outcomey == "vgoal" & input$Outcomex == "goaldif"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(vgoal),meanOutcomex=mean(goaldif)
        )

    }

  else

    if (input$Outcomey == "vgoal" & input$Outcomex == "hwinpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(vgoal),totalHW=sum(result=="H"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "vgoal" & input$Outcomex == "vwinpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(vgoal),totalHW=sum(result=="A"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "vgoal" & input$Outcomex == "drawpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(vgoal),totalHW=sum(result=="D"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "vgoal" & input$Outcomex == "hgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(vgoal),meanOutcomex=mean(hgoal)
        )

    }

  else






    # take 4


    if (input$Outcomey == "goaldif" & input$Outcomex == "Year"){

      df1 <- draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(goaldif)
        )
      z.s <- df1$Season
      df1 <- data.frame(df1,meanOutcomex=z.s)
    }

  else



    if (input$Outcomey == "goaldif" & input$Outcomex == "totgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(goaldif),meanOutcomex=mean(totgoal)
        )

    }

  else

    if (input$Outcomey == "goaldif" & input$Outcomex == "vgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(goaldif),meanOutcomex=mean(vgoal)
        )

    }

  else

    if (input$Outcomey == "goaldif" & input$Outcomex == "goaldif"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(goaldif),meanOutcomex=mean(goaldif)
        )

    }

  else

    if (input$Outcomey == "goaldif" & input$Outcomex == "hwinpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(goaldif),totalHW=sum(result=="H"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "goaldif" & input$Outcomex == "vwinpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(goaldif),totalHW=sum(result=="A"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "goaldif" & input$Outcomex == "drawpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(goaldif),totalHW=sum(result=="D"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "goaldif" & input$Outcomex == "hgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(meanOutcomey=mean(goaldif),meanOutcomex=mean(hgoal)
        )

    }

  else




    # take5

    if (input$Outcomey == "hwinpct" & input$Outcomex == "Year"){

      df1 <- draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="H"), totalG=length(result), meanOutcomey=totalHW/totalG)

      z.s <- df1$Season
      df1 <- data.frame(df1,meanOutcomex=z.s)
    }

  else



    if (input$Outcomey == "hwinpct" & input$Outcomex == "totgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="H"), totalG=length(result), meanOutcomey=totalHW/totalG,
        meanOutcomex=mean(totgoal)
        )

    }

  else

    if (input$Outcomey == "hwinpct" & input$Outcomex == "vgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="H"), totalG=length(result), meanOutcomey=totalHW/totalG,
        meanOutcomex=mean(vgoal)
        )

    }

  else

    if (input$Outcomey == "hwinpct" & input$Outcomex == "goaldif"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="H"), totalG=length(result), meanOutcomey=totalHW/totalG,
        meanOutcomex=mean(goaldif)
        )

    }

  else

    if (input$Outcomey == "hwinpct" & input$Outcomex == "hwinpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="H"), totalG=length(result), meanOutcomey=totalHW/totalG,
        totalHW=sum(result=="H"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "hwinpct" & input$Outcomex == "vwinpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="H"), totalG=length(result), meanOutcomey=totalHW/totalG,
        totalHW=sum(result=="A"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "hwinpct" & input$Outcomex == "drawpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="H"), totalG=length(result), meanOutcomey=totalHW/totalG,
        totalHW=sum(result=="D"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "hwinpct" & input$Outcomex == "hgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="H"), totalG=length(result), meanOutcomey=totalHW/totalG,
        meanOutcomex=mean(hgoal)
        )

    }

  else




    # take6

    if (input$Outcomey == "vwinpct" & input$Outcomex == "Year"){

      df1 <- draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="A"), totalG=length(result), meanOutcomey=totalHW/totalG)

      z.s <- df1$Season
      df1 <- data.frame(df1,meanOutcomex=z.s)
    }

  else



    if (input$Outcomey == "vwinpct" & input$Outcomex == "totgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="A"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  meanOutcomex=mean(totgoal)
        )

    }

  else

    if (input$Outcomey == "vwinpct" & input$Outcomex == "vgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="A"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  meanOutcomex=mean(vgoal)
        )

    }

  else

    if (input$Outcomey == "vwinpct" & input$Outcomex == "goaldif"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="A"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  meanOutcomex=mean(goaldif)
        )

    }

  else

    if (input$Outcomey == "vwinpct" & input$Outcomex == "hwinpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="A"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  totalHW=sum(result=="H"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "vwinpct" & input$Outcomex == "vwinpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="A"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  totalHW=sum(result=="A"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "vwinpct" & input$Outcomex == "drawpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="A"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  totalHW=sum(result=="D"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "vwinpct" & input$Outcomex == "hgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="A"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  meanOutcomex=mean(hgoal)
        )

    }

  else


    # take7

    if (input$Outcomey == "drawpct" & input$Outcomex == "Year"){

      df1 <- draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="D"), totalG=length(result), meanOutcomey=totalHW/totalG)

      z.s <- df1$Season
      df1 <- data.frame(df1,meanOutcomex=z.s)
    }

  else



    if (input$Outcomey == "drawpct" & input$Outcomex == "totgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="D"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  meanOutcomex=mean(totgoal)
        )

    }

  else

    if (input$Outcomey == "drawpct" & input$Outcomex == "vgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="D"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  meanOutcomex=mean(vgoal)
        )

    }

  else

    if (input$Outcomey == "drawpct" & input$Outcomex == "goaldif"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="D"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  meanOutcomex=mean(goaldif)
        )

    }

  else

    if (input$Outcomey == "drawpct" & input$Outcomex == "hwinpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="D"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  totalHW=sum(result=="H"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "drawpct" & input$Outcomex == "vwinpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="D"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  totalHW=sum(result=="A"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "drawpct" & input$Outcomex == "drawpct"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="D"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  totalHW=sum(result=="D"), totalG=length(result), meanOutcomex=totalHW/totalG
        )

    }

  else

    if (input$Outcomey == "drawpct" & input$Outcomex == "hgoal"){

      draw.data(input$type) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        group_by(Season, tier) %>%
        summarise(totalHW=sum(result=="D"), totalG=length(result), meanOutcomey=totalHW/totalG,
                  meanOutcomex=mean(hgoal)
        )

    }


  })


















###### Gantt plot tabpanel


df11 <- reactive({

  minyear1 <- input$yeargantt[1]
  maxyear1 <- input$yeargantt[2]

  x1 <- engsoccerdata2 %>%
    filter(Season >= minyear1) %>%
    filter(Season <= maxyear1) %>%
    filter(home==input$team1) %>%
    group_by(Season, tier) %>%
    tally %>%
    mutate(Season1=Season+1) %>%
    select(Season,Season1,tier)

  xx1 <- rbind(x1,data.frame(Season=NA,Season1=NA,tier=1:4))
  xx1 <- data.frame(xx1)

})


df22 <- reactive({

  minyear1 <- input$yeargantt[1]
  maxyear1 <- input$yeargantt[2]

  x2 <- engsoccerdata2 %>%
    filter(Season >= minyear1) %>%
    filter(Season <= maxyear1) %>%
    filter(home==input$team2) %>%
    group_by(Season, tier) %>%
    tally %>%
    mutate(Season1=Season+1) %>%
    select(Season,Season1,tier)

  xx2 <- rbind(x2,data.frame(Season=NA,Season1=NA,tier=1:4))
  xx2 <- data.frame(xx2)

})

df33 <- reactive({

  minyear1 <- input$yeargantt[1]
  maxyear1 <- input$yeargantt[2]

  x3 <- engsoccerdata2 %>%
    filter(Season >= minyear1) %>%
    filter(Season <= maxyear1) %>%
    filter(home==input$team3) %>%
    group_by(Season, tier) %>%
    tally %>%
    mutate(Season1=Season+1) %>%
    select(Season,Season1,tier)

  xx3 <- rbind(x3,data.frame(Season=NA,Season1=NA,tier=1:4))
  xx3 <- data.frame(xx3)

})

df44 <- reactive({

  minyear1 <- input$yeargantt[1]
  maxyear1 <- input$yeargantt[2]

  x4 <- engsoccerdata2 %>%
    filter(Season >= minyear1) %>%
    filter(Season <= maxyear1) %>%
    filter(home==input$team4) %>%
    group_by(Season, tier) %>%
    tally %>%
    mutate(Season1=Season+1) %>%
    select(Season,Season1,tier)

  xx4 <- rbind(x4,data.frame(Season=NA,Season1=NA,tier=1:4))
  xx4 <- data.frame(xx4)

})


# Generate the color palette


mycolors <- reactive({

  if (input$colortype=="redwhite") {
    mycolors <- c('red','orange','yellow','gray99')
  }

  else

    if (input$colortype=="bluewhite") {
      mycolors <- c('blue4','dodgerblue','aquamarine','aliceblue')
    }

  else

    if (input$colortype=="greens") {
      mycolors <- c('darkgreen','darkolivegreen3','darkseagreen3','darkseagreen1')
    }

  else

    if (input$colortype=="rainbow") {
      mycolors <- rainbow(15)
    }

})



######  Stats Tables Reactives...


# A function for generating the data.

# matchid is added to be able to remove duplicate rows when looking at "All teams" vs "All teams" condition

draw.data1 <- function(typetier){

  if(typetier=="all.tiers" & input$checkbox==F){
    dff <- engsoccerdata2 %>% mutate(matchid=row_number(Season))
  }
  if(typetier=="tier1"  & input$checkbox==F){
    dff <- engsoccerdata2 %>% mutate(matchid=row_number(Season)) %>% filter(tier==1)
  }
  if(typetier=="tier2"  & input$checkbox==F){
    dff <- engsoccerdata2 %>% mutate(matchid=row_number(Season)) %>% filter(tier==2)
  }
  if(typetier=="tier3"  & input$checkbox==F){
    dff <- engsoccerdata2 %>% mutate(matchid=row_number(Season)) %>% filter(tier==3)
  }
  if(typetier=="tier4"  & input$checkbox==F){
    dff <- engsoccerdata2 %>% mutate(matchid=row_number(Season)) %>% filter(tier==4)
  }
  if(typetier=="all.tiers" & input$checkbox==T){
    dff <- engsoccerdata2 %>% filter(Season!=1939) %>% mutate(matchid=row_number(Season))
  }
  if(typetier=="tier1"  & input$checkbox==T){
    dff <- engsoccerdata2 %>% filter(Season!=1939)  %>% mutate(matchid=row_number(Season)) %>% filter(tier==1)
  }
  if(typetier=="tier2"  & input$checkbox==T){
    dff <- engsoccerdata2 %>% filter(Season!=1939)  %>% mutate(matchid=row_number(Season)) %>% filter(tier==2)
  }
  if(typetier=="tier3"  & input$checkbox==T){
    dff <- engsoccerdata2 %>% filter(Season!=1939)  %>% mutate(matchid=row_number(Season)) %>% filter(tier==3)
  }
  if(typetier=="tier4"  & input$checkbox==T){
    dff <- engsoccerdata2 %>% filter(Season!=1939)  %>% mutate(matchid=row_number(Season)) %>% filter(tier==4)
  }

  return(dff)
}






#### Added series of awkward if-loops to be able to incorporate "All teams" option


df1tables <- reactive({



  if (input$team1tables!="All teams" & input$team2tables!="All teams"){


    minyear <- input$yeartables[1]
    maxyear <- input$yeartables[2]

    team1 <- input$team1tables
    team2 <- input$team2tables





    if(input$type1=="home"){

      df1tables <- draw.data1(input$typetier) %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        filter (home==team1 & visitor==team2)  %>%
        mutate (venue="home") %>%
        select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                goalsagainst=vgoal, tier, diff=goaldif, result, venue)

    }
    else

      if(input$type1=="away"){

        df1tables <- draw.data1(input$typetier) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          filter (home==team2 & visitor==team1) %>%
          mutate (diff = -goaldif, venue="away") %>%
          select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                  goalsagainst=hgoal, tier, diff, result, venue)

      }
    else

      if(input$type1=="both") {

        dfh <- draw.data1(input$typetier) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          filter (home==team1 & visitor==team2) %>%
          mutate (venue="home") %>%
          select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                  goalsagainst=vgoal, tier, diff=goaldif, result, venue)

        dfv <- draw.data1(input$typetier) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          filter (home==team2 & visitor==team1) %>%
          mutate (diff = -goaldif, venue="away") %>%
          select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                  goalsagainst=hgoal, tier, diff, result, venue)


        df1tables <- rbind(dfh,dfv)

      }
  }

  else

    if (input$team1tables=="All teams" & input$team2tables!="All teams"){


      minyear <- input$yeartables[1]
      maxyear <- input$yeartables[2]

      team2 <- input$team2tables





      if(input$type1=="home"){

        df1tables <- draw.data1(input$typetier) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          filter (visitor==team2)  %>%
          mutate (venue="home") %>%
          select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                  goalsagainst=vgoal, tier, diff=goaldif, result, venue)

      }
      else

        if(input$type1=="away"){

          df1tables <- draw.data1(input$typetier) %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            filter (home==team2) %>%
            mutate (diff = -goaldif, venue="away") %>%
            select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                    goalsagainst=hgoal, tier, diff, result, venue)

        }
      else

        if(input$type1=="both") {

          dfh <- draw.data1(input$typetier) %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            filter (visitor==team2) %>%
            mutate (venue="home") %>%
            select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                    goalsagainst=vgoal, tier, diff=goaldif, result, venue)

          dfv <- draw.data1(input$typetier) %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            filter (home==team2) %>%
            mutate (diff = -goaldif, venue="away") %>%
            select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                    goalsagainst=hgoal, tier, diff, result, venue)


          df1tables <- rbind(dfh,dfv)

        }
    }

  else

    if (input$team1tables!="All teams" & input$team2tables=="All teams"){


      minyear <- input$yeartables[1]
      maxyear <- input$yeartables[2]

      team1 <- input$team1tables





      if(input$type1=="home"){

        df1tables <- draw.data1(input$typetier) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          filter (home==team1)  %>%
          mutate (venue="home") %>%
          select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                  goalsagainst=vgoal, tier, diff=goaldif, result, venue)

      }
      else

        if(input$type1=="away"){

          df1tables <- draw.data1(input$typetier) %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            filter (visitor==team1) %>%
            mutate (diff = -goaldif, venue="away") %>%
            select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                    goalsagainst=hgoal, tier, diff, result, venue)

        }
      else

        if(input$type1=="both") {

          dfh <- draw.data1(input$typetier) %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            filter (home==team1) %>%
            mutate (venue="home") %>%
            select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                    goalsagainst=vgoal, tier, diff=goaldif, result, venue)

          dfv <- draw.data1(input$typetier) %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            filter (visitor==team1) %>%
            mutate (diff = -goaldif, venue="away") %>%
            select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                    goalsagainst=hgoal, tier, diff, result, venue)


          df1tables <- rbind(dfh,dfv)

        }

    }


  else

    if (input$team1tables=="All teams" & input$team2tables=="All teams"){


      minyear <- input$yeartables[1]
      maxyear <- input$yeartables[2]

      if(input$type1=="home"){

        df1tables <- draw.data1(input$typetier) %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          mutate (venue="home") %>%
          select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                  goalsagainst=vgoal, tier, diff=goaldif, result, venue, matchid)

      }
      else

        if(input$type1=="away"){

          df1tables <- draw.data1(input$typetier) %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            mutate (diff = -goaldif, venue="away") %>%
            select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                    goalsagainst=hgoal, tier, diff, result, venue, matchid)

        }
      else

        if(input$type1=="both") {

          dfh <- draw.data1(input$typetier) %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            mutate (venue="home") %>%
            select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                    goalsagainst=vgoal, tier, diff=goaldif, result, venue, matchid)

          dfv <- draw.data1(input$typetier) %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            mutate (diff = -goaldif, venue="away") %>%
            select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                    goalsagainst=hgoal, tier, diff, result, venue, matchid)


          df1tables <- rbind(dfh,dfv)

        }

    }


})


####

datasetInput <- reactive({



  if(input$dataset1=="Biggest Wins") {

    df1tables() %>% filter(diff>0) %>% arrange(desc(diff), desc(goalsfor)) %>%
      mutate(FT=paste(goalsfor,goalsagainst,sep="-"), GD=diff) %>%
      select(Season, team, opponent, FT, tier, GD, venue)

  }

  else


    if(input$dataset1=="Worst Losses") {

      df1tables() %>% filter(diff<0) %>% arrange(diff, goalsfor) %>%
        mutate(FT=paste(goalsfor,goalsagainst,sep="-"), GD=diff) %>%
        select(Season, team, opponent, FT, tier, GD, venue)

    }

  else


    if(input$dataset1=="Overall Record") {

      dfy <-
        df1tables() %>%
        group_by(team) %>%
        summarise(GP = n(),
                  W = sum(diff>0),
                  D = sum(diff==0),
                  L = sum(diff<0),
                  GF = sum(goalsfor),
                  GA = sum(goalsagainst),
                  GD = sum(diff),
                  Pts = ((W*3)+(D*1)),
                  Wpct = (W/GP),
                  PPG = Pts/GP,
                  GFPG = GF/GP,
                  GAPG = GA/GP
        )

      if (input$sortrecord=="Wins" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(W))  }
      if (input$sortrecord=="Games Played" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(GP))  }
      if (input$sortrecord=="Draws" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(D))  }
      if (input$sortrecord=="Goal Difference" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(GD))  }
      if (input$sortrecord=="Points" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(Pts))  }
      if (input$sortrecord=="Win Percentage" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(Wpct))  }
      if (input$sortrecord=="Losses" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(L))  }
      if (input$sortrecord=="Goals For" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(GF))  }
      if (input$sortrecord=="Goals Against" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(GA))  }
      if (input$sortrecord=="PPG" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(PPG))  }
      if (input$sortrecord=="Season" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(Season))  }
      if (input$sortrecord=="GFPG" & input$typead=="desc"){ dfy <- dfy %>% arrange(desc(GFPG))  }
      if (input$sortrecord=="GAPG" & input$typead=="desc"){ dfy <- dfy %>%  arrange(desc(GAPG))  }




      if (input$sortrecord=="Wins" & input$typead=="asc"){ dfy <- dfy %>% arrange(W)  }
      if (input$sortrecord=="Games Played" & input$typead=="asc"){ dfy <- dfy %>% arrange(GP)  }
      if (input$sortrecord=="Draws" & input$typead=="asc"){ dfy <- dfy %>% arrange(D)  }
      if (input$sortrecord=="Goal Difference" & input$typead=="asc"){ dfy <- dfy %>% arrange(GD)  }
      if (input$sortrecord=="Points" & input$typead=="asc"){ dfy <- dfy %>% arrange(Pts)  }
      if (input$sortrecord=="Win Percentage" & input$typead=="asc"){ dfy <- dfy %>% arrange(Wpct)  }
      if (input$sortrecord=="Losses" & input$typead=="asc"){ dfy <- dfy %>% arrange(L)  }
      if (input$sortrecord=="Goals For" & input$typead=="asc"){ dfy <- dfy %>% arrange(GF)  }
      if (input$sortrecord=="Goals Against" & input$typead=="asc"){ dfy <- dfy %>% arrange(GA)  }
      if (input$sortrecord=="PPG" & input$typead=="asc"){ dfy <- dfy %>% arrange(PPG)  }
      if (input$sortrecord=="Season" & input$typead=="asc"){ dfy <- dfy %>% arrange(Season)  }
      if (input$sortrecord=="GFPG" & input$typead=="asc"){ dfy <- dfy %>% arrange(GFPG)  }
      if (input$sortrecord=="GAPG" & input$typead=="asc"){ dfy <- dfy %>% arrange(GAPG)  }


      dfy$Pts <- sprintf('%1.0f', dfy$Pts)
      dfy$Wpct <- sprintf('%1.3f', dfy$Wpct)
      dfy$PPG <- sprintf('%1.3f', dfy$PPG)
      dfy$GFPG <- sprintf('%1.3f', dfy$GFPG)
      dfy$GAPG <- sprintf('%1.3f', dfy$GAPG)
      data.frame(dfy)



    }




  else


    if(input$dataset1=="Season Record") {

      dfx <-
        df1tables() %>%
        group_by(Season, team) %>%
        summarise(GP = n(),
                  W = sum(diff>0),
                  D = sum(diff==0),
                  L = sum(diff<0),
                  GF = sum(goalsfor),
                  GA = sum(goalsagainst),
                  GD = sum(diff),
                  Pts = ((W*3)+(D*1)),
                  Wpct = (W/GP),
                  PPG = Pts/GP,
                  GFPG = GF/GP,
                  GAPG = GA/GP
        ) %>%
        ungroup()

      if (input$sortrecord=="Wins" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(W))  }
      if (input$sortrecord=="Games Played" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(GP))  }
      if (input$sortrecord=="Draws" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(D))  }
      if (input$sortrecord=="Goal Difference" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(GD))  }
      if (input$sortrecord=="Points" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(Pts))  }
      if (input$sortrecord=="Win Percentage" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(Wpct))  }
      if (input$sortrecord=="Losses" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(L))  }
      if (input$sortrecord=="Goals For" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(GF))  }
      if (input$sortrecord=="Goals Against" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(GA))  }
      if (input$sortrecord=="PPG" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(PPG))  }
      if (input$sortrecord=="Season" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(Season))  }
      if (input$sortrecord=="GFPG" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(GFPG))  }
      if (input$sortrecord=="GAPG" & input$typead=="desc"){ dfx <- dfx %>% arrange(desc(GAPG))  }



      if (input$sortrecord=="Wins" & input$typead=="asc"){ dfx <- dfx %>% arrange(W)  }
      if (input$sortrecord=="Games Played" & input$typead=="asc"){ dfx <- dfx %>% arrange(GP)  }
      if (input$sortrecord=="Draws" & input$typead=="asc"){ dfx <- dfx %>% arrange(D)  }
      if (input$sortrecord=="Goal Difference" & input$typead=="asc"){ dfx <- dfx %>% arrange(GD)  }
      if (input$sortrecord=="Points" & input$typead=="asc"){ dfx <- dfx %>% arrange(Pts)  }
      if (input$sortrecord=="Win Percentage" & input$typead=="asc"){ dfx <- dfx %>% arrange(Wpct)  }
      if (input$sortrecord=="Losses" & input$typead=="asc"){ dfx <- dfx %>% arrange(L)  }
      if (input$sortrecord=="Goals For" & input$typead=="asc"){ dfx <- dfx %>% arrange(GF)  }
      if (input$sortrecord=="Goals Against" & input$typead=="asc"){ dfx <- dfx %>% arrange(GA)  }
      if (input$sortrecord=="PPG" & input$typead=="asc"){ dfx <- dfx %>% arrange(PPG)  }
      if (input$sortrecord=="Season" & input$typead=="asc"){ dfx <- dfx %>% arrange(Season)  }
      if (input$sortrecord=="GFPG" & input$typead=="asc"){ dfx <- dfx %>% arrange(GFPG)  }
      if (input$sortrecord=="GAPG" & input$typead=="asc"){ dfx <- dfx %>% arrange(GAPG)  }



      dfx$Pts <- sprintf('%1.0f', dfx$Pts)
      dfx$Wpct <- sprintf('%1.3f', dfx$Wpct)
      dfx$PPG <- sprintf('%1.3f', dfx$PPG)
      dfx$GFPG <- sprintf('%1.3f', dfx$GFPG)
      dfx$GAPG <- sprintf('%1.3f', dfx$GAPG)

      data.frame(dfx)



    }




  else



    if(input$dataset1=="Highest scoring games") {

      if (input$team1tables=="All teams" & input$team2tables=="All teams" ){

        df1tables() %>%
          distinct(matchid) %>%
          mutate(total.goals = goalsfor+goalsagainst,
                 FT=paste(goalsfor,goalsagainst,sep="-"), GD=diff) %>%
          arrange(desc(total.goals), desc(GD), desc(goalsfor))%>%
          select(Season, team, opponent, FT, tier, total.goals, GD)

      }

      else

        df1tables() %>% mutate(total.goals = goalsfor+goalsagainst,
                               FT=paste(goalsfor,goalsagainst,sep="-"), GD=diff) %>%
        arrange(desc(total.goals), desc(GD), desc(goalsfor))%>%
        mutate(Result =
                 ifelse((diff>0), "Win",
                        ifelse(diff<0, "Loss", "Draw"))) %>%
        select(Season, team, opponent, FT, tier, total.goals, GD, Result, venue)


    }

  else

    if(input$dataset1=="Highest scoring draws") {

      if (input$team1tables=="All teams" & input$team2tables=="All teams" ){

        df1tables() %>%
          distinct(matchid) %>%
          mutate(total.goals = goalsfor+goalsagainst,
                 FT=paste(goalsfor,goalsagainst,sep="-"), GD=diff) %>%
          arrange(desc(total.goals), desc(GD), desc(goalsfor)) %>%
          filter(GD==0) %>%
          select(Season, team, opponent, FT, tier, total.goals)


      }

      else




        df1tables() %>% mutate(total.goals = goalsfor+goalsagainst,
                               FT=paste(goalsfor,goalsagainst,sep="-"), GD=diff) %>%
        arrange(desc(total.goals), desc(GD), desc(goalsfor))%>%
        filter(GD==0) %>%
        select(Season, team, opponent, FT, tier, total.goals, venue)


    }



})



######  Here begins the reactives for Head to Head...


dft1 <- reactive({


  if (input$team2tvt!="All teams"){


    minyear <- input$yeartvt[1]
    maxyear <- input$yeartvt[2]
    team1 <- input$team1tvt
    team2 <- input$team2tvt

    if(input$typetvt=="home"){

      dft1 <- engsoccerdata2 %>%
        filter(Season >= minyear) %>%
        filter(Season <= maxyear) %>%
        filter (home==team1 & visitor==team2) %>%
        mutate (venue="home") %>%
        select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                goalsagainst=vgoal, tier, diff=goaldif, result, venue)

    }
    else

      if(input$typetvt=="away"){

        dft1 <- engsoccerdata2 %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          filter (home==team2 & visitor==team1) %>%
          mutate (diff = -goaldif, venue="away") %>%
          select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                  goalsagainst=hgoal, tier, diff, result, venue)

      }
    else

      if(input$typetvt=="both") {

        dfth <- engsoccerdata2 %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          filter (home==team1 & visitor==team2) %>%
          mutate (venue="home") %>%
          select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                  goalsagainst=vgoal, tier, diff=goaldif, result, venue)

        dftv <- engsoccerdata2 %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          filter (home==team2 & visitor==team1) %>%
          mutate (diff = -goaldif, venue="away") %>%
          select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                  goalsagainst=hgoal, tier, diff, result, venue)


        dft1 <- rbind(dfth,dftv)

      }


  }

  else
    if (input$team1tvt!="All teams" & input$team2tvt=="All teams"){

      minyear <- input$yeartvt[1]
      maxyear <- input$yeartvt[2]
      team1 <- input$team1tvt

      if(input$typetvt=="home"){

        dft1 <- engsoccerdata2 %>%
          filter(Season >= minyear) %>%
          filter(Season <= maxyear) %>%
          filter (home==team1) %>%
          mutate (venue="home") %>%
          select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                  goalsagainst=vgoal, tier, diff=goaldif, result, venue)

      }
      else

        if(input$typetvt=="away"){

          dft1 <- engsoccerdata2 %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            filter (visitor==team1) %>%
            mutate (diff = -goaldif, venue="away") %>%
            select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                    goalsagainst=hgoal, tier, diff, result, venue)

        }
      else

        if(input$typetvt=="both") {

          dfth <- engsoccerdata2 %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            filter (home==team1) %>%
            mutate (venue="home") %>%
            select (Season, team=home, opponent=visitor, goalsfor=hgoal,
                    goalsagainst=vgoal, tier, diff=goaldif, result, venue)

          dftv <- engsoccerdata2 %>%
            filter(Season >= minyear) %>%
            filter(Season <= maxyear) %>%
            filter (visitor==team1) %>%
            mutate (diff = -goaldif, venue="away") %>%
            select (Season, team=visitor, opponent=home, goalsfor=vgoal,
                    goalsagainst=hgoal, tier, diff, result, venue)


          dft1 <- rbind(dfth,dftv)

        }



    }


})




dft1checkbox1 <-  reactive({

  if (input$checkbox1==T){

    dft1()  %>% filter(Season!=1939)

  }

  else
    dft1()

})



dft2 <-  reactive({

  if(input$Outcometvt=="cgoald"){

    dft1.cgoald <- dft1checkbox1() %>%
      group_by(Season, team, tier) %>%
      summarize(diffsum=sum(diff)) %>%
      ungroup()%>%
      mutate(cumdiff=cumsum(diffsum))


    myseasons <- data.frame(Season = unique(engsoccerdata2$Season))
    myseasons <- myseasons %>% left_join(dft1.cgoald)

    myseasonsNA <- data.frame(Season=rep(NA,4), team=rep(NA,4), tier=c(1:4),
                              diffsum=rep(NA,4), cumdiff=rep(NA,4) )

    rbind(myseasons, myseasonsNA)  #use this one for plotting, has NAs for each tier(1:4))



  }

  else

    if(input$Outcometvt=="cpoints"){


      dfty <- dft1checkbox1() %>%
        mutate(teamres= ifelse(diff<0, 0, ifelse(diff>0, 3, 1)),
               oppres= ifelse(diff<0, 3, ifelse(diff>0, 0, 1)) ) %>%
        group_by(Season, team, tier) %>%
        summarize(teampoints=sum(teamres), opppoints=sum(oppres)) %>%
        ungroup() %>%
        mutate(cumteampoints= cumsum(teampoints),
               cumopppoints=cumsum(opppoints))


      myseasons <- data.frame(Season = unique(engsoccerdata2$Season))
      myseasons <- myseasons %>% left_join(dfty)

      teamname<-dfty[1:4,2]

      myseasonsNA <- data.frame(Season=rep(NA,4), team=teamname, tier=c(1:4),
                                teampoints=rep(NA,4), opppoints=rep(NA,4),
                                cumteampoints=rep(NA,4), cumopppoints=rep(NA,4))

      myseasons.dfty <- rbind(myseasons, myseasonsNA)  #use this one for plotting, has NAs for each tier(1:4))


      dfty1 <- myseasons.dfty %>% select(Season,teamx=team, tier, cpoints=cumteampoints)
      dfty2 <- myseasons.dfty %>% mutate(teamx="opponent") %>% select(Season,teamx,tier, cpoints=cumopppoints)

      thisone <- rbind(dfty1,dfty2)

      thisone %>% arrange (Season,teamx) #because have 4NA rows for team + opponent, need to remove 8NA rows below...


    }

  else

    if(input$Outcometvt=="hth"){


        dft1checkbox1() %>% mutate(result1 = ifelse(diff>0, "W", ifelse(diff<0, "L", "D")))



    }


  else


    if(input$Outcometvt=="cresults"){


      myseasons.prelim <- dft1checkbox1()   %>%
        mutate(result1 = ifelse(diff>0, "W", ifelse(diff<0, "L", "D"))) %>%
        group_by(Season,team, tier) %>%
        summarise(GP=n(), W=sum(result1=="W"), D=sum(result1=="D"), L=sum(result1=="L")) %>%
        ungroup() %>%
        mutate(Games = cumsum(GP), Wins = cumsum(W), Draws=cumsum(D), Losses=cumsum(L) ) %>%
        select (Season, team, Wins, Draws, Losses, tier) %>%
        gather (group,value,3:5)

      myseasons <- data.frame(Season = unique(engsoccerdata2$Season))
      myseasons <- myseasons %>% left_join(myseasons.prelim)

      myseasonsNA <- data.frame(Season=rep(NA,3), team=rep(NA,3), tier=rep(NA,3),
                                group=c("Wins", "Draws", "Losses"), value=rep(NA,3))

      rbind(myseasons, myseasonsNA)  #use this one for plotting, has NAs for each group(W/D/L)

    }

})


## Tier Shadow Reactive - this is a #$%@$^$ nightmare! - it could have been avoided, but I'm not going to fix it just yet.

dft2.tier <- reactive({

  if(input$checkbox2==T){

    if(input$Outcometvt=="cpoints"){
      N <- nrow(dft2())-8  #have 4NAs each for teams and opponents in this returned df

    }
    else

      if(input$Outcometvt=="cgoald"){
        N <- nrow(dft2())-4  #why need to do this for cgoald (and cpoints above) to get last segment but not cresults?

      }
    else {
      N <- nrow(dft2())
    }


    tb.rle <- rle(dft2()[1:N,3])

    indexdf <- data.frame(rleout= tb.rle[[1]],
                          beginrow =  ( ( cumsum(tb.rle[[1]]) - tb.rle[[1]] ) + 1 ),
                          endrow = cumsum(tb.rle[[1]])
    )

    indexdf$tier = dft2()[indexdf$beginrow, 3]

    indexdf1 <- indexdf[!is.na(indexdf[,4]),]  #remove NAs

    indexdf2 <- data.frame (xmin= dft2()[indexdf1[,2],1]  -.5,
                            xmax= dft2()[indexdf1[,3],1]  +.5,
                            ymin=-Inf,
                            ymax=Inf)  # each row is to be split into a unique dataframe

    indexdf2$id <- 1:nrow(indexdf2)
    indexdf2$tier <- indexdf1$tier

    indexdf2NA <- data.frame(xmin=rep(NA,4), xmax=rep(NA,4), ymin=rep(NA,4), ymax=rep(NA,4), id=rep(NA,4), tier= c(1:4))

    rbind(indexdf2,indexdf2NA)


  }
  else
    NULL

})




### head-to-head interactive ggtitle for hth records
hthtitle <- reactive({

  hthx1 <- input$team1tvt
  hthx2 <- input$team2tvt
  paste(hthx1, "'s record vs", hthx2, "by Season")
})


### for points
hthtitle2 <- reactive({

  hthx1 <- input$team1tvt
  hthx2 <- input$team2tvt
  paste("Cumulative points accrued by", hthx1, "and", hthx2, "in games between them by Season")
})

### for goal difference
hthtitle3 <- reactive({

  hthx1 <- input$team1tvt
  hthx2 <- input$team2tvt
  paste("Cumulative goal difference accrued by", hthx1, "vs", hthx2, "in games between them by Season")
})


### for results
hthtitle1 <- reactive({

  hthx1 <- input$team1tvt
  hthx2 <- input$team2tvt
  paste("Cumulative results between", hthx1, "and", hthx2, " by Season")
})









##########  Return Main Panel Output  ####################




 output$plot1 <- renderPlot({

   data = df1()

   g <-     ggplot(data, aes(x=meanOutcomex, y=meanOutcomey, colour=factor(tier))) +
            geom_point(shape=19, size=4) +
            scale_color_manual(values=c("1"="firebrick1", "2"="deepskyblue", "3"="forestgreen","4"="blueviolet")) +
            ylab("") + xlab("") +
            theme(legend.title=element_blank(),
                  text = element_text(color="grey23", size=10),
                  axis.text = element_text(size=rel(1.0)),
                  axis.text.x = element_text(color="grey23",size=rel(1.6)),
                  axis.text.y = element_text(color="grey23", size=rel(1.6)),
                  axis.title.x = element_text(size=rel(1.6)),
                  axis.title.y = element_text(size=rel(1.6)),
                  axis.ticks.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.grid.major.y = element_line(color="grey80"),
                  panel.grid.major.x = element_line(color="grey80"),
                  panel.grid.minor = element_blank(),
                  panel.background = element_rect(fill="ghostwhite"),
                  plot.background = element_rect(fill="ghostwhite")

            )

   g1 <- g + stat_smooth(aes(fill = factor(tier)), size=1, alpha = 0.05)

   ifelse(input$show.trend==T,
          print (g1),
          print(g))

  })




# Make plots

output$plot2 <- renderPlot({

  plot.new()
  par(mfrow = c(2, 2))
  par(mar = c(3, 1, 2, 1))

  plotGantt1(df11(), res.col='tier', start.col='Season', end.col='Season1',
             res.colors=mycolors(), plottitle=input$team1, minval=input$yeargantt[1],
             maxval=input$yeargantt[2])

  plotGantt1(df22(), res.col='tier', start.col='Season', end.col='Season1',
             res.colors=mycolors(), plottitle=input$team2, minval=input$yeargantt[1],
             maxval=input$yeargantt[2])

  plotGantt1(df33(), res.col='tier', start.col='Season', end.col='Season1',
             res.colors=mycolors(), plottitle=input$team3, minval=input$yeargantt[1],
             maxval=input$yeargantt[2])

  plotGantt1(df44(), res.col='tier', start.col='Season', end.col='Season1',
             res.colors=mycolors(), plottitle=input$team4, minval=input$yeargantt[1],
             maxval=input$yeargantt[2])


})


# Make table

output$view <- renderTable({
  head(datasetInput(), n = input$obs)
})



### Plots for Head to Head-

output$plot1tvt <- renderPlot({



  if (nrow(dft1())==0 ) {

    stop("There are no games between these two teams in the database given the current parameters - please try another opponent !")

  }

  else

    g <-  reactive({


      if(input$Outcometvt=="cgoald"  & input$checkbox2==F){


        ggplot(dft2(), aes(x=Season, y=cumdiff)) +
          ggtitle(hthtitle3()) + ylab("")+
          geom_point(size=4, shape=19, color=input$colortype1tvt) +
          geom_line(color=input$colortype1tvt) +
          xlim(1885,2015)

      }


      else


        if(input$Outcometvt=="cgoald"  & input$checkbox2==T){


          ggplot(dft2(), aes(x=Season, y=cumdiff)) +
            ggtitle(hthtitle3()) +  ylab("")+
            geom_point(size=4, shape=19, color=input$colortype1tvt) +
            geom_line(color=input$colortype1tvt) +
            xlim(1885,2015) +
            geom_rect(data=dft2.tier(), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                                            group=id, fill = factor(tier)),
                      alpha=0.15, inherit.aes = FALSE) +
            scale_fill_manual(values= c("firebrick1", "deepskyblue", "chartreuse4", "blueviolet"))


        }

      else

        if(input$Outcometvt=="cpoints" & input$checkbox2==F){

          ggplot(dft2(), aes(x=Season, y=cpoints, group=factor(teamx), color=factor(teamx))) +
            ggtitle(hthtitle2()) +  ylab("")+
            geom_point(size=4, shape=19) +
            scale_color_manual(values=c(input$colortype1tvt, input$colortype2tvt))  +
            xlim(1885,2015)

        }


      else

        if(input$Outcometvt=="cpoints" & input$checkbox2==T){

          ggplot(dft2(), aes(x=Season, y=cpoints, group=factor(teamx), color=factor(teamx))) +
            ggtitle(hthtitle2()) +  ylab("")+
            geom_point(size=4, shape=19) +
            scale_color_manual(values=c(input$colortype1tvt, input$colortype2tvt))  +
            xlim(1885,2015) +
            geom_rect(data=dft2.tier(), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                                            group=id, fill = factor(tier)),
                      alpha=0.15, inherit.aes = FALSE) +
            scale_fill_manual(values= c("firebrick1", "deepskyblue", "chartreuse4", "blueviolet"))

          #why doesn't the line of the opponent join up?
        }


      else

        if(input$Outcometvt=="hth"){

          if (input$team2tvt=="All teams" ) {

            stop("Please select another opponent - this chart needs specific opponents!")

          }

          else

            ggplot(dft2(), aes(Season, venue, color=result1)) +
            geom_tile(aes(fill = result1)) +  ylab("") +
            scale_fill_manual(values=c("D" = "orange", "L" = "white", "W" = "red")) +
            theme(legend.position="bottom") +
            coord_fixed(ratio=2.5) +
            ggtitle(hthtitle()) +
            theme(plot.title = element_text(face="bold", hjust=-0.05, vjust=8, size=rel(2.0))) +
            xlim(1885,2015)

        }

      else

        if(input$Outcometvt=="cresults" & input$checkbox2==F){

          ggplot(dft2(), aes(Season,value, color=group)) +
            geom_point(size=3.7, shape=19) +
            scale_color_manual(values=c("red", "orange1", "gray35")) +
            ggtitle(hthtitle1()) +  ylab("")+
            ylab("Total games") +
            theme(plot.title = element_text(face="bold", hjust=-0.05, vjust=8, size=rel(2.0))) +
            xlim(1885,2015)

        }



      else

        if(input$Outcometvt=="cresults" & input$checkbox2==T){

          ggplot(dft2(), aes(Season,value, color=group)) +
            geom_point(size=3.7, shape=19) +
            scale_color_manual(values=c("red", "orange1", "gray35")) +
            ggtitle(hthtitle1()) +  ylab("") +
            ylab("Total games") +
            theme(plot.title = element_text(face="bold", hjust=-0.05, vjust=8, size=rel(2.0))) +
            xlim(1885,2015) +
            geom_rect(data=dft2.tier(), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
                                            group=id, fill = factor(tier)),
                      alpha=0.15, inherit.aes = FALSE) +
            scale_fill_manual(values= c("firebrick1", "deepskyblue", "chartreuse4", "blueviolet"))


        }




    })


  g() +   xlab("") +

    theme(legend.title=element_blank(),
          text = element_text(color="grey23", size=10),
          axis.text = element_text(size=rel(1.0)),
          axis.text.x = element_text(color="grey23",size=rel(1.6)),
          axis.text.y = element_text(color="grey23", size=rel(1.6)),
          axis.title.x = element_text(size=rel(1.6)),
          axis.title.y = element_text(size=rel(1.6)),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major.y = element_line(color="grey80"),
          panel.grid.major.x = element_line(color="grey80"),
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill="ghostwhite"),
          plot.background = element_rect(fill="ghostwhite")

    )



})










}
)





