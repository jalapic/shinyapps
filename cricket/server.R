library(shiny)
library(dplyr)
library(ggvis)




shinyServer(function(input, output) {
  
  

#### Input raw data
  df <- read.csv("testbat.csv", stringsAsFactors=F, header=T)
  
  
## Match names in team column to drop down menu, but create new variable.  
mydf<-data.frame(teamid = 
                      c("Aus", "Ban", "Eng", "India", "NZ", "Pak", "SA", "SL", "WI", "Zim", "ICC"), 
   teamname =  c("Australia", "Bangladesh", "England", "India", "New Zealand", "Pakistan", 
    "South Africa", "Sri Lanka", "West Indies", "Zimbabwe",  "ICC World XI"))

df$teamlong <- mydf$teamname[match(df$team, mydf$teamid)]


## Set up home/overseas variable
df$overseas <- ifelse(df$teamlong==df$country, df$overseas <- FALSE, df$overseas <- TRUE)
  
  
  df1 <- reactive({
     
    minyear <- input$year[1]
    maxyear <- input$year[2]
    
    mininngs <- input$inngs[1]
    maxinngs <- input$inngs[2]
  
    oppteam <-  input$vteam[1]
    playerteam <-  input$hteam[1]
    
    xvarname <-  input$xvar
    yvarname <-  input$yvar
    
    allcountries <- unique(df$team) #added this to keep all countries in every df - ensures color consistency
    
  if(input$vteam != "All Teams" & input$hteam=="All Teams"){  df <- df %>% filter(opp==oppteam) }
  if(input$vteam != "All Teams" & input$hteam!="All Teams"){  df <- df %>% filter(opp==oppteam & teamlong==playerteam) }
  if(input$hteam != "All Teams" & input$vteam=="All Teams"){  df <- df %>% filter(teamlong==playerteam) }
  if(input$hteam != "All Teams" & input$vteam!="All Teams"){  df <- df %>% filter(teamlong==playerteam & opp==oppteam) }
  
  if(input$location=="home"){  df <- df %>% filter(overseas==F) }  
  if(input$location=="away"){  df <- df %>% filter(overseas==T) }  
  
  df1 <- df %>% 
      filter(yearx >= minyear & yearx <= maxyear) %>%
      group_by(player) %>% 
      summarize(totalruns=sum(runs,na.rm=T), 
                totalrows=n(), 
                totalna=sum(is.na(runs)), 
                totalinns = totalrows -totalna,
                totalno=sum(no==T),
                totalout = totalrows - totalna - totalno,
                avg = totalruns / totalout,
                debutyear = min(yearx),
                totalover50s = sum(runs >= 50, na.rm=T),
                total100s = sum(runs >= 100, na.rm=T),
                total50s = totalover50s - total100s,
                country = unique(team)
      ) %>%
      filter(totalout>0) %>%
      filter(totalinns >= mininngs & totalinns <=maxinngs) %>%
    mutate(id = row_number(player)) 
  
  df1 <- as.data.frame(df1)
  
  Nx <- which(colnames(df1)==xvarname)
  df1$xvarname1 <- df1[,Nx]
  
  
  Ny <- which(colnames(df1)==yvarname)
  df1$yvarname1 <- df1[,Ny]

  df1 <- as.data.frame(df1)


# Ensure factor levels are set for color consistency:

mylevels <- c("Aus", "Eng", "SA", "WI", "NZ", "India", "Pak", "SL", "Zim", "Ban", "ICC")
df1$country <- factor(df1$country, levels=mylevels)
df1 <-as.data.frame(df1)

})



# Function for generating tooltip text

    all_values <- function(x) {
      if(is.null(x)) return(NULL)
      
# Pick out the player with this ID
      df1play <- isolate(df1())

      paste0(df1play$player[x$id], 
             " - ",  
            df1play$country[x$id],
           "<br>Debut: ", df1play$debutyear[x$id],
           "<br>Total runs: ", df1play$totalruns[x$id],
           "<br>Avg: ", round(df1play$avg[x$id],digits=2)
)
    }

### Reactive expressions for the axes labels and title

axesdf <-  c("Total inninngs" = "totalinns",
             "Total runs" = "totalruns",
             "Average" = "avg",
             "Total not outs" = "totalno",
             "Total 50s" = "total50s",
             "Total 100s" = "total100s",
             "Debut year" = "debutyear")

xaxislabel <- reactive({
  
xvarname <-  input$xvar
names(axesdf)[axesdf == xvarname]
  
})

mytitle <- reactive({
  
  yvarname <-  input$yvar
  names(axesdf)[axesdf == yvarname]
  
})

### Hack function to make title and x-axes
add_title <- function(vis, ..., x_lab = "X units", title = "Plot Title") 
{
  add_axis(vis, "x", title = x_lab,  format="####") %>% 
    add_axis("x", orient = "top", ticks = 0, title = title,
             properties = axis_props(
               axis = list(stroke = "white"),
               labels = list(fontSize = 0)
             ), ...)
}




# A reactive expression with the ggvis plot

input_size <- reactive(input$size)


vis <- reactive({


df1() %>% 
    ggvis(x= ~xvarname1, y= ~yvarname1)  %>% 
    layer_points(size := input_size,
                 size.hover := 200,
                 fillOpacity := 0.65,
                 fillOpacity.hover := 1,
                 fill = ~country,
                 key := ~id) %>%
    add_tooltip(all_values, "hover") %>%
    add_title(title = paste(xaxislabel(), " against ", mytitle(), sep=" "), 
              x_lab = xaxislabel() )%>%
    add_axis("y", title = "")
  
  

  
})

vis %>% bind_shiny("plot1")

})

