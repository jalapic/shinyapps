library(shiny)
library(dplyr)
library(ggvis)
library(engsoccerdata)



shinyServer(function(input, output) {
  
### For axis variables
axis_vars  <- c("Season" = "Season",
                "Goals for" = "gf",
                "Goals against" = "ga",
                "Goal difference" = "diff",
                "Total points" = "pts",
                "Total wins" = "totwin",
                "Total losses" = "totloss",
                "Total draws" = "totdraw",
                "Win pct" = "winpct",
                "Loss pct" = "losepct",
                "Points per game" = "ptspg",
                "Goals for per game" = "gfpg",
                "GoalS against per game" = "gapg",
                "Goal difference per game" = "diffpg",
                "GF/game vs league average" = "gfpgx",
                "GA/game vs league average" = "gapgx"
  )  
  
  
  
## Get data columns and add color variable 

df.homes <- engsoccerdata2 %>%  mutate(venue="home") %>%
               filter(Season!=1939) %>% select(Season, team=home, opp=visitor, gf=hgoal, ga=vgoal, tier, venue)

df.visitor <- engsoccerdata2 %>%  mutate(venue="away") %>%
                filter(Season!=1939) %>% select(Season, team=visitor, opp=home, gf=vgoal, ga=hgoal, tier, venue)

df <- rbind(df.homes,df.visitor) %>% mutate(diff = gf-ga) 

# filter out teams with incomplete records
#had issues with the filtering - this is crappy way, but works
df <- df %>% mutate(xxx = paste(team,Season,sep="-"))
df <- df %>% filter(xxx!="Leeds City-1919" & xxx!="Port Vale-1919" &
                     xxx!="Accrington Stanley-1961" & xxx!="Wigan Borough-1931")






### First reactive - main calcs

df1 <- reactive({
  
  minyear <- input$year[1]
  maxyear <- input$year[2]
  
  xvarname <-  input$xvar
  yvarname <-  input$yvar
  

  xxx <- input$checkGroup  
  df <-  df[(df$tier %in% xxx == T),] #filter out non-selected tiers
  
  
  
  if(input$location == "both"){  df <- df }
  if(input$location == "home"){  df <- df %>% filter(venue=="home") }
  if(input$location == "away"){  df <- df %>% filter(venue=="away") }
  
  
  df1<-df %>% 
    filter(Season >= minyear & Season <= maxyear) %>%
    group_by(Season) %>%
    mutate(tiergp = n(),
           tiergoals = sum(gf),
           tiergfpg = tiergoals/tiergp) %>%
    group_by(team,Season, tier) %>%
    mutate(result1 = ifelse(diff>0, "win", ifelse(diff<0, "loss", "draw"))) %>%
    summarize(gp = n(),
              gf = sum(gf),
              ga = sum(ga),
              diff = sum(diff),
              gfpg = gf/gp,
              gapg = ga/gp,
              gfpgx = (gfpg - mean(tiergfpg)) / mean(tiergfpg),
              gapgx = (gapg-mean(tiergfpg)) / mean(tiergfpg),
              diffpg = diff/gp,
              totwin = sum(result1=="win"),
              totloss = sum(result1=="loss"),
              totdraw = sum(result1=="draw"),
              winpct = totwin/gp,
              losepct = totloss/gp,
              pts = (totwin*3 + totdraw),
              ptspg = pts/gp) %>%
    ungroup() %>%
    mutate(id = row_number(team)) 
  
  
## This duplicates x and y columns and gives them a fixed name to input into ggvis-plot
  df1 <- as.data.frame(df1)
  
  Nx <- which(colnames(df1)==xvarname)
  df1$xvarname1 <- df1[,Nx]
  
  
  Ny <- which(colnames(df1)==yvarname)
  df1$yvarname1 <- df1[,Ny]
  
  df1 <- as.data.frame(df1)
  

# Ensure factor levels are set for color consistency:
  
 mylevels <- 1:4
 df1$tier <- factor(df1$tier, levels=mylevels)
 df1 <-as.data.frame(df1)
  
})



# Function for generating tooltip text

all_values <- function(x) {
  if(is.null(x)) return(NULL)
  
  # Pick out the team/season with this ID
  df1play <- isolate(df1())
  

  
  
  
  paste0(df1play$team[x$id], 
         " - ",  
         df1play$Season[x$id],
         "/",
         df1play$Season[x$id]+1#,
      #   "<br>", 
      #   df1play$yvarname1[x$id]
  )
}





### A reactive expression with the ggvis plot

input_size <- reactive(input$size)
input_opacity <- reactive(input$opacity)


vis <- reactive({
  
  # Lables for axes
  xvar_name <- names(axis_vars)[axis_vars == input$xvar]
  yvar_name <- names(axis_vars)[axis_vars == input$yvar]
  
  
  
 myvis <- df1() %>% 
    ggvis(x= ~xvarname1, y= ~yvarname1)  %>% 
    layer_points(size := input_size,
                 size.hover := 200,
                 fillOpacity := input_opacity,
                 fillOpacity.hover := 1,
                 fill = ~tier,
                 key := ~id) %>%
    add_tooltip(all_values, "hover") %>% 
    add_axis("y", title = yvar_name)
    
if(input$xvar == "Season") {myvis <- myvis %>% add_axis("x", title = xvar_name,  format="####") }
if(input$xvar != "Season") {myvis <- myvis %>% add_axis("x", title = xvar_name) }


myvis
  
})

vis %>% bind_shiny("plot1")

})
