library(shiny)
library(engsoccerdata)
library(dplyr)
library(ggvis)



# Define UI
shinyUI(fluidPage(
  titlePanel("Comparing English soccer teams across time"),
  
fluidRow(
    column(3,

  wellPanel( 
    sliderInput("year", h5("Choose date range:"), 1888, 2014, value = c(1888, 2014), format="####"),
    br(),
    
    radioButtons("location", h5("Location:"),
                 list("Home & away" = "both", "Home only" = "home", "Away only" = "away"), inline = TRUE),
    br(),
    
    checkboxGroupInput("checkGroup", label = h5("Select tiers to include:"), 
                       choices = list("Tier 1" = 1, "Tier 2" = 2, "Tier 3" = 3, "Tier 4" = 4),
                       selected = c(1:4), inline = TRUE )
  ),
    
  wellPanel(
    h4("Select Variables"),
      
      selectInput("xvar", label = "X Axis:",
                  list("Season" = "Season",
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
                            "Goals against per game" = "gapg",
                            "Goal difference per game" = "diffpg",
                            "GF/game vs league average" = "gfpgx",
                            "GA/game vs league average" = "gapgx"
                       ),selected="diff"),
      
      
      selectInput("yvar", label = "Y Axis:",
                  list("Goals for" = "gf",
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
                  "Goals against per game" = "gapg",
                  "Goal difference per game" = "diffpg",
                  "GF/game vs league average" = "gfpgx",
                  "GA/game vs league average" = "gapgx"
                       ),selected="pts")
      ),
      

  wellPanel(
    h4("Prettify"),
        
      sliderInput("size", "Change dot size:", 10, 100, step = 5, value=55),
      sliderInput("opacity", "Change dot opacity:", 0, 1, step = 0.05, value=0.45)
    ), 
    
    
  
    
  wellPanel(
    
    br(),
    
    helpText("These visualizations allow the user to compare teams across seasons.   
Including all teams in all seasons in all tiers will lead to nearly 9000 data points so I highly recommend using the filters 
to make viewing easier.  The chart is best used for identifying outliers in different seasons, but can also be used for single seasons.
I assume 3 pts for a win throughout."),
    
    br(),
    
    helpText(h5("Author: James Curley")),
    
    helpText(p("Please contact",
               a(href ="https://twitter.com/jalapic", "James on twitter",target = "_blank"),
               " or at his",
               a(href ="http://curleylab.psych.columbia.edu/curley.html", "research page", target = "_blank"),
               ", for more information, to suggest improvements or report errors.")),
    
    br(),
    helpText(p("All code and data will be made available at ",
               a(href ="https://github.com/jalapic/shinyapps", "my GitHub page",target = "_blank")
    ))
  ) 
  
    
    
    
    
    ),
  
  column(9,
         ggvisOutput("plot1")
         )
  
  ))

)