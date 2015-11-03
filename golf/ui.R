
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

#### Begin dashboard ----

shinyUI(dashboardPage(skin="black",
                
  dashboardHeader(title = "Golf Analysis"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Introduction", tabName = "Introduction", icon = icon("info-circle")),
      menuItem("Distributions", tabName = "widgets", icon = icon("bar-chart-o")),
      menuItem("Scatter Plots", tabName = "dashboard", icon = icon("certificate")),
      menuItem("Table", tabName = "thirdtab", icon = icon("th")),
      menuItem("Comparisons", tabName = "PlayerComparison", icon = icon("bars")),
      menuItem("Factor Analysis", tabName = "FactorAnalysis", icon = icon("cube"))
      
      )
    
  ),
  
  dashboardBody(
  
    tabItems(

      
      
#### First tab - Scatterplots - content... ----
      tabItem(tabName = "dashboard",      
    
        
              fluidRow(
     box(title = "Scatterplot 1", solidHeader=TRUE,  ggvisOutput("plot1"),uiOutput("plot1_ui"), width=5),
     box(title = "Scatterplot 2", solidHeader=TRUE,  ggvisOutput("plot2"),uiOutput("plot2_ui"), width=5, collapsible = T),
     box(title = "Options", solidHeader=TRUE,  
         
         sliderInput("slider", h5("Dot Opacity:"), 0, 1, .95, sep = ""),
         
         selectInput("yearinput", label = h5("Select Year"), 
                     choices = c("All", as.list(levels(as.factor(pga$year)))), selected = "All"),
         
         selectInput("selectx1", label = h5("Select X1"), 
                     choices = list("Driving Accuracy" = "drivepct", 
                                    "Driving Distance" = "driveavg", 
                                    "Greens in Regulation" = "girpct",
                                    "Goes For Green" = "goforgreenpct", 
                                    "Sand Saves" = "sandsavepct", 
                                    "Scrambling" = "scramblingpct",
                                    "Three Putts" = "threeputtpct", 
                                    "One Putts" = "oneputtpct", 
                                    "Putts Average" = "putts.avg",
                                    "Birdie %" = "birdiepct", 
                                    "Scoring Average" = "score.avg", 
                                    "Birdie-Bogie Ratio" = "birdiebogieratio",
                                    "Par 3 Birdie %" = "par3.birdiepct", 
                                    "Par 4 Birdie %" = "par4.birdiepct", 
                                    "Par 5 Birdie %" = "par5.birdiepct", 
                                    "Top 10s" = "top10s", 
                                    "Firsts" = "firsts",
                                    "Seconds" = "seconds", 
                                    "Thirds" = "thirds", 
                                    "Money %" = "moneypct",
                                    "Total Money" = "totalmoney" 
                                    
                     ), 
                     selected = "driveavg"),
         
         
         
         selectInput("selecty1", label = h5("Select Y1"), 
                     choices = list("Driving Accuracy" = "drivepct", 
                                    "Driving Distance" = "driveavg", 
                                    "Greens in Regulation" = "girpct",
                                    "Goes For Green" = "goforgreenpct", 
                                    "Sand Saves" = "sandsavepct", 
                                    "Scrambling" = "scramblingpct",
                                    "Three Putts" = "threeputtpct", 
                                    "One Putts" = "oneputtpct", 
                                    "Putts Average" = "putts.avg",
                                    "Birdie %" = "birdiepct", 
                                    "Scoring Average" = "score.avg", 
                                    "Birdie-Bogie Ratio" = "birdiebogieratio",
                                    "Par 3 Birdie %" = "par3.birdiepct", 
                                    "Par 4 Birdie %" = "par4.birdiepct", 
                                    "Par 5 Birdie %" = "par5.birdiepct", 
                                    "Top 10s" = "top10s", 
                                    "Firsts" = "firsts",
                                    "Seconds" = "seconds", 
                                    "Thirds" = "thirds", 
                                    "Money %" = "moneypct",
                                    "Total Money" = "totalmoney" 
                                    
                     ), 
                     selected = "score.avg"),
         
      
         
         
         selectInput("selectx2", label = h5("Select X2"), 
                     choices = list("Driving Accuracy" = "drivepct", 
                                    "Driving Distance" = "driveavg", 
                                    "Greens in Regulation" = "girpct",
                                    "Goes For Green" = "goforgreenpct", 
                                    "Sand Saves" = "sandsavepct", 
                                    "Scrambling" = "scramblingpct",
                                    "Three Putts" = "threeputtpct", 
                                    "One Putts" = "oneputtpct", 
                                    "Putts Average" = "putts.avg",
                                    "Birdie %" = "birdiepct", 
                                    "Scoring Average" = "score.avg", 
                                    "Birdie-Bogie Ratio" = "birdiebogieratio",
                                    "Par 3 Birdie %" = "par3.birdiepct", 
                                    "Par 4 Birdie %" = "par4.birdiepct", 
                                    "Par 5 Birdie %" = "par5.birdiepct", 
                                    "Top 10s" = "top10s", 
                                    "Firsts" = "firsts",
                                    "Seconds" = "seconds", 
                                    "Thirds" = "thirds", 
                                    "Money %" = "moneypct",
                                    "Total Money" = "totalmoney" 
                                    
                     ), 
                     selected = "drivepct"),
         
         
            
         selectInput("selecty2", label = h5("Select Y2"), 
                     choices = list("Driving Accuracy" = "drivepct", 
                                    "Driving Distance" = "driveavg", 
                                    "Greens in Regulation" = "girpct",
                                    "Goes For Green" = "goforgreenpct", 
                                    "Sand Saves" = "sandsavepct", 
                                    "Scrambling" = "scramblingpct",
                                    "Three Putts" = "threeputtpct", 
                                    "One Putts" = "oneputtpct", 
                                    "Putts Average" = "putts.avg",
                                    "Birdie %" = "birdiepct", 
                                    "Scoring Average" = "score.avg", 
                                    "Birdie-Bogie Ratio" = "birdiebogieratio",
                                    "Par 3 Birdie %" = "par3.birdiepct", 
                                    "Par 4 Birdie %" = "par4.birdiepct", 
                                    "Par 5 Birdie %" = "par5.birdiepct", 
                                    "Top 10s" = "top10s", 
                                    "Firsts" = "firsts",
                                    "Seconds" = "seconds", 
                                    "Thirds" = "thirds", 
                                    "Money %" = "moneypct",
                                    "Total Money" = "totalmoney" 
                                    
                     ), 
                     selected = "score.avg"),
         
         radioButtons("radioscatter", label = h5("Trendlines"),
                      choices = list("Add" = 1, "Remove" = 2), 
                      selected = 2, inline=T),    
         
         
         width=2)
    
          ),
    
    fluidRow(
      box(title = "Pearson's Correlation:", solidHeader=TRUE,  htmlOutput("textp1"), width=5),
       box(title = "Pearson's Correlation:", solidHeader=TRUE,  htmlOutput("textp2"), width=5, collapsible = T)
)

      ),  #close tab1

#### Second tab - Distributions -  content... ----
#for some reason, buggy if this tab's code is placed above previous tab code...
tabItem(tabName = "widgets",
        
        fluidRow(
          box(title = "Histogram", solidHeader=TRUE,  ggvisOutput("plot1a"),uiOutput("plot1a_ui"), width=5),
          box(title = "Boxplots By Year", solidHeader=TRUE,  ggvisOutput("plot2a"),uiOutput("plot2a_ui"), width=5, collapsible = T, collapsed=T),
          box(title = "Options", solidHeader=TRUE,  
              
              sliderInput("slider0", h5("Bin Width:"), 0.1, 10, 3, sep = ""),
              
              selectInput("yearinput0", label = h5("Select Year"), 
                          choices = c("All", as.list(levels(as.factor(pga$year)))), selected = "All"),
              
              selectInput("selectx0", label = h5("Select Variable"), 
                          choices = list("Driving Accuracy" = "drivepct", 
                                         "Driving Distance" = "driveavg", 
                                         "Greens in Regulation" = "girpct",
                                         "Goes For Green" = "goforgreenpct", 
                                         "Sand Saves" = "sandsavepct", 
                                         "Scrambling" = "scramblingpct",
                                         "Three Putts" = "threeputtpct", 
                                         "One Putts" = "oneputtpct", 
                                         "Putts Average" = "putts.avg",
                                         "Birdie %" = "birdiepct", 
                                         "Scoring Average" = "score.avg", 
                                         "Birdie-Bogie Ratio" = "birdiebogieratio",
                                         "Par 3 Birdie %" = "par3.birdiepct", 
                                         "Par 4 Birdie %" = "par4.birdiepct", 
                                         "Par 5 Birdie %" = "par5.birdiepct", 
                                         "Top 10s" = "top10s", 
                                         "Firsts" = "firsts",
                                         "Seconds" = "seconds", 
                                         "Thirds" = "thirds", 
                                         "Money %" = "moneypct",
                                         "Total Money" = "totalmoney" 
                                         
                          ), 
                          selected = "driveavg"),
              
              radioButtons("radio", label = h5("Sort"), choices = list("Ascending" = 1, "Descending" = 2), selected = 1),
              
              numericInput("num", label = h5("Number"), value = 5),
              
              width=2)
          
        ),
        
        fluidRow(
          box(title = "Descriptive Statistics:", solidHeader=TRUE,  htmlOutput("textp1a"), width=5),

          box(title = "Shortlist:", solidHeader=TRUE,  formattableOutput("formattable1"), width=5, collapsible = T,
              collapsed=T)       
    )
        
        
        
), #close tab2

#### Third tab - Table - content... ----
tabItem(tabName = "thirdtab",

        fluidRow(
          
          box(title = h3("Player Stats"), solidHeader=TRUE,  formattableOutput("formattable2"), width=10, collapsible = T),
    
          box(title = "Options", solidHeader=TRUE,  
              
              selectInput("yearinput00", label = h5("Select Year"), 
                          choices = c("All", as.list(levels(as.factor(pga$year)))), selected = "All"),
              
              selectInput("selectx00", label = h5("Select X0"), 
                          choices = list("Driving Accuracy" = "drivepct", 
                                         "Driving Distance" = "driveavg", 
                                         "Greens in Regulation" = "girpct",
                                         "Goes For Green" = "goforgreenpct", 
                                         "Sand Saves" = "sandsavepct", 
                                         "Scrambling" = "scramblingpct",
                                         "Three Putts" = "threeputtpct", 
                                         "Putts Average" = "putts.avg",
                                         "Scoring Average" = "score.avg", 
                                         "Birdie-Bogie Ratio" = "birdiebogieratio",
                                         "Top 10s" = "top10s", 
                                         "Firsts" = "firsts"
                          ), 
                          selected = "drivepct"),
              
              radioButtons("radio00", label = h5("Sort"), choices = list("Ascending" = 1, "Descending" = 2), selected = 2),
              
              numericInput("num00", label = h5("Number of Players to Show"), value = 100),
              
              width=2)
        ) #close fluid row
  
), #close tab3 

#### Fourth tab: Player Comparison ----
tabItem(tabName = "PlayerComparison",
        
        fluidRow(
          box(title = h3("Player Comparisons"), solidHeader=TRUE,  plotOutput("plot4"), width=10, collapsible = T, collapsed=F, height=1000),
          box(title = "Options", solidHeader=TRUE,  
              
              selectInput("yearinput4", label = h5("Select Year"), 
                          choices = c("All", as.list(levels(as.factor(pga$year)))), selected = "2015"),
              
              selectInput("selectx4", label = h5("Sort By:"), 
                          choices = list("Name" = "name",
                                         "Driving Accuracy" = "drivepct", 
                                         "Driving Distance" = "driveavg", 
                                         "Greens in Regulation" = "girpct",
                                         "Goes For Green" = "goforgreenpct", 
                                         "Sand Saves" = "sandsavepct", 
                                         "Scrambling" = "scramblingpct",
                                         "Three Putts" = "threeputtpct", 
                                         "Putts Average" = "putts.avg",
                                         "Birdie %" = "birdiepct", 
                                         "Scoring Average" = "score.avg", 
                                         "Birdie-Bogie Ratio" = "birdiebogieratio",
                                         "Par 3 Birdie %" = "par3.birdiepct", 
                                         "Par 4 Birdie %" = "par4.birdiepct", 
                                         "Par 5 Birdie %" = "par5.birdiepct"
                          ), 
                          selected = "score.avg"),
          
              radioButtons("radio4", label = h5("Panel Type"),
                           choices = list("Solid" = 1, "None" = 2), 
                           selected = 1, inline=T),              
              
              radioButtons("radio4c", label = h5("Color Type"),
                           choices = list("Color" = 1, "B/W" = 2), 
                           selected = 1, inline=T),              
            
                width=2)
          
        ),  # End of Fluid Row 1.
        
        
        fluidRow(
          box(title = h3("Select Players (6-10)"), solidHeader=TRUE,  width=10, collapsible = T, collapsed=T,

              checkboxGroupInput("checkGroup", label = h5("Checkbox group"), 
                                 as.list(levels(as.factor(pga$name))),
                                 selected = c("Jordan Spieth", "Bubba Watson" , "Ernie Els" , "Matt Kuchar", "Justin Leonard", "Jason Day",
                                              "Justin Rose", "Ian Poulter", "Adam Scott"),
                                 inline = T)
                           
              )
        ) # End of Fluid Row 2.
        
        
), #close tab4 final tab no comma

#### Fifth tab:  Factor Analysis ----

tabItem(tabName = "FactorAnalysis",      
        
        
        fluidRow(
          box(title = "Loadings:", solidHeader=TRUE,  formattableOutput("formattable5"), width=4, collapsible = T, collapsed=F, height=500),   
          box(title = "Individual Scores Plot:", solidHeader=TRUE,  ggvisOutput("plot5a"),uiOutput("plot5a_ui"), width=6, collapsible=T, collapsed=T, height=500),
          box(title = "Options", solidHeader=TRUE,  
              
              
              selectInput("yearinput5", label = h5("Select Year"), 
                          choices = as.list(levels(as.factor(pga$year))), selected = 2015),
              
              sliderInput("slider5", h5("Loadings to Highlight:"), 0.1, 1, .3, sep = ""),
              
              radioButtons("radio5", label = h5("Low Loadings"),
                           choices = list("Remove" = 1, "Keep" = 2), 
                           selected = 1, inline=T), 
              
              radioButtons("lines5", label = h5("Factor Grid"),
                           choices = list("Remove" = 1, "Add" = 2), 
                           selected = 1, inline=T),    
              
              radioButtons("factors5", label = h5("Factors to Plot"),
                           choices = list("2" = 2, "3" = 3), 
                           selected = 2, inline=T),    
              width=2)
          
        ), #end fluidrow 1
        
        fluidRow(
                    
          box(title = "Proportion of Variance Explained:", solidHeader=TRUE,  htmlOutput("textp5b"), width=4, collapsible = T, collapsed=T),
         
          box(title = "Variables to Include:", solidHeader=TRUE,  width=6, collapsible = T, collapsed=T,
              
              checkboxGroupInput("checkGroup5", label=" ",
                                 choices = colnames(pga)[c(1,6,10,13,16,19,36,39,42,22,26)],
                                 selected = colnames(pga)[c(1,6,10,13,16,19,36,39,42,22,26)],
                                 inline = T)
              
          )
          
        ), #end fluidrow2
        
        
        fluidRow(
        
          box(title = "Factor 1 vs Scoring Average", solidHeader=TRUE,  ggvisOutput("plot5x"),uiOutput("plot5x_ui"), width=4,
              collapsed = T, collapsible = T),
          box(title = "Factor 2 vs Scoring Average", solidHeader=TRUE,  ggvisOutput("plot5y"),uiOutput("plot5y_ui"), width=4,
              collapsed = T, collapsible = T),
          box(title = "Factor 3 vs Scoring Average", solidHeader=TRUE,  ggvisOutput("plot5z"),uiOutput("plot5z_ui"), width=4,
              collapsed = T, collapsible = T)
          
        )  #end fluidrow3
          
),  #close tab5 - last tab no comma


#### Introduction Tab ----
tabItem(tabName = "Introduction",  
        
        fluidRow(
        
         box(
          title = " ", solidHeader = T,
          img(src="golf.png", height = 200, width = 150, align="left", position="absolute"),
          h1("Data Exploration with Dashboards"),
          h4("James Curley - Nov 2015"),
          br(),
          p("This dashboard illustrates a few ways that dashbaords help us to interact with and explore our data before diving into fuller analyses. The primary
            objective of this project is to use this dashboard as a teaching tool."),
          p("A few notes on the data.  I am using year end data from the PGA Tour 2004-2015. However, these only included players if they have played a sufficient number of rounds per season (around 50), so players are excluded for that year if they played fewer.  That's why you won't find Rory McIlroy or Tiger Woods in the 2015 dataset."),
       
          br(),
          br(),
          h4(strong("Packages")),
          p("Many packages were used in the making of this dashboard.  In particular,", 
             span("shiny", style = "color:blue"), 
            "and",
            span("shinydashboard", style = "color:blue"),
            "from RStudio make it", 
              em("fairly straightforward"), 
             " to build interactive web applications with R. For many more examples, go to the ",
              a("Shiny homepage.", 
                href = "http://www.rstudio.com/shiny")),
            
          p("Many thanks to Hadley Wickham for creating many, many packages that make data manipulation
             easy.  These include: ", 
            span("purrr, tidyr, dplyr, ", style = "color:blue"), 
            "and",
            span("reshape2", style = "color:blue"),
            ". Even more thanks to Hadley for development of", 
            span("ggplot2", style = "color:blue"), 
            "for beautiful plotting and Hadley (again) and Winston Chang for",
            span("ggvis", style = "color:blue"),
            " which enables interactive visualizations.  Finally many thanks to Ramnath Vaidyanathan, Joe Cheng, JJ Allaire, Yihui Xie, and Kenton Russell for the
             development of ",
          span("htmlwidgets", style = "color:blue"), 
          "which facilitates importing and using javascript libraries in R. This in turn allowed me to use the",
          span("formattable", style = "color:blue"), 
          " package created by Ren Kun to make pretty tables.  Thanks to Ren and Kenton Russell for making  it possible to 
          integrate this package with shiny."),
         
          br(),
          
          h4(strong("Contact")),
          p("Code will be available at my ", 
            span(a("GitHub", href = "http://www.github.com/jalapic"), style = "color:blue"),
            "page.  To get in touch, it's probably easiest to do so via ",
            span(a("twitter", href = "http://www.twitter.com/jalapic"), style = "color:blue"),
            " . Feedback appreciated."), 
           
        width=10)
        )
        
) #close intro tab no comma.

#### Final Closes ----

    ) #close all tabs
    ) #close dashboard body
    ) #close dashboard page
    ) #close shinyUI
    
    

