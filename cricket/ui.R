library(shiny)
library(dplyr)
library(ggvis)


# Define UI
shinyUI(pageWithSidebar(
  
  
  # Application title
  headerPanel("Test match batting"),
  
  sidebarPanel(
    
    sliderInput("year", "Choose date range:", 1877, 2014, value = c(1877, 2014), format="####"),
    sliderInput("inngs", "Minimum number of innings:", 1, 350, value = c(1, 350), format="####"),
        
    br(),
    
    selectInput("hteam", 
                label = "Team",
                choices = c("All Teams", "Australia", "Bangladesh",
                            "England", "India", "New Zealand", "Pakistan", 
                            "South Africa", "Sri Lanka", "West Indies",
                            "Zimbabwe",  "ICC World XI"), selected = "All teams"),
    
    selectInput("vteam", 
              label = "Opponent",
              choices = c("All Teams", "Australia", "Bangladesh",
                          "England", "India", "New Zealand", "Pakistan", 
                          "South Africa", "Sri Lanka", "West Indies",
                          "Zimbabwe",  "ICC World XI"), selected = "All teams"),

    radioButtons("location", "Location:",
                 list(
                   "All" = "all",
                   "Home only" = "home",
                    "Overseas only" = "away"
                 ), inline = TRUE),
    

    br(),
    

    
wellPanel(
  
    selectInput("xvar", label = "X Axis:",
                list("Total inninngs" = "totalinns",
                     "Total runs" = "totalruns",
                     "Average" = "avg",
                     "Total no" = "totalno",
                     "Total 50s" = "total50s",
                     "Total 100s" = "total100s",
                     "Debut year" = "debutyear"),selected="totinns"),
  
    selectInput("yvar", label = "Y Axis:",
                list("Total inninngs" = "totalinns",
                     "Total runs" = "totalruns",
                     "Average" = "avg",
                     "Total no" = "totalno",
                     "Total 50s" = "total50s",
                     "Total 100s" = "total100s",
                     "Debut year" = "debutyear"),selected="avg"),
    
  
  sliderInput("size", "Change dot size:", 10, 100, step = 5, value=65)
), 


  

  br(),
  
  helpText("Explore individual test batting statistics on this page.  You can vary the x- and y-axes using the drop down menus.
           Filter data by using other drop down menus and sliders above.
           You can also change the size of the dots as well as hover over individual data points to make them clearer.  On most platforms you should be able to 
           adjust the plot area by dragging from the bottom right corner."),
  
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
             )),
  
  
  
  br()
  
  
  
  
  
  ),
  
  
mainPanel(
  ggvisOutput("plot1")
)

))