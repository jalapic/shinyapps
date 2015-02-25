
shinyUI(dashboardPage(

  
  dashboardHeader(title = "Comparing Seasons"),
  dashboardSidebar(),
  dashboardBody(

    fluidRow(
      box(title = "Season trends", status = "primary",  plotOutput("plot1", height = 600)),
      
      box(
        title = "Inputs", status = "primary",  collapsible = TRUE,
        sliderInput("slider", "Seasons to include:", 1888, 2014, c(1888,2014), sep = ""),
        
        selectInput("select", label = h4("Select Team"), 
                    choices = list("Arsenal" = "Arsenal", 
                                   "Aston Villa" = "Aston Villa",
                                   "Burnley"="Burnley", 
                                   "Chelsea"="Chelsea", 
                                   "Crystal Palace" = "Crystal Palace", 
                                   "Everton" = "Everton", 
                                   "Hull City"="Hull City", 
                                   "Leicester City"="Leicester City",
                                   "Liverpool"="Liverpool", 
                                   "Manchester City"="Manchester City", 
                                   "Manchester United"="Manchester United", 
                                   "Newcastle United"="Newcastle United", 
                                   "Queens Park Rangers"="Queens Park Rangers",
                                   "Southampton"="Southampton", 
                                   "Stoke City"="Stoke City", 
                                   "Sunderland"="Sunderland", 
                                   "Swansea City"="Swansea City", 
                                   "Tottenham Hotspur"="Tottenham Hotspur", 
                                   "West Bromwich Albion"="West Bromwich Albion",
                                   "West Ham United"="West Ham United"
                    ), 
                    selected = "Liverpool"),
        
        textInput("text1", label = h4("Enter first Season to compare e.g. 2013 for 2013/14 "), value = "2013"),
        textInput("text2", label = h4("Enter next Season to compare"), value = "2014"),
        
        
        
        selectInput("select1", label = h4("Select Outcome Variable"), 
                    choices = list("Cumulative points" = "Cumpts", 
                                   "Cumulative goals scored" = "CumGF",
                                   "Cumulative goals against" = "CumGA",
                                   "Cumulative goal difference" = "CumGD"),
                    selected = "Cumpts"),
        
        
        
        radioButtons("radio", label = h4("Select color scheme"),
                     choices = list("Reds" = 1, "Blues" = 2, "Grays" = 3), 
                     selected = 1, inline=T)
        
      )
    ),
    
    fluidRow(
      box(title = "More Information", background = "black", width=12, collapsible = TRUE,

        helpText(h3("Author: James Curley")),
        
        helpText(p("Please be patient when launching the application - This is a large dataset and may take a while to load!")),
                   
                   
                   helpText(p("Please contact",
                   a(href ="https://twitter.com/jalapic", "James on twitter",target = "_blank"),
                   " or at his",
                   a(href ="http://curleylab.psych.columbia.edu/curley.html", "research page", target = "_blank"),
                   ", for more information, to suggest improvements or report errors.")),
        
        helpText(p("All code and data will be made available at ",
                   a(href ="https://github.com/jalapic/shinyapps", "my GitHub page",target = "_blank")
        ))
        
        
        
        
        
        )
    )
      
    
  )
)
)