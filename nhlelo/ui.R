library(ggplot2)
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(XML)
library(PlayerRatings)


shinyUI(dashboardPage(skin="black",
                      
                      
                      dashboardHeader(title = "NHL ELO Ratings"),
                      
                      
                      dashboardSidebar(
                        
                        sidebarMenu(
                          
                          menuItem("ELO Ratings", tabName = "elo", icon = icon("trophy")),
                                                    
                          menuItem("About", tabName = "about", icon = icon("question-circle")),
                          
                          menuItem("Source code", icon = icon("file-code-o"), 
                                   href = "https://github.com/jalapic/shinyapps"),
                          
                          menuItem(
                            list(
                              sliderInput("slider1", h5("kfac:"), 1, 100, 15, sep = ""),
                              sliderInput("slider2", h5("Home bias:"), 0, 1000, 0, sep = "")
                            )
                            )
                        )
                        
                        ),
                      
                      
                      
                      
                      dashboardBody(
                        
                        tags$head(
                          tags$style(type="text/css", "select { max-width: 360px; }"),
                          tags$style(type="text/css", ".span4 { max-width: 360px; }"),
                          tags$style(type="text/css",  ".well { max-width: 360px; }")
                        ),
                        
                        tabItems(
                         
                          
                          tabItem(tabName = "about",
                                 
                                  HTML('<br/>'),
                                  
                                  fluidRow(
                                    box(title = "Author: James Curley", background = "black", width=5, collapsible = TRUE,
                                                                                
                                        helpText(p(strong("This is a quick overview of some of the possibilities and opportunities that shiny dashboard can provide for exploring and analyzing data and teaching data analysis."))),
                                        
                                        helpText(p(strong("I'm mainly using this version to explore some of the code and features behind the shiny dashboard layout - so not everything is as optimal as it could be !"))),
                                        
                                        helpText(p("Please contact",
                                                   a(href ="https://twitter.com/jalapic", "James on twitter",target = "_blank"),
                                                   " or at his",
                                                   a(href ="http://curleylab.psych.columbia.edu/curley.html", "research page", target = "_blank"),
                                                   ", for more information, to suggest improvements or report errors.")),
                                        
                                        helpText(p("All code and data is available at ",
                                                   a(href ="https://github.com/jalapic/shinyapps", "my GitHub page",target = "_blank"),
                                                   "or click the 'source code' link on the sidebar on the left."
                                        ))
                                        
                                        
                                        
                                        
                                        
                                    )
                                  )
                          ),
                          
                          
                          
                          
                        tabItem(tabName = "elo",
                          
                          
                        column(width=3,
                          
                        box(
                            title = "NHL ELO", width = NULL, height=300, background = "navy",
                            "The plot to the right shows the ELO rating over time for the 2014/15 NHL Season.
                            Individual lines can be selected by picking a team from the drop-down menu. 
                            Colors of the lines can be changed by entering color names in the text boxes.
                            The ELO rating relies on a constant 'kfac', which can be adjusted using the first slider.
                            The second slider adjusts the 'gamma' or weighting given to the signifcance of home-field advantage on
                            the ratings. When gamma=0, home-field advantage is ignored."
                          ),
                         
                        box(width = NULL, 
                          selectInput("teams", label = "Select Team:",
                                          choices = as.list(c("Anaheim Ducks",
                                                              "Arizona Coyotes",
                                                              "Boston Bruins",
                                                              "Buffalo Sabres",
                                                              "Calgary Flames"       
                                                              ,"Carolina Hurricanes"   
                                                              ,"Chicago Blackhawks"    
                                                              ,"Colorado Avalanche"    
                                                              ,"Columbus Blue Jackets" 
                                                              ,"Dallas Stars"         
                                                              ,"Detroit Red Wings"     
                                                              ,"Edmonton Oilers"       
                                                              ,"Florida Panthers"      
                                                              ,"Los Angeles Kings"     
                                                              ,"Minnesota Wild"       
                                                              ,"Montreal Canadiens"    
                                                              ,"Nashville Predators"   
                                                              ,"New Jersey Devils"     
                                                              ,"New York Islanders"    
                                                              ,"New York Rangers"     
                                                              ,"Ottawa Senators"       
                                                              ,"Philadelphia Flyers"   
                                                              ,"Pittsburgh Penguins"   
                                                              ,"San Jose Sharks"       
                                                              ,"St. Louis Blues"      
                                                              ,"Tampa Bay Lightning"   
                                                              ,"Toronto Maple Leafs"   
                                                              ,"Vancouver Canucks"     
                                                              ,"Washington Capitals"   
                                                              ,"Winnipeg Jets")
                                          ), selected = "New York Rangers")
                          ),
                        
                        box(title = "Pick color range:", width=NULL, 
                            
                            div(id="XX1A",textInput(inputId="boxa", label="top", value = "red")),
                            tags$head(tags$style(type="text/css", "#XX1A {display: inline-block}")),
                            tags$head(tags$style(type="text/css", "#boxa {max-width: 150px}")),
                            
                            #HTML('<br/>'),
                            
                            div(id="XX1B",textInput(inputId="boxb", label="middle", value = "orange")),
                            tags$head(tags$style(type="text/css", "#XX1B {display: inline-block}")),
                            tags$head(tags$style(type="text/css", "#boxb {max-width: 150px}")),
                            
                            #HTML('<br/>'),
  
                            div(id="XX1C",textInput(inputId="boxc", label="bottom", value = "black")),
                            tags$head(tags$style(type="text/css", "#XX1C {display: inline-block}")),
                            tags$head(tags$style(type="text/css", "#boxc {max-width: 150px}")),
                            
                            HTML('<br/>')                            
                          
                        )
                        ),
  
                        column(width=7,
                               
                               box(title = "Changes in ELO Rating over the 2014-15 NHL Season", width=NULL, plotOutput("plot1", height = 600))    
                        
                        
                        ),
                        
                        column(width=2,
                               
                               box(title = "Current Rating", width=NULL, htmlOutput("html1", height = 550))    
                               
                               
                        )
                        
                          
                          
                          
                        
                      
)
)
)
)
)