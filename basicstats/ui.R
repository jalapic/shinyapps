library(grid)
library(reshape2)
library(ggplot2)
library(compete)
library(shiny)
library(shinydashboard)
library(dplyr)



shinyUI(dashboardPage(skin="black",
                      
                      
                      dashboardHeader(title = "Exploring Data"),
                      
                      
                      dashboardSidebar(
                        
                        sidebarMenu(
                          
                          menuItem("Student T-tests", tabName = "ttest", icon = icon("star-o")),
                          
                          menuItem("One-Way ANOVA", tabName = "anova", icon = icon("star")),
                          
                          menuItem("About", tabName = "about", icon = icon("question-circle")),
                          
                          menuItem("Source code", icon = icon("file-code-o"), 
                                   href = "https://github.com/jalapic",
                                   badgeLabel = "new", badgeColor = "orange"),
                          
                          menuItem(
                            list(
                              textInput("text", label = h5("Set seed number (type any integer):"), value = "101"),
                              textInput("text1", label = h5("Number of permutations:"), value = "1000")
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
                                  h2("About this App"),
                                  
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
                          
                          
                          
                          
                        tabItem(tabName = "ttest",
                          
                          
                        fluidRow(
                          
                          box(
                            title = "Exploring Student T-tests", width = 3, height=300, background = "navy",
                            "Change the sample size, means and standard deviations of two groups (A & B) on the right hand size.
                            The boxplot shows the distribution of the two groups and the T-test results window lists the outcome of an independent Student's T-test.
                            On the bottom row, the middle panel shows the results of performing a randomized t-test.
                            The first panel on the bottom row represents rolling p-values if adding one observation at a time to equal samples from each group.
                            The seed can be fixed using the left-hand sidebar (e.g. compare 101 to 10!)."
                          ),
                          
                          box(title = "Boxplots", width=3,  status = "primary",  plotOutput("plot2", height = 250)),
                          
                          box(title = "T-test results:",  width=3, status = "primary",  htmlOutput("text3", height=250)),
                          
                          box(
                          title = "Group A Filters", status = "warning", collapsible=F,
                          sliderInput("slider1", h5("Sample size - group A:"), 0, 100, 10, sep = ""),
                          textInput("textxA", label = h5("Mean:"), value = "12"),
                          textInput("textsdA", label = h5("Standard Deviation:"), value = "4.2"),
                          hr(),
                          width=3
                          
                        ),
                        
                        fluidRow(
                          
                          box(title = "Rolling p-values when successively adding observations:", width=5, status = "primary",  plotOutput("plotX", height = 250)),
                                                    
                          box(title = "Randomized T-tests:", width=4, status = "primary",  plotOutput("plotY", height = 250)),
                          
                          box(
                            title = "Group B Filters", status = "warning", collapsible=F,
                            sliderInput("slider2", h5("Sample size - group B:"), 0, 100, 10, sep = ""),
                            textInput("textxB", label = h5("Mean:"), value = "11"),
                            textInput("textsdB", label = h5("Standard Deviation:"), value = "6.7"),
                            hr(),
                            width=3
                            
                          )
                          
                        
                          
                        )
                        
                        
                        )
                        ),
                        

                  tabItem(tabName = "anova",
                                
                        
                      fluidRow(
                        
                        box(
                          title = "Exploring One-Way ANOVAs", width = 3, background = "olive", height=300,
                          "In this tab, enter the sample size, mean and standard deviation of each of the three groups on the right.
                           The data are ploted in the boxplots panel and the results of a One-Way Anova (applying Welch's correction) are in the results panel.
                          
                           Results for pairwise-comparisons are shown on the bottom row. 
                           Plots are differences based on Tukey multiple comparisons of means 95% family-wise confidence level.
                           Also shown are multiple pairwise comparisons of p' values adjusted using Holm's method."
                        ),
                        
                        
                        box(title = "Boxplots", status = "success",  plotOutput("plotA", height=250), width=4
                        ),
                        
                        
                        box(title = "One-Way ANOVA results:", status = "success",  htmlOutput("text4"), width=3
                        ),
                      
               
              box(title = "Group Metrics", status = "success", width=2, 
                
                div(id="XX1A",textInput(inputId="an", label="A - n", value = 12)),
                tags$head(tags$style(type="text/css", "#XX1A {display: inline-block}")),
                tags$head(tags$style(type="text/css", "#an {max-width: 50px}")),
                
                div(id="XX1B",textInput(inputId="amean", label=" mean", value = 11.3)),
                tags$head(tags$style(type="text/css", "#XX1B {display: inline-block}")),
                          tags$head(tags$style(type="text/css", "#amean {max-width: 50px}")),
                
                div(id="XX1C",textInput(inputId="asd", label="  sd", value = 4.5)),
                tags$head(tags$style(type="text/css", "#XX1C {display: inline-block}")),
                tags$head(tags$style(type="text/css", "#asd {max-width: 50px}")),
                
                HTML('<br/>', '<br/>'),
                
                div(id="XX2A",textInput(inputId="bn", label="B - n", value = 14)),
                tags$head(tags$style(type="text/css", "#XX2A {display: inline-block}")),
                tags$head(tags$style(type="text/css", "#bn {max-width: 50px}")),
                
                div(id="XX2B",textInput(inputId="bmean", label=" mean", value = 10.5)),
                tags$head(tags$style(type="text/css", "#XX2B {display: inline-block}")),
                tags$head(tags$style(type="text/css", "#bmean {max-width: 50px}")),
                
                div(id="XX2C",textInput(inputId="bsd", label="  sd", value = 3.8)),
                tags$head(tags$style(type="text/css", "#XX2C {display: inline-block}")),
                tags$head(tags$style(type="text/css", "#bsd {max-width: 50px}")),
                
                HTML('<br/>', '<br/>'),
                
                div(id="XX3A",textInput(inputId="cn", label="C - n", value = 10)),
                tags$head(tags$style(type="text/css", "#XX3A {display: inline-block}")),
                tags$head(tags$style(type="text/css", "#cn {max-width: 50px}")),
                
                div(id="XX3B",textInput(inputId="cmean", label=" mean", value = 13.3)),
                tags$head(tags$style(type="text/css", "#XX3B {display: inline-block}")),
                tags$head(tags$style(type="text/css", "#cmean {max-width: 50px}")),
                
                div(id="XX3C",textInput(inputId="csd", label="  sd", value = 7.1)),
                tags$head(tags$style(type="text/css", "#XX3C {display: inline-block}")),
                tags$head(tags$style(type="text/css", "#csd {max-width: 50px}"))
                
              )
              ),
              
              
              fluidRow(
                
                box(title = "Tukey Comparison Plots", status = "success",  plotOutput("plotB", height=250), width=5
                ),
                
                box(title = "Pairwise Comparisons Results:", status = "success",  htmlOutput("text5"), width=3
                )
                
                
              )
              
              
                        )    
                        
                        
                        
                        
                      )
)
)
)
