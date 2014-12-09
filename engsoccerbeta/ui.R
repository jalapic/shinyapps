library(shiny)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(engsoccerdata)
library(tidyr)


# Define UI
shinyUI(pageWithSidebar(

  # Application title
  headerPanel("Exploring Historical English Soccer Data"),

  sidebarPanel(
    conditionalPanel(
      'input.dataset === "Season Scatterplots"',

    sliderInput("year", "Season", 1888, 2014, value = c(1888, 2014), format="####"),

    br(),


    checkboxGroupInput("type", "Select Tiers to Display:",
                       levels(factor(engsoccerdata2$tier)),selected=levels(factor(engsoccerdata2$tier))),


  br(),

  selectInput("Outcomey", label = "Y Axis:",
              list("Total goals per game" = "totgoal",
                   "Home goals per game" = "hgoal",
                   "Visitor goals per game" = "vgoal",
                   "Goal difference per game" = "goaldif",
                   "Home win percentage" = "hwinpct",
                   "Away win percentage" = "vwinpct",
                   "Draw percentage" = "drawpct"),selected="totgoal"
              ),


  selectInput("Outcomex", label = "X Axis:",
              list("Total goals per game" = "totgoal",
                   "Home goals per game" = "hgoal",
                   "Visitor goals per game" = "vgoal",
                   "Goal difference per game" = "goaldif",
                   "Home win percentage" = "hwinpct",
                   "Away win percentage" = "vwinpct",
                   "Draw percentage" = "drawpct",
                   "Year"= "Year" ),selected="Year"
  ),

  br(),


  checkboxInput("show.trend", "Show trendlines", FALSE),

  br(),


  helpText("Explore season by season trends across all tiers on this page"),

  br(),

  helpText(h5("Authors: James Curley & Song Qi")),

  helpText(p("Please contact",
              a(href ="https://twitter.com/jalapic", "James on twitter",target = "_blank"),
             " or at his",
              a(href ="http://curleylab.psych.columbia.edu/curley.html", "research page", target = "_blank"),
             ", or ",
              a(href ="http://www.songqiuestc.com/", "Song at his research page",target = "_blank"),
             ", for more information, to suggest improvements or report errors.")),



  br()
    ),



  conditionalPanel(
    'input.dataset === "Stats tables"',

    sliderInput("yeartables", "Season", 1888, 2014, value = c(1888, 2014), format="####"),

    br(),

    radioButtons("type1", "Include:",
                 list("Home & Away" = "both",
                      "Home only" = "home",
                      "Away only" = "away"
                 ), inline = TRUE),

    selectInput("team1tables", label = "Team",
                choices = c("All teams", as.list(levels(as.factor(engsoccerdata2$home)))), selected = "All"),



    selectInput("team2tables", label = "Opponent",
                choices = c("All teams", as.list(levels(as.factor(engsoccerdata2$home)))), selected = "All"),


    selectInput("dataset1", "Choose record:",
                choices = c("Overall Record",
                            "Season Record",
                            "Biggest Wins",
                            "Worst Losses",
                            "Highest scoring games",
                            "Highest scoring draws"
                )),

    radioButtons("typetier", "",
                 list("All Tiers" = "all.tiers",
                      "Tier 1" = "tier1",
                      "Tier 2" = "tier2",
                      "Tier 3" = "tier3",
                      "Tier 4" = "tier4"
                 ), inline = TRUE),


    conditionalPanel(
      condition="input.dataset1 == 'Overall Record' | input.dataset1 == 'Season Record'",

      selectInput("sortrecord", "Sort records by:",
                  choices = c("Games Played",
                              "Wins",
                              "Draws",
                              "Losses",
                              "Goals For",
                              "Goals Against",
                              "Goal Difference",
                              "Points",
                              "Win Percentage",
                              "PPG",
                              "GFPG",
                              "GAPG")),

      radioButtons("typead", "",
                   list("Descending" = "desc",
                        "Ascending" = "asc"
                   ), inline = TRUE)


    ),



    br(),

    checkboxInput("checkbox", label = "Remove 1939/40?", value = FALSE),

    br(),

    numericInput("obs", "Number of observations to view:", 20),

    helpText("Explore all-time records in tabular form on this page"),

    br(),

    helpText(h5("Authors: James Curley & Song Qi")),

    helpText(p("Please contact",
               a(href ="https://twitter.com/jalapic", "James on twitter",target = "_blank"),
               " or at his",
               a(href ="http://curleylab.psych.columbia.edu/curley.html", "research page", target = "_blank"),
               ", or ",
               a(href ="http://www.songqiuestc.com/", "Song at his research page",target = "_blank"),
               ", for more information, to suggest improvements or report errors.")),

    br()
  ),






  conditionalPanel(
    'input.dataset === "Head to Head"',

    sliderInput("yeartvt", "Season", 1888, 2014, value = c(1888, 2014), format="####"),

    br(),

    radioButtons("typetvt", "Include:",
                 list("Home & Away" = "both",
                      "Home only" = "home",
                      "Away only" = "away"
                 ), inline = TRUE),

    selectInput("team1tvt", label = "Team",
                choices = as.list(levels(as.factor(engsoccerdata2$home))), selected = "Aston Villa"),



    selectInput("team2tvt", label = "Opponent",
                choices = c("All teams", as.list(levels(as.factor(engsoccerdata2$home)))), selected = "Everton"),



    selectInput("Outcometvt", label = "Outcome:",
                list("Cumulative results" = "cresults",
                     "Cumulative points" = "cpoints",
                     "Cumulative goal difference" = "cgoald",
                     "Head to Head Results Chart" = "hth")),


    checkboxInput("checkbox1", label = "Remove 1939/40?", value = FALSE),


    conditionalPanel(
      condition="input.Outcometvt == 'cresults' | input.Outcometvt == 'cgoald' | input.Outcometvt == 'cpoints'",

      checkboxInput("checkbox2", label = "Add tier shadows?", value = FALSE)

    ),


    conditionalPanel(
      condition="input.Outcometvt == 'cgoald' | input.Outcometvt == 'cpoints'",

      br(),
      br(),

      radioButtons("colortype1tvt", "Team  color:",
                   list("red" = "red",
                        "dark blue" = "darkblue",
                        "light blue" = "cornflowerblue",
                        "green" = "darkgreen",
                        "orange" = "orange",
                        "yellow" = "yellow",
                        "purple" = "purple",
                        "claret" = "maroon",
                        "black" = "black"
                   ), inline = TRUE),


      radioButtons("colortype2tvt", "Opponent color:",
                   list("red" = "red",
                        "dark blue" = "darkblue",
                        "light blue" = "cornflowerblue",
                        "green" = "darkgreen",
                        "orange" = "orange",
                        "yellow" = "yellow",
                        "purple" = "purple",
                        "claret" = "maroon",
                        "black" = "black"
                   ), inline = TRUE, selected = "black")
    ),



  br(),


    helpText("Explore head to head records between teams on this page. Note - this is a beta version and some graphical hiccups are occurring - particularly with the head-to-head results chart. We will try to fix these in time."),

    br(),

    helpText(h5("Authors: James Curley & Song Qi")),

    helpText(p("Please contact",
               a(href ="https://twitter.com/jalapic", "James on twitter",target = "_blank"),
               " or at his",
               a(href ="http://curleylab.psych.columbia.edu/curley.html", "research page", target = "_blank"),
               ", or ",
               a(href ="http://www.songqiuestc.com/", "Song at his research page",target = "_blank"),
               ", for more information, to suggest improvements or report errors.")),

    br()
  ),






  conditionalPanel(
    'input.dataset === "Season Gantt Charts"',

    sliderInput("yeargantt", "Season", 1888, 2014, value = c(1888, 2014), format="####"),

    br(),

    selectInput("team1", label = "Team 1:",
                choices = as.list(levels(as.factor(engsoccerdata2$home))), selected = "Everton"),

    selectInput("team2", label = "Team 2:",
                choices = as.list(levels(as.factor(engsoccerdata2$home))), selected = "Barnsley"),

    selectInput("team3", label = "Team 3:",
                choices = as.list(levels(as.factor(engsoccerdata2$home))), selected = "Walsall"),

    selectInput("team4", label = "Team 4:",
                choices = as.list(levels(as.factor(engsoccerdata2$home))), selected = "Rochdale"),


    br(),

    radioButtons("colortype", "Color Scheme:",
                 list("Classic" = "redwhite",
                      "Blues" = "bluewhite",
                      "Greens" = "greens",
                      "Ugly rainbow" = "rainbow"
                 )),


    br(),


    helpText("Explore sequences in and transitions between tiers with Gantt plots"),

    br(),

    helpText(h5("Authors: James Curley & Song Qi")),

    helpText(p("Please contact",
               a(href ="https://twitter.com/jalapic", "James on twitter",target = "_blank"),
               " or at his",
               a(href ="http://curleylab.psych.columbia.edu/curley.html", "research page", target = "_blank"),
               ", or ",
               a(href ="http://www.songqiuestc.com/", "Song at his research page",target = "_blank"),
               ", for more information, to suggest improvements or report errors.")),



    br()

    )

),










  # Show the main display
  mainPanel(
    tabsetPanel(
      id='dataset',
   tabPanel('Season Scatterplots', plotOutput("plot1")),
   tabPanel('Stats tables', tableOutput("view")),
   tabPanel('Head to Head', plotOutput("plot1tvt")),
   tabPanel('Season Gantt Charts', plotOutput("plot2"))
    )
  )
  ))
