## UI
# setwd("/Users/Ollie/Documents/Uni/Auckland University/2015/Summer Scholarship/RShiny")

## Require package shiny
library(shiny)

## Require package shinysky
require(shinysky)

###### Have added cheese to bait 2 and egg bait 3 first observation of Tauhoa; remove later as is fake!!!

########## START UI ##########

shinyUI(fluidPage(

  ## Sidebar at top
  fluidRow(
    column(3,

           ## Select Area
           selectInput("selectArea",
                       label = h4("Select School"),
                       choices = c("Combined", "Tomarata", "Tauhoa", "Kaipara Flats"),
                       selected = "Combined"
           ),
           br(),
           br(),
           ## Submit button
           actionButton("submit", label = "Draw Graph", styleclass = "success")
           #uiOutput("submit")
    ),

    column(3,
           #h4("Specific plots:"),

           ## Select to view by person or line ("by")
           radioButtons("selectBy",
                        label = h4("Display a plot for each:"),
                        choices = c("Species", "Bait", "Placement",  "Person", "Trap Type"), ##"No Selection"),
                        selected  = "Species"
           )
    ),

    column(3,
           ## Select Subject ("subject")
           radioButtons("selectSubject",
                        label = h4("Colour Catches By:"),
                        choices = c("Bait", "Placement", "Species", "Person", "Trap Type"),
                        selected="Bait"
           )
    ),


    column(3,
           uiOutput("selectYear")
    )
  ),

  hr(),
  hr(),

  ## Main Panel
  fluidRow(
    column(4,
           uiOutput("breakdowntitle"),
           uiOutput("by"),
           uiOutput("subby"),
           uiOutput("colourtitle"),
           uiOutput("subject")
           #conditionalPanel(
            # condition = "input.subby.length = 0",
             #actionButton("download", "Download Graphics")
           #)
    ),
    column(8,
           plotOutput("overall"),
           busyIndicator("Please Wait", wait = 0),
           uiOutput("stackedplot")
           #uiOutput("blah")
    )
    #column(6,
    #  ## Expand by input$selectBy checkbox
    #  uiOutput("expand")
    #),
    #column(6,
    #  uiOutput("breakdowntitle"),
    #  uiOutput("by"),
    #  uiOutput("subby")
    #),
    #column(3,
    #  uiOutput("submit")
    #)
  ),


  fluidRow(
    #column(5.2,
    #uiOutput("breakdowntitle"),
    #uiOutput("by")
    #),
    #column(5.2,
    #  br(),
    #  br(),
    #  br(),
    #  uiOutput("subby")
    #)

  )

  ## Submit button
  #uiOutput("submit")
  #br(),
  #uiOutput("wait")
  #uiOutput("download")
  #downloadButton('downloadData',
  #  "Download Graphics")


  #imageOutput("plot1.png"),
  #fluidRow(
  #  column(10,
  #    plotOutput("overall")
  #  )
  #),

  #fluidRow(
  #  column(10,
  #    uiOutput("stackedplot")
  #  )
  #)

  #imageOutput("gif")



))
