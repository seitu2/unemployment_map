#UI Code
shinyUI(fluidPage(
  titlePanel("Labor Force Trends"),
#set up variable control panel
  sidebarLayout(
    sidebarPanel(
      #This column contains the user controls 
      uiOutput("slider_year"),
      uiOutput("slider_month"),
      uiOutput("choose_variable")
    ),
  #All output goes in this row.
  mainPanel(
   tabsetPanel(
    tabPanel("Documentation",
             tags$h1("How to Use"),
             tags$p("This application allows you to map Labor Force Statistics from the",
                    a("Current Population Survey",
                    href = "http://www.bls.gov/cps/"), "for all counties
                    or states for a given month and year."),
             br(),
             tags$p("You can select a desired date by moving the",tags$b("Choose Year"),"and",
                           tags$b("Choose Month"),"sliders to the desired date.  A simple warning
                     message will alert you if you have selected a date for which is there is no available data."),
             tags$p("You can select the following variables for mapping using the",
                    tags$b("Variable"),"drop-down menu."),
             tags$ul(
               tags$li("Unemployment Rates"), 
               tags$li("Employed Persons"), 
               tags$li("Unemployed Persons"),
               tags$li("Labor Force")),
             tags$p("In addition, you can map the 12-month change or the 12-month
                    percentage change of each variable."),
             
             tags$p("There are four tabs for viewing data:"),
             tags$ul(
               tags$li(tags$b("County Map:")," Thematic map displaying county-level data."), 
               tags$li(tags$b("State Chart:")," Bar Chart showing values for each state."), 
               tags$li(tags$b("State Map:"), " Thematic Map displaying state-level data."),
               tags$li(tags$b("Best and Worst States:")," Simple table showing States with highest and 
                       lowest values.")),
             tags$p("The maps will also mark the counties in each state, or
                    the states in the state map, with the highest and lowest values."),
             
             tags$p("Code used to create this app is available", a("here.",href ="https://github.com/seitu2/unemployment_map/"))
             
             
    
    ),
    tabPanel("County Map",plotOutput('plot1')),
    tabPanel("State Chart",plotOutput('plot2')),
    tabPanel("State Map",plotOutput('plot3')),
    tabPanel("Best and Worst States",dataTableOutput('table2'))))
  )
))