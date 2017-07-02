library(shinyjs)
library(shiny)
library(DT)
library("RPPlotUtils")
library("RPToolsDB")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-326'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")
# Output Data Path
outputDataPath = paste(repoPath,'OutputData/', user,'/',JIRACode,'/',sep="")



all_bigdata_group_events <- readRDS(file=paste0(outputDataPath,"bigdata_all_group_events.rds"))



# Use a fluid Bootstrap layout
fluidPage(    
  shinyjs::useShinyjs(),
  # Give the page a title
  titlePanel("Ravenpack Minutes Event profiles"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("ravenpack_type",
                  "RAVENPACK RELEASE",
                  c("RPNA","RDBA")),
      
      selectInput("russell_universe",
                  "RUSSELL UNIVERSE",
                  c("R1000","R2000")),
      
      selectInput("aggregate_criteria",
                  "CATEGORY OR GROUP",
                  c("CATEGORY","GROUP"),
                  "GROUP"),
      sliderInput("event_number_event_filtering", "Minimum number of event observations",
                  50, 500, 50, step = 10),

      selectInput("my_event",
                  "EVENT",
                  as.character(all_bigdata_group_events),
                  "analyst-ratings"),
      
      selectInput("my_plot",
                  "EVENT",
                  c("RETURNS_SIGNIFICANCE","VOL_VOL"),
                  "RETURNS"),
      
      checkboxInput("sort_profiles", "Sort", FALSE),
      
      
      selectInput("sentiment_criteria",
                  "SENTIMENT",
                  c("BEST","POSITIVE","NEGATIVE","SPREAD"),"BEST"),
    
      textOutput("filtering_criteria"),
      hr(),
      selectInput("relevance",
                  "RELEVANCE",
                  c("LOW","MEDIUM","HIGH"),"HIGH"),
      
      selectInput("event_relevance",
                  "EVENT RELEVANCE",
                  c("LOW","MEDIUM","HIGH"),"HIGH"),
      
      selectInput("similarity_gap_filter",
                  "SIMILARITY EVENT DAYS",
                  c("0","1","7","10","90","186","365"),"1"),

      
      selectInput("localSource",
                  "NEWS SOURCE",
                  c("DJPR","PREMIUM_PACK","WEB_NON_PREMIUM"),"DJPR"),

      hr(),
      helpText("Ravenpack News Data")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("eventMinutesPlotHigh"),
      plotOutput("eventMinutesPlotLow")
    )
    
  )
)