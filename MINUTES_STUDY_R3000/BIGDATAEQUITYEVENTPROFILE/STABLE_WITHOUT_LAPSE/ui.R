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
shinyUI(fluidPage(    
  shinyjs::useShinyjs(),
  tags$head(tags$script("
        Shiny.addCustomMessageHandler('activeNavs', function(nav_label) {
            $('#mynavlist a:contains(\"' + nav_label + '\")').parent().removeClass('disabled');
        });
   ")),
  # Give the page a title
  titlePanel("Minutes Event profiles explorer"),
  
  
  
  # Generate a row with a sidebar
  fluidRow(      
    # Define the sidebar with one input
    column(2,
           h4("Plot 1 Selection"),
           selectInput("ravenpack_type",
                       "RAVENPACK RELEASE",
                       c("RPNA","RBDA"),
                       "RBDA"),
           
           #       selectInput("russell_universe",
           #                   "RUSSELL UNIVERSE",
           #                   c("R1000","R2000")),
           
           selectInput("aggregate_criteria",
                       "CATEGORY OR GROUP",
                       c("CATEGORY","GROUP"),
                       "GROUP"),
           sliderInput("event_number_event_filtering", "Minimum number of event observations",
                       50, 500, 50, step = 10),
           
           selectInput("my_event",
                       "EVENT",
                       as.character(all_bigdata_group_events)),
           
           selectInput("my_plot",
                       "PLOT TYPE",
                       c("RETURNS_SIGNIFICANCE","VOL_VOL"),
                       "RETURNS"),
           
           # checkboxInput("sort_profiles", "Sort", FALSE),
           
           selectInput("sentiment_criteria",
                       "SENTIMENT",
                       c("POSITIVE","NEGATIVE","SPREAD"),"POSITIVE"),
           
           selectInput("relevance",
                       "RELEVANCE",
                       c("LOW","MEDIUM","HIGH"),"HIGH"),
           
           selectInput("event_relevance",
                       "EVENT RELEVANCE",
                       c("LOW","MEDIUM","HIGH"),"HIGH"),
           
           selectInput("similarity_gap_filter",
                       "SIMILARITY EVENT DAYS",
                       c("0","1","7","10","90","186","365"),"0"),
           
           
           selectInput("localSource",
                       "NEWS SOURCE",
                       c("DJPR","PREMIUM_PACK","WEB_NON_PREMIUM"),"DJPR"),
           textOutput("filtering_criteria")
           
    ),
    #     column(8,
    #            plotOutput("eventMinutesPlotHigh"),
    #            plotOutput("eventMinutesPlotLow"),
    #            actionButton("plot_best_profiles", "Compute best profiles pictures for your selection"),
    #            checkboxInput("fix_group", "Fix group or parameters", FALSE),
    #            textOutput("best_plot_path")
    #            ),
    column(8,

                        
           tabsetPanel(id = "mynavlist",
                       type = "tabs", 
                       tabPanel(title ="Comparative Plots",
                                # value = "tab1",
                                plotOutput("eventMinutesPlotHigh"),
                                plotOutput("eventMinutesPlotLow")), 
                       tabPanel(title="Event best Profiles Summary",
                                # value="tab2",
                                fluidRow(
                                  column(3, selectInput("inside_periods",
                                                        "Classification periods",
                                                        c("PRE","POST","PREPOST"),"POST")),
                                  column(3, selectInput("inside_methodo",
                                                        "Classification methodology",
                                                        c("RETURN","VOLATILITY"),"RETURN")),
                                  column(3, selectInput("inside_weighting",
                                                        "weighting",
                                                        c("NONE","RANK","RANK+CARD"),"RANK+CARD"))
                                ),
                                DT::dataTableOutput("best_profiles_table")),
                       tabPanel(title = "Event Best Profile Plot",
                                # value="tab3",
                                plotOutput("bestProfilePlotRet"),
                                plotOutput("bestProfilePlotStat"),
                                plotOutput("bestProfilePlotVola"),
                                plotOutput("bestProfilePlotVolu")),
                       tabPanel(title="Global Best Profiles Summary",
                                # value="tab2",
                                fluidRow(
                                  column(3, selectInput("periods",
                                                        "Classification periods",
                                                        c("PRE","POST","PREPOST"),"POST")),
                                  column(3, selectInput("methodo",
                                                         "Classification methodology",
                                                         c("RETURN","VOLATILITY"),"RETURN")),
                                  column(3, selectInput("weighting",
                                                        "weighting",
                                                        c("NONE","RANK","RANK+CARD"),"RANK+CARD"))
                                ),
                                DT::dataTableOutput("global_best_profiles_table")),
                       tabPanel(title = "Global Best Profile Plot",
                                # value="tab3",
                                plotOutput("globalBestProfilePlotRet"),
                                plotOutput("globalBestProfilePlotStat"),
                                plotOutput("globalBestProfilePlotVola"),
                                plotOutput("globalBestProfilePlotVolu"))
           )
    ),
    
    
    column(2,
           h4("Plot 2 selection"),
           selectInput("sec_ravenpack_type",
                       "RAVENPACK RELEASE",
                       c("RPNA","RBDA"),
                       "RPNA"),
           

           selectInput("sec_aggregate_criteria",
                       "CATEGORY OR GROUP",
                       c("CATEGORY","GROUP"),
                       "GROUP"),
           sliderInput("sec_event_number_event_filtering", "Minimum number of event observations",
                       50, 500, 50, step = 10),
           
           selectInput("sec_my_event",
                       "EVENT",
                       as.character(all_bigdata_group_events)),
           
           
           selectInput("sec_sentiment_criteria",
                       "SENTIMENT",
                       c("POSITIVE","NEGATIVE","SPREAD"),"POSITIVE"),
           
           selectInput("sec_relevance",
                       "RELEVANCE",
                       c("LOW","MEDIUM","HIGH"),"HIGH"),
           
           selectInput("sec_event_relevance",
                       "EVENT RELEVANCE",
                       c("LOW","MEDIUM","HIGH"),"HIGH"),
           
           selectInput("sec_similarity_gap_filter",
                       "SIMILARITY EVENT DAYS",
                       c("0","1","7","10","90","186","365"),"0"),
           
           
           selectInput("sec_localSource",
                       "NEWS SOURCE",
                       c("DJPR","PREMIUM_PACK","WEB_NON_PREMIUM"),"DJPR"),
           
           textOutput("sec_filtering_criteria")
    )
  )
)
)