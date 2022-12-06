library(shiny)
# log scale
JScode <-
  "$(function() {
    setTimeout(function(){
      var vals = [0];
      var powStart = 1;
      var powStop = 7;
      for (i = powStart; i <= powStop; i++) {
        var val = Math.pow(10, i);
        val = parseFloat(val.toFixed(8));
        vals.push(val);
      }
      $('#range').data('ionRangeSlider').update({'values':vals})
    }, 5)})"
library(ggplot2)
shinyUI(fluidPage(
  headerPanel("Seasonality of Infectious diseases in England and Wales"),
  tags$head(tags$style("#summary{
                      position: relative;
                      display: inline-block;
                       width: 20%;
                       height: 10%;
                       top: 10px;
                       padding: 10% 0;
                       border-radius:50%;
                       line-height:0;
                       /* further display options */
                       @shadow: rgba(0, 0, 0, .1);
                       @shadow-length: 4px;
                       -webkit-box-shadow: 0 @shadow-length 0 0 @shadow;
                       box-shadow: 0 @shadow-length 0 0 @shadow;
                       text-shadow: 0 @shadow-length 0 @shadow;
                       background: #428bca;
                       color: white;
                       font-family: Helvetica, Arial Black, sans;
                       font-size: 24px;
                       text-align: center;
                       }"
  )),
  sidebarPanel( 
    strong("Periodicity of seasonality"),
    helpText("Based on TBATS (Exponential smoothing state space model with Box-Cox
transformation, ARMA errors, Trend and Seasonal components) model applied to time series, as described in De Livera, Hyndman & Snyder (2011)."),
    checkboxInput("checkbox", label = "Weekly", value = FALSE),
    checkboxInput("checkbox2", label = "Monthly", value = FALSE),
    checkboxInput("checkbox3", label = "Quarterly", value = FALSE),
    checkboxInput("checkbox4", label = "None detected", value = FALSE),
    tags$head(tags$script(HTML(JScode))),
    sliderInput("range", 
                            label = "Total number of cases",
                            min = 0, max = 1e+7, value = c(0,1e+7)),
                uiOutput("Box1"),
                uiOutput("Box2"), 
    helpText("Application built in Rstudio (0.98.507) using infectious disease data from the LabBase national surveillance database (1989-2015) from Public Health England and root organism descriptions from Wikipedia.")
                ),
  mainPanel(
    tabsetPanel(type = "tabs", 
                tabPanel("Description", textOutput('description')),
                tabPanel("Total cases", textOutput("summary")),
                tabPanel("Time series", plotOutput('timeseries')),
                tabPanel("Month Plot", plotOutput("plot")), 
                tabPanel("Decomposition", plotOutput('decompose')),
                tabPanel("TBATS model", plotOutput('forecast'))
    ))
))


