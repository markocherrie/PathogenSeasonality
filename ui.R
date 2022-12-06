library(shiny)
library(dygraphs)
# log scale code adapted form http://stackoverflow.com/questions/30502870/shiny-slider-on-logarithmic-scale
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
  
  # for the header font
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Roboto+Slab');
                    "))
  ),
  
  headerPanel(
    fluidRow(
      column(11, h1("Pathogen seasonality and links with weather in England and Wales", 
                    style = "font-family: 'Roboto Slab', cursive;
                    font-weight: bold; font-size: 39px")),
      column(1, tags$a(href="https://www.data-mashup.org.uk",img(height = 79.5225, width = 78.384, src = "medmi.png"),
                       tags$a(href="https://cresh.org.uk/",img(height = 79.5225, width = 78.384, src = "cresh_twitter_md.png"))))
      ), windowTitle = "Pathogen seasonality and links with weather in England and Wales"),
  
  
  # For the Total Cases font  
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
    strong("Seasonality"),
    helpText("Based on TBATS (Exponential smoothing state space model with Box-Cox
transformation, ARMA errors, Trend and Seasonal components) model applied to time series."),
    helpText(   a("De Livera, Hyndman & Snyder (2011)",  target="_blank",  href="http://www.tandfonline.com/doi/abs/10.1198/jasa.2011.tm09771")
    ),
    checkboxInput("checkbox4", label = "Seasonality detected", value = FALSE),
    tags$head(tags$script(HTML(JScode))),
    sliderInput("range", 
                            label = "Total number of cases",
                            min = 0, max = 1e+7, value = c(0,1e+7)),
                uiOutput("Box1"),
                uiOutput("Box2"), 
    selectInput("variable", "Weather Variable",
                  list("Air Frost (days below 0°C)" = "Air.frost",
                       "Ground Frost (days below 0°C)"  = "Ground.frost",
                       "MLS Pressure (mean hPa) "  = "MLSPressure",
                       "Rainfall (mm)"  = "Rainfall",
                       "Rain days (>1 mm)"  = "Raindays.1",
                       "Rain days (>10 mm)" = "Raindays.10",
                       "Relative Humidity (mean %)"  = "Relative.humidity",
                       "Snow lying days (>50% of the ground covered by snow at 0900)"  = "Snow.lying",
                       "Sunshine (hours per day)"  = "Sunshine",
                       "Temperature (max daily °C)"  = "Max.temp",
                       "Temperature (mean daily °C)" = "Mean.temp",
                       "Temperature (min daily °C)"  = "Min.temp",
                       "Vapour Pressure (mean hPa)" = "Vapour.pressure",
                       "Wind Speed (mean knots)"  = "Mean.wind.speed"
                       )),
      
    helpText("This work was funded by the MED MI project (MR/K019341/1). Application built in Rstudio (0.98.507) using infectious disease data from the LabBase 2 national surveillance database (1989-2015) from Public Health England, UKCP09 weather data from the Met Office (2001-2011) and root organism descriptions from Wikipedia (Accessed July 2015).")
                ),
  mainPanel(
    
    # code for supression of error message from dygraph from https://groups.google.com/forum/#!topic/shiny-discuss/FyMGa2R_Mgs
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"),
    tabsetPanel(type = "tabs", 
                tabPanel("Description", textOutput('description')),
                tabPanel("Total cases", textOutput("summary")),
                tabPanel("Time series", dygraphOutput('timeseries')),
                tabPanel("Month Plot", plotOutput("plot")), 
                tabPanel("Decomposition", plotOutput('decompose')),
                tabPanel("Forecast", plotOutput('forecast')),
                tabPanel("Weather Scatterplot", plotOutput('weather'))
    ))
))


