library(shiny)
library(ggplot2)
library(ISOweek)
library(xts)
library(forecast)
library(season)
library(tseries)
library(TTR)
library(reshape)
library(Hmisc)
library(ggplot2)
library(devtools)
library(dygraphs)

mydata <- readRDS("data/mydata.rds")
pop <- readRDS("data/pop.rds")
season <-readRDS("data/season2.rds")
weather <-readRDS("data/weather.rds")
wiki<-read.csv("data/wiki.csv")
# clean up wiki entry
wiki$text<-gsub("\\[.*?\\]", "", wiki$text)
wiki$text<-iconv(wiki$text, "latin1", "ASCII", sub="")

# simple cap function from http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

# Sort out data type
mydata<-merge(mydata, wiki, by="Root.organism.name", all = TRUE)
mydata$Organism.name<-as.character(mydata$Organism.name)
mydata$Root.organism.name<-as.character(mydata$Root.organism.name)
mydata$COUNT.OPIE.id<-as.numeric(gsub(",","", mydata$COUNT.OPIE.id))
mydata<-within(mydata, {total_count=ave(COUNT.OPIE.id, Organism.name, FUN=sum)})
mydata$total_count<-log10(mydata$total_count)
mydata<-merge(mydata, season, by="Organism.name", all = TRUE)
mydata$seasonality[is.na(mydata$seasonality)] <- FALSE
mydata <- mydata[order(mydata$Root.organism.name,mydata$Organism.name),]


shinyServer(function(input, output) {
  
  output$Box1 = renderUI({
          selectInput('Root', 
                      'Root Organism Name', 
                      c("Select Name", unique(mydata$Root.organism.name[which(mydata$total_count>=input$range[1] 
                                                                              & mydata$total_count<=input$range[2] &      
                                                                                mydata$seasonality==input$checkbox4
                                                                              )])),
                      "Select Name")
  })
  
  output$Box2 = renderUI(
        selectInput('organism', 
                      'Organism Name', 
                      c("Select Name", unique(mydata$Organism.name[which(mydata$Root.organism.name == input$Root 
                                                                         & mydata$total_count>=input$range[1] 
                                                                         & mydata$total_count<=input$range[2] &      
                                                                           mydata$seasonality==input$checkbox4)])),
                      "Select Name")
  )    
  
  subdata0 = reactive(mydata[which(mydata$total_count>=input$range[1] & mydata$total_count<=input$range[2]),])
  subdata1 = reactive(subdata0()[which(subdata0()$Root.organism.name == input$Root),])
  subdata2 = reactive(subdata1()[which(subdata1()$Organism.name == input$organism),])
  
# Generate a month plot 
  output$plot <- renderPlot({
    if (is.null(input$Root) || is.null(input$organism)){return("Select Name")                                      
    } 
    else if (input$Root == "Select Name" | input$organism == "Select Name"){return("Select Name")
    }
    else 
      newdata <-subdata2()
    pathogen <- c(newdata$Organism.name)
    pathogen<-unique(pathogen[duplicated(pathogen)])
    pathogen2<-tolower(pathogen)
    pathogen2<-sapply(pathogen2, simpleCap)
    newdata$date<-ISOweek2date(paste0(newdata$Year,"-W",sprintf("%02d",newdata$Week),"-1"))
    mergepop<-merge(newdata,pop, by="Year")
    mergepop$month<-format(mergepop$date, "%m")
    mergepop$month<-as.numeric(mergepop$month)
    mergepop$COUNT.OPIE.id<-as.numeric(as.character(mergepop$COUNT.OPIE.id))
    mergepop<-within(mergepop, {mean_count_month = ave(COUNT.OPIE.id,month, FUN=sum)/Pop*100000})
    mergepop2<-subset(mergepop,!duplicated(mergepop$mean_count_month))
    mergepop3<-within(mergepop2, {mean_count_month2 = ave(mean_count_month, month)})
    mergepop4<-subset(mergepop3,!duplicated(mergepop3$mean_count_month2))
    mergepop5<-subset(mergepop4, select=c("month", "mean_count_month2"))
    mergepop5$mean<-mean(mergepop5$mean_count_month2)
    mergepop5 <- mergepop5[order(mergepop5$month),]
    plotCircular(area1 = mergepop5$mean_count_month2,
                 dp = 2, labels = month.abb, main=paste(pathogen2, "cases per 100,000"), lines=TRUE, pieces.col=c("#FFE135"),
                 scale = 0.7) }, height=620, width=600, res=85)
  
# Generate a decomposition of the data
output$decompose <- renderPlot({
  if (is.null(input$Root) || is.null(input$organism)){return("Organism not chosen")                                      
  } 
  else if (input$Root == "Select Name" | input$organism == "Select Name"){return("Organism not chosen")
  }
  else
    newdata <-subdata2()
  pathogen <- c(newdata$Organism.name)
  pathogen<-unique(pathogen[duplicated(pathogen)])
  pathogen2<-tolower(pathogen)
  pathogen2<-sapply(pathogen2, simpleCap)
  newdata<-subset(newdata, select=c("Year", "Week", "COUNT.OPIE.id"))
  xts2<-xts(newdata, ISOweek2date(paste0(newdata$Year,"-W",sprintf("%02d",newdata$Week),"-1")), format = "%Y %U %u")
  xts2$Year<-NULL
  xts2$Week<-NULL
  ts.month <- apply.monthly(as.xts(xts2),FUN=sum)
  ts.month<-ts(ts.month, freq=12)
  decompose <- decompose(ts.month)
  decomp.plot <- function(x, main = NULL, ...) 
  { 
    if(is.null(main)) 
      main <- paste("Decomposition of", pathogen2) 
    plot(cbind(observed = x$random + if (x$type == "additive") 
      x$trend + x$seasonal 
      else x$trend * x$seasonal, trend = x$trend, seasonal = x$seasonal, 
      random = x$random), xlab="Year", main = main, ...) 
  } 
  decomp.plot(decompose) 
} , height=600, width=600,res=85)


# Generate a time series of the data
  output$timeseries <- renderDygraph({
    if (is.null(input$Root) || is.null(input$organism)){return("")                                      
    } 
    else if (input$Root == "Select Name" | input$organism == "Select Name"){return("")
    }
    else
      newdata <-subdata2()
    pathogen <- c(newdata$Organism.name)
    pathogen<-unique(pathogen[duplicated(pathogen)])
    pathogen2<-tolower(pathogen)
    pathogen2<-sapply(pathogen2, simpleCap)
    newdata<-subset(newdata, select=c("Year", "Week", "COUNT.OPIE.id"))
    xts2<-xts(newdata, ISOweek2date(paste0(newdata$Year,"-W",sprintf("%02d",newdata$Week),"-1")), format = "%Y %U %u")
    xts2$Year<-NULL
    xts2$Week<-NULL
    xts2$Cases<-xts2$COUNT.OPIE.id
    xts2$COUNT.OPIE.id<-NULL
    dygraph(xts2,xlab="Date", ylab="Cases", 
            main=paste("Time Series of", pathogen2)) %>%
      dyOptions(colors = c("#ef2f2f")) %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod=1) %>%
      dyLegend(show = "onmouseover")
  })
  
# Generate a TBATS forecast of the data
  output$forecast <- renderPlot({
    if (is.null(input$Root) || is.null(input$organism)){return("")                                      
    } 
    else if (input$Root == "Select Name" | input$organism == "Select Name"){return("")
    }
    else
      newdata <-subdata2()
    pathogen <- c(newdata$Organism.name)
    pathogen<-unique(pathogen[duplicated(pathogen)])
    pathogen2<-tolower(pathogen)
    pathogen2<-sapply(pathogen2, simpleCap)
    newdata<-subset(newdata, select=c("Year", "Week", "COUNT.OPIE.id"))
    xts2<-xts(newdata, ISOweek2date(paste0(newdata$Year,"-W",sprintf("%02d",newdata$Week),"-1")), format = "%Y %U %u")
    xts2$Year<-NULL
    xts2$Week<-NULL
    timeseries2<-ts(xts2, freq=365.25/7, start=1988+31/7/365.25)
    timeseries2<-na.remove(timeseries2)
    ts.month <- apply.monthly(as.xts(xts2),FUN=sum)
    ts.month<-ts(ts.month, freq=12)
    # testing seasonality
    fit <- tbats(timeseries2)
    plot(forecast(fit), main=paste("TBATS forecast for", pathogen2), ylab="Cases", xlab="Year")  
  }, height=600, width=600, res=85)

# scatterplot
# Create a reactive text
output$weather <- renderPlot({
  if (is.null(input$Root) || is.null(input$organism)){return("")                                      
  } 
  else if (input$Root == "Select Name" | input$organism == "Select Name"){return("")
  }
  else
  newdata <-subdata2()
  pathogen <- c(newdata$Organism.name)
  pathogen<-unique(pathogen[duplicated(pathogen)])
  pathogen2<-tolower(pathogen)
  pathogen2<-sapply(pathogen2, simpleCap)
  newdata<-subset(newdata, subset= Year>2000 & Year<2012, select=c("Year", "Week", "COUNT.OPIE.id"))
  newdata$date<-ISOweek2date(paste0(newdata$Year,"-W",sprintf("%02d",newdata$Week),"-1"))
  mergepop<-merge(newdata,pop, by="Year")
  mergepop$month<-format(mergepop$date, "%m")
  mergepop$month<-as.numeric(mergepop$month)
  mergepop$COUNT.OPIE.id<-as.numeric(as.character(mergepop$COUNT.OPIE.id))
  mergepop<-within(mergepop, {mean_count_month = ave(COUNT.OPIE.id,month,FUN=sum)/Pop*100000})
  mergepop2<-within(mergepop, {total_count = ave(COUNT.OPIE.id,month,Year, FUN=sum)})
  mergepop2<-subset(mergepop2,!duplicated(mergepop2$mean_count_month))
  mergepop10<-merge(mergepop2,weather, by=c("Year", "month"))
  mergepop10 <- mergepop10[order(mergepop10$Year, mergepop10$month),]
  mergepop11<-subset(mergepop10, select=c("Year", "month", "total_count", "Max.temp","Mean.temp", "Min.temp","Ground.frost","Air.frost","Mean.wind.speed","MLSPressure","Raindays.1","Raindays.10","Rainfall","Relative.humidity","Snow.lying" ,"Sunshine","Vapour.pressure"))
  zt=cbind(mergepop10$total_count ,mergepop10$Max.temp, mergepop10$Mean.temp, mergepop10$Min.temp, mergepop10$Ground.frost, mergepop10$Air.frost, mergepop10$Mean.wind.speed, mergepop10$MLSPressure, mergepop10$Raindays.1, mergepop10$Raindays.10, mergepop10$Rainfall, mergepop10$Relative.humidity, mergepop10$Snow.lying, mergepop10$Sunshine, mergepop10$Vapour.pressure)  ## Create time series
  #zt=Lag(zt, shift = 1)
  colnames(zt) <- c("Cases","Max.temp","Mean.temp", "Min.temp","Ground.frost","Air.frost","Mean.wind.speed","MLSPressure","Raindays.1","Raindays.10","Rainfall","Relative.humidity","Snow.lying" ,"Sunshine","Vapour.pressure")
  zt2<-data.frame(zt)
# the regression results text from here: http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph
# the smooth regression line from here: https://gist.github.com/kdauria/524eade46135f6348140
 stat_smooth_func <- function(mapping = NULL, data = NULL,
                               geom = "smooth", position = "identity",
                               ...,
                               method = "auto",
                               formula = y ~ x,
                               se = TRUE,
                               n = 80,
                               span = 0.75,
                               fullrange = FALSE,
                               level = 0.95,
                               method.args = list(),
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               xpos = NULL,
                               ypos = NULL) {
    layer(
      data = data,
      mapping = mapping,
      stat = StatSmoothFunc,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        method = method,
        formula = formula,
        se = se,
        n = n,
        fullrange = fullrange,
        level = level,
        na.rm = na.rm,
        method.args = method.args,
        span = span,
        xpos = xpos,
        ypos = ypos,
        ...
      )
    )
  }
  
## Had to do a quick fix for below, to just show r2, as formula wasn't working...
  StatSmoothFunc <- ggproto("StatSmooth", Stat,
                            
                            setup_params = function(data, params) {
                              # Figure out what type of smoothing to do: loess for small datasets,
                              # gam with a cubic regression basis for large data
                              # This is based on the size of the _largest_ group.
                              if (identical(params$method, "auto")) {
                                max_group <- max(table(data$group))
                                
                                if (max_group < 1000) {
                                  params$method <- "loess"
                                } else {
                                  params$method <- "gam"
                                  params$formula <- y ~ s(x, bs = "cs")
                                }
                              }
                              if (identical(params$method, "gam")) {
                                params$method <- mgcv::gam
                              }
                              
                              params
                            },
                            
                            compute_group = function(data, scales, method = "auto", formula = y~x,
                                                     se = TRUE, n = 80, span = 0.75, fullrange = FALSE,
                                                     xseq = NULL, level = 0.95, method.args = list(),
                                                     na.rm = FALSE, xpos=NULL, ypos=NULL) {
                              if (length(unique(data$x)) < 2) {
                                # Not enough data to perform fit
                                return(data.frame())
                              }
                              
                              if (is.null(data$weight)) data$weight <- 1
                              
                              if (is.null(xseq)) {
                                if (is.integer(data$x)) {
                                  if (fullrange) {
                                    xseq <- scales$x$dimension()
                                  } else {
                                    xseq <- sort(unique(data$x))
                                  }
                                } else {
                                  if (fullrange) {
                                    range <- scales$x$dimension()
                                  } else {
                                    range <- range(data$x, na.rm = TRUE)
                                  }
                                  xseq <- seq(range[1], range[2], length.out = n)
                                }
                              }
                              # Special case span because it's the most commonly used model argument
                              if (identical(method, "loess")) {
                                method.args$span <- span
                              }
                              
                              if (is.character(method)) method <- match.fun(method)
                              
                              base.args <- list(quote(formula), data = quote(data), weights = quote(weight))
                              model <- do.call(method, c(base.args, method.args))
                              
                              m = model
                              eq <- substitute(~~italic(r)^2~"="~r2, 
                                               list(a = format(coef(m)[1], digits = 3), 
                                                    b = format(coef(m)[2], digits = 3), 
                                                    r2 = format(summary(m)$r.squared, digits = 3)))
                              func_string = as.character(as.expression(eq))
                              
                              if(is.null(xpos)) xpos = min(data$x)*0.9
                              if(is.null(ypos)) ypos = max(data$y)*0.9
                              data.frame(x=xpos, y=ypos, label=func_string)
                              
                            },
                            
                            required_aes = c("x", "y")
  )
   ggplot(zt2, aes_string(x=input$variable, y="Cases")) +
    stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE) +
    geom_smooth(method="lm",se=FALSE) +
    geom_point(shape=1, color="darkgrey") +    # Use hollow circles
    geom_smooth(method=lm, color="#FF1493") +
    ggtitle(paste("Weather Scatterplot for", pathogen2))+
    theme_classic() +
    theme(plot.title = element_text(face = "bold"))
  }, res=85)
  
# Generate a summary of the data
  output$summary <- renderText({
    if (is.null(input$Root) || is.null(input$organism)){return("")                                      
    } 
    else if (input$Root == "Select Name" || input$organism == "Select Name"){return("")
    }
    else 
    newdata <-subdata2()
    pathogen<- c(newdata$Organism.name)
    pathogen<-unique(pathogen[duplicated(pathogen)])
    pathogen2<-tolower(pathogen)
    pathogen2<-sapply(pathogen2, simpleCap)
    mydata<-within(newdata, {total_count=ave(COUNT.OPIE.id, Organism.name, FUN=sum)})
    mydata$total_count<-prettyNum(mydata$total_count,big.mark=",",scientific=FALSE)
    unique(mydata$total_count)
  })

# Description of organism
  output$description <- renderText({
    if (is.null(input$Root)){return("")                                      
    } 
    else if (input$Root == "Select Name"){return("")
    }
    else 
      newdata<-subdata2()
      newdata$text<-as.character(newdata$text)
      unique(newdata$text)
    })
})
