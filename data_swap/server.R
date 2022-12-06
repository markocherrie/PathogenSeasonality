library(shiny)
library(ggplot2)
library(ISOweek)
library(xts)
library(forecast)
library(season)
library(forecast)
library(tseries)
library("TTR")
library("RcppArmadillo")

mydata <- readRDS("data/mydata.rds")
pop <- readRDS("data/pop.rds")
season <-readRDS("data/merged5.rds")
wiki<-read.csv("data/wiki.csv")
# get rid of refs
wiki$text<-gsub("\\[.*?\\]", "", wiki$text)

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
mydata$seasonality[is.na(mydata$seasonality)] <- TRUE
mydata$weekly_seasonality[is.na(mydata$weekly_seasonality)] <- FALSE
mydata$Monthly_seasonality[is.na(mydata$Monthly_seasonality)] <- FALSE
mydata$Quarterly_seasonality[is.na(mydata$Quarterly_seasonality)] <-FALSE
mydata <- mydata[order(mydata$Root.organism.name,mydata$Organism.name),]


shinyServer(function(input, output) {
  
  output$Box1 = renderUI({
          selectInput('Root', 
                      'Root Organism Name', 
                      c("Select Name", unique(mydata$Root.organism.name[which(mydata$total_count>=input$range[1] 
                                                                              & mydata$total_count<=input$range[2] &      
                                                                                mydata$weekly_seasonality==input$checkbox &
                                                                                mydata$Monthly_seasonality==input$checkbox2 &
                                                                                mydata$Quarterly_seasonality==input$checkbox3 &
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
                                                                           mydata$weekly_seasonality==input$checkbox &
                                                                           mydata$Monthly_seasonality==input$checkbox2 &
                                                                           mydata$Quarterly_seasonality==input$checkbox3&
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
                 scale = 0.7) }, height=600, width=600)
  
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
  xts2<- xts(newdata,  as.POSIXct(paste0(newdata$Year, " ", newdata$Week, " 1"), format = "%Y %U %u"))
  xts2$Year<-NULL
  xts2$Week<-NULL
  ts.month <- apply.monthly(as.xts(xts2),FUN=sum)
  ts.month<-ts(ts.month, freq=12)
  decompose <- decompose(ts.month)
  decomp.plot <- function(x, main = NULL, ...) 
  { 
    if(is.null(main)) 
      main <- paste("Monthly decomposition of", pathogen2, "time series") 
    plot(cbind(observed = x$random + if (x$type == "additive") 
      x$trend + x$seasonal 
      else x$trend * x$seasonal, trend = x$trend, seasonal = x$seasonal, 
      random = x$random), xlab="Year", main = main, ...) 
  } 
  decomp.plot(decompose) 
} , height=600, width=600)

  
# Generate a time series of the data
  output$timeseries <- renderPlot({
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
    xts2<- xts(newdata,  as.POSIXct(paste0(newdata$Year, " ", newdata$Week, " 1"), format = "%Y %U %u"))
    xts2$Year<-NULL
    xts2$Week<-NULL
    timeseries2<-ts(xts2, freq=365.25/7, start=1988+31/7/365.25)
    timeseries2<-na.remove(timeseries2) 
    plot(timeseries2, pch=".", col="lightgrey", main=paste("Time Series of", pathogen2), xlab="Year", ylab="Count")
    sm<-ma(timeseries2, order=12)
    lines(sm,col="red")
    legend("topleft", title="Legend", c("Observations","Moving average"),
           lty=c(1,1), 
           lwd=c(2.5,2.5),col=c("lightgrey","red"))
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
    xts2<- xts(newdata,  as.POSIXct(paste0(newdata$Year, " ", newdata$Week, " 1"), format = "%Y %U %u"))
    xts2$Year<-NULL
    xts2$Week<-NULL
    ts.month <- apply.monthly(as.xts(xts2),FUN=sum)
    ts.month<-ts(ts.month, freq=12)
    fit <- tbats(ts.month)
    plot(forecast(fit), main=paste("TBATS forecast for", pathogen2), ylab="Count", xlab="Year")  
  }, height=600, width=600)
  
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
