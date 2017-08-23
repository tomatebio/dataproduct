#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


#source("/home/carlos/Downloads/vacine/vacine.r")
#source("vacine.r")

library(stringr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(leaflet)
library(plotly)
library(forecast)
vacine<-read.csv("vacine.csv")

vacineClear<-vacine[,-14:-27]


vacineClear$Outbreak<- gsub(".*[Ff]lu*","Flu",vacineClear$Outbreak)
vacineClear$Outbreak<-as.factor(vacineClear$Outbreak)
vacineClear$Long<-as.numeric(as.vector(vacineClear$Long))

tmp_date<-str_split(vacineClear$Date,"-")

vacineClear$Country<-str_trim(vacineClear$Location.1.1)
extractDate<-function(x){
  return(x[1])
}

tmp_date2<-sapply(tmp_date,extractDate)

tmp_date2[grep("\\.",tmp_date2)]<-as.numeric(tmp_date2[grep("\\.",tmp_date2)])

# imputacao de data
tmp_date2[grep("/",tmp_date2,invert=TRUE)]<-paste0("1/",tmp_date2[grep("/",tmp_date2,invert=TRUE)])

vacineClear$Date<-as.Date(paste0("1/",tmp_date2), format="%d/%m/%Y")

vacineClear$Year<-as.numeric(format(vacineClear$Date,"%Y"))



vacine_tbl<-tbl_df(vacineClear) 



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
output$Overview<-renderText(" Here we can see the impact of a hoax about vaccine.  I compiled data from https://www.cfr.org/interactives/GH_Vaccine_Map/ and  format in nice way.
                            You can check in radio button is side bar and see historical and geographical distribuition of diseases")
  
   dis<-reactive({ c(ifelse(input$Measles,"Measles",""), 
                     ifelse(input$Polio,"Polio",""),
                     ifelse(input$Other,"Other",""),
                    ifelse(input$Rubella,"Rubella",""),
                    ifelse(input$Whooping_Cough,"Whooping Cough",""),
                    ifelse(input$PMumps,"Mumps",""))     })
 
   years<-reactive({
     yy<-input$slider1
   })
  output$Cases<-renderTable({
    vacine_tbl %>%
      group_by(Outbreak) %>%
      summarise(Cases=sum(Cases))
  })
  
  output$Fatal<-renderTable({
    vacine_tbl %>%
      group_by(Outbreak) %>%
      summarise(Fatalities=sum(Fatalities)) 
  })
  output$gt<-renderText({getwd()})
  output$Map<-renderLeaflet({
    vacine_tbl %>%
      select(lat=Lat,lng=Long,Category=Category) %>%
      filter(Category %in% dis()) %>%
      leaflet() %>%
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions())
  })
  output$Cases<-renderPlotly({
    resu<-vacine_tbl %>%
      group_by(Year,Category) %>%
      summarise(Cases=sum(Cases)) %>%
    filter(Category %in% dis())
    ggplotly(ggplot(resu,aes(x=Year,y=Cases,color=Category)) + geom_line())
  })
  output$prev<-renderPlot({
    tt<-vacine_tbl %>%
      filter(Category %in% dis()) %>%
      group_by(Date) %>%
      summarise(Cases=sum(Cases)) %>%
      select(Cases) %>%
      ts(start=c(2007,1),frequency=12)
    d.arima <- auto.arima(tt)
    d.forecast <- forecast(d.arima, level = c(95), h = years())
    autoplot(d.forecast,main="Prevision", ylab="Cases")
    
  })

})
