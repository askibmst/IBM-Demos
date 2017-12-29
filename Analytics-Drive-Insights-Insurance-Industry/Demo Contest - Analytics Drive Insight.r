
library(dplyr)
library(corrplot)
library(ggplot2)
library(caret)
library(party)
library(fastAdaboost)
library(ibmos2sparkR)
library(e1071)
library(rpart.plot)
library(rpart)
library(ggpubr)
suppressWarnings(suppressMessages(library(dplyr)))
library(highcharter)
library(lubridate)
library(ggthemes)
suppressWarnings(suppressMessages(library(tidyr)))
library(viridis)
library(ggplot2)

# @hidden_cell
# This function accesses a file in your Object Storage. The definition contains your credentials.
# You might want to remove those credentials before you share your notebook.
####Incert your code here to import data #####

df.data.1 <-  read.csv(file = getObjectStorageFileWithCredentials_2c31d44f8d214b3487b0bb239a8df945("AnalyticsSellerDemoContest", "Vehicle_Collision.csv"))
##############################################
head(df.data.1)


Vehicle = df.data.1
dim(df.data.1)

## Create the date and time variable 
Vehicle$DATE = as.Date(Vehicle$DATE, "%m/%d/%y") 
Vehicle$hour = strptime(as.character(Vehicle$TIME),format = "%H:%M")

## Date and time extration 
Vehicle$month = month(Vehicle$DATE,label = T)
Vehicle$day = wday(Vehicle$DATE,label = T)
Vehicle$hour = hour(Vehicle$hour)

head(Vehicle)

#####  Number of Accidents Per Person
## Calculate number of accidents by area
Vehicle = as.data.frame(Vehicle)
temp_1 = Vehicle %>% group_by(BOROUGH) %>% summarise(n=n())
## Sign the popolation of each Borough 
temp_1$pop = rep(0,dim(temp_1)[1])
temp_1$pop = ifelse(temp_1$BOROUGH=="MANHATTAN",1644158,temp_1$pop)
temp_1$pop = ifelse(temp_1$BOROUGH=="BRONX",1455444,temp_1$pop)
temp_1$pop = ifelse(temp_1$BOROUGH=="QUEENS",2339150,temp_1$pop)
temp_1$pop = ifelse(temp_1$BOROUGH=="STATEN ISLAND",474558,temp_1$pop)
temp_1$pop = ifelse(temp_1$BOROUGH=="BROOKLYN",2636735,temp_1$pop)
temp_1$per_cap = temp_1$n/temp_1$pop

options(repr.plot.width=5, repr.plot.height=5)
temp_1 %>% filter(BOROUGH!="") %>% ggplot(aes(x=BOROUGH,y=per_cap))+
  geom_bar(stat="identity")+
  ggtitle("Number of Accidents Per Person in each Borough")+
  ylab("Number of Accidents Per Person")    

options(repr.plot.width=9, repr.plot.height=5)
Vehicle %>% filter(BOROUGH!="") %>%  group_by(DATE,BOROUGH) %>% summarise(n=mean(n())) %>% na.omit() %>%
  ggplot(aes(x=DATE, y=n, colour=BOROUGH, group=BOROUGH)) + geom_line()+geom_point(size=2,shape=1)+theme_hc(bgcolor = "darkunica") + scale_x_date('month') +
  scale_fill_hc("darkunica") +ggtitle("Borough Accidents(Mean) by Time")+geom_text(aes(label=ifelse(n>150,n,"")), size=3,hjust=1.8)


plot2 = Vehicle %>% filter(BOROUGH!="") %>% group_by(month,BOROUGH) %>% summarise(n=mean(n())) %>% na.omit() %>%
  ggplot(aes(x=month, y=n, fill=BOROUGH)) +
  geom_bar(position="dodge",stat = "identity")+geom_text(aes(label=n), vjust=1.5, colour="black",
                                                         position=position_dodge(.9), size=3)+ggtitle("Mean Number of Collisions Per Month")+
  ylab('Number of Collisions')


options(repr.plot.width=7, repr.plot.height=5)
Vehicle %>% group_by(BOROUGH,day)%>% summarise(n=mean(n())) %>% filter(BOROUGH!="") %>%
  ggplot(aes(x=day, y=n, fill=BOROUGH)) +
  geom_bar(position="dodge",stat = "identity")+geom_text(aes(label=n), vjust=1.5, colour="black",
                                                         position=position_dodge(.9), size=3)+ggtitle("Mean Number of Collisions Per Day")+
  ylab('Number of Collisions')

Table_1 <- Vehicle %>% select(PERSONS_KILLED,PERSONS_INJURED,VEHICLE_1_FACTOR,VEHICLE_2_FACTOR,VEHICLE_3_FACTOR,VEHICLE_4_FACTOR,VEHICLE_5_FACTOR) %>% gather(type,value,1:2) %>% gather(vehicle_type,cause,1:5) %>% filter(value!=0,cause!="",cause!="Unspecified")
Table_2 <- Table_1 %>% select(-vehicle_type) %>% group_by(type,cause) %>% summarise(total=sum(value,na.rm=T))
options(repr.plot.width=9, repr.plot.height=5.5)
ggplot(data = Table_2, aes(x = cause, y = log(total), fill = type)) +
  geom_bar(data = subset(Table_2, type=="PERSONS_INJURED"),
           stat = "identity") +
  geom_bar(data = subset(Table_2, type=="PERSONS_KILLED"),
           stat = "identity",
           position = "identity",
           mapping = aes(y = -log(total))) +
  scale_y_continuous(labels = abs) +
  coord_flip()+ggtitle('Causes of Accidents') +
  xlab('Cause')+
  ylab('Log Total')



######################## Predictin ####################### 
summary = Vehicle %>% filter(BOROUGH!="") %>% filter(VEHICLE_1_TYPE!="")%>% group_by(DATE,BOROUGH,VEHICLE_1_TYPE,hour)%>% summarise(n=sum(n()))
summary$pop = rep(0,dim(summary)[1])
summary$pop = ifelse(summary$BOROUGH=="MANHATTAN",1644158,summary$pop)
summary$pop = ifelse(summary$BOROUGH=="BRONX",1455444,summary$pop)
summary$pop = ifelse(summary$BOROUGH=="QUEENS",2339150,summary$pop)
summary$pop = ifelse(summary$BOROUGH=="STATEN ISLAND",474558,summary$pop)
summary$pop = ifelse(summary$BOROUGH=="BROOKLYN",2636735,summary$pop)
summary$per_mi_cap = summary$n*1000000/summary$pop
summary$DATE = as.Date(summary$DATE, "%m/%d/%y") 
summary$month = month(summary$DATE,label = T)
summary$day = wday(summary$DATE,label = T)


head(summary)

## Add if that day is weekday
summary$weekend = rep('false',dim(summary)[1])
summary$weekend = ifelse(summary$day == "Sun" | summary$day == "Sat", 'true', summary$weekend)
summary = as.data.frame(summary)

### linear regression 
trunc = 1:26000
train = summary[trunc,]
test = summary[-trunc,]
fit = lm(per_mi_cap ~ BOROUGH + VEHICLE_1_TYPE + month + weekend + hour, data = summary)

prediction = predict(fit, summary)
compar = as.data.frame(cbind(summary$per_mi_cap,prediction))
names(compar) = c('Actuall_Value','Prediction')
# compar[1:50,]

mean(abs(compar$Actuall_Value-compar$Prediction)/compar$Actuall_Value)


newdat = data.frame(BOROUGH = "STATEN ISLAND",
                    VEHICLE_1_TYPE = "PASSENGER VEHICLE",
                    hour = 15,
                    month = "Mar",
                    weekend = "false"
                    )
pred_one = predict(fit, newdat)
pred_one

options(repr.plot.width=6, repr.plot.height=4)
ggplot(summary, aes(x=per_mi_cap)) +
    geom_histogram(bins = 50, fill="#FF6666") + 
    xlab('Number of Accidents per Million People') +
    ylab("Count")

Risk = function(input) {
  if (input<=2) {
    return("Low")
  } else
      if(input>3 & input<=4) {
          return("Medium")
      }
      else 
          return("High")
}
Risk(input = pred_one)









#################################### Code for Shiny  - Run this part in Rstudio #############################
library(leaflet)
library(shinydashboard)
library(leaflet)
library(osrm)
# Define UI for application that draws a accident map
vars = c(
  "Motorists" = "MOTORISTS_INJURED",
  "Pedestrians" = "PEDESTRIANS_INJURED",
  "Cyclists" = "CYCLISTS_INJURED"
)
# Vehicle$month = as.character(Vehicle$month)
var_mon = c(
  "January" = "Jan",
  "Febuary" = "Feb",
  "March" = "Mar",
  "April" = "Apr",
  "May" = "May",
  "June" = "Jun",
  "July" = "Jul",
  "August" = "Aug",
  "September" = "Sep",
  "October" = "Oct",
  "November" = "Nov",
  "December" = "Dec"
)
ui = fluidPage(
  titlePanel("Vehicle Collision"),
  navbarPage("Number of Collisions", id="nav",
             tabPanel("Interactive map",
                      div(class="outer"),
                      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                    draggable = TRUE, top = 110, left = "auto", right = 20, bottom = "auto",
                                    width = 650, height = "auto",
                                    
                                    h2("Collision explorer"),
                                    
                                    selectInput("months", "Month", var_mon),
                                    selectInput("type", "Persona", vars),
                                    #conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
                                                     # Only prompt for threshold when coloring or sizing by superzip
                                                     #numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
                                    #) ,
                                    
                                    plotOutput('plot2', height = 500)
                                    #plotOutput("scatterCollegeIncome", height = 250)
                      ),
                      mainPanel(
                        plotOutput('plot1',width = "100%", height = "800px")
                      )
             ),
             tabPanel("Data explorer",
                      fluidRow(
                        column(4,
                               selectInput("BOROUGH", "Borough", c("All Boroughs", unique(Vehicle$BOROUGH)))
                        ),
                        column(4,
                               selectInput("Months","Month", c("All",as.character(sort(unique(Vehicle$month))))
                               )
                        ),
                        column(4,
                               selectInput("hour", "Hour in Day", c("All Day", sort(unique(Vehicle$hour))))
                        )
                      ),
                      # Create a new row for the table.
                      fluidRow(
                        DT::dataTableOutput("table")
                      )
             )
             
  )
)

# Define server logic required to draw a histogram
server = function(input, output) {
  # Filter data based on selections
  months = 'Dec'
  df_1 = Vehicle[Vehicle$month == months,]
  df_2 <- df_1 %>% select(LATITUDE_2,LONGITUDE_2,PEDESTRIANS_INJURED,CYCLISTS_INJURED,MOTORISTS_INJURED) %>% gather(type,value,3:5) %>% na.omit() %>% group_by(LATITUDE_2,LONGITUDE_2,type) %>% summarise(total=sum(value,na.rm=T)) %>% filter(total!=0)
  
  output$plot1 <- renderPlot({
  g1 <- ggmap(nyc)+geom_point(data=subset(df_2,type == input$type), 
                              aes(x=LONGITUDE_2, y=LATITUDE_2, colour=total),size=1,alpha=0.2) +
    ggtitle("Number of People Injured")+scale_color_continuous(low = "red",  high = "black")
  print(g1)
  })
  
  Table_1 <- df_1 %>% select(PERSONS_KILLED,PERSONS_INJURED,VEHICLE_1_FACTOR,VEHICLE_2_FACTOR,VEHICLE_3_FACTOR,VEHICLE_4_FACTOR,VEHICLE_5_FACTOR) %>% gather(type,value,1:2) %>% gather(vehicle_type,cause,1:5) %>% filter(value!=0,cause!="",cause!="Unspecified")
  Table_2 <- Table_1 %>% select(-vehicle_type) %>% group_by(type,cause) %>% summarise(total=sum(value,na.rm=T))
  output$plot2 = renderPlot({
    g2 = ggplot(data = Table_2, aes(x = cause, y = log(total), fill = type)) + 
      geom_bar(data = subset(Table_2, type=="PERSONS_INJURED"),
               stat = "identity") +
      geom_bar(data = subset(Table_2, type=="PERSONS_KILLED"),
               stat = "identity",
               position = "identity",
               mapping = aes(y = -log(total))) +
      scale_y_continuous(labels = abs) +
      coord_flip()+ggtitle('Causes of Accidents') +
      xlab('Cause')+
      ylab('Log Total')
    print(g2)
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    data <- Vehicle
    datavars <- names(data) %in% c("LATITUDE", "LONGITUDE", "LOCATION","VEHICLE_2_TYPE","VEHICLE_3_TYPE",
                                     "VEHICLE_4_TYPE","VEHICLE_5_TYPE","VEHICLE_3_FACTOR","VEHICLE_4_FACTOR","VEHICLE_5_FACTOR",
                                     "DATE_TIME","LATITUDE_2","LONGITUDE_2","OFF_STREET_NAME")
    newdata <- data[!datavars]
    if (input$BOROUGH != "All Boroughs") {
      newdata <- newdata[newdata$BOROUGH == input$BOROUGH,]
    }
    if (input$Months != "All") {
      newdata <- newdata[newdata$month == input$Months,]
    }
    if (input$hour != "All Day") {
      newdata <- newdata[newdata$hour == input$hour,]
    }
    datavars2 <- names(newdata) %in% c("month","hour")
    newdata2 = newdata[!datavars2]
  }))
  
}
# Run the application 
shinyApp(ui = ui, server = server)
