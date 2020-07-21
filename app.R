#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(jsonlite)
library(httr)
library(shinyWidgets)
library(ggplot2)
library(vcd)
library(MASS)

#hard coded column names and descriptions
txt_col<-c('RECORD_ID','CAMPNO','MAKETXT','MODELTXT',
            'YEARTXT','MFGCAMPNO','COMPNAME','MFGNAME',
            'BGMAN','ENDMAN','RCLTYPECD','POTAFF','ODATE',
            'INFLUENCED_BY','MFGTXT','RCDATE','DATEA','RPNO',
            'FMVSS','DESC_DEFECT','CONEQUENCE_DEFECT',
            'CORRECTIVE_ACTION','NOTES','RCL_CMPT_ID',
            'MFR_COMP_NAME','MFR_COMP_DESC','MFR_COMP_PTNO')

col_desc<-c('RUNNING SEQUENCE NUMBER','NHTSA CAMPAIGN NUMBER',
            'VEHICLE/EQUIPMENT MAKE','VEHICLE/EQUIPMENT MODEL',
            'MODEL YEAR','MFR CAMPAIGN NUMBER','COMPONENT DESCRIPTION',
            'MANUFACTURER THAT FILED DEFECT/NONCOMPLIANCE REPORT',
            'BEGIN DATE OF MANUFACTURING','END DATE OF MANUFACTURING',
            'VEHICLE, EQUIPMENT OR TIRE REPORT',
            'POTENTIAL NUMBER OF UNITS AFFECTED','DATE OWNER NOTIFIED BY MFR',
            'RECALL INITIATOR',
            'MANUFACTURERS OF RECALLED VEHICLES/PRODUCTS',
            'REPORT RECEIVED DATE','RECORD CREATION DATE',
            'REGULATION PART NUMBER',
            'FEDERAL MOTOR VEHICLE SAFETY STANDARD NUMBER',
            'DEFECT SUMMARY','CONSEQUENCE SUMMARY',
            'CORRECTIVE SUMMARY','RECALL NOTES','RECALLED COMPONENT NUM',
            'MANUFACTURER-SUPPLIED COMPONENT NAME',
            'MANUFACTURER-SUPPLIED COMPONENT DESCRIPTION',
            'MANUFACTURER-SUPPLIED COMPONENT PART NUMBER')
names(col_desc)<-txt_col 
# list all makes we care about
makes<-sort(c('BMW','MINI','MERCEDES','LEXUS','NISSAN','AUDI','VOLKSWAGEN','HYUNDAI',
         'INFINITI','KIA','MAZDA','LAND ROVER','VOLVO','HONDA','JAGUAR','ACURA',
         'TOYOTA','MOPAR','SUBARU','PORSCHE'))

# Define UI
ui <- dashboardPage(

    # Application title
    dashboardHeader(title="Automotive Recall App"),

    # Sidebar tabs and ui components
    dashboardSidebar(
        pickerInput("make_box",
                        "Filter on Make:",
                        choices=makes,
                        selected = makes,
                        multiple=T,
                        options = list(`actions-box`=T)),
        uiOutput('model_box'),
        uiOutput('year_box'),
        checkboxGroupButtons("level",
                             "Select Level of Summary:",
                             choices=c("Make"="MAKETXT",
                                       "Model"="MODELTXT",
                                       "Year"="YEARTCT"))
        ),
    
        # show the meat and potatoes
        dashboardBody(
            DT::dataTableOutput("table"),
           plotOutput("test"),
           plotOutput('cummulative')
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
# Read in data. uncomment once out of development
    # temp <- tempfile()
    # download.file("https://www-odi.nhtsa.dot.gov/downloads/folders/Recalls/FLAT_RCL.zip",temp)
    # data <- read_delim(unz(temp, "FLAT_RCL.txt"),'\t',col_names = FALSE)
    # unlink(temp)
    # 
    # names(data)<-txt_col
    # data$BGMAN<-as.Date(as.character(data$BGMAN),'%Y%m%d')
    # data$RCDATE<-as.Date(as.character(data$RCDATE),'%Y%m%d')
    # data$DATEA<-as.Date(as.character(data$DATEA),'%Y%m%d')

    output$model_box<-renderUI({
        data0<-(data %>% filter(MAKETXT %in% input$make_box) %>% distinct(MODELTXT))[[1]]
        pickerInput('model_box','Filter on Model',
                    choices=sort(data0),
                    selected = sort(data0),
                    multiple=T,
                    options = list(`actions-box`=T))
    })
    
    
        
    output$year_box<-renderUI({
        data1<-(data %>% filter(MAKETXT %in% input$make_box) %>% distinct(YEARTXT))[[1]]
        pickerInput('year_box','Filter on Year',
                    choices=sort(data1),
                    selected = sort(data1),
                    multiple=T,
                    options = list(`actions-box`=T))
    })
    output$table <- DT::renderDataTable({
        DT::datatable(data %>% 
                          filter(MAKETXT %in% input$make_box,
                                 MODELTXT %in% input$model_box,
                                 YEARTXT %in% input$year_box) %>% 
                          group_by(MAKETXT,MODELTXT,YEARTXT) %>% 
                          summarise(recalls=n()) %>% 
                          group_by(!!!syms(input$level)) %>% 
                          summarise(recalls=sum(recalls,na.rm = T),
                                divisor=n()) %>% 
                          mutate(avg_recalls=recalls/divisor) %>% 
                          dplyr::select(input$level,recalls,avg_recalls)
                      )
    })
    
    # notes: add average amount of recalls per make
    # create predictive model for number of recalls
    
    pdata<-reactive({
        data %>% mutate(days_since=as.integer(RCDATE-BGMAN)) %>% 
            filter(MAKETXT %in% input$make_box, YEARTXT<=2010,
                   days_since>0,days_since<7300) 
        })
    

        ex<-as.integer(data$RCDATE-data$BGMAN)
        ex<-ex[!is.na(ex)]
        ex<-ex[ex>0]
        fit1<-fitdistr(ex,"exponential")
        # ks.test(x,"pexp",fit1$estimate)
    
    output$test<-renderPlot({
        # hist(pdata()$days_since, freq = FALSE, breaks = 50)
        # curve(dexp(x, rate = fit1$estimate), from = 0, col = "red", add = TRUE)
        holder<-pdata()
        holder$curve<-dexp(holder$days_since,rate=fit1$estimate)
        ggplot(holder,aes(x=days_since)) +
            geom_histogram(binwidth = 120,boundary=0,color="black",fill="grey") 
            # geom_line(aes(y=curve))
    })
    output$cummulative<-renderPlot({
        ggplot(pdata(),aes(x=days_since))+
            geom_histogram(aes(y=cumsum(..count..)), binwidth = 120, boundary = 0,
                           color = "black", fill = "grey")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
