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
                        multiple=T)
        ),
    
        # show the meat and potatoes
        dashboardBody(
           DT::dataTableOutput("table")
        )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # temp <- tempfile()
    # download.file("https://www-odi.nhtsa.dot.gov/downloads/folders/Recalls/FLAT_RCL.zip",temp)
    # data <- read_delim(unz(temp, "FLAT_RCL.txt"),'\t',col_names = FALSE)
    # unlink(temp)
    # 
    # names(data)<-txt_col
    
    output$table <- DT::renderDataTable({
        print(input$make_box)
        DT::datatable(data %>% filter(MAKETXT %in% input$make_box) %>% distinct(MAKETXT) %>% arrange(MAKETXT))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
