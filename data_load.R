
library(vcd)
library(MASS)
library(tidyverse)
library(jsonlite)
library(httr)
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

makes<-sort(c('BMW','MINI','MERCEDES','LEXUS','NISSAN','AUDI','VOLKSWAGEN','HYUNDAI',
              'INFINITI','KIA','MAZDA','LAND ROVER','VOLVO','HONDA','JAGUAR','ACURA',
              'TOYOTA','MOPAR','SUBARU','PORSCHE'))

# Read in data. uncomment once out of development
temp <- tempfile()
download.file("https://www-odi.nhtsa.dot.gov/downloads/folders/Recalls/FLAT_RCL.zip",temp)
data <- read_delim(unz(temp, "FLAT_RCL.txt"),'\t',col_names = FALSE)
unlink(temp)

names(data)<-txt_col
data$BGMAN<-as.Date(as.character(data$BGMAN),'%Y%m%d')
data$RCDATE<-as.Date(as.character(data$RCDATE),'%Y%m%d')
data$days_since<-as.integer(data$RCDATE-data$BGMAN)
tenyr<-data %>% filter(YEARTXT<=2010,MAKETXT %in% makes,
                       days_since>0,days_since<7300)

ex<-tenyr$days_since
ex<-ex[!is.na(ex)]
ex<-ex[ex>0]
fit1<-fitdistr(ex,"lognormal")
est1<-fit1$estimate
chisq.test(x=ex, p=dlnorm(ex,est1[[1]],est1[[2]]), rescale.p=TRUE)

fit2<-fitdistr(ex,'geometric')
est2<-fit2$estimate

save(list=c("data","est2","col_desc","makes"),file = "input.RDA")
