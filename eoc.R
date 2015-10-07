setwd("./data/EOC")
system("cut -f $(head -1 EOC.csv |sed 's/,/\'$'\n/g' |grep -ni 'Churn_In_Range_IND' |cut -f1 -d:) -d, EOC.csv >premodel.csv")

csvfile<-"/mnt/R/data/EOC/EOC.csv"
outputfile<-"premodel.csv"
colName<-c("AccountNumber", "ActivationDate", "Churn_Week", "Churn_In_Range_IND", "Churn_Before_Range_IND", "Churn_After_Range_IND","Churn_Date", "Express_Repair_PRESENT")
system.time(header<-read.csv(csvfile, header=FALSE, stringsAsFactors=FALSE, nrows=1))
colNum<-paste0(which(header %in% colName),collapse=',')
systemCmd<-paste("cut -d, -f",colNum, csvfile,">",outputfile )
system(systemCmd)


library(openxlsx)
#count.fields("/mnt/R/data/EOC/EOC.csv",sep=",")[1:2]
#system.time(EOC <-read.csv("/mnt/R/data/EOC/EOC.csv",colClasses=c("character","time",rep("numeric",718)),nrows=197378)[,c("AccountNumber", "ActivationDate", "Churn_Week", "Churn_In_Range_IND", "Churn_Before_Range_IND", "Churn_After_Range_IND","Churn_Date", "Express_Repair_PRESENT")])  #60

system.time(EOC <-read.csv(outputfile,nrows=197378,stringsAsFactors=FALSE)) #104 the first time/72 the second time
EOC$INSTALL2CHURN<-difftime(convertToDate(EOC$Churn_Date),convertToDate(EOC$ActivationDate),units='days')
            
system.time(EOC<-read.xlsx("/mnt/R/data/EOC/EOC.xlsx"))
EOC<-read.csv("cut -d, -f1,2 /mnt/R/data/EOC/EOC.csv")
table(EOC$Express_Repair_PRESENT,EOC$Churn_In_Range_IND)
eoc.model<-glm(Churn_In_Range_IND ~ Express_Repair_PRESENT,family=poisson, data=EOC) ;summary(eoc.model)

# Get Binary tree model
library(rpart)#install.packages("rpart")
par(mfrow = c(1,2), xpd = NA)
eoc.rpart<-rpart(Churn_In_Range_IND ~ Express_Repair_PRESENT,data=EOC)
plot(eoc.rpart)




library(openxlsx)#install.packages("xlsx");install.packages("openxlsx")
dataLab1<-read.xlsx("/mnt/data/EOC/EOC_CSV_DATA_LABS_DATA_ONLY.xlsx",sheetIndex=1,header=TRUE)
dataLab2<-read.xlsx("/mnt/data/EOC/EOC_CSV_DATA_LABS_DATA_ONLY.xlsx",sheet=2,colNames=TRUE)
dataLab3<-read.xlsx("/mnt/data/EOC/EOC_CSV_DATA_LABS_DATA_ONLY.xlsx",sheet=3,colNames=TRUE)
nita<-read.xlsx("/mnt/data/EOC/NTIA_Historical_Options_NTIA_Append_20140626.xlsx",sheetIndex=1,header=TRUE)
gc()