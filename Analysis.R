library(ggplot2)
library(zoo)


genetics <- read.csv("working_data.csv", header=TRUE, stringsAsFactors = FALSE, na=c(""," ","NA","N/A","na","n/a"))


genetics$DOS <- as.Date(genetics$DOS, "%m/%d/%y")


genetics["YearMon"] <-as.yearmon(genetics$DOS)

genetics["Quarter"] <- NA


genetics$Quarter <- ifelse(genetics$DOS >= as.Date("07/01/2014", "%m/%d/%Y") 
                           & genetics$DOS <= as.Date("9/30/2014", "%m/%d/%Y"),"Q3.2014",NA)

genetics$Quarter <- ifelse(is.na(genetics$Quarter)==TRUE & genetics$DOS >= as.Date("10/01/2014", "%m/%d/%Y") 
                           & genetics$DOS <= as.Date("12/31/2014", "%m/%d/%Y"),"Q4.2014",genetics$Quarter)


genetics$Quarter <- ifelse(is.na(genetics$Quarter)==TRUE & genetics$DOS >= as.Date("01/01/2015", "%m/%d/%Y") 
                           & genetics$DOS <= as.Date("03/31/2015", "%m/%d/%Y"),"Q1.2015",genetics$Quarter)

genetics$Quarter <- ifelse(is.na(genetics$Quarter)==TRUE & genetics$DOS >= as.Date("04/01/2015", "%m/%d/%Y") 
                           & genetics$DOS <= as.Date("06/30/2015", "%m/%d/%Y"),"Q2.2015",genetics$Quarter)


genetics$Quarter <- ifelse(genetics$DOS >= as.Date("07/01/2015", "%m/%d/%Y") 
                         & genetics$DOS <= as.Date("9/30/2015", "%m/%d/%Y"),"Q3.2015",genetics$Quarter)

genetics$Quarter <- ifelse(is.na(genetics$Quarter)==TRUE & genetics$DOS >= as.Date("10/01/2015", "%m/%d/%Y") 
                           & genetics$DOS <= as.Date("12/31/2015", "%m/%d/%Y"),"Q4.2015",genetics$Quarter)

genetics$Quarter <- ifelse(is.na(genetics$Quarter)==TRUE & genetics$DOS >= as.Date("01/01/2016", "%m/%d/%Y") 
                           & genetics$DOS <= as.Date("03/31/2016", "%m/%d/%Y"),"Q1.2016",genetics$Quarter)

genetics$Quarter <- ifelse(is.na(genetics$Quarter)==TRUE & genetics$DOS >= as.Date("04/01/2016", "%m/%d/%Y") 
                           & genetics$DOS <= as.Date("06/30/2016", "%m/%d/%Y"),"Q2.2016",genetics$Quarter)

genetics$Quarter <- ifelse(is.na(genetics$Quarter)==TRUE & genetics$DOS >= as.Date("07/01/2016", "%m/%d/%Y") 
                           & genetics$DOS <= as.Date("9/30/2016", "%m/%d/%Y"),"Q3.2016",genetics$Quarter)






genetics <- genetics[genetics$DOS >= as.Date("07/01/2014","%m/%d/%Y"),]





locqt <- as.data.frame.table(table(genetics$Location,genetics$Quarter))
names(locqt) <- c("Location","Quarter","Patients")
write.csv(locqt,"locqt.csv",row.names = FALSE)

WCEC <- UofL

ptxmonth <- data.frame(table(genetics$YearMon))
names(ptxmonth) <- c("yrMon", "Patients")
ptxmonth <- ptxmonth[ptxmonth$Patients != 0,]















### Develop graphics for analysis



ggplot(data= ptxmonth, aes(x=as.factor(yrMon), y=Patients, group=0)) +
  geom_line(colour="black", linetype="solid", size=0.75) +
  geom_point(colour="black", size=2, shape=21, fill="white") + theme(panel.grid.major = element_line(colour = "lightblue"), 
    axis.title = element_text(size = 14, 
        face = "bold", colour = "gray35"), 
    axis.text = element_text(size = 12, colour = "gray35", 
        vjust = 0.25), axis.text.x = element_text(size = 12, 
        vjust = 0), plot.title = element_text(size = 17, 
        face = "bold", colour = "gray35"), 
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white", 
        linetype = "solid")) +labs(title = "Number of Patients by Month (2015 - 2016)", 
    x = NULL, y = "Number of Patients")






