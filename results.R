library(ggplot2)
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


table(genetics$Result)


 
posresult <- c("positive","Positive","Positive/VUS","positive/VUS","Positive/VUS/VUS","VLP","VLP/VUS")
negresult <- c("Negative")
varresult <- c("VUS","VUS/VUS","VUS/VUS/VUS","VUS/VUS/VUS/VUS")

genetics$Result <- ifelse(genetics$Result %in% posresult, "Positive",genetics$Result)
genetics$Result <- ifelse(genetics$Result %in% negresult, "Negative",genetics$Result)
genetics$Result <- ifelse(genetics$Result %in% varresult, "Varient",genetics$Result)


results <- as.data.frame.table(table(genetics$Result,genetics$Quarter))

names(results) <- c("Result","Quarter","Patients")

results <- results[results$Result != "pending",]

results$Result <- factor(results$Result, levels=c("Positive","Varient","Negative"))

results <- aggregate(results$Patients, by=list(results$Result), FUN=sum)
names(results) <- c("Result","Patients")

ggplot(data = results, aes(x = factor(Result) ,y = Patients,fill = Result)) +
  geom_bar(stat="identity",colour="black") + theme(axis.line = element_line(size = 0), 
    axis.ticks = element_line(linetype = "blank"), 
    panel.grid.major = element_line(colour = NA), 
    panel.grid.minor = element_line(colour = NA), 
    axis.title = element_text(size = 17, 
        face = "bold", colour = "gray23"), 
    axis.text = element_text(size = 16, face = "bold", 
        colour = "gray36", hjust = 0.75), 
    axis.text.y = element_text(size = 0), 
    plot.title = element_text(size = 21, 
        face = "bold", colour = "gray23"), 
    panel.background = element_rect(fill = "white"), 
    plot.background = element_rect(fill = "white"), 
    legend.position = "none") +labs(title = "Genetic Testing Results by Outcome (July 2014 - June 2016)", 
    x = NULL, y = NULL) + scale_fill_manual(values=c("#46c691","#49c9c4","#4da1cc")) + theme(axis.text = element_text(size = 25, 
    hjust = 0.5))+
  geom_text(aes(label=results$Patients), position="identity", vjust=-0.25,size = 7) 
 + theme(axis.text = element_text(size = 21))
