library(ggplot2)
genetics <- read.csv("working_data.csv", header=TRUE, stringsAsFactors = FALSE, na=c(""," ","NA","N/A","na","n/a"))

genetics$DOS <- as.Date(genetics$DOS, "%m/%d/%y")
genetics <- genetics[genetics$DOS >= as.Date("07/01/2014","%m/%d/%Y"),]


posresult <- c("positive","Positive","Positive/VUS","positive/VUS","Positive/VUS/VUS","VLP","VLP/VUS")
negresult <- c("Negative")
varresult <- c("VUS","VUS/VUS","VUS/VUS/VUS","VUS/VUS/VUS/VUS")

genetics$Result <- ifelse(genetics$Result %in% posresult, "Positive",genetics$Result)
genetics$Result <- ifelse(genetics$Result %in% negresult, "Negative",genetics$Result)
genetics$Result <- ifelse(genetics$Result %in% varresult, "Varient",genetics$Result)


results <- as.data.frame.table(table(genetics$Result,genetics$Gene))

names(results) <- c("Result","Gene","Patients")

positives <- results[results$Result == "Positive" & results$Patients !=0,]



positives$Gene <- ifelse(positives$Patients <=1,"Other",as.character(positives$Gene))


positives <- aggregate(positives$Patients, by=list(as.character(positives$Gene)), FUN=sum)
names(positives) <- c("Gene","Patients")
positives["Result"] <- "Positive"

positives <- positives[order(positives$Patients),]

derp <- c("#449ebd",
  "#458dbe",
  "#467cbf",
  "#476bc0",
  "#495ac2",
  "#4b4ac3",
  "#5e4bc4",
  "#724dc5",
  "#854ec7",
  "#984fc8","#46c691","#49c9c4")
library(plyr)
library(RColorBrewer)
positives <- ddply(positives, .(Result), 
              transform, pos = cumsum(Patients) - (0.5 * Patients)
)
Lab.palette <- colorRampPalette(c("#f2f2f2", "#bc438c"), space = "Lab")


ggplot(positives, aes(x = Result, y = Patients)) +
  geom_bar(aes(fill = Gene), stat="identity") + scale_fill_manual(values=Lab.palette(13)) +
  geom_text(aes(label = paste(Patients,Gene,sep="\n"), y = pos), size = 4) + theme(panel.grid.major = element_line(linetype = "blank"), 
    panel.grid.minor = element_line(linetype = "blank"), 
    axis.title = element_text(size = 15, 
        face = "bold"), axis.text = element_text(size = 16, 
        face = "bold"), plot.title = element_text(size = 22, 
        face = "bold"), panel.background = element_rect(fill = NA), 
    plot.background = element_rect(fill = "white", 
        colour = NA), legend.position = "none") +labs(title = "Positive Results by Gene", 
    x = NULL, y = "Positive Results")+
  coord_flip()

  
                   