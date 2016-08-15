library(ggplot2)
library(zoo)
library('Cairo')

genetics <- read.csv("working_data.csv", header=TRUE, stringsAsFactors = FALSE, na=c(""," ","NA","N/A","na","n/a"))


genetics$DOS <- as.Date(genetics$DOS, "%Y-%m-%d")


genetics["YearMon"] <-as.yearmon(genetics$DOS)

genetics <- genetics[genetics$DOS >= as.Date("07/01/2015","%m/%d/%Y"),]



ptxmonth <- data.frame(table(genetics$YearMon))
names(ptxmonth) <- c("yrMon", "Patients")
ptxmonth <- ptxmonth[ptxmonth$Patients != 0,]





ptxloc <- data.frame(table(genetics$Location, genetics$YearMon))
names(ptxloc) <- c("Location","yrMon","Patients")
ptxloc <- ptxloc[ptxloc$Patients != 0,]

table(ptxloc$yrMon)



### Develop graphics for analysis


CairoWin()
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

ggsave(file="ptxmonth.png", type="cairo-png", width = 12, height = 5, units ="in",dpi = 300)



### pt by loc

CairoWin()
CustomPallet <- c("#44c577","#46c691","#47c8ab","#49c9c4","#4bb8cb","#4da1cc","#4e8bce","#5075cf")

CBS <- ggplot(data= ptxloc[ptxloc$Location=="CBS",], aes(x=as.factor(yrMon), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#44c577")+ 
  geom_point(colour="black", size=2, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 12, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 12, vjust = 0), plot.title = element_text(size = 17, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white",linetype = "solid")) +
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(1,5) + xlim("Jul 2015","Oct 2015", "Jan 2016","Apr 2016","Jul 2016") +
  labs(title = "CBS", x = NULL, y = "Number of Patients")



Flaget <- ggplot(data= ptxloc[ptxloc$Location=="Flaget",], aes(x=as.factor(yrMon), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#46c691")+ 
  geom_point(colour="black", size=2, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 12, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 12, vjust = 0), plot.title = element_text(size = 17, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white", linetype = "solid")) +
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(1,5) +
  labs(title = "Flaget", x = NULL, y =NULL)


SJC <- ggplot(data= ptxloc[ptxloc$Location=="SJC",], aes(x=as.factor(yrMon), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#47c8ab")+ 
  geom_point(colour="black", size=2, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 12, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 12, vjust = 0), plot.title = element_text(size = 17, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white", linetype = "solid")) +
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(1,5) +
  labs(title = "Corbin", x = NULL, y =NULL)


multiplot(CBS,Flaget,SJC, cols=3)














ggsave(file="ptxloc.png", type="cairo-png", width = 12, height = 5, units ="in",dpi = 300)



multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}









