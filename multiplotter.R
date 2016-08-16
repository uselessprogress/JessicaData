locqt <- read.csv("locqt.csv", header=TRUE)


library(ggplot2)
library(zoo)



CustomPallet <- c("#44c577","#46c691","#47c8ab","#49c9c4","#4bb8cb","#4da1cc","#4e8bce","#5075cf")

locqt <- locqt[locqt$Quarter != "Q3.2016",]

locqt$Quarter <- factor(locqt$Quarter,levels =c("Q3.2014","Q4.2014","Q1.2015","Q2.2015","Q3.2015","Q4.2015","Q1.2016","Q2.2016"))

locqt <- locqt[order(locqt$Quarter),]





CBS <- ggplot(data= locqt[locqt$Location=="CBS",], aes(x=factor(Quarter), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#44c577")+ 
  geom_point(colour="black", size=3, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 8, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 8, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 7, vjust = 0), plot.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white",linetype = "solid")) + 
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(0,10)  +
  labs(title = "CBS", x = NULL, y = "Number of Patients")


Flaget <- ggplot(data= locqt[locqt$Location=="Flaget",], aes(x=factor(Quarter), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#47c8ab")+ 
  geom_point(colour="black", size=3, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 8, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 8, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 7, vjust = 0), plot.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white",linetype = "solid")) + 
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(0,8)  +
  labs(title = "Flaget", x = NULL, y = NULL)



SJC <- ggplot(data= locqt[locqt$Location=="SJC",], aes(x=factor(Quarter), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#46c691")+ 
  geom_point(colour="black", size=3, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 8, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 8, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 7, vjust = 0), plot.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white",linetype = "solid")) + 
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(0,8)  +
  labs(title = "Corbin", x = NULL, y = "Number of Patients")








MCNE <- ggplot(data= locqt[locqt$Location=="MCNE",], aes(x=factor(Quarter), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#5075cf")+ 
  geom_point(colour="black", size=3, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 8, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 8, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 7, vjust = 0), plot.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white",linetype = "solid")) + 
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(0,8)  +
  labs(title = "Jewish Northeast", x = NULL, y = NULL)


MCE <- ggplot(data= locqt[locqt$Location=="MCE",], aes(x=factor(Quarter), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#4e8bce")+ 
  geom_point(colour="black", size=3, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 8, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 8, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 7, vjust = 0), plot.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white",linetype = "solid")) + 
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(0,40)  +
  labs(title = "Jewish East", x = NULL, y = "Number of Patients")



SJE <- ggplot(data= locqt[locqt$Location=="SJE",], aes(x=factor(Quarter), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#4bb8cb")+ 
  geom_point(colour="black", size=3, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 8, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 8, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 7, vjust = 0), plot.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white",linetype = "solid")) + 
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(0,100)  +
  labs(title = "St. Joe East", x = NULL, y = "Number of Patients")





SJH <- ggplot(data= locqt[locqt$Location=="SJH",], aes(x=factor(Quarter), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#49c9c4")+ 
  geom_point(colour="black", size=3, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 8, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 8, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 7, vjust = 0), plot.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white",linetype = "solid")) + 
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(0,4)  +
  labs(title = "St. Joe Main", x = NULL, y = "Number of Patients")




SJL <- ggplot(data= locqt[locqt$Location=="SJL",], aes(x=factor(Quarter), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#4da1cc")+ 
  geom_point(colour="black", size=3, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 8, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 8, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 7, vjust = 0), plot.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white",linetype = "solid")) + 
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(0,6)  +
  labs(title = "London", x = NULL, y = NULL)


UL <- ggplot(data= locqt[locqt$Location=="WCEC",], aes(x=factor(Quarter), y=Patients, group=Location)) +
  geom_line(aes(colour = Location), size=0.75) +scale_color_manual(values="#4da1cc")+ 
  geom_point(colour="black", size=3, shape=21, fill="white") + 
  theme(panel.grid.major = element_line(colour = "lightblue"), 
        axis.title = element_text(size = 8, face = "bold", colour = "gray35"), 
        axis.text = element_text(size = 8, colour = "gray35", vjust = 0.25), 
        axis.text.x = element_text(size = 7, vjust = 0), plot.title = element_text(size = 14, face = "bold", colour = "gray35"), 
        panel.background = element_rect(fill = "white"), 
        plot.background = element_rect(fill = "white",linetype = "solid")) + 
  guides(fill=FALSE) +
  theme(legend.title=element_blank()) +
  theme(legend.position="none") + ylim(0,20)  +
  labs(title = "U of L", x = NULL, y = NULL)


















mp <- multiplot(CBS,
          SJH,
          MCE,
          
          SJC,
          SJE,
          MCNE,
          
          Flaget,
          SJL,UL, cols=2)




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






