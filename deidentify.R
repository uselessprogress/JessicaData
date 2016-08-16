
### Read in excel sheet and deidentify patients
library(xlsx)
Identified_Data <- read.csv("patient database.csv", header=T, stringsAsFactors = FALSE)
Identified_Data$PatID <- as.integer(unclass(factor(Identified_Data$Patient)))

Working_Data <- data.frame(Identified_Data[,2:14])
write.csv(Working_Data, "working_data.csv", row.names = FALSE)

