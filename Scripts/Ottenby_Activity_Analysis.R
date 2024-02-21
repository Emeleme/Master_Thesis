################################################################################
# Apr 2023 - Giuseppe.Bianco@biol.lu.se - CAnMove - Lund University
################################################################################
setwd("E:/Users/mlmah/OneDrive/Documentos/MLME/Maestria/Animal Ecology 2022-2024/Tesis/BackUp_Ottenby")
################################################################################
library(plyr)
library(RSQLite)
library(ggplot2)
################################################################################
main <- function()
{
  make.raw.dataset()
  load("Ottenby_Activity_Dataset_RAW.RData", .GlobalEnv)
  plot.raw.data()
}

### Build Dataset ##############################################################
make.raw.dataset <- function()
{
  # Connect to db
  con <- dbConnect(drv=RSQLite::SQLite(), dbname="ottenby2023-05-29.db")
  db <- dbReadTable(con, "activity")
  
  # Covert channels to bits
  db <- cbind(db, t(sapply(db$channel, function(x){as.numeric(intToBits(x)[1:8])})))
  
  # Rename channels
  names(db)[4:11] <- c("PirA", "VibA", "PirB", "VibB","PirC", "VibC","PirD", "VibD")
  
  # Add database entry number
  db$Entry <- 1:nrow(db)
  
  # Make a long format
  db <- ddply(db, .(Entry, DateTime=ts), summarise,
               Cage = paste(substr(data, 3, 3), LETTERS[1:4], sep = ""),
               PIR = c(PirA, PirB, PirC, PirD),
               VIB = c(VibA, VibB, VibC, VibD))
  db$DateTime <- as.POSIXct(db$DateTime)
  
  # Save dataset
  db.raw <- db
  save(db.raw, file="Ottenby_Activity_Dataset_RAW.RData")
  print("File Saved!")
}

### Plot Raw Data Dataset ######################################################
plot.raw.data <- function(db = db.raw)
{
  # db$VIB <- !db$VIB
  db <- subset(db, substr(Cage, 1, 1) != 3) # Remove sensor 3
  db <- subset(db, PIR == 1) # Get only ON
  db$Cage <- paste("Cage", db$Cage)
  
  # Convert Time with today date!
  db$Time <- format(as.POSIXct(db$DateTime) ,format="%H:%M")
  db$Time <- as.POSIXct(db$Time, format="%H:%M")
  
  # Get Date too
  db$Date <- as.Date(db$DateTime)
  
  # plot
  pdf(file="Figure_Actogram_PIR.pdf", width=12, height=12)
  
  p1 <- ggplot(db, aes(Time, Date)) + geom_tile(aes(fill = PIR)) + ## <---
    scale_y_reverse() +
    scale_x_datetime(date_labels = "%H:%M", date_minor_breaks = "1 hour", 
                     expand = c(0, 100)) + xlab("Local Time (UTC +2)") +
    scale_y_date(date_labels = "%b %d", breaks = "1 day", expand=c(0, 0)) +
    facet_wrap(Cage~., ncol=4) + theme_bw() + theme(legend.position="none")
  
  print(p1)
  dev.off( )
}
################################################################################
main()
################################################################################

