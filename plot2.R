library(data.table)

getwd()

destfile <- "./week1/Dataset.zip"
outDir <- "./week1/uitgepakt"
urlie <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

if(!dir.exists("week1")) {
  dir.create("week1")
} 

download.file(urlie, destfile)
unzip(destfile, exdir=outDir)

file <- list.files(outDir, recursive = T)
Dataset <- fread(paste0(outDir, "/", file))

# just a check
Dataset[, .N]
Dataset[, .N, Date]

# making the subset, only containing data from the dates 2007-02-01 and 2007-02-02
Databoy <- Dataset[Date %in% c('1/2/2007', '2/2/2007')]

# a check again
Databoy[, .N, Date]
str(Databoy)

# I want to create a new variabele that combines date and time in the right format, since I am not very handy 
# I need a lot of intermediate steps to achieve this

Databoy[, ORG_Date := Date]
Databoy[, ORG_Time := Time]

Databoy[, Date := strptime(ORG_Date, "%d/%m/%Y")]

# check to check if conversion went right
Databoy[,.N, .(Date, ORG_Date)]
str(Databoy)

# combine date and time in one variabele
Databoy$Supertime <- paste(Databoy$Date, Databoy$Time)

# convert into right format
Databoy[, Supertime := strptime(Supertime, "%Y-%m-%d %H:%M:%S")]

# final checks on date and time
class(Databoy$Supertime)
Databoy[,.N, .(Supertime, Date, Time)]

# Some variables to be plotted must be converted from character to numeric value, otherwise the plots will explode.
# First a selection of these columns, then defining a function that helps converting, finally an action that
# makes the actual conversion happen:

columnboys <- names(Databoy)[3:9]
class(columnboys)
convertboy <- function(x) as.numeric(x)

# conversion applied to selected columns
Databoy[,(columnboys):=lapply(.SD, convertboy), .SDcols = columnboys]

# check to see if conversion happened
str(Databoy)


### Now it is finally time to make a plot

# plot 2

# first got to change the language, I am from the Netherdutchlands and there we have different abbreviations for weekdays
Sys.setlocale("LC_TIME", "English_UK")

with(Databoy, plot(x = Supertime, y = Global_active_power, ylab = "Global Active Power (kilowatts)", xlab = "", type = "l"))

dev.copy(png, file = "plot2.png", width = 480, height = 480)
dev.off()