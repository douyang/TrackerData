#############
# David Ouyang
# Ouyangd@stanford.edu
# 11-26-2017
#############

install.packages(c("ggplot2", "plyr", "stringr", "reshape2", "scales", "lubridate"))

library(ggplot2)
library(plyr)
library(reshape2)
#library(strptime)
library(stringr)
library(scales)
library(lubridate)

setwd("C:\\Users\\David\\Dropbox\\StatisticalAnalysis\\Tracker Code Blue")
dir()

# This file has the first two lines removed so its a well structured CSV file

data <- read.csv("DataOnly3-24-17.csv")
data$formattedTime <- as.POSIXct(strptime(data$EventOccurredOnLocal, "%m/%d/%Y %H:%M"))
data$duration <- as.numeric(as.difftime(as.character(data$DurationSeconds, "%H:%M:%S")))
str(data)

data <- data[data$EventCategory1 == "Locating",]

table(data$Location)
#write.csv(table(data$Location), "Locations.csv")

###### Will manually create file for visualization with geographic data
#
#   x, y         1        2          3         4 
#     1         B301    B302       B303      B304
#     2       HW B301
#     3                              NS        Pyxis
#     4                              NS        Pyxis
#     5       HW C301
#     6         C301    C302       C303      C304
#  	7,8,9  for C2
##############


locations <- read.csv("LocationsWithCoordinates.csv")
str(locations)

ggplot(data = locations[locations$Freq > 0,], aes(x = x,y = y,label = Var1)) + geom_text(check_overlap = TRUE, size = 4)
#ggsave("LocationsLayout.png", width = 13.9, height = 8.7)



smallData <- data.frame(Location = data$Location, formattedTime = data$formattedTime, User = data$LocatedStaffFullNameLastFirstMiddle1, duration = data$duration)


smallDataWithLocation <- merge(smallData, locations, by.x = "Location", by.y = "Var1", all.x = TRUE) 
str(smallDataWithLocation)

qplot(smallDataWithLocation[smallDataWithLocation$User == "Float-Pool- Rn, Lucy ",]$formattedTime, binwidth = 5*60)

ggplot(smallDataWithLocation[smallDataWithLocation$User == "Float-Pool- Rn, Lucy ",]) + geom_path(aes(x,y, color = as.numeric(formattedTime ))) + 
 scale_colour_gradient()
#ggsave("FloatRNPath.png", width = 13.9, height = 8.7)

#install.packages("installr")
library(installr)

#install.packages("Rtools")
#setInternet2(TRUE)
#updateR()
#install.packages("devtools")
library(devtools)
#install.packages("withr")
#devtools::install_github("RcppCore/Rcpp")
#devtools::install_github("dgrtwo/gganimate")
#install.packages("animation")
#install.packages("ImageMagick")
# Installed imagemagick seperately

library(gganimate)
library(animation)

ani.options(convert = "C:/PROGRA~1/ImageMagick-7.0.7-Q16/magick.exe") 

ggplot(smallDataWithLocation[smallDataWithLocation$User == "Float-Pool- Rn, Lucy ",], aes(x,y, size = duration, color = duration)) + 
geom_jitter()
#ggsave("FloatRNOverallTimeSpent.png", width = 13.9, height = 8.7)



smallDataWithLocation$random <- floor(runif(63905, min = 1, max = 5))
onePerson <- smallDataWithLocation[smallDataWithLocation$User == "Float-Pool- Rn, Lucy ",]
secondPerson <- smallDataWithLocation[smallDataWithLocation$User == "Casey, Lisa ",]


topSix = c("Float-Pool- Rn, Lucy ", "Casey, Lisa ", "Kautz, Acacia ", "Patel, Arpita ", "Tsang, Jennifer ") # "Bryan, Alexis ",
twoPerson <- smallDataWithLocation[smallDataWithLocation$User %in% topSix,]


theme_set(theme_bw())
p <- ggplot(onePerson, aes(x,y, size = duration, color = duration)) + geom_point()
animation <- ggplot(onePerson, aes(x,y, size = duration, color = duration, frame = random)) + geom_point()
gganimate(animation)
gganimate(animation,interval = .2, "output-random.gif")


animation <- ggplot(onePerson, aes(x,y, size = duration, color = duration, frame = formattedTime)) + geom_point()
#gganimate(animation)
gganimate(animation,interval = .2, "OneNurseOverTime.gif")


animation <- ggplot(secondPerson, aes(x,y, size = duration, color = duration, frame = formattedTime)) + geom_point()
#gganimate(animation)
gganimate(animation,interval = .2, "TwoNurseOverTime.gif")



ggplot(twoPerson , aes(x,y, size = duration, color = User)) + 
geom_jitter() + scale_color_discrete(labels=c("RN1","RN2","RN3", "RN4", "RN5", "RN6"))
#ggsave("SixNursesOverallTimeSpent.png", width = 13.9, height = 8.7)


topTwo= c( "Kautz, Acacia ", "Bryan, Alexis ")
twoPerson <- smallDataWithLocation[smallDataWithLocation$User %in% topTwo,]

twoPerson <- twoPerson[twoPerson$formattedTime > strptime("03/24/2017 12:00:00", "%m/%d/%Y %H:%M:%S"),]
length(unique(twoPerson$formattedTime))

animation <- ggplot(twoPerson, aes(x,y, size = duration, color = User, frame = formattedTime)) + geom_point() +
scale_color_discrete(labels=c("RN1","RN2","RN3", "RN4", "RN5"))#, "RN6"))
#gganimate(animation)
gganimate(animation,interval = .2, "FiveSimultaneousNursesOverTime.gif")

qplot(twoPerson$formattedTime, fill = twoPerson$User)
ggsave("TimeDistribution.png")





) + geom_path(aes(x,y, color = as.numeric(formattedTime ))) + 
 scale_colour_gradient()




qplot(data = smallDataWithLocation, aes(x,y,group = User, color = formattedTime))

table(data$LocatedStaffFullNameLastFirstMiddle1)
head(sort(table(data$LocatedStaffFullNameLastFirstMiddle1), decreasing = TRUE))








