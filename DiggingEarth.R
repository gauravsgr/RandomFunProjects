#--------------------------------------------------------------------------#
#        Digging the hole throught the center of the Earth                 #
#--------------------------------------------------------------------------#

library(xlsx)
library(ggmap)
library(mapdata)
library(ggplot2)

#The xls file is in http://esa.un.org/unpd/wup/CD-ROM/WUP2011-F13-Capital_Cities.xls
Capitals <- read.xlsx("D:\\R_Working_Dir\\InputData\\WUP2011-F13-Capital_Cities.xls", sheetName="Capital_Cities", startRow=13,header=TRUE)
names(Capitals) = gsub("\\.", "", names(Capitals))

#Obtain symmetric coordinates for each capital
Capitals$LatitudeSym <- -Capitals$Latitude
Capitals$LongitudeSym <- -sign(Capitals$Longitude)*(180-abs(Capitals$Longitude))

#Using the google maps api to perform reverse Geocoding and applying 1 to the drowned cities
Capitals$DigResult <- apply(Capitals, 1, function(x) {unlist(revgeocode(c(as.numeric(x[11]),as.numeric(x[10]))))})
Capitals$Drowned <- is.na(Capitals$DigResult)*1

#Percentage of population drowned
sum(Capitals$Drowned*Capitals$Populationthousands)/sum(Capitals$Populationthousands)

#Representing graphically
world <- map_data("world")

opt <- theme(legend.position="none",
             axis.ticks=element_blank(),
             axis.title=element_blank(),
             axis.text =element_blank(),
             plot.title = element_text(size = 35),
             panel.background = element_rect(fill="turquoise1"))
p <- ggplot()
p <- p + geom_polygon(data=world, aes(x=long, y=lat, group = group),colour="white", fill="lightgoldenrod2" )
p <- p + geom_point(data=Capitals, aes(x=Longitude, y=Latitude, color=Drowned, size = Populationthousands)) + scale_size(range = c(2, 20), name="Population (thousands)")
p <- p + labs(title = "What if you dig a hole through the Earth?")
p <- p + scale_colour_gradient(low = "brown", high = "deepskyblue2")
p <- p + annotate("rect", xmin = -135, xmax = -105, ymin = -70, ymax = -45, fill = "white")
p <- p + annotate("text", label = "Drowned", x = -120, y = -60, size = 6, colour = "deepskyblue2")
p <- p + annotate("text", label = "Saved", x = -120, y = -50, size = 6, colour = "brown")
p <- p + geom_point(aes(x = -120, y = -65), size=8, colour="deepskyblue2")
p <- p + geom_point(aes(x = -120, y = -55), size=8, colour = "brown")
p + opt
