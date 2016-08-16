#--------------------------------------------------------------------------#
#                    Doing the Kmeans on JPEG images                       #
#--------------------------------------------------------------------------#

# Load the package
install.packages('jpeg')
library(jpeg)
url <- "http://www.wall321.com/thumbnails/detail/20120304/colorful%20birds%20tropical%20head%203888x2558%20wallpaper_www.wall321.com_40.jpg"

# Download the file and save it as "Image.jpg" in the directory
dFile <- download.file(url, "Image.jpg")
# Read the image from the working directory downloaded above
img <- readJPEG("Image.jpg") 
img1 <- readJPEG("Image.jpg", TRUE)
plot(img)
# Get the image dimensions
imgDm <- dim(img)
str(img)

# Assign the RGB channels to data frame
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3])
)

m = as.matrix(x = rep(1:5), y = seq(1:5), z = rep(5:3))
dim(m)
head(imgRGB)
library(ggplot2)

# Function to create the ggplot theme for plotting the image on canvas
drawImage <- function() {
  theme(
    panel.background = element_rect(
      size = 3,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      size = 2),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      size = rel(1.2),
      face = "bold"),
    axis.title.y = element_text(
      size = rel(1.2),
      face = "bold"),
    plot.title = element_text(
      size = 20,
      face = "bold",
      vjust = 1.5)
  )
}

# Plot the image
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
  labs(title = "Original Image: facebook Profile Pic") +
  xlab("x") +
  ylab("y") +
  drawImage()

# Performing the clustering and plotting again
kClusters <- 12
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y") + 
  drawImage()