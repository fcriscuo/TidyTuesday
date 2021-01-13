#TidyTuesday tutorial  2021Jan12
#source: https://josephlewis.github.io/aspect.html

if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, scales, caTools, randomForest, caret, e1071)
# read in the artworks file
artworks <- read.csv("the-tate-collection.csv", stringsAsFactors = FALSE, sep = ";")

str(artworks)
#remove uneeded columns
drop <- c("accession_number", "artistRole", "artistId", "dateText", 
          "creditLine", "units", "inscription", "thumbnailCopyright",
          "thumbnailUrl", "url")
artworks_rem <- artworks[,  !(names(artworks) %in% drop)]
str(artworks_rem)
ggplot(artworks_rem) + 
  geom_density(aes(year, fill = "red"), alpha = 0.3) + 
  geom_density(aes(acquisitionYear, fill = "blue"), alpha = 0.3) +
  scale_fill_manual(name = NULL, values = c("red" = "red", "blue" = "blue"), 
                    labels=c("blue" = "Acquisition Year", "red" = "Year Artwork Created")) + 
  theme_bw()

# Year of Artwork Creation versus Year Acquired by Tate Gallery
#cut() converts a numeric value to a factor defined inby breaks
artworks_rem$halfcentury <- cut(artworks_rem$acquisitionYear, breaks = c(1800, 1850, 1900, 1950, 2000, 2050),
                                labels= c("1800-1849", "1850-1899", "1900-1949", "1950-1999","2000-2050"),
                               include.lowest = TRUE )
ggplot(artworks_rem, aes(year))+
  geom_histogram(aes(fill=halfcentury), bins =50) +
  labs(fill = "Year Artwok Acquired", y = "Count of Artworks") +
  theme_bw()

#Pre-Tate Gallery Artwork Acquistion
artworks_pre_tate <- artworks_rem %>%
  filter(year < 1897)

ggplot(artworks_pre_tate) +ß
  geom_histogram(aes(year), bin = 30) +
  theme_bw()

#look at the artists in the pre-Tate collection
sort(table(artworks_pre_tate$artist), decreasing = T)[1:5]

#What percentage of the pre-Tate collection belongs to Turner
sort(table(artworks_pre_tate$artist), decreasing = T)[1] / sum(table(artworks_pre_tate$artist))

#visualize this
#n.b. plotting a boolean (paint is/is not a Turner)
ggplot(artworks_pre_tate) +
  geom_histogram(aes(year, fill = artist == "Turner, Joseph Mallord William"), bins = 30) +
  labs(fill = "Artwork by JMW Turner") +
  theme_bw()

#Pre-tate Turners
artworks_turner <- artworks_pre_tate[artworks_pre_tate$artist == "Turner, Joseph Mallord William",]
#Plot year acquired for Turners
ggplot(artworks_turner) +
  geom_histogram(aes(year, fill = halfcentury), bins = 30) +
  labs(fill = "Year Artwork Acquired", y= "Count of Artworks")+
  theme_bw()

#Explore the Aspect Ratio (height of artwork / width of artwork) over time
#create a specialized dataframe
#n.b The artwork with an AR of 3750 is a specialized piece; not an outlier
artworks_ar <- artworks_rem [ !(is.na(artworks_rem$height & artworks_rem$width & artworks_rem$year)),]
artworks_ar$aspectratio <-  artworks_ar$height / artworks_ar$width
ggplot(artworks_ar) +
  geom_point(aes(year,aspectratio)) +
  theme_bw()

#ARs increase around 1950 => Postmoderism
artworks_ar_1950 <- artworks_ar %>%
  filter(year >= 1950)
ggplot(artworks_ar_1950) +
  geom_point(aes(year,aspectratio)) +
  theme_bw()

#visualize the actual shape of the paintings
artworks_ar$postmodern <-  ifelse(artworks_ar$year >= 1950, 'yes', 'no' )
artworks_ar <- artworks_ar[rev(order(artworks_ar$postmodern)),]
ggplot(artworks_ar, aes(xmin = 0, xmax= width, ymin = 0, ymax = height, color = postmodern)) +
  geom_rect(alpha = 0) +
  labs(color="Postmodern?", x= "Width", y = "Height")+
  theme_bw()

#Use the Aspect Ratio to predict the year of creation
artworks_ar$century <- cut(artworks_ar$year, breaks = c(1500,1550,1600,1650,1700,1750,1800,1850,1900,1950,2000,2050),
                           labels = c("1500-1549", "1550-1600","1600-1649","1650-1699","1700-1749","1750-1799",
                                      "1800-1849","1850-1899","1900-1949", "1950-1999","2000-2050"), 
                           include.lowest = TRUE)
sample <- sample.split(artworks_ar$century, SplitRatio = 0.7)
train <- artworks_ar[sample,]
test <- artworks_ar[!sample,]
rf <- randomForest(train$century ~ aspectratio, ntree = 500, data = train)
print(rf)

train$predicted_response <- predict(rf, train)
print(confusionMatrix(data = train$predicted_response, reference = train$century))
                                                      
test$predicted_response <- predict(rf, test)
print(confusionMatrix(data=test$predicted_response, reference = test$century))




