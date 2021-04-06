
# ---- installing the packages required for this assignment.
install.packages("ggplot2")
# ---- Enabling Libraries
library("ggplot2")

# --------------- Part 1 -------------------------------------------------------
# --------------- Import data into data frame
# ---- Data is downloaded from the link mentioned below
# ---- https://www.cso.ie/en/media/csoie/census/documents/saps2011files/AllThemesTablesCTY.csv

dataset <- read.csv("AllThemesTablesCTY.csv", 
                    header = TRUE)

# --------------- EDA - Exploring data

dataset
nrow(dataset)
ncol(dataset)
head(dataset, n=10)
tail(dataset, n= 6)
str(dataset)

# I have taken out male populatin data from the entire dataset.
# 1:34 are rows and 1:37 are columns
subset <- dataset[c(1:34),c(4:37)]

# sum of all the counties to get the total figure
totalPopulation <- colSums (subset, 
                            na.rm = FALSE, 
                            dims = 1)

# --------------- Histogram - R 
hist(subset$T1_1AGE0M, 
     main="Distribution of Population for AGE = 0 Gender = Male", 
     xlab="Population", 
     col = 'skyblue3', 
     breaks = 20, 
     ylim = c(0,10),
     xlim = c(0,4000), 
     las=1)

# --------------- Histogram GGPlot2

ggplot(subset, aes(T1_1AGE0M)) + 
  geom_histogram( binwidth = 100, 
                  colour = "black", 
                  fill = "skyblue3") +
  ggtitle("Distribution of Population for AGE = 0 Gender = Male")+
  labs(x = "Population", 
       y = "Frequency")


# ---------------- Scatter Plot - R
plot(subset$T1_1AGE0M,
     subset$T1_1AGE1M, 
     xlab = "Age: 0 Gender: Male",
     ylab = "Age: 1 Gender: Male",
     main = "Correlation between 0 and 1 year old Male poplation distribution by county",
     col = "red")

# ---------------- Scatter Plot GGPLOT2
ggplot(subset, 
       aes(T1_1AGE0M,T1_1AGE1M)) + 
  geom_point(colour = "skyblue3",size = 4, alpha = 0.5)+ 
  geom_smooth(method="lm",color="darkred", size = 0.5, alpha = 0.2)+
  ggtitle("Correlation between 0 and 1 year old Male poplation distribution by county")+
  labs(x = "Age: 0 Gender: Male", 
       y = "Age: 1 Gender: Male")


# ----------------- Part 2 -----------------------------------------------------
# ------------ Import data into data frame

customerSpend <- read.csv("Customer Spend.csv", 
                          header = TRUE, 
                          sep = ",")

# ------------ Promote First row as headers
# ----- Replace 'NA' with 'Year'
customerSpend[1,1] <- 'Year'
# ----- assign first row as names
names(customerSpend) <- customerSpend[1, ]
# ----- delete first row
customerSpend <- customerSpend[-c(1),]

# ------------ EDA - Exploring data

# ----- Objects can now be accessed by names.
# attach(customerSpend)
# ----- Print the data frame, number of rows & columns, first & last four rows, 
# ----- and finally structure.
customerSpend
nrow(customerSpend)
ncol(customerSpend)
head(customerSpend, n = 4)
tail(customerSpend, n = 4)
str(customerSpend)

# ----- replacing ',' with an empty string and typecasting from data type - 
# ----- character to numeric .
# ----- first column is not disturbed.
for ( colIndex in 1:8 ) {
  customerSpend[,colIndex] <- gsub("[[:punct:]]",
                                   '', 
                                   customerSpend[,colIndex])
  customerSpend[,colIndex] <- as.numeric(customerSpend[,colIndex])  
}
# ---- Years is declared as factor (earlier character)
customerSpend[,1] <- factor(customerSpend[,1])

# ------------ The R base graphics time series
customerSpendsts <- ts(customerSpend[,2:8], 
                       start = 2009, 
                       end = 2020, 
                       frequency = 1)
class(customerSpendsts)
# ---- Plot the time series
ts.plot(customerSpendsts,main="Time series analysis of Customer spends",
        gpars = list(xlab="year",
                     ylab="sales value",
                     lty=c(1:7)))

# ------------ GGPlot2

# ---- setting high penalty for scientific display
options("scipen"=99) 

# ---- time series using geom_line()
ggplot(data = customerSpend, aes(x = Year, group = 1)) + 
  geom_line(aes(y = Cisco, color = "Cisco")) +
  geom_line(aes(y = Dell, color = "Dell"),linetype="twodash") +
  geom_line(aes(y = BSI, color = "BSI"),linetype="longdash") +
  geom_line(aes(y = Medtronic, color = "Medtronic"),linetype="dotted") +
  geom_line(aes(y = IBM, color = "IBM"),linetype="dotdash") +
  geom_line(aes(y = Microsoft, color = "Microsoft"),linetype="dashed") +
  geom_line(aes(y = Amazon, color = "Amazon"),linetype="F1") +
  scale_color_discrete(name = "Companies") +
  ggtitle("Time series analysis of Customer spends")+
  labs(y = "Companies Sales Value", x = "Year")
  
 
# ----------------- Part 3 -----------------------------------------------------
# ------------ working with map using basic R, ggplot2 AND leaflet library.

# ---- installing the packages required for this part and Enabling them.

install.packages("readxl")
library(readxl)

# ---- load the data 
Cork_Towns <- read_excel("Cork Townlands.xlsx")
head(Cork_Towns)
str(Cork_Towns)

# ---- Filtering the data
Cork_Towns$LATITUDE <- round(Cork_Towns$LATITUDE, digits = 2)

Unique_Lats <- unique( Cork_Towns[ , 4 ] )

# ---- Create data frame Selected_Towns and initializing it with a single row.

# ---- Looping to get unique rows based on Latitude column
# ---- Create empty data frame
# ---- Based on the length of Unique_Lats nrow of data frame is selected
Selected_Towns <- data.frame(matrix(NA,    
                                    nrow = 97,
                                    ncol = 5))

index <- 1

names(Selected_Towns) <- c("NAME_TAG","CO_NAME", "AREA", "LATITUDE","LONGITUDE")
str(Unique_Lats$LATITUDE)

for (value in c(1:length(Cork_Towns$NAME_TAG))) {
  if(Cork_Towns$LATITUDE[value] %in% Unique_Lats$LATITUDE)
  {
    Selected_Towns[index,1:5] <- Cork_Towns[value,1:5]
    for (lat_Index in c(1:length(Unique_Lats$LATITUDE))) {
      if(Selected_Towns$LATITUDE[index] == Unique_Lats$LATITUDE[lat_Index])
      {
        Unique_Lats$LATITUDE[lat_Index] <- 0
      }
    }
    index <- index + 1
  }
}

# ---- Longitude.

Selected_Towns$LONGITUDE <- round(Selected_Towns$LONGITUDE, digits = 2)

Unique_Longs <- unique( Selected_Towns[ , 5 ] )

# ---- Create data frame Selected_Towns and initializing it with a single row.

# ---- Looping to get unique rows based on Latitude column
# ---- Create empty data frame
# ---- Based on the length of Unique_Longs nrow of data frame is selected
Selected_Towns_Final <- data.frame(matrix(NA,    
                                          nrow = 64,
                                          ncol = 5))

index <- 1

names(Selected_Towns_Final) <- c("NAME_TAG","CO_NAME", "AREA", "LATITUDE","LONGITUDE")
str(Unique_Longs)

for (value in c(1:length(Selected_Towns$NAME_TAG))) {
  if(Selected_Towns$LONGITUDE[value] %in% Unique_Longs)
  {
    Selected_Towns_Final[index,1:5] <- Selected_Towns[value,1:5]
    for (lat_Index in c(1:length(Unique_Longs))) {
      if(Selected_Towns_Final$LONGITUDE[index] == Unique_Longs[lat_Index])
      {
        Unique_Longs[lat_Index] <- 0
      }
    }
    index <- index + 1
  }
}

# ---- Long = East, Lat = North
range(Selected_Towns_Final$LATITUDE)
range(Selected_Towns_Final$LONGITUDE)
plot(y = c(51.2,52.5),
     x = c(-8.2,-9.88),
     type="n",
     xlab="Easting",
     ylab="Northing")

# ---- Towns name is displayed based on Eastings and northings
for (index in 1:length(Cork_Towns$NAME_TAG)) {
  text(Selected_Towns_Final$LONGITUDE[index],
       Selected_Towns_Final$LATITUDE[index],
       Selected_Towns_Final$NAME_TAG[index],cex=0.6)
}

# ---- Towns in qplot

qplot(LONGITUDE,
      LATITUDE,
      data=Selected_Towns_Final,
      geom="text",
      label=NAME_TAG,size=1,
      main = "Townlands of County Cork") +
      coord_equal() +
      labs(legend.position = "none")

# ---- Towns in ggplot

# ---- Town names is colored and sized based on respective Area 

ggplot(Selected_Towns_Final, 
       aes(x=LONGITUDE, y=LATITUDE)) + 
       geom_text(aes(size=AREA,color=AREA, label=NAME_TAG)) +
       theme(legend.title = element_text("none")) +
       ggtitle("Townlands in Cork")
       
# ---- Town names is colored  

ggplot(Selected_Towns_Final, 
       aes(x=LONGITUDE, y=LATITUDE)) + 
       geom_text(aes(color=NAME_TAG, label=NAME_TAG)) + 
       theme(legend.position = "none") +
       ggtitle("Townlands in Cork")
       

# ------------ working with map using leaflet library.

# ---- installing the packages required for this part and Enabling them.

install.packages("leaflet")
library(leaflet)

# Show first 20 rows from `Cork_Towns` dataset

leaflet(data = Selected_Towns_Final) %>% addTiles() %>%
  addMarkers(~LONGITUDE, 
             ~LATITUDE, 
             popup = ~as.character(NAME_TAG), 
             label = ~as.character(NAME_TAG))

