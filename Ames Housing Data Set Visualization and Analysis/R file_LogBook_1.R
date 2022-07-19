#Install the required packages
#Read the Packages
library(readxl) 
library(psych)
library(ggplot2)
library(tidyverse)

#Read the excel sheet into variable ames
ames <- read_excel("D:/R_Folder/ames_train.xlsx")

#Check quality of Saleprice variable
hist(ames$SalePrice)

# :: Summarize the Data ::

#Analyze the Columns with NA
colSums(is.na(ames))

#Summarise the data
summary(ames)


#::::: Data Quality Issues and Action ::::::

#Re-summarize the Saleprice to MEAN above 780K USD
ames$SalePrice[ames$SalePrice > 780000] <- mean(ames$SalePrice, na.rm = TRUE)

#Re-summarize the Year.Built consisting 999 with median year
ames$Year.Built[ames$Year.Built == 999] <- round(mean(ames$Year.Built, na.rm = TRUE))

#Change the rating for Overall.Qual which consists of 11 to median value
ames$Overall.Qual[ames$Overall.Qual == 11] <- median(ames$Overall.Qual, na.rm = TRUE)

#Convert Overall.Qual into factor
ames$Overall.Qual <- as.factor(ames$Overall.Qual)

#For Gr.Liv.Area the value above 2200 is changed to mean value
ames$Gr.Liv.Area[ames$Gr.Liv.Area > 2200] <- mean(ames$Gr.Liv.Area , na.rm = TRUE)

#Garage area 900 are changed to mean value
ames$Garage.Area[ames$Garage.Area > 900] <- mean(ames$Garage.Area , na.rm = TRUE)

#Convert the missing values into Mean for Garage area
ames$Garage.Area[is.na(ames$Garage.Area)] <- mean(ames$Garage.Area , na.rm = TRUE)

#Convert the NA in Garage.Cars to mean value
ames$Garage.Cars[is.na(ames$Garage.Cars)] <- round(mean(ames$Garage.Cars, na.rm = TRUE))

#For Total.Bsmt.SF values above 2000 are converted into mean value
ames$Total.Bsmt.SF[ames$Total.Bsmt.SF > 2000] <- mean(ames$Total.Bsmt.SF, na.rm = TRUE)

#Convert the missing values into Mean for Total.Bsmt.SF
ames$Total.Bsmt.SF[is.na(ames$Total.Bsmt.SF)] <- mean(ames$Total.Bsmt.SF, na.rm = TRUE)

#Blank Basement Quality are converted into 'Not Available'
ames$Bsmt.Qual[is.na(ames$Bsmt.Qual)] <- 'Not Available'

#:: GGPLOT VISUALISATIONS::

#1. First visualisation comparing Sales Price and Built.Year with respect to MS.Zoning
#and also changing the names for MS.Zoning
#Change name for all the zones into fullform
ames$MS.Zoning[ames$MS.Zoning =='A (agr)'] <- 'Agriculture'
ames$MS.Zoning[ames$MS.Zoning =='C (all)'] <- 'Commercial'
ames$MS.Zoning[ames$MS.Zoning =='FV'] <- 'Floating Village Residential'
ames$MS.Zoning[ames$MS.Zoning =='I (all)'] <- 'Industrial'
ames$MS.Zoning[ames$MS.Zoning =='RH'] <- 'Residential High Density'
ames$MS.Zoning[ames$MS.Zoning =='RL'] <- 'Residential Low Density'
ames$MS.Zoning[ames$MS.Zoning =='RM'] <- 'Residential Medium Density'
#YearBuilt Vs Sales Price - Geom Points with respect to Zoning
ames %>% ggplot(aes(x=(Year.Built), y=(SalePrice), colour = MS.Zoning))+
  geom_point()+
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 1), colour = 'black')+
  labs(title = 'Comparison of Sales Price and Construction Year',
       x="Original construction Year", y= "Sales price ($)", colour = "Zone Type")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#2. Overall Quality with respect to Sales Price (GeomBar)
ggplot(ames)+ 
  geom_bar(mapping = aes(x = Overall.Qual, y=(SalePrice), fill = Overall.Qual),
           stat = "Summary", fun.y = "mean")+
  labs(title = "Average House Price By Overall Quality", x="Overall Quality of the house (Rating)", y= "Sales Price ($)", fill = "Overall Quality")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
scale_fill_brewer(palette = "Dark2")

#3 Living area Vs Sales Price and rename the Building Types
#Renaming the Building types
ames$Bldg.Type[ames$Bldg.Type == "1Fam"] <- "Single-family Detached"
ames$Bldg.Type[ames$Bldg.Type == "2fmCon"] <- "Two-family Conversion"
ames$Bldg.Type[ames$Bldg.Type == "Duplex"] <- "Duplex"
ames$Bldg.Type[ames$Bldg.Type == "Twnhs"] <- "Townhouse Inside Unit"
ames$Bldg.Type[ames$Bldg.Type == "TwnhsE"] <- "Townhouse End Unit"
#Comparison of Sales Price and living area with Building Type (Boxplot)
ames %>% ggplot(aes(x=Gr.Liv.Area,y=SalePrice, fill = Bldg.Type))+
  geom_boxplot()+
  labs(title = "Comparison of Sales Price and living area with Building Type", x="Above grade living area (square feet)", y= "Sales Price ($)", fill = "Building Type")+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
scale_fill_brewer(palette = "Dark2")

#4. Year date VS Garage Area - Geom Points with respect to Garage Cars
ames %>% ggplot(aes(x=(Year.Built), y=(Garage.Area),color = as.factor(na.exclude(Garage.Cars))))+
  geom_point()+
  geom_smooth(method = lm, formula = y ~ x, colour = 'black')+
  labs(title = 'Comparison between garage area and construction year',x="Original construction Year", y= "Size of garage in square feet", color = 'Number of Cars')

#5. Total Basement VS Sales Price with Year Sold and also changing the name
#Change the name for Basement Qualities
ames$Bsmt.Qual[ames$Bsmt.Qual == 'Ex'] <- 'Excellent (100+ inches)'
ames$Bsmt.Qual[ames$Bsmt.Qual == 'Fa'] <- 'Good (90-99 inches)'
ames$Bsmt.Qual[ames$Bsmt.Qual == 'Gd'] <- 'Typical (80-89 inches)'
ames$Bsmt.Qual[ames$Bsmt.Qual == 'Po'] <- 'Fair (70-79 inches)'
ames$Bsmt.Qual[ames$Bsmt.Qual == 'TA'] <- 'Poor (<70 inches)'
ames$Bsmt.Qual[ames$Bsmt.Qual == 'NA'] <- 'No Basement'
#Total Basement VS Sales Price - Creation of Geom Points with respect to Base Quality
ames %>% ggplot(aes(y=SalePrice,x=Total.Bsmt.SF,color = Bsmt.Qual))+
  geom_point()+
  stat_smooth(aes(group = 1), method = "lm", formula = y ~ x)+
  labs(title = 'Sales Price Vs Total basement area with Basement Quality',x="Basement area (Sq ft)", y= "Sales Price ($)", color = 'Basement Quality')+
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
