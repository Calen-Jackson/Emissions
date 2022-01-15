# BTMA 431 - L01 
# FINAL PROJECT CODE 
# Emma (Da) Yoon, Carter Devries, Calen Jackson, Luc Legere, Maryam Ahmed 
# December 08, 2021 

#Downloading necessary packages
install.packages("httr")
install.packages("XML")
install.packages("stringr")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyverse")
install.packages('rvest')

#Loading packages
library("tidyverse")
library("rvest")
library("httr")
library("XML")
library("stringr")
library("ggplot2")
library("dplyr")
library(readxl)
library(ggplot2)


#--------------------------------Question 1-------------------------------------


#We want to see the data without scientific notation - so we are going to set that
options(scipen = 100)

#Importing our emissions data csv file
file <- "https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv"
emissions<- read.csv(file)

## keep a raw copy of emissions so that we do not need to read every time
emissions_raw <- emissions

#Web Scraping a list of all the countries in the world + their ISO3 codes
#We used the following link to structure the web scraping code:
#https://towardsdatascience.com/scraping-data-from-wikipedia-tables-3efa04c6b53f
url = "https://www.fao.org/countryprofiles/iso3list/en/"

country_list_html = read_html(url)
country_list_html %>%
  html_nodes(css = "table")

country_list_table =
  country_list_html %>%
  html_nodes(css = "table") %>%
  nth(1) %>%
  html_table(fill = TRUE)

#Filtering emissions data using the ISO3 codes that we web scraped
emissions<-emissions %>%
  filter(iso_code %in% country_list_table$ISO3) 

#There are missing countries so we use setdiff function that shows which ones are missing...
#San Marino, Tokelau, Micronesia and Monaco are missing...
#We removed them using filtering from our data
missing_countries<-setdiff(country_list_table$ISO3,emissions$iso_code)

#Filtering for just 2018 emissions data 
emissions_2018<-emissions %>% filter(year %in% '2018')

#Formatting only necessary columns that are going to be the ISO3 code, country names and total emissions in GT
emissions_2018<-emissions_2018[, c("iso_code","country","co2")]

#Importing GDP data into environment 
GDPCountries_csv<- read.csv("GDPCountries.csv")

#Changing the headers of GDPCountries_csv data frame to relevant ones 
#Initially there was 4 blank rows of data in the csv near the top so we removed them
names(GDPCountries_csv) <- as.matrix(as.character(GDPCountries_csv[4, ]))
GDPCountries_csv <- GDPCountries_csv[-4, ]
GDPCountries_csv[] <- lapply(GDPCountries_csv, function(x) type.convert(as.character(x)))

#Filter GDPCountries_csv by equivalent ISO3 codes
GDPCountries_filtered<-GDPCountries_csv %>%
  filter(`Country Code` %in% emissions_2018$iso_code)

#Formatting column names
names(GDPCountries_filtered)[names(GDPCountries_filtered)==('Country Code')]<-'iso_code'

#GDP data and Emissions data were off by two rows...
#Realized that cook island and niue was excluded from GDP data using the code below
require(dplyr)
GDP_missing_countries<-anti_join(emissions_2018, GDPCountries_filtered, by="iso_code")

#Because of the uneven data, we decided to remove the following countries...
#Cook Island and Niue
emissions_2018<-emissions_2018%>%
  filter(iso_code %in% GDPCountries_filtered$iso_code)

#Keeping only the necessary columns - ISO3 code, country name and 2018 data
GDPCountries_2018<-GDPCountries_filtered[, c("iso_code","Country Name","2018")]

#Merging the GDP and Emissions data sets by ISO3 code
GDP_Emissions_table_2018<-merge(GDPCountries_2018,emissions_2018,by='iso_code')

#Keeping only the necessary columns
GDP_Emissions_table_2018<-GDP_Emissions_table_2018[, c("iso_code","Country Name","2018","co2")]

#Renaming columns
names(GDP_Emissions_table_2018)[names(GDP_Emissions_table_2018)==('2018')]<-'GDP2018'
names(GDP_Emissions_table_2018)[names(GDP_Emissions_table_2018)==('co2')]<-'Emissions2018'

#We want to remove rows that are missing data 
GDP_Emissions_table_2018<-na.omit(GDP_Emissions_table_2018)

#Regression analysis using the Emissions data as a predictor of GDP2018.
GDP_Emissions_regression<- lm(GDP2018 ~ Emissions2018, data = GDP_Emissions_table_2018)
summary(GDP_Emissions_regression)

#We were able to reject our null hypothesis as the p-value was 2.2e-16 which is less than 0.05.

#----------------------- Follow up on question 1 analysis ----------------------

#Creating a data frame for 2019 GDP data
GDPCountries_2019<-GDPCountries_filtered[, c("iso_code","Country Name","2019")]

#Filtering for only 2019 emissions
emissions_2019<-emissions %>% filter(year %in% '2019')

#Keeping only the necessary columns
emissions_2019<-emissions_2019[, c("iso_code","country","co2")]

#Filtering 2019 emissions to include only the countries from the first part of our question
emissions_2019<-emissions_2019 %>%
  filter(iso_code %in% GDPCountries_filtered$iso_code)

#Merging the new 2019 Emissions and GDP data
GDP_Emissions_table_2019<-merge(GDPCountries_2019,emissions_2019,by='iso_code')

#Keeping only the necessary columns
GDP_Emissions_table_2019<-GDP_Emissions_table_2019[, c("iso_code","Country Name","2019","co2")]

#Renaming columns
names(GDP_Emissions_table_2019)[names(GDP_Emissions_table_2019)==('2019')]<-'GDP2019'
names(GDP_Emissions_table_2019)[names(GDP_Emissions_table_2019)==('co2')]<-'Emissions2019'

#We want to remove rows with missing data 
GDP_Emissions_table_2019<-na.omit(GDP_Emissions_table_2019)

#Predict GDP values for all of the chosen countries for 2019 and add into table

#Create a vector from emissions value in GT of 2020.
Emissions<-GDP_Emissions_table_2019$Emissions2019

#For loop to predict each countries 2019 GDP using 2019 Emissions
#Created an equation using emissions as a predictor variable for GDP with the values from the regression outputs
for(i in 1:length(Emissions)){
  GDP_Emissions_table_2019$PredictGDP2019[i]<-GDP_Emissions_regression$coefficients[1]+(GDP_Emissions_regression$coefficients[2]*Emissions[i])
}

#Performing a T test for the predicted 2019 GDP values and the actual 2019 GDP values
t_test<-t.test(GDP_Emissions_table_2019$PredictGDP2019,GDP_Emissions_table_2019$GDP2019)
t_test

#Returned a p-value of 0.9716 and therefore cannot reject our null hypothesis for the t-test

#--------------------------------Question 2-------------------------------------

#emissions <- emissions_raw

#Filtering for just 2018 emissions data 
emissions_2018<-emissions %>% filter(year %in% '2018') %>% select(2,4) 


# Renaming Columns
colnames(emissions_2018) <- c("Country","Co2")

# Reading The life Expectancy 
Life.Expectancy<- read.csv("life-expectancy-vs-gdp-per-capita.csv")
Life.Expectancy <- Life.Expectancy %>% 
  select(1,4) %>% filter(Life.Expectancy$Year == "2018") 

# Renaming Columns
colnames(Life.Expectancy) <- c("Country", "LifeExpectancy")

# Merging data tables to one using country 
Life.Expectancy.Emmission <- merge(Life.Expectancy, emissions_2018, by= "Country")

# Regression Model 
Life.Expct.Regression <- lm(LifeExpectancy ~ Co2 , data = Life.Expectancy.Emmission)

# Summary of report 
summary(Life.Expct.Regression)

# P.value = 0.1103 > 0.05 = Fail to reject to the null  

#--------------------- Follow up on question 2 analysis -----------------------

# Reading the files 
Happines.Score.2018  <- read_excel("WHR2018Chapter2OnlineData.xls", 
                                   sheet = "Figure2.2") %>% select(1,2)

# Changing value to percentage 
Happines.Score.2018[,2] <- Happines.Score.2018[,2]/10

# Merging Tables 
Emmission.Happiness.2018 <- merge(emissions_2018,Happines.Score.2018, by= "Country")

# Creating a regression model 
Happiness.Regression <- lm(`Happiness score` ~ Co2, data= Emmission.Happiness.2018)

# Summary of report 
summary(Happiness.Regression)

ggplot(data = Emmission.Happiness.2018, aes(x = `Happiness score`, y = `Co2`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Happiness Score - Co2",
       x = "Happiness Score",
       y = "Co2") + 
  ggtitle("Happiness Score - Co2") + 
  theme(plot.title = element_text(hjust = 0.5))


#------------------------------- Question 3 ------------------------------------

co2 <- emissions_raw

climate_ratings_file <- "Climate-Policy-Ratings.csv"
climate_ratings <- read.csv(climate_ratings_file)

## filter co2 to only contain data from countries in our climate ratings file
#co2 <- filter(co2, country %in% climate_ratings$Country)
countries <- climate_ratings$Country
score <- climate_ratings$Score

#address name descrepencies between datasets:
#czech republic = czechia
#korea = south korea
#chinese taipei = taiwan
countries[which(countries == "Chinese Taipei")] = "Taiwan"
countries[which(countries == "Czech Republic")] = "Czechia"
countries[which(countries == "Korea")] = "South Korea"

## filter github data to only contain a few columns
keeps <- c("country","year", "co2", "co2_per_capita")
co2 = co2[keeps]

## sort alphabetically
co2 <- co2[order(co2$country),]

## filter to only include countries we have scores for
co2 <- filter(co2, country %in% countries)


## extract the years we want to work with from co2 data
co2_2002 <- filter(co2, year == 2002)
co2_2020 <- filter(co2, year == 2020)

#create data frame (we will use this to consolidate all data we need in one place ) - added co2 amounts
df <- data.frame(countries, score)

## order alphabetically so we can work easily with this
df <- df[order(df$countries),]

## Re-indexing the rows so that we have 1 - 60 alphabetically
row.names(df) <- NULL

## Adding co2 vales to consolidated dataframe - Probably easier way to do this
df$Co2_2020 <- co2_2020$co2

## calculate differences between emissions now and in the past for all countries - also calculating a % difference between 2020 and 2002
differences_2020_2002 <- co2_2020$co2 - co2_2002$co2
Percentage_differences_2020_2002 <- (co2_2020$co2 - co2_2002$co2) / co2_2020$co2

## add them to our data frame
df$Difference_2002 <- differences_2020_2002
df$Percentage_Diference_2002 <- Percentage_differences_2020_2002


# regression test
fit_2002 <- lm(Percentage_Diference_2002 ~ score,
               data = df)

summary(fit_2002)

plot(df$score, df$Percentage_Diference_2002)

## There is a negative relationship (higher score = higher "negative" percentage diff) 
## between score and percentage difference for 2002 it appears (p-value = .04778 > .05)


#----------------------- Follow up on question 3 analysis ----------------------

## Re-ranking countries based on a new score that considers Climate Policy Rating, along with % difference in emissions from 2002 - 2020.
## Question to be evaluated is whether or not in applying this new measure, the best (Luxembourg) and worst (Australia) country based on Climate Policy Score alone will change?

## Measure will be created as follows: Climate Policy Score as a % (Original value / highest possible score of 20) - (Percentage Difference from 2002 -2020)
## Note that the percentage diff is subtracted rather than added (a negative value means that emissions decreased)
## This should therefore benefit countries that have been able to reduce their emissions, and hinder those who haven't


## Create a new column for the Climate Policy Score as a percentage
df$score_as_percent <- df$score / 20

## Create the measure
df$Policy_Plus_Emissions_Score <- df$score_as_percent - df$Percentage_Diference_2002

## Adding two new columns that track the ranking of countries based on the original Climate Policy Score, and the new measure
## Also adding a column that looks at the ranking change
df$Climate_Policy_Rank <- rank(-df$score)
df$Policy_Plus_Emissions_Rank <- rank(-df$Policy_Plus_Emissions_Score)
df$Rank_Change <- df$Climate_Policy_Rank - df$Policy_Plus_Emissions_Rank


## Wilcoxon will test for Null: no difference in median between two sets of numbers -- Alt: Difference in Median between two sets of numbers
## Essentially we can test if there is a significant change in the rankings (we can use 0 to represent our first ranking "change")
## Results show that there is no difference between the rankings -- Interesting because we do see the top / bottom country change and multiple countries out / underperform

wilcox.test(0, df$Rank_Change, mu = 0, conf.int = T)




