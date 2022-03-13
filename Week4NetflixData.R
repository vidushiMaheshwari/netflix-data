# install.packages("stringr")
library(stringr)

# install.packages("corrplot")
library(corrplot)

# install.packages("dplyr")
library(dplyr)

netflixData <- read.csv('all-weeks-countries.csv', header = TRUE)
# Since we have country ISO, we don't need country name and hence can remove 
# that column. Moreover we couldn't even use it in our program. 
netflixData <- netflixData[,-(1)]
netflixData <- netflixData[, -(3)]
netflixData <- netflixData[, -(5)]

netflixData <- netflixData[, -(3)]

summary(netflixData)

#let's take two countries only
# I will create two tables corresponding to each country... 
# Each table would contain the name of shows corresponding to 
# that country and the overall cumulative rank for that show

# NOTE that each week is of 20 entries

#Lets try to scrape the data for one week

country1Table <- netflixData[netflixData$country_iso2 == "US", ]
# View(country1Table)
country1Table <- country1Table[,-(1)]

dataForOneWeek <- function(x, dataframe) {
  return (dataframe[((20 * (x - 1)): 20 * x), ])
}

country1TableWeekN <- dataForOneWeek(x= 1, dataframe= country1Table)
# View(country1TableWeekN)

#########################################

country2Table <- netflixData[netflixData$country_iso2 == "AU", ]
country2Table <- country2Table[,-(1)]

country2TableWeekN <- dataForOneWeek(x= 1, dataframe = country2Table)
# View(country2TableWeekN)

#########################################

aggregateTable <- left_join(country2TableWeekN, country1TableWeekN, by= "show_title")

aggregateTable <-aggregateTable[complete.cases(aggregateTable), ]

aggregateTable <- distinct(aggregateTable)

View(aggregateTable)

### Condensing all this inside a function.. Main project to be run

dataForOneWeek <- function(x, dataframe) {
  return (dataframe[((20 * (x - 1)): 20 * x), ])
}

findSize <- function(country1, country2, weekNumber, x) {
  country1Table <- netflixData[netflixData$country_iso2 == country1, ]
  country1TableWeekN <- dataForOneWeek(weekNumber, country1Table)
  
  country2Table <- netflixData[netflixData$country_iso2 == country2, ]
  country2TableWeekN <- dataForOneWeek(weekNumber, country2Table)
  
  aggregateTable <- left_join(country2TableWeekN, country1TableWeekN, by= "show_title")
  
  aggregateTable <-aggregateTable[complete.cases(aggregateTable), ]
  aggregateTable <- distinct(aggregateTable)
  
  return (nrow(aggregateTable))
}

## Creating final database table
numUnique <- length(unique(netflixData$country_iso2))

aggregateCountries <- data.frame(matrix(nrow = numUnique, ncol = numUnique))

i <- 1

## simply labeling it properly
for (country1 in unique(netflixData$country_iso2)) {
  print(length(country1))
  print(country1)
  colnames(aggregateCountries)[i] <- country1
  rownames(aggregateCountries)[i] <- country1
  i <- i + 1
}

## entering data in the aggregate countries table

colNumber <- 1

while (colNumber != ncol(aggregateCountries)) {
  
  rowNumber <- 1
  while (rowNumber != nrow(aggregateCountries)) {
    print("----------------------")
    print(unique(netflixData$country_iso2)[colNumber])
    print(unique(netflixData$country_iso2)[rowNumber])
    print("-----------------------------")
    aggregateCountries[colNumber, rowNumber] <- findSize(country1 = unique(netflixData$country_iso2)[colNumber], 
                                             country2 = unique(netflixData$country_iso2)[rowNumber],
                                             weekNumber = 1)
    rowNumber <- rowNumber + 1
  }
  colNumber <- colNumber + 1
}

View(aggregateCountries)

## We can now plot the correlation of any country with all the countries
# For example, I used the example of India whose scatter plot shows it's most common trends
# to align with Pakistan and Bangladesh (which can be found by corresponsing index)

plot <- plot.default(aggregateCountries["IN"], xlab = "country index", 
                      ylab = "number of collisions")



print(unique(netflixData$country_iso2)[62])
print(unique(netflixData$country_iso2)[6])

## Note, that we ignore the the highest point since it represents the number of collisions
# with the same country