library(maps)
library(ggplot2)
library(sqldf)

# Import the us counties covid19 dataset
covid <- read.csv('/Users/ronanwalsh/Documents/Development/Masters/Data Viz/Assignment 2/Covid/us_counties_covid19_daily.csv')
sum(is.na(fifa$value_eur))

# Select particular state and county to analyse
chosenState = 'California'
chosenCounty <- 'Los Angeles'

# Import the county and state datasets
counties <- map_data("county")
states <- map_data("state")

######## States number of incidents
statesNumInc <- sqldf("select state, fips, max(cases) as numCases, max(deaths) as numDeaths from covid group by state")
statesNumInc$state <- tolower(statesNumInc$state)

# Plot number of cases and deaths per state
ggplot(statesNumInc, aes(map_id = state)) + geom_map(aes(fill = numCases), map = states)  +
  expand_limits(x = states$long, y = states$lat)

ggplot(statesNumInc, aes(map_id = state)) + geom_map(aes(fill = numDeaths), map = states)  +
  expand_limits(x = states$long, y = states$lat)

# Num Deaths and Cases per state
ggplot(statesNumInc, aes(x=state, y=numCases)) + geom_bar(stat = "identity", fill = 'blue') + coord_flip()

ggplot(statesNumInc, aes(x=state, y=numDeaths)) + geom_bar(stat = "identity", fill = 'blue') + coord_flip()


# Counties of state - get the number of cases and deaths per county for chosen state
newYorkCounties <- subset(counties, region==tolower(chosenState))

stateCountiesStr <- paste("select county, max(cases) as numCases, max(deaths) as numDeaths from covid where state='",chosenState,"' and county !='Unknown' group by county",sep='')
countyNumInc <- sqldf(stateCountiesStr)
countyNumInc$county <- tolower(countyNumInc$county)

library(stringr)
library(dplyr)

# Need to create a sub region to join on with the map data for county
countyNumInc$subregion <- tolower(countyNumInc$county)
countyDetails <- inner_join(newYorkCounties, countyNumInc, by = "subregion")

# Map of counties by number of incidents
newYorkCountiesLess <- subset(newYorkCounties, select = -c(region))
ca_base <- ggplot(data = newYorkCounties, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = "gray")
ca_base

elbow_room1 <- ca_base + 
  geom_polygon(data = countyDetails, aes(fill = numCases), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() 

elbow_room1  + scale_fill_gradient(trans = "log10") + ggtitle(paste("Number of total Cases in ",chosenState,sep=''))



####### States number of deaths
statesNumInc <- sqldf("select state, fips, max(deaths) as numCases from covid group by state")
statesNumInc$state <- tolower(statesNumInc$state)

ggplot(statesNumInc, aes(map_id = state)) + geom_map(aes(fill = numCases), map = states)  +
  expand_limits(x = states$long, y = states$lat)


####### Deaths per day
deathsPerDayForState <- sqldf(paste("select date, state, max(cases) as numCases, max(deaths) as numDeaths from covid where state='",chosenState,"' group by date",sep=''))

str(deathsPerDayForState)
deathsPerDayForState$dateF <-as.Date(deathsPerDayForState$date)

ggplot(deathsPerDayForState,aes(x=dateF, y=numCases)) + geom_point() + geom_smooth(method = 'loess') 
ggplot(deathsPerDayForState,aes(x=dateF, y=numDeaths)) + geom_point() + geom_smooth(method = 'loess')



ggplot(deathsPerDayForState,aes(x=dateF, y=numCases)) + geom_point() + geom_smooth(method = 'loess') + scale_x_date(breaks = '2 month')
ggplot(deathsPerDayForState,aes(x=dateF, y=numDeaths)) + geom_point() + geom_smooth(method = 'loess') + scale_x_date(breaks = '2 month')


ggplot(deathsPerDayForState,aes(x=dateF, y=numCases)) + geom_point() + geom_smooth(method = 'loess') + scale_x_date(breaks = '1 month') + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(deathsPerDayForState,aes(x=dateF, y=numDeaths)) + geom_point() + geom_smooth(method = 'loess') + scale_x_date(breaks = '1 month') + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(deathsPerDayForState,aes(x=dateF, y=numCases)) + geom_point() + geom_smooth(method = 'loess') + scale_x_date(breaks = '1 month') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y="Number of Cases", x="Date")
ggplot(deathsPerDayForState,aes(x=dateF, y=numDeaths)) + geom_point() + geom_smooth(method = 'loess') + scale_x_date(breaks = '1 month') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y="Number of Deaths", x="Date")


####### Counties with highest Cases and Deaths
numbersPerCountyForState <- sqldf(paste("select county, max(cases) as numCases, max(deaths) as numDeaths from covid where state='",chosenState,"' group by county",sep=''))

ggplot(numbersPerCountyForState, aes(x=county, y=numCases)) + geom_bar(stat = "identity", fill = 'blue') + coord_flip()

ggplot(numbersPerCountyForState, aes(x=county, y=numDeaths)) + geom_bar(stat = "identity", fill = 'blue') + coord_flip()

# Get the top 8 counties for the state
numbersPerCountyForState <- numbersPerCountyForState[order(numbersPerCountyForState$numDeaths, decreasing = TRUE),]
numbersPerCountyForStateTop5 <- head(numbersPerCountyForState, 8)
numbersPerCountyForStateTop5BarPlot <- ggplot(numbersPerCountyForStateTop5, aes(x=reorder(county,-numDeaths), y=numDeaths, fill=county)) + geom_bar(stat = "identity", position = 'dodge') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y="Number of Deaths", x="County") + ggtitle(paste("Most Deaths Per County - Top 8 for ",chosenState,sep=''))
numbersPerCountyForStateTop5BarPlot



countyNums <- sqldf(paste("select date, state, county, max(cases) as numCases, max(deaths) as numDeaths from covid where county='",chosenCounty,"' group by date",sep=''))
countyNums$dateF <- as.Date(countyNums$date)

ggplot(countyNums,aes(x=dateF, y=numCases)) + geom_point() + geom_smooth(method = 'loess') + scale_x_date(breaks = '1 month') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y="Number of Cases", x="Date")
ggplot(countyNums,aes(x=dateF, y=numDeaths)) + geom_point() + geom_smooth(method = 'loess') + scale_x_date(breaks = '1 month') + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + labs(y="Number of Deaths", x="Date") + ggtitle(paste("Cumulative Deaths per Day for ", chosenCounty,sep=''))




