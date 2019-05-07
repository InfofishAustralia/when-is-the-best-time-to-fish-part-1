#####################################################################################################################
## AUTHOR : Stefan Sawynok, Infofish Australia
##
## This analysis is part 1 of an article that has a look at an age old question - when is the best time to go fishing
## The dataset is collected in a single impoundment - thus fish cannot escape
## The species is BARRAMUNDI a large catadromous species (Lates calcarifer) popular in Northern Queensland Australia
## Two datasets are used as the base - monitoring data collected over a year by general fishers
##                                   - competition data collected in a single fishing competition
## Within the datasets each fish is reported with length and date/time caught
## Additionally there is tide (while the impoundment is not tidal this is a proxy for moon position/phase)
## Additionally there is environmental data (temperature, wind and rainfall)
##
## The purpose of this analysis is to introduce fishers to using R to answer fishing questions and is attached
## To an explanatory article in Fishing Monthly Magazines (This is part 1).
##
## For those interested in machine learning, Part 2 of this article uses machine learning to predict when to fish
##
## Objective - to improve our ability to predict when to go fishing using actual fishing data.
##
#####################################################################################################################

require(dplyr)
require(ggplot2)

#####################################################################################################################
##
## load up our two datasets
## monitoring data is a record of fish caught and reported via track my fish app during monitoring
## competition data is a record of fish caught and reported via a competition using the same app in the same region
##
#####################################################################################################################

monitoringdata<-read.csv('data/monitoring.csv')
competitiondata<-read.csv('data/competition.csv')

#####################################################################################################################
## have a look at the structure of our dataset
#####################################################################################################################

head(monitoringdata)

#####################################################################################################################
## we can also look at the structure of the dataset in a spreadsheet format
#####################################################################################################################

View(monitoringdata)

#####################################################################################################################
##
## Another useful way to look at the dataset is the Summary function, which provides summary statistics for all 
## statistics 
##
#####################################################################################################################

summary(monitoringdata)
summary(competitiondata)


#####################################################################################################################
##
## Firstly, lets have a look for any biases in the fishing results due to a single fisher catching a lot of fish
## Fishing, like many sports produces a pareto distribution in results but even more so as time is not a constraint
## In fishing, those with more time available and good fishing abiltity have a better chance to catch fish.
## This however produces a skewed dataset where the results of the dominant fisher will affect the results.
## Thus in this example we will split the dataset into two, seperating the two top fishers from the rest.
## This will provide a more interesting comparrison, between highly productive fishers, ordinary fishers and competitions.
##
## To do this we will use a graphics package called ggplot2.
##
#####################################################################################################################

# generate a table with the summary of data
# tally up the number of fish per fisher as well as the cumulative total and the % of the total

monitoringfisherspareto <- monitoringdata %>% dplyr::mutate(idfisher=as.factor(paste0('F',idfisher))) %>% dplyr::group_by(idfisher) %>% dplyr::summarise(No.Fish.Caught = n()) %>% 
  dplyr::arrange(desc(No.Fish.Caught)) %>% dplyr::mutate(cumtotal=cumsum(No.Fish.Caught), cumperc = cumtotal/nrow(monitoringdata) * 100)
monitoringfisherspareto <- monitoringfisherspareto  %>% dplyr::mutate(rownum=seq(1:nrow(monitoringfisherspareto)))
# Have a look at the summary table

View(monitoringfisherspareto)

# plot the summary table as a pareto distribution

ggplot(monitoringfisherspareto, aes(x=rownum, y=No.Fish.Caught)) + 
  geom_bar(stat="identity", aes(fill = No.Fish.Caught)) +
  geom_line(aes(x=rownum, y = cumtotal, color = No.Fish.Caught)) +
  geom_point(aes(x=rownum, y = cumtotal, color = No.Fish.Caught), pch = 19) 


#####################################################################################################################
##
##  Repeat for competition fishers.  Time is a big productivity constraint in a competition and the best
##  fishers need good conditions to get much larger numbers of fish than the rest.  Time as a productivity constraint
##  shows how Time limits are more effective at constraining highly productive fishers than bag limits or slot limits.  
##  Highly productive fishers are able to reach their bag limit more often.  Across a fishing competition season with
##  bag constraints (eg best 5) highly productive fishers will achieve their full bag more often than other fishers.
##  This measure can be used as a measure to grade different fishers in terms of their competition cred.
##
##  Note, most Barramundi competitions are not constrained by bag limits.
##
#####################################################################################################################

# generate a table with the summary of data
# tally up the number of fish per fisher as well as the cumulative total and the % of the total

competitionfisherspareto  <- competitiondata %>% dplyr::mutate(idfisher=as.factor(paste0('F',idfisher))) %>% dplyr::group_by(idfisher) %>% dplyr::summarise(No.Fish.Caught = n()) %>% 
  dplyr::arrange(desc(No.Fish.Caught)) %>% dplyr::mutate(cumtotal=cumsum(No.Fish.Caught), cumperc = cumtotal/nrow(competitiondata) * 100)

competitionfisherspareto <- competitionfisherspareto  %>% dplyr::mutate(rownum=seq(1:nrow(competitionfisherspareto)))

# plot the summary table as a pareto distribution

ggplot(competitionfisherspareto, aes(x=rownum, y=No.Fish.Caught)) + 
  geom_bar(stat="identity", aes(fill = No.Fish.Caught)) +
  geom_line(aes(x=rownum, y = cumtotal, color = No.Fish.Caught)) +
  geom_point(aes(x=rownum, y = cumtotal, color = No.Fish.Caught), pch = 19) 


#####################################################################################################################
##
## Comparing the two, the monitoring data is dominated by two fishers, while the competition data
## is closer to a straight line, though if all the fishers who caught 0 (75 fishers) were included this would be a
## Pareto.  In this example, we are only looking at successful fishers to see what might influence success but in the 
## next part those results will be inculded.
## 
##  The fact that 2 fishers dominate the monitoring data will however bias the results as their "patterns of success"
##  will dominate the analysis.  Because of this we split the monitoring dataset in two, one with the two top fishers
##  and one with the rest.  That will be more useful and fun in comparing as we will be comparing the most successful
##  fishers, with a more general group and a competition group.
##
##  If the results look similar across all three then the habits of the fish are more likely to be influencing the 
##  results than the abilty of the fishers.
##
##  Lets find out!
##
#####################################################################################################################

# select the best two fishers into a new dataframe
hpfmonitoringdata <- monitoringdata %>% dplyr::filter(idfisher %in% c(1400,1180)) %>% dplyr::mutate(dataset='hpfmonitoring')

# select the remaining fishers into a new dataframe
generalmonitoringdata <- monitoringdata %>% dplyr::filter(!idfisher %in% c(1400,1180)) %>% dplyr::mutate(dataset='generalmonitoring')

# select the remaining fishers into a new dataframe
competitiondata <- competitiondata  %>% dplyr::mutate(dataset='competition')

# now combine the data into a single dataset so that we can easily combine them on charts
combineddataset <- rbind(hpfmonitoringdata,generalmonitoringdata,competitiondata)
#####################################################################################################################
##
## lets start with comparing the means of the two sets of fish.  A large difference in the mean could indicate that
## the groups are targetting different classes of fish.  In this case the means are similar, with competition fishers
## catching slightly larger fish.
##
#####################################################################################################################

mean(combineddataset[combineddataset$dataset=='hpfmonitoring',]$length)
mean(combineddataset[combineddataset$dataset=='generalmonitoring',]$length)
mean(combineddataset[combineddataset$dataset=='competition',]$length)

#####################################################################################################################
##
## start with plotting histograms side by side of the fish lengths to see if the fishers are targetting the same fish
## here you can see that both sets of fishers are getting similar results in the same area.  Independent sampling via
## echosounder and electrofishing yield similar results confirming that both are targetting the same fish.
##
#####################################################################################################################

par(mfrow=c(1,3))
hist(hpfmonitoringdata$length,xlab="Length (cm)",main="Histogram - Best 2 Fishers, Monitored Fish",col="red")
hist(generalmonitoringdata$length,xlab="Length (cm)",main="Histogram - General Fishers, Monitored Fish",col="green")
hist(competitiondata$length,xlab="Length (cm)",main="Histogram - Competition Fish",col="blue")

#####################################################################################################################
##
## That is nice but lets go with something that combines a bit more information
## using ggplot, we can do the same chart as well as the density of the data.
##
##
## This shows some difference in the dataset.  The average side of fish in the competition fishers is higher 
## and tht size classes are narrower.  This is most likely due to the competition fishers using less techniques and 
## fishing in areas with similar size classes.  Monitoring fishers have more time to wander and experiment.
##
## Another factor is that the competition fishers are fishing in the peak of summer when the fish are in deeper water
## and more spread out.
##
#####################################################################################################################


ggplot(combineddataset, aes(x=length)) + 
  geom_histogram(breaks=seq(300, 1000, by=25),aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#6666FF") + facet_grid(dataset ~ .)


#####################################################################################################################
##
## now lets have a look at the times where fish are caught to see if there are major difference in when they were
## successful in catching fish.  In the monitoring fishers can fish when they want to, in the competition fishers had
## a continuous 48 hour period to catch fish in.  In the case of the competition data, catches at midnight are
## excluded because there are manual entered items where the time was not correctly recorded
##
## Note that in the competition, fishers were more successful at night while monitoring fishers were more successful
## in the morning.
##
#####################################################################################################################


ggplot(combineddataset, aes(x=hour.catch)) + 
  geom_histogram(breaks=seq(1, 23, by=1),aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#6666FF") + facet_grid(dataset ~ .)


#####################################################################################################################
##
## While there are differences the overall trends in the datasets they are similar in shape.  So now we move into 
## digging into how the environmental variables might influence the results.
##
## As of the end of this part we have improved our prediction in one way, that being there is a better chance of 
## catching fish early in the morning or late afternoon/evening.
##
#####################################################################################################################


# save the combined dataset for use in later analysis.

saveRDS(combineddataset,'dataframes/combineddataset.RDS')