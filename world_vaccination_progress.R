# Importing the necessary libraries
library(dplyr)
library(magrittr)
library(tidyr)
library(ggplot2)
library(funModeling)
library(Hmisc)
library(reshape2)
library(gplots)
library(scales)
# library(tidyverse)


##Reading the dataset
covid_vaccdf=read.csv('country_vaccinations.csv')

### Data Understanding
## Column names
names(covid_vaccdf)
# [1] "country"                             "iso_code"                           
# [3] "date"                                "total_vaccinations"                 
# [5] "people_vaccinated"                   "people_fully_vaccinated"            
# [7] "daily_vaccinations_raw"              "daily_vaccinations"                 
# [9] "total_vaccinations_per_hundred"      "people_vaccinated_per_hundred"      
# [11] "people_fully_vaccinated_per_hundred" "daily_vaccinations_per_million"     
# [13] "vaccines"                            "source_name"                        
# [15] "source_website"

## Summary statistics of the numerical variables 
covid_numdf=covid_vaccdf[,sapply(covid_vaccdf,is.numeric)]

summary<-data.frame(mean=sapply(covid_numdf, mean, na.rm=TRUE),
                    sd=sapply(covid_numdf, sd, na.rm=TRUE), 
                    min=sapply(covid_numdf, min, na.rm=TRUE),
                    max=sapply(covid_numdf, max, na.rm=TRUE),
                    median=sapply(covid_numdf, median, na.rm=TRUE), 
                    length=sapply(covid_numdf, length),
                    miss.val=sapply(covid_numdf, function(x)  
                      sum(length(which(is.na(x)))))) 
format(round(summary, 3), nsmall = 3)
#                                       mean           sd       min           max     median    length miss.val
# total_vaccinations                  5250013.054 21632689.705 0.000 297734000.000 446285.000 15666.000 6229.000
# people_vaccinated                   3302753.617 12337158.612 0.000 149462265.000 339801.000 15666.000 6912.000
# people_fully_vaccinated             1672177.892  7225403.215 1.000 108926627.000 181810.500 15666.000 9164.000
# daily_vaccinations_raw               137196.540   529413.972 0.000  11601000.000  16130.000 15666.000 7738.000
# daily_vaccinations                    79484.446   364159.585 0.000   7205286.000   6495.000 15666.000  201.000
# total_vaccinations_per_hundred           16.131       23.842 0.000       215.710      6.650 15666.000 6229.000
# people_vaccinated_per_hundred            11.679       15.622 0.000       112.750      5.130 15666.000 6912.000
# people_fully_vaccinated_per_hundred       5.945       10.307 0.000       102.950      2.480 15666.000 9164.000
# daily_vaccinations_per_million         3007.501     4693.065 0.000    118759.000   1504.000 15666.000  201.000

# There are quite a few missing values in most of these variables, need to handle them using data preprocessing techniques

# Dropping observations with missing total_vaccination & people_vaccinated variables. We are left with 8676 observations
covid_vaccdf=covid_vaccdf[complete.cases(covid_vaccdf$total_vaccinations,covid_vaccdf$people_vaccinated),]

# Filling the missing cases in the remaining numeric variables with their respective median values
covid_vaccdf[,sapply(covid_vaccdf, is.numeric)] <- lapply(covid_vaccdf[,sapply(covid_vaccdf, is.numeric)], function(x){x <- ifelse(is.na(x), median(x, na.rm  = TRUE), x)})

## Categorical variables are Country, iso_code,vaccines,source_name and source_website

covid_catdf=covid_vaccdf[,sapply(covid_vaccdf,is.character)]
summary<-data.frame(length=sapply(covid_catdf, length),miss.val=sapply(covid_catdf, function(x)  sum(length(which(is.na(x))))),length_unique_values=sapply(covid_catdf[,sapply(covid_catdf,is.character)],function(x){length(unique(x))})) 
summary
#                   length miss.val length_unique_values
# country         15666        0                  196
# iso_code        15666        0                  196
# date            15666        0                  144
# vaccines        15666        0                   34
# source_name     15666        0                  120
# source_website  15666        0                  190

# There are no missing values in the categorical variable front. Information extracted from 120 different web sources across the world suggests that 34 kinds of vaccines have been used across 196 countries over a period of 144 days.

## Converting the date column to 'date' datatype
covid_vaccdf$date=as.Date(covid_vaccdf$date,'%Y-%m-%d')

### Data Description/Visualization/Correlations

## Looking back at missing values count in each of the variables
apply(covid_vaccdf,2,function(x){sum(is.na(x))})
# country                            iso_code                                date 
# 0                                   0                                   0 
# total_vaccinations                   people_vaccinated             people_fully_vaccinated 
# 0                                   0                                   0 
# daily_vaccinations_raw                  daily_vaccinations      total_vaccinations_per_hundred 
# 0                                   0                                   0 
# people_vaccinated_per_hundred people_fully_vaccinated_per_hundred      daily_vaccinations_per_million 
# 0                                   0                                   0 
# vaccines                         source_name                      source_website 
# 0                                   0                                   0 

# Visualizations

## Looking at the correlation of numerical variables - Fig1. 

# Heatmap of correlation matrix - Fig1
covid_numdf=covid_vaccdf[,sapply(covid_vaccdf,is.numeric)]
heatmap.2(cor(covid_numdf), Rowv = FALSE, Colv = FALSE, dendrogram = "none",cellnote = round(cor(covid_numdf),2),main ="title",notecol = "black", key = FALSE, trace = 'none', margins = c(10,10))

# Looks like the variable pairs are highly correlated
# i) total_vaccinated_per_hundred - people_vaccinated_per_hundred
# ii)  people_vaccinated - people_vaccinated_per_hundred
# iii) total_vaccinations - people_vaccinations
# iv) daily_vaccinations_raw - daily_vaccinations

# Dropping one of the variables from the highly correlated variables
covid_vaccdf$people_vaccinated=NULL


## Looking at the distribution of each numerical variable - Fig2
plot_num(covid_numdf)

## Looking at the distribution of vaccines and their usage across whole data - Fig3.
freq(covid_catdf$vaccines)

#Oxford/AstraZeneca, Pfizer & Moderna are the most used ones.

## Top countries with vaccinations-Fig4
covid_vaccdf=covid_vaccdf%>%group_by(country)%>%mutate(vacc_count=total_vaccinations[length(total_vaccinations)])
check=unique(covid_vaccdf[,c('country','vacc_count')])
check=check[order(-check$vacc_count),][1:10,]
ggplot(check) + geom_bar(aes(x = reorder(country,vacc_count), y = vacc_count), stat = "identity")+ scale_y_continuous(labels=comma) +coord_flip()
#country        vacc_count
# <chr>               <dbl>
#   1 United States   251973752
# 2 India           162603603
# 3 United Kingdom   51225890
# 4 Brazil           46542392
# 5 England          42864836
# 6 Germany          33565445
# 7 Turkey           24467719
# 8 France           23949989
# 9 Italy            22644364
# 10 Indonesia        21367754

# USA, India, UK are dominating in terms of vaccination rollouts

## Top countries with fully vaccinated people- Fig5
covid_vaccdf=covid_vaccdf%>%group_by(country)%>%mutate(vacc_count_full=people_fully_vaccinated[length(people_fully_vaccinated)])
check=unique(covid_vaccdf[,c('country','vacc_count_full')])
check=check[order(-check$vacc_count_full),][1:10,]
ggplot(check) + geom_bar(aes(x = reorder(country,vacc_count_full), y = vacc_count_full), stat = "identity")+ scale_y_continuous(labels=comma) +coord_flip()
top_vaccine_countries=check$country
check
# country        vacc_count_full
# <chr>                    <dbl>
# 1 United States        108926627
# 2 India                 31544713
# 3 United Kingdom        16291719
# 4 Brazil                15037978
# 5 England               13632735
# 6 Turkey                10076124
# 7 Mexico                 8790655
# 8 Indonesia              8339055
# 9 Russia                 8284150
# 10 Germany                7360108

# USA, India & UK also have more people with full vaccinations done-Fig6

## Daily vaccinations trend for top 10 countries
check=covid_vaccdf%>%group_by(date,country)%>%summarise(daily_vaccinations=sum(daily_vaccinations))
ggplot(data=check[check$country%in%top_vaccine_countries[1:5],], aes(x = date, y = daily_vaccinations, group=country)) +geom_line(aes(color=country))+scale_x_date(labels = date_format("%b %Y"))

# India & USA dominates in terms of number of vaccinations done daily

## Daily vaccination per million trend for top 5 countries- Fig7
check=covid_vaccdf%>%group_by(date,country)%>%summarise(daily_vaccinations_per_million=sum(daily_vaccinations_per_million))
ggplot(data=check[check$country%in%top_vaccine_countries[1:5],], aes(x = date, y = daily_vaccinations_per_million, group=country)) +geom_line(aes(color=country))+scale_x_date(labels = date_format("%b %Y"))

#USA, UK & England stands out in this scenario whereas India and Brazil is failing to match miserably

## Preferred vaccines across top 10 countries

check=unique(check[c('country','vaccines')])
check[check$country%in%top_vaccine_countries,c('country','vaccines')]
# country        vaccines                                                        
# <chr>          <chr>                                                           
#   1 Brazil         Oxford/AstraZeneca, Sinovac                                     
# 2 England        Moderna, Oxford/AstraZeneca, Pfizer/BioNTech                    
# 3 Germany        Johnson&Johnson, Moderna, Oxford/AstraZeneca, Pfizer/BioNTech   
# 4 India          Covaxin, Oxford/AstraZeneca                                     
# 5 Indonesia      Oxford/AstraZeneca, Sinovac                                     
# 6 Mexico         CanSino, Oxford/AstraZeneca, Pfizer/BioNTech, Sinovac, Sputnik V
# 7 Russia         EpiVacCorona, Sputnik V                                         
# 8 Turkey         Pfizer/BioNTech, Sinovac                                        
# 9 United Kingdom Moderna, Oxford/AstraZeneca, Pfizer/BioNTech                    
# 10 United States  Johnson&Johnson, Moderna, Pfizer/BioNTech  


