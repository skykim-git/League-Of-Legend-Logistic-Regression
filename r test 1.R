### League of Legend 10 K Diamond rank game data analysis using linear regression and estimation methods. 

# purpose is to find the relationship between multiple factors and the win rate(logistic regression) 

# Data Sourses 
# https://www.kaggle.com/datasets/bobbyscience/league-of-legends-diamond-ranked-games-10-min?resource=download

# command for window 
league_data <- read.table("C:/Users/Sky/Desktop/League Diamond Team linear regression/high_diamond_ranked_10min.csv",sep = ",", header = TRUE)
# command for mac
league_data <- read.table("/Users/friday/Desktop/League-Linear-Regression-R git/League-Linear-Rregression/high_diamond_ranked_10min.csv",sep = ",", header = TRUE)

### PART 1: Logistic Regression (STAT 306)

library(GGally)

league_data
str(league_data)

# Treaming the Data
blue_team_stats <- league_data[,2:21] 

str(blue_team_stats)

# check for correlation and get rid of the high correlation (causing nearly singular matrix)
blue_team_stats_first_100 <- blue_team_stats[0:100,]
blue_team_stats_first_100
ggpairs(blue_team_stats_first_100)

# dropping highly correlated variables
blue_team_stats_dropped = subset(blue_team_stats, select = 
                                   -c(blueEliteMonsters,
                                      blueTotalGold,
                                      blueTotalExperience,
                                      blueGoldDiff,
                                      blueExperienceDiff,
                                      blueCSPerMin,
                                      blueGoldPerMin) )

## model_1 : which no difference statistics, only highly correlated variables are removed
logistic_1 <- glm( blueWins ~., data = blue_team_stats_dropped, family = binomial)
summary(logistic_1)

## model_2 : importance of the difference statistics
blue_team_stats_dropped_diff = subset(blue_team_stats, select = -c(blueEliteMonsters,
                                                                blueTotalGold,
                                                                blueTotalExperience,
                                                                blueCSPerMin,
                                                                blueGoldPerMin) )
logistic_2 <-glm( blueWins ~., data = blue_team_stats_dropped_diff, family = binomial)

summary(logistic_2)

## model_3 : adding difference in kills statistics
blue_team_stats_killDiff <- blue_team_stats
killdiff <- league_data$blueKills - league_data$redKills
blue_team_stats_killDiff['killDiff'] <- killdiff

blue_team_stats_killDiff$killDiff

# checking for correlation between killDiff and other variables
cor(blue_team_stats_killDiff$blueGoldDiff,blue_team_stats_killDiff$killDiff)#high
cor(blue_team_stats_killDiff$blueExperienceDiff,blue_team_stats_killDiff$killDiff)#high
cor(blue_team_stats_killDiff$blueAvgLevel,blue_team_stats_killDiff$blueTotalMinionsKilled)#0.5
  

blue_team_stats_killDiff_dropped = subset(blue_team_stats_killDiff, select = -c(blueEliteMonsters,blueTotalGold,
                                                                blueTotalExperience,
                                                                blueCSPerMin,
                                                                blueGoldPerMin,
                                                                blueKills,
                                                                blueDeaths) )

logistic_3 <-glm( blueWins ~., data = blue_team_stats_killDiff_dropped, family = binomial)
summary(logistic_3)

# high correlation between killDiff and gold diff/exp diff, dropped. 
blue_team_stats_killDiff_dropped_2 = subset(blue_team_stats_killDiff, select = -c(blueEliteMonsters,blueTotalGold,
                                                                                blueTotalExperience,
                                                                                blueCSPerMin,
                                                                                blueGoldPerMin,
                                                                                blueKills,
                                                                                blueDeaths,
                                                                                blueGoldDiff,
                                                                                blueExperienceDiff) )

## model_4 : highly correlated ones are removed to avoid nearly singular matrices
logistic_4 <-glm( blueWins ~., data = blue_team_stats_killDiff_dropped_2, family = binomial)
summary(logistic_4)

with(summary(logistic_2), 1 - deviance/null.deviance)





