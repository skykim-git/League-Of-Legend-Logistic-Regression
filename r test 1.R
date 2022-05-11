### League of Legend 10 K Diamond rank game data analysis using linear regression and estimation methods. 

# purpose is to find the relationship between multiple factors and the win rate(logistic regression) 

# Data Sourses 
# https://www.kaggle.com/datasets/bobbyscience/league-of-legends-diamond-ranked-games-10-min?resource=download

# command for window 
league_data <- read.table("C:/Users/Sky/Desktop/League Diamond Team linear regression/high_diamond_ranked_10min.csv",sep = ",", header = TRUE)
# command for mac
league_data <- read.table("/Users/friday/Desktop/League-Linear-Regression-R git/League-Linear-Rregression/high_diamond_ranked_10min.csv",sep = ",", header = TRUE)

### PART 1: Linear Regression (STAT 306)

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

## dropping highly correlated variables
blue_team_stats_dropped = subset(blue_team_stats, select = 
                                   -c(blueEliteMonsters,
                                      blueTotalGold,
                                      blueTotalExperience,
                                      blueGoldDiff,
                                      blueExperienceDiff,
                                      blueCSPerMin,
                                      blueGoldPerMin) )

logistic_1 <- glm( blueWins ~., data = blue_team_stats_dropped, family = binomial)
summary(logistic_1)

## importance of the difference
blue_team_stats_dropped_diff = subset(blue_team_stats, select = -c(blueEliteMonsters,
                                                                blueTotalGold,
                                                                blueTotalExperience,
                                                                blueCSPerMin,
                                                                blueGoldPerMin) )
logistic_2 <-glm( blueWins ~., data = blue_team_stats_dropped_diff, family = binomial)

summary(logistic_2)

blue_team_stats_killDiff <- blue_team_stats
killdiff <- league_data$blueKills - league_data$redKills
blue_team_stats_killDiff['killDiff'] <- killdiff

blue_team_stats_killDiff$killDiff

cor(blue_team_stats_killDiff$blueGoldDiff,blue_team_stats_killDiff$killDiff)#high
cor(blue_team_stats_killDiff$blueExperienceDiff,blue_team_stats_killDiff$killDiff)#high
cor(blue_team_stats_killDiff$blueAvgLevel,blue_team_stats_killDiff$blueTotalMinionsKilled)#0.5
  

blue_team_stats_killDiff_dropped = subset(blue_team_stats_killDiff, select = -c(blueEliteMonsters,blueTotalGold,
                                                                blueTotalExperience,
                                                                blueCSPerMin,
                                                                blueGoldPerMin,
                                                                blueKills,
                                                                blueDeaths) )

model4 <-glm( blueWins ~., data = blue_team_stats_killDiff_dropped, family = binomial)
summary(model4)

# high correlation between killDiff and gold diff/exp diff, dropped. 
blue_team_stats_killDiff_dropped_2 = subset(blue_team_stats_killDiff, select = -c(blueEliteMonsters,blueTotalGold,
                                                                                blueTotalExperience,
                                                                                blueCSPerMin,
                                                                                blueGoldPerMin,
                                                                                blueKills,
                                                                                blueDeaths,
                                                                                blueGoldDiff,
                                                                                blueExperienceDiff) )

model5 <-glm( blueWins ~., data = blue_team_stats_killDiff_dropped_2, family = binomial)
summary(model5)

model5$aic

with(summary(logistic_2), 1 - deviance/null.deviance)

model6<-glm( blueWins ~killDiff, data = blue_team_stats_killDiff_dropped_2, family = binomial)
summary(model6)
## killdiff not really significant

# bc the gold gained by kill varies, so not necessarly connected to the win, and more kills can lead to disadvantage(death risky)

## conclusion, drag is more associated compared with other achivements

## correlation 1

cor(blue_team_stats_killDiff_dropped$blueKills-blue_team_stats_killDiff_dropped$blueDeaths,blue_team_stats_killDiff_dropped$killDiff)


### PART 2: Statistical Estimation

### normal mean?

blue_team_stats
str(blue_team_stats)

mean(blue_team_stats$blueKills) # mle ## find condifence interval
                                # bayesian

## re-doing glm?



