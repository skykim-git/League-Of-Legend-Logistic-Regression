### League of Legend 10 K Diamond rank game data analysis using linear regression and estimation methods. 

# purpose is to find the relationship between multiple factors and the win rate(logistic) and kills(for rn warmup)

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

plot(league_data$blueKills~league_data$blueWardsPlaced)



league_data$blueDeaths
league_data$blueDragons

# Treaming Data
blue_team_stats <- league_data[,2:21] #including game id colum , (identifier)
blue_team_stats_first_100 <- blue_team_stats[0:100,]
blue_team_stats_first_100
str(blue_team_stats)



# check for correlation and get rid of the high correlation (causeing singular matrix)
ggpairs(blue_team_stats_first_100)


# data got rid of due to high correlation

## total gold and gold per min corr : 1.00
## total cs and cs per min :1.00

## differences, omitted for now, but can be a stat indicating the cs difference

## elite monster = herald + dragons, keep each, noe the elite

## total gold, could be indicated by kills, minions, etc. 

## total experience ~ avg level, only kepp the avg level because it is what effects the performance, one stronger stat

## dropping such variables
blue_team_stats_first_100_dropped = subset(blue_team_stats_first_100, select = -c(blueEliteMonsters,blueTotalGold,
                                                   blueTotalExperience,blueGoldDiff,
                                                   blueExperienceDiff,blueCSPerMin,
                                                   blueGoldPerMin) )

blue_team_stats_first_100_dropped

model <- lm(blue_team_stats_first_100_dropped$blueKills~blue_team_stats_first_100_dropped$)

model <- lm(blueKills~.,blue_team_stats_first_100_dropped)

forward <- regsubsets(blueKills ~., data=blue_team_stats_first_100_dropped, method="exhaustive")
summary(model)

## logistic regresssion

## shows object related achievements are more likely linked with the winning

model1 <- glm( blueWins ~., data = blue_team_stats_first_100_dropped, family = binomial)

## for the total data

blue_team_stats <- league_data[,2:21] #including game id colum , (identifier)
blue_team_stats_dropped = subset(blue_team_stats, select = -c(blueEliteMonsters,blueTotalGold,
                                                                                  blueTotalExperience,blueGoldDiff,
                                                                                  blueExperienceDiff,blueCSPerMin,
                                                                                  blueGoldPerMin) )

model2 <- glm( blueWins ~., data = blue_team_stats_dropped, family = binomial)
model2

blue_team_stats_dropped_3 = subset(blue_team_stats, select = -c(blueEliteMonsters,blueTotalGold,
                                                                  blueTotalExperience,
                                                                  blueCSPerMin,
                                                                  blueGoldPerMin) )
model3 <-glm( blueWins ~., data = blue_team_stats_dropped_3, family = binomial)

summary(model3)

blue_team_stats_killDiff <- blue_team_stats
killdiff <- league_data$blueKills - league_data$redKills
blue_team_stats_killDiff['killDiff'] <- killdiff

blue_team_stats_killDiff$killDiff
  

blue_team_stats_killDiff_dropped = subset(blue_team_stats_killDiff, select = -c(blueEliteMonsters,blueTotalGold,
                                                                blueTotalExperience,
                                                                blueCSPerMin,
                                                                blueGoldPerMin,
                                                                blueKills,
                                                                blueDeaths) )

model4 <-glm( blueWins ~., data = blue_team_stats_killDiff_dropped, family = binomial)
summary(model4)

## killdiff not really significant

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



