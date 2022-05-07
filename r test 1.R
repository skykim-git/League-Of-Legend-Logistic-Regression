### LOOKS like shit


### https://www.kaggle.com/datasets/bobbyscience/league-of-legends-diamond-ranked-games-10-min?resource=download
league_data <- read.table("C:/Users/Sky/Desktop/League Diamond Team linear regression/high_diamond_ranked_10min.csv",sep = ",", header = TRUE)

league_data

plot(league_data$blueKills~league_data$blueWardsPlaced)

str(league_data)

league_data$blueDeaths
league_data$blueKills

first_100 <- league_data[1:100, ]

first_100

plot(first_100$blueKills~first_100$blueWardsPlaced)

plot(first_100$blueTotalGold~first_100$blueKills)