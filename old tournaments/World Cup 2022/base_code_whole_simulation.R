library(tidyverse)
library(dplyr)
library(footBayes)
library(loo)


wc_data <- read.csv2("results.csv", sep = ",")
wc_data_train <- wc_data  %>%
  select(date, home_team, away_team,
         home_score, away_score, tournament) %>%
  filter(as.Date(wc_data$date) > "2018-08-01" )


fifa_ranking <- read.csv2("fifa_ranking.csv", sep=",", header=TRUE)
fifa_ranking$total_points <- as.numeric(as.vector(fifa_ranking$total_points))/(10^3)
fifa_ranking_clean <- fifa_ranking %>%
  select(country_full, total_points )
fifa_ranking_clean <- fifa_ranking_clean[63707:dim(fifa_ranking)[1], ]
colnames(fifa_ranking_clean) <- c("team_name", "ranking")

# redefining times from 1 to 42
times <- #paste(
  substr(wc_data_train$date, 1, 4)
# ,"-", sep="", substr(wc_data_train$date, 6, 7)

times <- as.factor(times)
levels(times) <- c(1:length(levels(times)))
wc_data_train$date <- as.numeric(as.vector(times))
wc_data_train <- arrange(wc_data_train, date)

wc_data_train <- wc_data_train %>%
  filter(!is.na(wc_data_train$home_score) & !is.na(wc_data_train$away_score) )



low_teams <- c(
  "Haiti","Sint Maarten","CuraÃÂÃÂ§ao",
  "Grenada","Cuba","Turks and Caicos Islands",
  "Jersey", "Hitra", "Isle of Man",
  "Yorkshire", "Panjab", "Somaliland",
  "Kernow", "Barawa", "Chagos Islands",
  "Cascadia", "Parishes of Jersey", "Alderney",
  "Yoruba Nation",   "Matabeleland", "Biafra",
  "Mapuche", "Maule Sur", "Aymara", "Saint Helena",
  "Shetland", "Ynys MÃÂÃÂ´n", "Orkney", "Guernsey", "Western Isles"  )

other_discarded_teams <- wc_data_train %>% filter( !(home_team %in% fifa_ranking_clean$team_name ))
no_teams <- c(low_teams, other_discarded_teams$home_team)
wc_1 <- wc_data_train %>% filter( !(home_team %in% no_teams ))
wc_2 <- wc_1 %>% filter( !(away_team %in% no_teams ))
wc_data_train <- wc_2

# clean Fifa ranking to be ready for Stan!

wc_train_teams <- unique(wc_data_train$home_team)
wc_ranking <- fifa_ranking_clean %>% filter( team_name %in% wc_train_teams)


# Matchday1 train data
ngames_matchday1 <- 16
wc_data_train_matchday_1 <- data.frame(
  date = rep(length(levels(times))+1, ngames_matchday1),
  home_team = c("Qatar", "England" , "Senegal",
                "United States", "Argentina", "Denmark",
                "Mexico", "France", "Morocco", "Germany",
                "Spain","Belgium",
                "Switzerland","Uruguay",
                "Portugal","Brazil"),
  
  away_team = c("Ecuador", "Iran", "Netherlands",
                "Wales", "Saudi Arabia", "Tunisia",
                "Poland", "Australia", "Croatia",
                "Japan",
                "Costa Rica","Canada",
                "Cameroon","South Korea",
                "Ghana","Serbia"),
  
  home_score = c(0,6,0,1,1,0,0,4,0,1,7,1,1,0,3,2),
  away_score = c(2,2,2,1,2,0,0,1,0,2,0,0,0,0,2,0),
  tournament = rep("World Cup 2022", ngames_matchday1 ))

ngames_matchday2 <- 16
wc_data_train_matchday2 <- data.frame(
  date = rep(length(levels(times))+2, ngames_matchday2),
  home_team = c("Wales","Qatar","Netherlands",
                "England", "Tunisia","Poland",
                "France","Argentina", "Japan","Germany",
                "Belgium","Croatia","Cameroon","Brazil",
                "Portugal","South Korea"),
  
  away_team = c( "Iran",  "Senegal","Ecuador",
                 "United States", "Australia",
                 "Saudi Arabia",
                 "Denmark","Mexico","Costa Rica","Spain",
                 "Morocco","Canada","Serbia","Switzerland",
                 "Uruguay","Ghana"),
  
  home_score = c(0,1,1,0,0,2,2,2,0,1,0,4,3,1,2,2),
  away_score = c(2,3,1,0,1,0,1,0,1,1,2,1,3,0,0,3),
  tournament = rep("World Cup 2022",ngames_matchday2))

ngames_matchday3 <- 16
wc_data_test <- data.frame(
  date = rep(length(levels(times))+3, ngames_matchday3),
  home_team = c("Ecuador", "Netherlands", 
                "Iran", "Wales",
                "Tunisia", "Australia",
                "Poland", "Saudi Arabia",
                "Croatia", "Canada",
                "Japan", "Costa Rica",
                "South Korea", "Ghana",
                "Serbia","Cameroon"),
  
  away_team = c( "Senegal", "Qatar",
                 "United States",  "England",
                 "France", "Denmark",
                 "Argentina", "Mexico",
                 "Belgium", "Morocco",
                 "Spain",  "Germany",
                 "Portugal", "Uruguay",
                 "Switzerland", "Brazil"),
  
  home_score = rep(NA, ngames_matchday3),
  away_score = rep(NA, ngames_matchday3),
  tournament = rep(NA, ngames_matchday3))


wc_data_stan <-rbind(wc_data_train,
                     wc_data_train_matchday_1,
                     wc_data_train_matchday2,
                     wc_data_test)


## dynamic models
n_iter <- 2000