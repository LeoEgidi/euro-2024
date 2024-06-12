#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Libraries 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(dplyr)
library(footBayes)
library(loo)
library(ggplot2)
library(patchwork)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Global variables
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
DATA_PATH="data/"
IMG_PATH="imgs/"
STAN_ITERS=2000
STAN_CORES=4
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data preparation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#-------------------------------------------------------------------------------
# Rankings dataset
#-------------------------------------------------------------------------------
fifa_ranking<-  read.csv2(paste0(DATA_PATH, "fifa_ranking.csv"),
                         sep=",",
                         header=TRUE)
fifa_ranking$total_points <- as.numeric(as.vector(fifa_ranking$total_points))/(10^3)

fifa_ranking_2024<- fifa_ranking %>% 
  filter(rank_date == "2024-04-04") %>% 
  select(country_full, total_points)
  # %>%
#arrange(desc(total_points)) %>%
  #slice(1:150)
colnames(fifa_ranking_2024) <- c("team_name", "ranking")

fifa_ranking_2024<- fifa_ranking_2024 %>%
  mutate(team_name=ifelse(team_name=="Czechia", "Czech Republic", team_name))

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Results dataset
#-------------------------------------------------------------------------------
euro_data <- read.csv2(paste0(DATA_PATH, "results.csv"),
                       sep = ",",
                       header = TRUE)
euro_data_train <- euro_data  %>%
  select(date, home_team, away_team, home_score, away_score, tournament) %>%
  filter(year(as.Date(euro_data$date)) >= 2020 ) # keep only last 4 years
# Se vogliamo togliere le amichevoli basta un filter(tournament != "Friendly")

times <- substr(euro_data_train$date, 1, 4) # Così considero annualmente...valutare se prendere anche il mese
times <- as.factor(times)
levels(times) <- c(1:length(levels(times)))
euro_data_train$date <- as.numeric(as.vector(times))
euro_data_train <- arrange(euro_data_train, date)

# Remove rows with missing values
euro_data_train <- euro_data_train %>%
  filter(!is.na(euro_data_train$home_score) & !is.na(euro_data_train$away_score) )


low_teams <- c("Faroe Islands", "Latvia",  "Malta",
"San Marino", "Liechtenstein", "Gibraltar","Andorra",
"Haiti","Sint Maarten","CuraÃÂÃÂÃÂÃÂ§ao",
"Grenada","Cuba","Turks and Caicos Islands",
"Jersey", "Hitra", "Isle of Man",
"Yorkshire", "Panjab", "Somaliland",
"Kernow", "Barawa", "Chagos Islands",
"Cascadia", "Parishes of Jersey", "Alderney",
"Yoruba Nation",   "Matabeleland", "Biafra",
"Mapuche", "Maule Sur", "Aymara", "Saint Helena",
"Shetland", "Ynys MÃÂÃÂÃÂÃÂ´n", "Orkney", "Guernsey", "Western Isles","Timor-Leste","Antigua and Barbuda")
# 
# Keep only matches where teams are not in low_teams
euro_data_train <- euro_data_train %>%
   filter(!(home_team %in% low_teams) & !(away_team %in% low_teams))

# Guarantee the teams are in the ranking
euro_data_train <- euro_data_train %>%
  filter((home_team %in% fifa_ranking_2024$team_name) & (away_team %in% fifa_ranking_2024$team_name) )

#-------------------------------------------------------------------------------
# Prepare stan parameters
#-------------------------------------------------------------------------------
euro_train_teams <- unique(euro_data_train$home_team)
euro_ranking <- fifa_ranking_2024 %>% filter( team_name %in% euro_train_teams)

ngames_prev <- 12
euro_data_test <- data.frame(
  date = rep(length(levels(times))+1, ngames_prev),
  
  home_team = c("Germany","Hungary","Spain","Italy",
                "Poland","Slovenia","Serbia","Romania",
                "Belgium","Austria","Turkey","Portugal"),
  
  away_team = c("Scotland","Switzerland","Croatia","Albania",
                "Netherlands","Denmark","England","Ukraine",
                "Slovakia","France","Georgia","Czech Republic"),
  
  home_score = rep(NA, ngames_prev),
  away_score = rep(NA, ngames_prev),
  tournament = rep(NA, ngames_prev))

euro_data_stan <-rbind(euro_data_train, euro_data_test)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Model Training
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fit <- stan_foot(data = euro_data_stan[,-6],
                 model="diag_infl_biv_pois",
                 iter = 2000, cores = 4,
                 predict= ngames_prev,
                 ranking = euro_ranking,
                 dynamic_type = "seasonal",
                 ind_home = "FALSE") 


prob <- foot_prob(fit, euro_data_stan[,-6])
colnames(prob$prob_table) <- c("home", "away",
                               "home win", "draw", "away win", "mlo")
knitr::kable(prob$prob_table)
prob$prob_plot
