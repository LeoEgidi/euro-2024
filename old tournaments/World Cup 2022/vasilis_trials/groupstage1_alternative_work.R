## ----global_options, include=FALSE------------
knitr::opts_chunk$set(fig.align = 'center', warning=FALSE, message=FALSE, fig.asp=0.625, fig.height =10, fig.width = 8, out.width='750px', dpi=200, 
                      dev='png', global.par = TRUE, dev.args=list(pointsize=10), fig.path = 'figs/')
library(MASS)

## ----setup, include=FALSE---------------------
library(knitr)
local({
  hook_plot = knit_hooks$get('plot')
  knit_hooks$set(plot = function(x, options) {
    paste0('\n\n----\n\n', hook_plot(x, options))
  })
})


## ----data2, echo = FALSE, out.width="70%",  eval = TRUE, fig.height= 18, fig.width =11----

## BEFORE STARTING:

# To make the following code working properly, download the lastly updated version of the 'footBayes' package from Github:

# library(devtools)
# install_github("leoegidi/footBayes")

library(tidyverse)
library(footBayes)
library(loo)
# library(lubridate)
install.packages("anytime")   # Install anytime package
library("anytime") 

wc_data <- read.csv2(file=file.choose(), sep = ",")
# Keep last 3 years of all matches (friendly and qualifiers)
wc_data_train <- wc_data  %>%
  select(date, home_team, away_team,
         home_score, away_score, tournament) %>%
  filter(as.Date(wc_data$date) > "2018-08-01" )
# Convert month, year of wc data
wc_data_train$date_months<-format(as.Date(wc_data_train$date), "%Y-%m")


fifa_ranking <- read.csv2(file="C:\\Users\\vasileios palaskas\\Dropbox\\World Cup 2022\\fifa_ranking.csv", 
                          sep=",", header=TRUE)
fifa_ranking$total_points <- as.numeric(as.vector(fifa_ranking$total_points))/(10^3)
fifa_ranking_clean <- fifa_ranking %>%
  select(country_full, total_points,rank_date ) 
# Proper convert it
fifa_ranking_clean$rank_date <-anydate(fifa_ranking_clean$rank_date )
fifa_ranking_clean$date_months<-format(as.Date(fifa_ranking_clean$rank_date), "%Y-%m")

fifa_ranking_clean
# Filter the date as the wc_data train
fifa_ranking_clean <- fifa_ranking_clean %>%
  filter(as.Date(fifa_ranking_clean$rank_date) > "2018-08-01" )

fifa_ranking_clean$date_months<-format(as.Date(fifa_ranking_clean$rank_date), "%Y-%m")


# fifa_ranking_clean <- fifa_ranking_clean[63707:dim(fifa_ranking)[1], ]
colnames(fifa_ranking_clean) <- c("team_name", "ranking","date","date_months" )

# redefining times from 1 to 42
# times <- #paste(
#   substr(wc_data_train$date, 1, 4)
# # ,"-", sep="", substr(wc_data_train$date, 6, 7)
# 
# times <- as.factor(times)
# levels(times) <- c(1:length(levels(times)))
# wc_data_train$date <- as.numeric(as.vector(times))
wc_data_train <- arrange(wc_data_train, date)

wc_data_train <- wc_data_train %>%
  filter(!is.na(wc_data_train$home_score) & !is.na(wc_data_train$away_score) )


# get rid of low and not existing teams
low_teams <- c(
  #"Faroe Islands", "Latvia",  "Malta",
  #"San Marino", "Liechtenstein", "Gibraltar",
  #"Andorra", 
  "Jersey", "Hitra", "Isle of Man",
  "Yorkshire", "Panjab", "Somaliland",
  "Kernow", "Barawa", "Chagos Islands",
  "Cascadia", "Parishes of Jersey", "Alderney",
  "Yoruba Nation",   "Matabeleland", "Biafra",
  "Mapuche", "Maule Sur", "Aymara", "Saint Helena",
  "Shetland", "Ynys MÃ´n", "Orkney", "Guernsey", "Western Isles"  )
other_discarded_teams <- wc_data_train %>% filter( !(home_team %in% fifa_ranking_clean$team_name ))
no_teams <- c(low_teams, other_discarded_teams$home_team)
wc_1 <- wc_data_train %>% filter( !(home_team %in% no_teams ))
wc_2 <- wc_1 %>% filter( !(away_team %in% no_teams ))
wc_data_train <- wc_2

# clean Fifa ranking to be ready for Stan!

wc_train_teams <- unique(wc_data_train$home_team)
wc_ranking <- fifa_ranking_clean %>% filter( team_name %in% wc_train_teams)

ngames_prev <- 16
wc_data_test <- data.frame(
  date = rep(length(levels(times))+1, ngames_prev),
  home_team = c("Qatar", "England" , "Senegal",
                "United States", "Argentina", "Denmark",
                "Mexico", "France", "Morocco", "Germany",
                "Spain", "Belgium", "Switzerland", "Uruguay", 
                "Portugal", "Brazil"),
  
  away_team = c("Ecuador", "Iran", "Netherlands",
                "Wales", "Saudi Arabia", "Tunisia", 
                "Poland", "Australia", "Croatia", "Japan",
                "Costa Rica", "Canada", "Cameroon", "South Korea",
                "Ghana", "Serbia"),
  
  home_score = rep(NA, ngames_prev),
  away_score = rep(NA, ngames_prev),
  tournament = rep(NA, ngames_prev),
  date_months=rep(NA, ngames_prev)
  )

wc_data_stan <-rbind(wc_data_train, wc_data_test)
dim(wc_ranking)
dim(wc_data_train)
dim(wc_data_test)
## dynamic models
n_iter <- 2000
fit <- stan_foot(data = wc_data_stan[,-c(6,7)],
                 model="diag_infl_biv_pois",
                 iter = n_iter, cores = 4, 
                 predict= ngames_prev,
                 ranking = wc_ranking,
                 dynamic_type = "seasonal",
                 ind_home = "FALSE") # diag. inflated biv pois

prob <- foot_prob(fit, wc_data_stan[,-6])
colnames(prob$prob_table) <- c("home", "away",
                               "home win", "draw", "away win", "mlo")
knitr::kable(prob$prob_table)
prob$prob_plot

