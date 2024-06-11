library(tidyverse)
library(footBayes)
library(loo)

nations_data <- read.csv2(file="nations.csv", sep = ",")
euro_qual_data <- read.csv2(file="euro_qual.csv", sep = ",")
wc_qual_data <- read.csv2(file="wc.csv", sep = ",")
#View(nations_data)
#View(euro_qual_data)
#View(wc_qual_data)


nations_ristr <- nations_data  %>%
  select(date_GMT, home_team_name, away_team_name,
         home_team_goal_count, away_team_goal_count)
nations_ristr <- nations_ristr[-c(163, 164),]

euro_qual_ristr <- euro_qual_data  %>%
  select(date_GMT, home_team_name, away_team_name,
         home_team_goal_count, away_team_goal_count)

wc_qual_ristr <- wc_qual_data  %>%
  select(date_GMT, home_team_name, away_team_name,
         home_team_goal_count, away_team_goal_count)
wc_qual_ristr <- wc_qual_ristr[c(1:75),]

fifa_ranking <- read.csv2(file="ranking_fifa.csv", header=FALSE)
#fifa_ranking <- fifa_ranking[c(1:48),]
colnames(fifa_ranking) <- c("team_name", "ranking")




euro_data <- rbind(nations_ristr, euro_qual_ristr, wc_qual_ristr)
# redefining times from 1 to 12
times <- paste(substr(euro_data$date_GMT, 1, 3),
               substr(euro_data$date_GMT, 8, 11))
times <- as.factor(times)
levels(times) <- c(2, 1, 9, 5, 8, 4, 7, 3, 6)


euro_data$date_GMT <- as.numeric(as.vector(times))
euro_data <- arrange(euro_data, date_GMT)

# drop low teams

low_teams <- c("Faroe Islands", "Latvia",  "Malta",
               "San Marino", "Liechtenstein", "Gibraltar",
               "Andorra")

euro_data1<- euro_data %>% filter(is.na(match(home_team_name, low_teams)))
euro_data2<- euro_data1 %>% filter(is.na(match(away_team_name, low_teams)))
euro_data <- euro_data2
ngames_prev <- 12
euro_data_test <- data.frame(
                  date_GMT = rep(10, ngames_prev),
                  home_team_name = c("Italy", "Wales",
                                   "Denmark", "Russia",
                                   "England", "Austria",
                                   "Netherlands", "Scotland",
                                   "Poland", "Spain",
                                   "Hungary", "Germany"),

                  away_team_name = c("Turkey", "Switzerland",
                                   "Finland", "Belgium",
                                   "Croatia", "FYR Macedonia",
                                   "Ukraine", "Czech Republic",
                                   "Slovakia", "Sweden",
                                   "Portugal", "France"),

                  home_team_goal_count = c(3,1,0,0,1,3,3,0,1,0, 0,0),
                  away_team_goal_count = c(0,1,1,3,0,1,2,2,2,0,3,1))

euro_data <-rbind(euro_data, euro_data_test)

euro_data_test2 <- data.frame(
  date_GMT = rep(11, ngames_prev),
  home_team_name = c("Russia", "Turkey",
                      "Italy", "Ukraine",
                     "Denmark", "Netherlands",
                     "Sweden", "Croatia",
                     "England", "Hungary", 
                     "Germany", "Spain"),
  away_team_name = c("Finland", "Wales",
                     "Switzerland", "FYR Macedonia",
                      "Belgium", "Austria",
                     "Slovakia",  "Czech Republic",
                     "Scotland", "France",
                     "Portugal", "Poland"),
  
  home_team_goal_count = c(1,0,3,2,1,2,1,1,0,1,4,1),
  away_team_goal_count = c(0,2,0,1,2,0,0,1,0,1,2,1))

euro_data <-rbind(euro_data, euro_data_test2)

euro_data_test3 <- data.frame(
  date_GMT = rep(12, ngames_prev),
  home_team_name = c( "Italy","Switzerland",  
                      "Ukraine", "Netherlands",
                     "Denmark", "Finland", 
                     "Scotland","England",
                     "Sweden", "Spain",
                     "Portugal", "Germany" ),
  away_team_name = c("Wales", "Turkey",
                     "Austria", "FYR Macedonia",
                     "Russia", "Belgium", 
                     "Croatia", "Czech Republic",
                     "Poland","Slovakia",  
                    "France","Hungary"),
  
  home_team_goal_count = c(1,3,0,3,4,0,1,1,3,5,2,2),
  away_team_goal_count = c(0,1,1,0,1,2,3,0,2,0,2,2))

euro_data <-rbind(euro_data, euro_data_test3)

ngames_prev <- 8
euro_data_test_round16 <- data.frame(
  date_GMT = rep(13, ngames_prev),
  home_team_name = c("Wales", "Italy",
                     "Netherlands", "Belgium",
                     "Croatia", "France",
                     "England", "Sweden"),
  away_team_name = c( "Denmark", "Austria",
                      "Czech Republic", "Portugal",
                      "Spain", "Switzerland",
                      "Germany", "Ukraine"),
  home_team_goal_count = c(0,2,0,1,3,3,2,1),
  away_team_goal_count = c(4,1,2,0,5,3,0,2))

euro_data <-rbind(euro_data, euro_data_test_round16)

ngames_prev <- 4
euro_data_test_q <- data.frame(
  date_GMT = rep(14, ngames_prev),
  home_team_name = c("Switzerland", "Belgium", 
                     "Czech Republic", "Ukraine"),
  away_team_name = c( "Spain", "Italy", "Denmark", "England"),
  home_team_goal_count = c(1,1,1,0),
  away_team_goal_count = c(1,2,2,4))

euro_data <-rbind(euro_data, euro_data_test_q)

ngames_prev <- 2
euro_data_test_semi <- data.frame(
  date_GMT = rep(15, ngames_prev),
  home_team_name = c("Italy", "England"),
  away_team_name = c( "Spain",  "Denmark" ),
  home_team_goal_count = c(1,2),
  away_team_goal_count = c(1,1))

euro_data <-rbind(euro_data, euro_data_test_semi)

ngames_prev <- 2
euro_data_test_final <- data.frame(
  date_GMT = rep(16, ngames_prev),
  home_team_name = c( "England", "Spain"  ),
  away_team_name = c( "Italy", "Denmark" ),
  home_team_goal_count = rep(NA, ngames_prev),
  away_team_goal_count = rep(NA, ngames_prev))

euro_data <-rbind(euro_data, euro_data_test_final)




p_aggr <- mean(c(0.624,0.289,0.086,0.745,0.662,0.626,0.587,0.357,0.18,0.224,0.535,0.455,
               0.474, 0.36,0.524,0.628,0.47,0.584,0.58,0.238,0.152,0.224,0.326,0.223,
               0.553,0.553,0.308,0.743,0.694,0.769,0.454,0.677,0.466,0.677,0.276,0.199))

p_aggr_16 <- mean(c(0.344,0.212,0.19,0.657,0.269,0.224,0.614,0.246))

p_quarti <- mean(c(0.252, 0.195, 0.463, 0.559))

p_semi <- mean(c(0.269, 0.269))

p_final <- 0.331

p_tot <- mean(c(0.624,0.289,0.086,0.745,0.662,0.626,0.587,0.357,0.18,0.224,0.535,0.455,
             0.474, 0.36,0.524,0.628,0.47,0.584,0.58,0.238,0.152,0.224,0.326,0.223,
             0.553,0.553,0.308,0.743,0.694,0.769,0.454,0.677,0.466,0.677,0.276,0.199,
             0.344,0.212,0.19,0.657,0.269,0.224,0.614,0.246,
             0.252, 0.195, 0.463, 0.559, 0.269, 0.269,
             0.331))

psuedo_R2_tot <- prod(c(0.624,0.289,0.086,0.745,0.662,0.626,0.587,0.357,0.18,0.224,0.535,0.455,
                        0.474, 0.36,0.524,0.628,0.47,0.584,0.58,0.238,0.152,0.224,0.326,0.223,
                        0.553,0.553,0.308,0.743,0.694,0.769,0.454,0.677,0.466,0.677,0.276,0.199,
                        0.344,0.212,0.19,0.657,0.269,0.224,0.614,0.246,
                        0.252, 0.195, 0.463, 0.559, 0.269, 0.269,
                        0.331))^(1/51)
## Static models

fit1 <- stan_foot(data = euro_data,
                  model="double_pois", predict= ngames_prev,
                  iter = 2000) # double poisson
print(fit1, pars =c("home"))
foot_abilities(fit1, euro_data, cex.var=0.6)
log_lik_1 <- extract_log_lik(fit1)
loo1 <- loo(fit1)
foot_prob(fit1, euro_data)


fit1_rank <- stan_foot(data = euro_data,
                  model="double_pois",
                  ranking = fifa_ranking, 
                  predict=ngames_prev,
                  iter = 2000) # double poisson
print(fit1_rank, pars =c("home", "gamma"))
foot_abilities(fit1_rank, euro_data, cex.var=0.6)
log_lik_1_rank <- extract_log_lik(fit1_rank)
loo1_rank <- loo(fit1_rank)
foot_prob(fit1_rank, euro_data)


fit2 <- stan_foot(data = euro_data,
                  model="biv_pois", predict=ngames_prev,
                  iter = 2000) # biv poisson
print(fit2, pars =c("home", "rho"))
foot_abilities(fit2, euro_data)
log_lik_2 <- extract_log_lik(fit2)
loo2 <- loo(fit2)
foot_prob(fit2, euro_data)

fit2_rank <- stan_foot(data = euro_data,
                       model="biv_pois",
                       ranking = fifa_ranking, predict=ngames_prev,
                       iter = 2000) # biv poisson
print(fit2_rank, pars =c("home", "gamma"))
foot_abilities(fit2_rank, euro_data, cex.var=0.6)
log_lik_2_rank <- extract_log_lik(fit2_rank)
loo2_rank <- loo(fit2_rank)
foot_prob(fit2_rank, euro_data)


fit3 <- stan_foot(data = euro_data,
                  model="skellam",
                  iter = 2000) # skellam
print(fit3, pars =c("home"))
foot_abilities(fit3, euro_data, cex.var=0.6)
log_lik_3 <- extract_log_lik(fit3)
loo3 <- loo(fit3)

## dynamic models

fit4_rank <- stan_foot(data = euro_data,
                  model="double_pois",
                  iter = 2000, cores = 4,
                  predict=ngames_prev,
                  ranking = fifa_ranking,
                  dynamic_type = "seasonal") # double poisson
foot_abilities(fit4_rank, euro_data)
log_lik_4_rank <- extract_log_lik(fit4_rank)
loo4_rank <- loo(fit4_rank)
foot_prob(fit4_rank, euro_data)

# expected goals
sims <-rstan::extract(fit4_rank)
exp_h <- round(apply(sims$theta_home_prev, 2, median),2)
exp_a <- round(apply(sims$theta_away_prev, 2, median),2)
exp_table <- cbind(euro_data_test_q$home_team_name, euro_data_test_q$away_team_name, exp_h, exp_a )
colnames(exp_table) <- c("home", "away",
                         "exp_home", "exp_away")

## global winner probabilties

att <- sims$att
def <- sims$def
gamma <- sims$gamma
home <- sims$home
theta_home_prev <- sims$theta_home_prev
theta_away_prev <- sims$theta_away_prev
y_prev <- sims$y_prev

team_home <- euro_data_test_semi$home_team_name
team_away <- euro_data_test_semi$away_team_name

final_team_1 = final_team_2 = c()

# first semifinal

  final_team_1[y_prev[,1,1] > y_prev[,1,2]] <- team_home[1]

  final_team_1[y_prev[,1,1] < y_prev[,1,2]] <- team_away[1]

  final_team_1[y_prev[,1,1] == y_prev[,1,2]] <- sample(
                         c(team_home[1], team_away[1]), 
                         sum(y_prev[,1,1] == y_prev[,1,2]),
                         replace = TRUE,
                         c(foot_prob(fit4_rank, euro_data)$prob_table[1,3],
                           foot_prob(fit4_rank, euro_data)$prob_table[1,4]+
                             foot_prob(fit4_rank, euro_data)$prob_table[1,5]))
  

# second semifinal
  
  final_team_2[y_prev[,2,1] > y_prev[,2,2]] <- team_home[2]
  
  final_team_2[y_prev[,2,1] < y_prev[,2,2]] <- team_away[2]
  
  final_team_2[y_prev[,2,1] == y_prev[,2,2]] <- sample(
    c(team_home[2], team_away[2]), 
    sum(y_prev[,2,1] == y_prev[,2,2]),
    replace = TRUE,
    c(foot_prob(fit4_rank, euro_data)$prob_table[2,3],
      foot_prob(fit4_rank, euro_data)$prob_table[2,4]+
        foot_prob(fit4_rank, euro_data)$prob_table[2,5]))

# final simulation
N <- 442
teams <- unique(euro_data$home_team_name)
team_home <- match( euro_data$home_team_name, teams)
team_away <- match( euro_data$away_team_name, teams)
team1 <- team_home[1:N]
team2 <- team_away[1:N]
team1_prev <- team_home[(N+1):(N+ngames_prev)]
team2_prev <- team_away[(N+1):(N+ngames_prev)]

colnames(fifa_ranking) <- c("rank_team", "points")
team_order <- match(teams, fifa_ranking$rank_team)

fifa_ranking[,1] <- fifa_ranking$rank_team[team_order]
fifa_ranking[,2] <- fifa_ranking$points[team_order]
fifa_ranking[,2] <- (as.numeric(as.vector(fifa_ranking[,2]))-mean(as.numeric(as.vector(fifa_ranking[,2]))))/(2*sd(as.numeric(as.vector(fifa_ranking[,2]))))
ranking <- fifa_ranking[,2]
y_prev_final <- matrix(NA, dim(theta_home_prev)[1] ,2)
theta_home_prev_final = theta_away_prev_final = c()
final_team_1_prev = final_team_2_prev = c()
final_team_1_prev[final_team_1=="Italy"] <- 18
final_team_1_prev[final_team_1=="Spain"] <- 17
final_team_2_prev[final_team_2=="England"] <- 10
final_team_2_prev[final_team_2=="Denmark"] <- 35

for (s in 1:dim(theta_home_prev)[1]){
  theta_home_prev_final[s] = exp(home[s]+att[s,15,final_team_1_prev[s]]+
                             def[s,15, final_team_2_prev[s]]+
                             (gamma[s]/2)*(ranking[final_team_1_prev[s]]-ranking[final_team_2_prev[s]]));
  theta_away_prev_final = exp(att[s,15,final_team_2_prev[s]]+
                             def[s,15, final_team_1_prev[s]]-
                             (gamma[s]/2)*(ranking[final_team_1_prev[s]]-ranking[final_team_2_prev[s]]));
} 
  y_prev_final[,1] = rpois(dim(theta_home_prev)[1], theta_home_prev_final);
  y_prev_final[,2] = rpois(dim(theta_home_prev)[1], theta_away_prev_final);
  
  winner_team <- c()
  
  winner_team[y_prev_final[,1] > y_prev_final[,2]] <- final_team_1[y_prev_final[,1] > y_prev_final[,2]]
  
  winner_team[y_prev_final[,1] < y_prev_final[,2]] <- final_team_2[y_prev_final[,1] < y_prev_final[,2]]
  
  for (s in 1:sum(y_prev_final[,1] == y_prev_final[,2])){
  winner_team[y_prev_final[,1] == y_prev_final[,2]][s] <- sample(
    c(final_team_1[y_prev_final[,1] == y_prev_final[,2]][s],
    final_team_2[y_prev_final[,1] == y_prev_final[,2]][s]),
    1,
    replace = TRUE)
  }
  
  ord <- sort.int(100*as.double(table((winner_team)))/dim(theta_away_prev)[1], decreasing = TRUE, index.return = TRUE)
  final_probabilities <- cbind(names(table(winner_team))[ord$ix], 
                               ord$x
                               )
  colnames(final_probabilities) <- c("Winning team", "Winning %")
  



fit5 <- stan_foot(data = euro_data,
                  model="skellam",
                  iter = 2000, cores = 4,
                  predict=ngames_prev,
                  #ranking = fifa_ranking,
                  dynamic_type = "seasonal") # skellam
foot_abilities(fit5, euro_data)
log_lik_5 <- extract_log_lik(fit5)
loo5 <- loo(fit5)
foot_prob(fit5, euro_data)


fit5_rank <- stan_foot(data = euro_data,
                  model="skellam",
                  iter = 2000, cores = 4,
                  predict=ngames_prev,
                  ranking = fifa_ranking,
                  dynamic_type = "seasonal") # skellam
foot_abilities(fit5_rank, euro_data)
log_lik_5_rank <- extract_log_lik(fit5_rank)
loo5_rank <- loo(fit5_rank)
foot_prob(fit5_rank, euro_data)


fit6_rank <- stan_foot(data = euro_data,
                       model="biv_pois",
                       iter = 2000, cores = 4,
                       predict=ngames_prev,
                       ranking = fifa_ranking,
                       dynamic_type = "seasonal") # biv_pois
foot_abilities(fit6_rank, euro_data)
log_lik_6_rank <- extract_log_lik(fit6_rank)
loo6_rank <- loo(fit6_rank)
foot_prob(fit6_rank, euro_data)







