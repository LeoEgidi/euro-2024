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
library(bayesplot)
library(ggpmisc)
library(gridExtra)
library(grid)
library(MLmetrics)

# Data from national teams' matches from 1871 until WC Cup 2022.
wc_data <- read.csv2("C:\\Users\\vasileios palaskas\\Dropbox\\World Cup 2022\\results.csv", sep = ",")
wc_data_train <- wc_data  %>%
  select(date, home_team, away_team,
         home_score, away_score, tournament) %>%
  filter(as.Date(wc_data$date) > "2018-08-01" )# keep the last 3 years of observations

# Fifa rankings of national teams per year

fifa_ranking <- read.csv2("C:\\Users\\vasileios palaskas\\Dropbox\\World Cup 2022\\fifa_ranking.csv", sep=",", header=TRUE)
fifa_ranking$total_points <- as.numeric(as.vector(fifa_ranking$total_points))/(10^3)
fifa_ranking_clean <- fifa_ranking %>%
  select(country_full, total_points ) 
fifa_ranking_clean <- fifa_ranking_clean[63707:dim(fifa_ranking)[1], ]# Keep the latest available FIFA Rankings of each team
colnames(fifa_ranking_clean) <- c("team_name", "ranking")

#Keep year as a date variable to be used later for dynamic priors inclusion.
times <-substr(wc_data_train$date, 1, 4)

times <- as.factor(times)
levels(times) <- c(1:length(levels(times)))
wc_data_train$date <- as.numeric(as.vector(times))
wc_data_train <- arrange(wc_data_train, date)

wc_data_train <- wc_data_train %>%
  filter(!is.na(wc_data_train$home_score) & 
           !is.na(wc_data_train$away_score) )# remove matches with either NA home or away score


# get rid of low and not existing teams
low_teams <- c(
  "Haiti","Sint Maarten","CuraÃÂ§ao",
  "Grenada","Cuba","Turks and Caicos Islands",
  "Jersey", "Hitra", "Isle of Man",
  "Yorkshire", "Panjab", "Somaliland",
  "Kernow", "Barawa", "Chagos Islands",
  "Cascadia", "Parishes of Jersey", "Alderney",
  "Yoruba Nation",   "Matabeleland", "Biafra",
  "Mapuche", "Maule Sur", "Aymara", "Saint Helena",
  "Shetland", "Ynys MÃÂ´n", "Orkney", "Guernsey", "Western Isles"  )

other_discarded_teams <- wc_data_train %>% filter( !(home_team 
                                                     %in% fifa_ranking_clean$team_name ))
no_teams <- c(low_teams, other_discarded_teams$home_team)
wc_1 <- wc_data_train %>% filter( !(home_team %in% no_teams ))
wc_2 <- wc_1 %>% filter( !(away_team %in% no_teams ))
wc_data_train <- wc_2

# clean Fifa ranking to be ready for Stan!

wc_train_teams <- unique(wc_data_train$home_team)
wc_ranking <- fifa_ranking_clean %>% filter( team_name %in% wc_train_teams)



# Load Stan Model objects run for each matchday 1, 2, 3 of Group Phase.
setwd("C:/Users/vasileios palaskas/Desktop/matchdays_1_2")

load("fit_diag_infl_biv_pois_matchday1")
load("fit_biv_pois_matchday1")
load("fit_double_pois_matchday1")

load("fit_diag_infl_biv_pois_matchday2")
load("fit_biv_pois_matchday2")
load("fit_double_pois_matchday2")

load("fit_diag_infl_biv_pois_matchday3")
load("fit_biv_pois_matchday3")
load("fit_double_pois_matchday3")

load("fit_diag_infl_biv_pois_matchday4")
load("fit_biv_pois_matchday4")
load("fit_double_pois_matchday4")
# Loo evaluation-----

# 
# # Matchday 1
# loo(fit_diag_infl_biv_pois_matchday1)# 22398.2 170.1
# loo(fit_biv_pois_matchday1)# 18105.2 119.9
# loo(fit_double_pois_matchday1)# 18100.6 121.3
#                       
# # Matchday 2
# loo(fit_diag_infl_biv_pois_matchday2)# 21935.6 169.0
# loo(fit_biv_pois_matchday2)# 17766.8 119.1
# loo(fit_double_pois_matchday2)#  17764.0 120.3
# 
# # Matchday 3
# loo(fit_diag_infl_biv_pois_matchday3)# 22050.8 169.8
# loo(fit_biv_pois_matchday3) # 17865.2 119.5
# loo(fit_double_pois_matchday3)# 17857.3 121.0
#                       
# Posterior predictive distribution of the goal difference for out-sample matches -----------------------
# (matchdays 1 & 2 of World Cup)

# Matchday 1----------
goal_diff_pred_test_fit_diag_infl_biv_pois_matchday1 <-rstan::extract(fit_diag_infl_biv_pois_matchday1,pars="y_prev" )
goal_diff_pred_test_fit_diag_infl_biv_pois_matchday1<-goal_diff_pred_test_fit_diag_infl_biv_pois_matchday1$y_prev[,,1]-
  goal_diff_pred_test_fit_diag_infl_biv_pois_matchday1$y_prev[,,2]

goal_diff_pred_test_fit_biv_pois_matchday1 <-rstan::extract(fit_biv_pois_matchday1 ,pars="y_prev")
goal_diff_pred_test_fit_biv_pois_matchday1<-goal_diff_pred_test_fit_biv_pois_matchday1$y_prev[,,1]-
  goal_diff_pred_test_fit_biv_pois_matchday1$y_prev[,,2]

goal_diff_pred_test_fit_double_pois_matchday1 <-rstan::extract(fit_double_pois_matchday1,pars="y_prev" )
goal_diff_pred_test_fit_double_pois_matchday1<-goal_diff_pred_test_fit_double_pois_matchday1$y_prev[,,1]-
  goal_diff_pred_test_fit_double_pois_matchday1$y_prev[,,2]


# Matchday 2----------
goal_diff_pred_test_fit_diag_infl_biv_pois_matchday2 <-rstan::extract(fit_diag_infl_biv_pois_matchday2,pars="y_prev" )
goal_diff_pred_test_fit_diag_infl_biv_pois_matchday2<-goal_diff_pred_test_fit_diag_infl_biv_pois_matchday2$y_prev[,,1]-
  goal_diff_pred_test_fit_diag_infl_biv_pois_matchday2$y_prev[,,2]

goal_diff_pred_test_fit_biv_pois_matchday2 <-rstan::extract(fit_biv_pois_matchday2 ,pars="y_prev")
goal_diff_pred_test_fit_biv_pois_matchday2<-goal_diff_pred_test_fit_biv_pois_matchday2$y_prev[,,1]-
  goal_diff_pred_test_fit_biv_pois_matchday2$y_prev[,,2]

goal_diff_pred_test_fit_double_pois_matchday2 <-rstan::extract(fit_double_pois_matchday2,pars="y_prev" )
goal_diff_pred_test_fit_double_pois_matchday2<-goal_diff_pred_test_fit_double_pois_matchday2$y_prev[,,1]-
  goal_diff_pred_test_fit_double_pois_matchday2$y_prev[,,2]


# Matchday 3----------
goal_diff_pred_test_fit_diag_infl_biv_pois_matchday3 <-rstan::extract(fit_diag_infl_biv_pois_matchday3,pars="y_prev" )
goal_diff_pred_test_fit_diag_infl_biv_pois_matchday3<-goal_diff_pred_test_fit_diag_infl_biv_pois_matchday3$y_prev[,,1]-
  goal_diff_pred_test_fit_diag_infl_biv_pois_matchday3$y_prev[,,2]

goal_diff_pred_test_fit_biv_pois_matchday3 <-rstan::extract(fit_biv_pois_matchday3 ,pars="y_prev")
goal_diff_pred_test_fit_biv_pois_matchday3<-goal_diff_pred_test_fit_biv_pois_matchday3$y_prev[,,1]-
  goal_diff_pred_test_fit_biv_pois_matchday3$y_prev[,,2]

goal_diff_pred_test_fit_double_pois_matchday3 <-rstan::extract(fit_double_pois_matchday3,pars="y_prev" )
goal_diff_pred_test_fit_double_pois_matchday3<-goal_diff_pred_test_fit_double_pois_matchday3$y_prev[,,1]-
  goal_diff_pred_test_fit_double_pois_matchday3$y_prev[,,2]

# Matchday 4----------
goal_diff_pred_test_fit_diag_infl_biv_pois_matchday4 <-rstan::extract(fit_diag_infl_biv_pois_matchday4,pars="y_prev" )
goal_diff_pred_test_fit_diag_infl_biv_pois_matchday4<-goal_diff_pred_test_fit_diag_infl_biv_pois_matchday4$y_prev[,,1]-
  goal_diff_pred_test_fit_diag_infl_biv_pois_matchday4$y_prev[,,2]

goal_diff_pred_test_fit_biv_pois_matchday4 <-rstan::extract(fit_biv_pois_matchday4 ,pars="y_prev")
goal_diff_pred_test_fit_biv_pois_matchday4<-goal_diff_pred_test_fit_biv_pois_matchday4$y_prev[,,1]-
  goal_diff_pred_test_fit_biv_pois_matchday4$y_prev[,,2]

goal_diff_pred_test_fit_double_pois_matchday4 <-rstan::extract(fit_double_pois_matchday4,pars="y_prev" )
goal_diff_pred_test_fit_double_pois_matchday4<-goal_diff_pred_test_fit_double_pois_matchday4$y_prev[,,1]-
  goal_diff_pred_test_fit_double_pois_matchday4$y_prev[,,2]


# Combine both out-sample post. pred. distribution 
# of goal differences from each fitted model 
# for Matchdays 1 & 2 & 3--------------------
goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4<-cbind(
  goal_diff_pred_test_fit_diag_infl_biv_pois_matchday1,
  goal_diff_pred_test_fit_diag_infl_biv_pois_matchday2,
  goal_diff_pred_test_fit_diag_infl_biv_pois_matchday3,
  goal_diff_pred_test_fit_diag_infl_biv_pois_matchday4
  
)

goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4<-cbind(
  goal_diff_pred_test_fit_biv_pois_matchday1,
  goal_diff_pred_test_fit_biv_pois_matchday2,
  goal_diff_pred_test_fit_biv_pois_matchday3,
  goal_diff_pred_test_fit_biv_pois_matchday4
  
)

goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4<-cbind(
  goal_diff_pred_test_fit_double_pois_matchday1,
  goal_diff_pred_test_fit_double_pois_matchday2,
  goal_diff_pred_test_fit_double_pois_matchday3,
  goal_diff_pred_test_fit_double_pois_matchday4
  
)

# Actual goal differences of matches included within 
# Matchdays 1-2-3------------------------------------
ngames_prev <- 16
wc_data_test_matchday_1 <- data.frame(
  date = rep(length(levels(times))+1, ngames_prev),
  home_team = c("Qatar", "England" , "Senegal",
                "United States", "Argentina", "Denmark",
                "Mexico", "France", "Morocco", "Germany",
                "Spain","Belgium",
                "Switzerland","Uruguay",
                "Portugal","Brazil"),
  
  away_team = c("Ecuador", "Iran", "Netherlands",
                "Wales", "Saudi Arabia", "Tunisia",
                "Poland", "Australia", "Croatia", "Japan",
                "Costa Rica","Canada",
                "Cameroon","South Korea",
                "Ghana","Serbia"),
  
  home_score = c(0,6,0,1,1,0,0,4,0,1,7,1,1,0,3,2),
  away_score = c(2,2,2,1,2,0,0,1,0,2,0,0,0,0,2,0),
  tournament = rep("World Cup 2022", ngames_prev))

ngames_matchday2 <- 16
wc_data_test_matchday_2 <- data.frame(
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
  tournament = rep("World Cup 2022", ngames_matchday2  ))

# Matchday 3
ngames_matchday3 <- 16
wc_data_test_matchday_3 <- data.frame(
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
  
  home_score = c(1,2,0,0,1,1,0,1,0,1,2,2,2,0,2,1),
  away_score = c(2,0,1,3,0,0,2,2,0,2,1,4,1,2,3,0),
  tournament = rep("World Cup 2022", ngames_matchday3))

# Matchday 4
ngames_matchday4 <- 8
wc_data_test_matchday_4 <- data.frame(
  date = rep(length(levels(times))+2, ngames_matchday4),
  home_team = c("Netherlands", "Argentina", 
                "France","England",
                "Japan","Brazil" ,
                "Morocco", "Portugal"),
  
  away_team = c( "United States", "Australia",
                 "Poland", "Senegal",
                 "Croatia", "South Korea",
                 "Spain", "Switzerland"),
  
  home_score = c(3,2,3,3,1,4,0,6 ),
  away_score = c(1,1,1,0,1,1,0,1 ),
  tournament = rep("World Cup 2022",ngames_matchday4))


wc_data_test_matchday_1_2_3_4<-rbind(wc_data_test_matchday_1,
                                   wc_data_test_matchday_2,
                                   wc_data_test_matchday_3,
                                   wc_data_test_matchday_4)

goal_diff_test<-wc_data_test_matchday_1_2_3_4$home_score-
  wc_data_test_matchday_1_2_3_4$away_score
# Until now, we keep the available results
# goal_diff_test<-goal_diff_test[c(1:46)]
# goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4<-goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4[,c(1:46)]
# goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4<-goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4[,c(1:46)]
# goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4<-goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4[,c(1:46)]

# Obtain the probabilistic predictions from each fitted model
# of the results 1 X 2 for each match included 
# within Matchdays 1 & 2 & 3---------
calculate_probs_results<-function(posterior_goal_difference){
  prob_home_win<-length(posterior_goal_difference[posterior_goal_difference>0])/length(posterior_goal_difference)
  prob_away_win<-length(posterior_goal_difference[posterior_goal_difference<0])/length(posterior_goal_difference)
  prob_draw_win<-length(posterior_goal_difference[posterior_goal_difference==0])/length(posterior_goal_difference)
  
  return(c(prob_home_win,prob_draw_win,prob_away_win))
}

# Initialisation of the matrices with probabilities of each match outcome
pred_results_diag_infl_biv_pois<-matrix(NA,nrow=dim(goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4)[2],
                                        ncol=3)
pred_results_biv_pois<-matrix(NA,nrow=dim(goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4)[2],
                              ncol=3)
pred_results_double_pois<-matrix(NA,nrow=dim(goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4)[2],
                                 ncol=3)
# In this loop, we calculate for each fitted model the probabilities of results
# using the posterior predict. distrib. of goal differences for each match (n_test matches)
for (i in 1:length(goal_diff_test)){
  pred_results_diag_infl_biv_pois[i,]<-calculate_probs_results(goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4[,i])
  pred_results_biv_pois[i,]<-calculate_probs_results(goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4[,i])
  pred_results_double_pois[i,]<-calculate_probs_results(goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4[,i])
}
colnames(pred_results_diag_infl_biv_pois)<-colnames(pred_results_biv_pois)<-colnames(pred_results_double_pois)<-
  c("Home_Win","Draw","Away_Win")



# Error metrics Calculation (MAE, Accuracy, etc..)-------------------

# Function to obtain summarised all cv measures
rmse <- function(y, yhat) {
  sqrt(mean((yhat - y)^2))
}
mae <- function(y, yhat) {
  mean(abs(yhat - y))
}
accuracy<-function(y, yhat) {
  mean(y==yhat)
}

cv_check_mae <- function(y_actual, y_pred) {
  #---Post. pred. distribution of MAE
  mae_dist_fp_pred <- apply(y_pred, 1, mae, y = y_actual)
  
  return(c(
    avg_bayesian_mae = mean(mae_dist_fp_pred),
    sd_bayesian_mae = sd(mae_dist_fp_pred)
  ))
}

# Tables of Bayesian metrics (Avg and within brackets the sd)--------------------

# diag. infl. biv. poisson
mae_metrics_diag_infl_biv_pois<-round(cv_check_mae(y_actual = goal_diff_test,
                                                   y_pred=goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4),
                                      2)

avg_sd_diag_infl_biv_pois<-data.frame(cbind( paste0(mae_metrics_diag_infl_biv_pois[1]," (",
                                                    mae_metrics_diag_infl_biv_pois[2], ")")
)) 
colnames(avg_sd_diag_infl_biv_pois)<-c("MAE")

#  Biv. poisson

mae_metrics_biv_pois<-round(cv_check_mae(y_actual = goal_diff_test,
                                         y_pred=goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4),
                            2)

avg_sd_biv_pois<-data.frame(cbind( paste0(mae_metrics_biv_pois[1]," (",
                                          mae_metrics_biv_pois[2], ")"))
)
colnames(avg_sd_biv_pois)<-c("MAE")

# Double Poisson
mae_metrics_double_pois<-round(cv_check_mae(y_actual = goal_diff_test,
                                            y_pred=goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4),2
) 
avg_sd_double_pois<-data.frame(cbind( paste0(mae_metrics_double_pois[1]," (",
                                             mae_metrics_double_pois[2], ")")
) )

colnames(avg_sd_double_pois)<-c("MAE")                       
# Graphical comparison----------------------


plot1<-ppc_bars(goal_diff_test,
                goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4,
                prob = 0.95, width = 0.9, 
                size = 1, fatten = 3,
                freq = TRUE)+labs(y="")+theme(legend.position="none")+
  annotate(geom = "table", x =  7, y = 10,label = list(avg_sd_diag_infl_biv_pois) )+ 
  scale_x_continuous(breaks=c(seq(-4,8,1)),limits=c(-4,8))+
  scale_y_continuous(breaks=c(seq(0,18,6)),limits=c(0,19))+
  ggtitle("Diag. Infl. Biv. Poisson")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 25, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 25, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))

#+scale_x_continuous(breaks = seq(-3,7,1), limits = c(-3,7))

plot2<-ppc_bars(goal_diff_test,goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4,
                prob = 0.95, width = 0.9, 
                size = 1, fatten = 3,
                freq = TRUE)+
  labs(y="Number of games")+theme(legend.position="none")+annotate(geom = "table", x =  7,
                                                                   y = 10,label = list(avg_sd_biv_pois)  
  )+scale_x_continuous(breaks=c(seq(-4,8,1)),limits=c(-4,8))+
  scale_y_continuous(breaks=c(seq(0,18,6)),limits=c(0,19))+
  ggtitle(" Biv. Poisson")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 25, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 25, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))
#+scale_x_continuous(breaks = seq(-3,7,1), limits = c(-3,7))+
plot3<-ppc_bars(goal_diff_test,goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4,
                prob = 0.95, width = 0.9, 
                size = 1, fatten = 3,
                freq = TRUE)+
  labs(x="Goal Difference",y="")+theme(legend.position="none")+annotate(geom = "table", x =  7,
                                                                        y = 10,label = list(avg_sd_double_pois)  
  )+scale_x_continuous(breaks=c(seq(-4,8,1)),limits=c(-4,8))+
  scale_y_continuous(breaks=c(seq(0,18,6)),limits=c(0,19))+ggtitle("Double. Poisson")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 25, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 25, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))


## Combine these plots
setwd("C:/Users/vasileios palaskas/Dropbox/World Cup 2022/Models_Comparison")
pdf(file=paste0("WC_2022_Out_Sample_PPC",".pdf"), width =15.5, height =8.5)

grid.arrange(plot1, plot2,plot3, nrow=3,
             top =textGrob("WC 2022 (Until the Phase 16) \n Graphical Posterior Predictive Checks",
                           gp = gpar(fontface = "bold", fontsize = 20)))
dev.off()

#### Classification analysis for our models-----------------------------------------------

# Actual goal difference transformation required for the graphs
goal_diff_test_transformed<-goal_diff_test
goal_diff_test_transformed[goal_diff_test_transformed>=1]<-1# Home win
goal_diff_test_transformed[goal_diff_test_transformed<0]<--1# Away win
# goal_diff_test_transformed_factor<-factor(goal_diff_test_transformed,
#                                    levels=c(1,0,-1), labels=  c("Home_Win","Draw","Away_Win"))


# Post. pred. distr. of goal difference based on the Diag. Infl. Bivariate Poisson

goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4_transformed<-goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4
goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4_transformed[goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4_transformed<0]<--1
goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4_transformed[goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4_transformed>0]<-1
# Post. pred. distr. of goal difference based on the Bivariate Poisson


goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4_transformed<-goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4
goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4_transformed[goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4_transformed<0]<--1
goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4_transformed[goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4_transformed>0]<-1

# Post. pred. distr. of goal difference based on the Double Poisson

goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4_transformed<-goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4
goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4_transformed[goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4_transformed<0]<--1
goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4_transformed[goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4_transformed>0]<-1


cv_check_classification <- function(y_actual, y_pred,probs_y_pred) {
  # Poster. Pred. distribution of error metrics (where possible)
  
  accuracy_dist_fp_pred <- apply(y_pred, 1,accuracy, y = y_actual)# accuracy
  # Data processing required for multi class log-loss
  y_actual_transformed<-y_actual
  y_actual_transformed[y_actual_transformed>0]<-1# Home win
  y_actual_transformed[y_actual_transformed<0]<--1# Away win
  y_actual_transformed_factor<-factor(y_actual_transformed,
                                      levels=c(1,0,-1), labels=  c("Home_Win","Draw","Away_Win"))
  log_loss_dist_fp_pred <-   MultiLogLoss(y_true = y_actual_transformed_factor,  probs_y_pred)
  
  
  return(c(
    avg_bayesian_accuracy = mean(accuracy_dist_fp_pred)*100,
    sd_bayesian_accuracy = sd(accuracy_dist_fp_pred)*100,
    log_loss=log_loss_dist_fp_pred
  ))
}
# Classification metrics

# Diagon. Infl. Biv. Poisson---
clas_metrics_diag_infl_biv_pois<-round(cv_check_classification(y_actual = goal_diff_test_transformed,
                                                               y_pred=goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4_transformed,
                                                               pred_results_diag_infl_biv_pois),
                                       2)

classification_metrics_diag_infl_biv_pois<-data.frame(cbind( paste0(clas_metrics_diag_infl_biv_pois[1]," (",
                                                                    clas_metrics_diag_infl_biv_pois[2], ")"),
                                                             paste0(clas_metrics_diag_infl_biv_pois[3])
) )

colnames(classification_metrics_diag_infl_biv_pois)<-c("Accuracy","Log-Loss")
# Biv. Poisson-----
clas_metrics_biv_pois<-round(cv_check_classification(y_actual = goal_diff_test_transformed,
                                                     y_pred=goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4_transformed,
                                                     pred_results_biv_pois),
                             2)

classification_metrics_biv_pois<-data.frame(cbind( paste0(clas_metrics_biv_pois[1]," (",
                                                          clas_metrics_biv_pois[2], ")"),
                                                   paste0(clas_metrics_biv_pois[3])
) )

colnames(classification_metrics_biv_pois)<-c("Accuracy","Log-Loss")

# Double. Poisson-----
clas_metrics_double_pois<-round(cv_check_classification(y_actual = goal_diff_test_transformed,
                                                        y_pred=goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4_transformed,
                                                        pred_results_double_pois),
                                2)

classification_metrics_double_pois<-data.frame(cbind( paste0(clas_metrics_double_pois[1]," (",
                                                             clas_metrics_double_pois[2], ")"),
                                                      paste0(clas_metrics_double_pois[3] )
) )

colnames(classification_metrics_double_pois)<-c("Accuracy","Log-Loss")

# Plots visualizing the posterior predictive distribution of outomes related
# with the observed ones ' frequencies

plot1<-ppc_bars(goal_diff_test_transformed,
                goal_diff_pred_test_fit_diag_infl_biv_pois_matchday_1_2_3_4_transformed,
                prob = 0.95, width = 0.9, 
                size = 1, fatten = 3,
                freq = TRUE)+annotate(geom = "table",
                                      x =  7,
                                      y = 4,label = list(classification_metrics_diag_infl_biv_pois)  
                )+scale_x_discrete(limits=c(-1,0,1),labels=c("<0","0",">0"))+
  labs(x="",y="")+
  ggtitle("Diag. Infl. Bivariate Poisson")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 25, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 25, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))+theme(legend.position="none")

plot2<-ppc_bars(goal_diff_test_transformed,
                goal_diff_pred_test_fit_biv_pois_matchday_1_2_3_4_transformed,
                prob = 0.95, width = 0.9, 
                size = 1, fatten = 3,
                freq = TRUE)+annotate(geom = "table",
                                      x =  7,
                                      y = 4,label = list(classification_metrics_biv_pois)  
                )+scale_x_discrete(limits=c(-1,0,1),labels=c("<0","0",">0"))+
  labs(x="",y="Number of Matches")+
  ggtitle("Bivariate Poisson")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 25, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 25, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))+theme(legend.position="none")                   

plot3<-ppc_bars(goal_diff_test_transformed,
                goal_diff_pred_test_fit_double_pois_matchday_1_2_3_4_transformed,
                prob = 0.95, width = 0.9, 
                size = 1, fatten = 3,
                freq = TRUE)+annotate(geom = "table",
                                      x =  7,
                                      y = 4,label = list(classification_metrics_double_pois)  
                )+scale_x_discrete(limits=c(-1,0,1),labels=c("<0","0",">0"))+
  labs(x="Goal Difference",y="")+
  ggtitle("Double. Poisson")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 25, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 25, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))+theme(legend.position="none")
##---Combine these plots

pdf(file=paste0("WC_2022_Out_Sample_PPC_Classification",".pdf"), width =15.5, height =8.5)

grid.arrange(plot1, plot2,plot3, nrow=3,
             top =textGrob("WC 2022 (Until the Phase 16) \n Graphical Posterior Predictive Checks",
                           gp = gpar(fontface = "bold", fontsize = 20)))
dev.off()
