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


wc_data <- read.csv2("C:\\Users\\vasileios palaskas\\Dropbox\\World Cup 2022\\results.csv", sep = ",")
wc_data_train <- wc_data  %>%
  select(date, home_team, away_team,
         home_score, away_score, tournament) %>%
  filter(as.Date(wc_data$date) > "2018-08-01" )


fifa_ranking <- read.csv2("C:\\Users\\vasileios palaskas\\Dropbox\\World Cup 2022\\fifa_ranking.csv", sep=",", header=TRUE)
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
  "Shetland", "Ynys MÃÂ´n", "Orkney", "Guernsey", "Western Isles"  )
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
  tournament = rep(NA, ngames_prev))

wc_data_stan <-rbind(wc_data_train, wc_data_test)


## dynamic models
n_iter <- 2000
fit_diag_infl_biv_pois <- stan_foot(data = wc_data_stan[,-6],
                 model="diag_infl_biv_pois",
                 iter = n_iter, cores = 2, chains=2,
                 predict= ngames_prev,
                 ranking = wc_ranking,
                 dynamic_type = "seasonal",
                 ind_home = "FALSE") # diag. inflated biv pois
save(fit_diag_infl_biv_pois,file="fit_diag_infl_biv_pois_matchday1")
fit_diag_infl_biv_pois_matchday1<-fit_diag_infl_biv_pois
save(fit_diag_infl_biv_pois_matchday1,
     file="fit_diag_infl_biv_pois_matchday1")


fit_biv_pois <- stan_foot(data = wc_data_stan[,-6],
                                    model="biv_pois",
                                    iter = n_iter,cores = 2, chains=2, 
                                    predict= ngames_prev,
                                    ranking = wc_ranking,
                                    dynamic_type = "seasonal",
                                    ind_home = "FALSE") # diag. inflated biv pois
save(fit_biv_pois,file="fit_biv_pois_matchday1")
fit_biv_pois_matchday1<-fit_biv_pois
save(fit_biv_pois_matchday1,file="fit_biv_pois_matchday1")

fit_double_pois <- stan_foot(data = wc_data_stan[,-6],
                         model="double_pois",
                         iter = n_iter, cores = 2, chains=2,
                         predict= ngames_prev,
                         ranking = wc_ranking,
                         dynamic_type = "seasonal",
                         ind_home = "FALSE") # diag. inflated biv pois

save(fit_double_pois,file="fit_double_pois_matchday1")
fit_double_pois_matchday1<-fit_double_pois
save(fit_double_pois_matchday1,file="fit_double_pois_matchday1")

## Goodness-of-fit analysis---------------------
# Load models
setwd("C:/Users/vasileios palaskas/Dropbox/World Cup 2022/Models_Comparison")
                      
load("fit_diag_infl_biv_pois_matchday1")
load("fit_biv_pois_matchday1")
load("fit_double_pois_matchday1")
# Loo measures
loo_fit_diag_infl_biv_pois<-loo(fit_diag_infl_biv_pois_matchday1)# 22398.2 170.1
loo_fit_biv_pois<-loo(fit_biv_pois_matchday1)#18105.2 119.9
loo_fit_double_pois<-loo(fit_double_pois_matchday1)# 18100.6 121.3

loo_compare(loo_fit_diag_infl_biv_pois_matchday1,loo_fit_biv_pois_matchday1,
            loo_fit_double_pois_matchday1 )
# Error metrics--------
# extracting the replications
goal_diff_pred_test_fit_diag_infl_biv_pois <-rstan::extract(fit_diag_infl_biv_pois,pars="y_prev" )
goal_diff_pred_test_fit_diag_infl_biv_pois<-goal_diff_pred_test_fit_diag_infl_biv_pois$y_prev[,,1]

goal_diff_pred_test_fit_biv_pois <-rstan::extract(fit_biv_pois ,pars="y_prev")
goal_diff_pred_test_fit_biv_pois<-goal_diff_pred_test_fit_biv_pois$y_prev[,,1]

goal_diff_pred_test_fit_double_pois <-rstan::extract(fit_double_pois,pars="y_prev" )
goal_diff_pred_test_fit_double_pois<-goal_diff_pred_test_fit_double_pois$y_prev[,,1]

goal_diff_test<-wc_data_test$home_score-
                  wc_data_test$away_score

# Function to obtain summarised all cv measures----
rmse <- function(y, yhat) {
  sqrt(mean((yhat - y)^2))
}
mae <- function(y, yhat) {
  mean(abs(yhat - y))
}
accuracy<-function(y, yhat) {
  mean(y==yhat)
}

cv_check <- function(y_actual, y_pred) {
  #---Bayesian measures
  # rmse_dist_fp_pred <- apply(y_pred, 1, rmse, y = y_actual)
  mae_dist_fp_pred <- apply(y_pred, 1, mae, y = y_actual)
  rmse_dist_fp_pred <- apply(y_pred, 1, rmse, y = y_actual)
  accuracy_dist_fp_pred <- apply(y_pred,
                                 1,accuracy, y = y_actual)
  
  return(c(
    avg_bayesian_mae = mean(mae_dist_fp_pred),
    # bayesian_rmse = mean(rmse_dist_fp_pred),
    avg_bayesian_rmse = mean(rmse_dist_fp_pred),
    avg_bayesian_accuracy = mean(accuracy_dist_fp_pred),
    
    sd_bayesian_mae = sd(mae_dist_fp_pred),
    # bayesian_rmse = mean(rmse_dist_fp_pred),
    sd_bayesian_rmse = sd(rmse_dist_fp_pred),
    sd_bayesian_accuracy = sd(accuracy_dist_fp_pred)
  ))
}


metrics_diag_infl_biv_pois<-round(cv_check(y_actual = goal_diff_test,
                                           y_pred=goal_diff_pred_test_fit_diag_infl_biv_pois),
                                  2)# diag. infl. biv_pois

metrics_biv_pois<-round(cv_check(y_actual = goal_diff_test,
         y_pred=goal_diff_pred_test_fit_biv_pois),
      2)# diag. infl. biv_pois

metrics_double_pois<-round(cv_check(y_actual = goal_diff_test,
         y_pred=goal_diff_pred_test_fit_double_pois),2
      )# diag. infl. biv_pois

# Tables of Bayesian metrics (Avg and within brackets their sd)----
# diag. infl. biv. poisson
avg_sd_diag_infl_biv_pois<-data.frame(cbind( paste0(metrics_diag_infl_biv_pois[1]," (",
                                             metrics_diag_infl_biv_pois[4], ")"),
                                      paste0(metrics_diag_infl_biv_pois[2]," (",
                                             metrics_diag_infl_biv_pois[5], ")"),
                                      paste0(metrics_diag_infl_biv_pois[3]," (",
                                             metrics_diag_infl_biv_pois[6], ")")
) )

colnames(avg_sd_diag_infl_biv_pois)<-c("MAE","RMSE","Accuracy")
#  biv. poisson
avg_sd_biv_pois<-data.frame(cbind( paste0(metrics_biv_pois[1]," (",
                                                    metrics_biv_pois[4], ")"),
                                             paste0(metrics_biv_pois[2]," (",
                                                    metrics_biv_pois[5], ")"),
                                             paste0(metrics_biv_pois[3]," (",
                                                    metrics_biv_pois[6], ")")
) )

colnames(avg_sd_biv_pois)<-c("MAE","RMSE","Accuracy")
# Double Poisson
avg_sd_double_pois<-data.frame(cbind( paste0(metrics_double_pois[1]," (",
               metrics_double_pois[4], ")"),
           paste0(metrics_double_pois[2]," (",
                  metrics_double_pois[5], ")"),
           paste0(metrics_double_pois[3]," (",
                  metrics_double_pois[6], ")")
          ) )
    
colnames(avg_sd_double_pois)<-c("MAE","RMSE","Accuracy")       
          


           


# Graphical comparison----------------------
plot1<-ppc_bars(goal_diff_test,goal_diff_pred_test_fit_diag_infl_biv_pois,
                prob = 0.95, width = 0.9, 
                size = 1, fatten = 3,
                freq = TRUE)+labs(y="")+scale_y_continuous(breaks = seq(0,8,4), limits = c(0,8))+
  ggtitle("Diag. Infl. Biv. Poisson")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
      axis.title.x = element_text( size = 25, angle = 0, hjust = .5, vjust = 0),
      axis.title.y = element_text( size = 25, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))+
  theme(legend.position="none")+annotate(geom = "table", x =  10,
                                         y = 6,label = list(avg_sd_diag_infl_biv_pois)  
  )



plot2<-ppc_bars(goal_diff_test,goal_diff_pred_test_fit_biv_pois,
                prob = 0.95, width = 0.9, 
                size = 1, fatten = 3,
                freq = TRUE)+
  labs(y="Number of games")+
  ggtitle(" Biv. Poisson")+scale_y_continuous(breaks = seq(0,8,4), limits = c(0,8))+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 25, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 25, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))+
  theme(legend.position="none")+annotate(geom = "table", x =  10,
                                         y = 6,label = list(avg_sd_biv_pois)  
  )

plot3<-ppc_bars(goal_diff_test,goal_diff_pred_test_fit_double_pois,
                prob = 0.95, width = 0.9, 
                size = 1, fatten = 3,
                freq = TRUE)+labs(x="Goal Difference")+
  labs(x="Goal Difference",y="")+scale_y_continuous(breaks = seq(0,8,4), limits = c(0,8))+
  ggtitle("Double. Poisson")+
  theme(axis.text.x = element_text( size = 23, angle = 0, hjust = .5, vjust = .5),
        axis.text.y = element_text( size = 23, angle = 0, hjust = 1, vjust = 0),  
        axis.title.x = element_text( size = 25, angle = 0, hjust = .5, vjust = 0),
        axis.title.y = element_text( size = 25, angle = 90, hjust = .5, vjust= 0),
        plot.title  =element_text( size = 23))+annotate(geom = "table", x =  10,
                    y = 6,label = list(avg_sd_double_pois)  
        )

# Combine to a single plot our plots
grid.arrange(plot1, plot2,plot3, nrow=3)


        