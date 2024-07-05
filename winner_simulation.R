rm(list=ls())
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Libraries 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(dplyr)
library(ggplot2)
library(footBayes)
library(rstan)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Global variables
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
DATA_PATH="data/"
IMG_PATH="imgs/"
TABLES_PATH="tables/"
MODELS_PATH="models/"
INFLATED_RUN=TRUE
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data preparation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
euro_data_stan<- read.csv(paste0(DATA_PATH,"euro_data_stan.csv"))
euro_ranking<- read.csv(paste0(DATA_PATH,"euro_ranking.csv"))
euro_train_teams=unique(euro_data_stan$home_team)


#load(paste0(MODELS_PATH,"knockout_model.RData"))
#pars=extract(fit)
load(paste0(MODELS_PATH,"quarter-final_pars.RData"))
simulate_match<- function(matchname,teams,rankings,pars){
  #teams=sort(teams)
  HomeTeam <- strsplit(matchname,'-')[[1]][1]
  AwayTeam <- strsplit(matchname,'-')[[1]][2]
  
  HomeIdx= match(HomeTeam,teams)
  AwayIdx= match(AwayTeam,teams)
  
  HomeAtt= pars$att[,9,HomeIdx]
  AwayAtt= pars$att[,9,AwayIdx]
  
  HomeDef= pars$def[,9,HomeIdx]
  AwayDef= pars$def[,9,AwayIdx]
  
  gamma= pars$gamma
  p= pars$prob_of_draws
  
  HomeRank=rep(rankings %>% filter( team_name == HomeTeam) %>% select(ranking) %>% as.numeric(),4000)
  AwayRank=rep(rankings %>% filter( team_name == AwayTeam) %>% select(ranking) %>% as.numeric(),4000)
  
  l1= exp(HomeAtt + HomeDef + gamma/2 *(HomeRank - AwayRank))
  l2= exp(AwayAtt + AwayDef - gamma/2 *(HomeRank - AwayRank))
  l3= exp(pars$rho)
  
  # Modello direttamente la differenza retu
  GoalDiffs = vector("numeric",4000)
  GoalDiffs=rpois(n=4000, lambda=l1+l3)-rpois(n=4000, lambda=l2+l3)
  
  
  #.............................................................................
  # Indexes to inflate (OPTIONAL)
  #.............................................................................
  if(INFLATED_RUN){
    idx_inflated= runif(4000)<=p
    GoalDiffs[idx_inflated]=0
  }
  #.............................................................................
  
  
  Pr_H=mean(GoalDiffs > 0)
  Pr_D=mean(GoalDiffs == 0)
  Pr_A=mean(GoalDiffs < 0)
  
  Winner= ifelse(GoalDiffs>0,HomeTeam,ifelse(GoalDiffs<0,AwayTeam,"Draw"))
  
  # Manage the draws: we "flip the coin" for the winner!
  n_draws=sum(GoalDiffs == 0)
  pr_coin= Pr_H+Pr_D/2
  winner_choice=rbinom(n=n_draws, size=1, prob=pr_coin)
  winner_choice=ifelse(winner_choice==1,HomeTeam,AwayTeam)
  Winner[Winner=="Draw"]=winner_choice

  # Ritornare TUTTI i samples non può scalare....
  # Quindi ritorno solo il vincitore in base alle probabilità stimate dalle catene
  
  # Free the memory
  Pr_Hnew=mean(Winner==HomeTeam)
  if(runif(1)<=Pr_Hnew){
    return(HomeTeam)
  }else{
    return(AwayTeam)
  }
  
}


N_sim=10000
winner=rep(NA,N_sim)

for(i in 1:N_sim){
  #-----------------------------------------------------------------------------
  # SIMULAZIONE QUARTI
  #-----------------------------------------------------------------------------
  cat("...running simulation ",i,"/",N_sim,",...\n")
  quarti1_winner=simulate_match(matchname="Spain-Germany",
                 teams=euro_train_teams,
                 rankings=euro_ranking,
                 pars=pars)
  
  quarti2_winner=simulate_match(matchname="Portugal-France",
                 teams=euro_train_teams,
                 rankings=euro_ranking,
                 pars=pars)
  
  quarti3_winner=simulate_match(matchname="England-Switzerland",
                    teams=euro_train_teams,
                    rankings=euro_ranking,
                    pars=pars)
  
  quarti4_winner=simulate_match(matchname="Netherlands-Turkey",
                    teams=euro_train_teams,
                    rankings=euro_ranking,
                    pars=pars)
  
  
  #-----------------------------------------------------------------------------
  # SIMULAZIONE SEMIFINALI
  #-----------------------------------------------------------------------------

  semifinal1 <-paste(quarti1_winner, quarti2_winner, sep = "-")
  semifinal2 <-paste(quarti3_winner, quarti4_winner, sep = "-")
  
  semifinal1_winner=simulate_match(matchname=semifinal1,
                                teams=euro_train_teams,
                                rankings=euro_ranking,
                                pars=pars)
  semifinal2_winner=simulate_match(matchname=semifinal2,
                                   teams=euro_train_teams,
                                   rankings=euro_ranking,
                                   pars=pars)
  
  #-----------------------------------------------------------------------------
  # SIMULAZIONE FINALE
  #-----------------------------------------------------------------------------
  
  final<- paste(semifinal1_winner, semifinal2_winner, sep = "-")
  
  winner[i]=simulate_match(matchname=final,
                           teams=euro_train_teams,
                           rankings=euro_ranking,
                           pars=pars)
  #-----------------------------------------------------------------------------
}

winner_probs=table(winner)/N_sim*100
winner_df= data.frame(
  Team=names(winner_probs),
  WinningProbability=as.numeric(winner_probs)
)

if(INFLATED_RUN){
  write.csv(winner_df, paste0(DATA_PATH,"winner_probs_inflated.csv"), row.names = FALSE)
}else{
  write.csv(winner_df, paste0(DATA_PATH,"winner_probs.csv"), row.names = FALSE)
}