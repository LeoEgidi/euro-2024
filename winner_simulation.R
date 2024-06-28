#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Libraries 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(dplyr)
library(ggplot2)
library(footBayes)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Global variables
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
DATA_PATH="data/"
IMG_PATH="imgs/"
TABLES_PATH="tables/"
MODELS_PATH="models/"
STAN_ITERS=2000
STAN_CORES=4
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Data preparation
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
euro_data_stan<- read.csv(paste0(DATA_PATH,"euro_data_stan.csv"))
euro_ranking<- read.csv(paste0(DATA_PATH,"euro_ranking.csv"))

load(paste0(MODELS_PATH,"knockout_model.RData"))
pars=extract(fit)

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
  GoalDiffs = rep(0,4000)
  for(s in 1:4000){
    x= rpois(1,l1[s]+l3[s])
    y= rpois(1,l2[s]+l3[s])
    GoalDiffs[s]=x-y
  }
  
  Pr_H=mean(GoalDiffs > 0)
  Pr_D=mean(GoalDiffs == 0)
  Pr_A=mean(GoalDiffs < 0)
  
  Winner= ifelse(GoalDiffs>0,HomeTeam,
                  ifelse(GoalDiffs<0,AwayTeam,"Draw"))
  
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
  # SIMULAZIONE OTTAVI
  #-----------------------------------------------------------------------------
  cat("...running simulation ",i,"/",N_sim,",...\n")
  ottavi1_winner=simulate_match(matchname="Switzerland-Italy",
                 teams=euro_train_teams,
                 rankings=euro_ranking,
                 pars=pars)
  
  ottavi2_winner=simulate_match(matchname="England-Slovakia",
                 teams=euro_train_teams,
                 rankings=euro_ranking,
                 pars=pars)
  
  ottavi3_winner=simulate_match(matchname="Romania-Netherlands",
                    teams=euro_train_teams,
                    rankings=euro_ranking,
                    pars=pars)
  
  ottavi4_winner=simulate_match(matchname="Austria-Turkey",
                    teams=euro_train_teams,
                    rankings=euro_ranking,
                    pars=pars)
  ottavi5_winner=simulate_match(matchname="Germany-Denmark",
                    teams=euro_train_teams,
                    rankings=euro_ranking,
                    pars=pars)
  
  ottavi6_winner=simulate_match(matchname="Spain-Georgia",
                    teams=euro_train_teams,
                    rankings=euro_ranking,
                    pars=pars)
  
  ottavi7_winner=simulate_match(matchname="France-Belgium",
                    teams=euro_train_teams,
                    rankings=euro_ranking,
                    pars=pars)
  
  ottavi8_winner=simulate_match(matchname="Portugal-Slovenia",
                    teams=euro_train_teams,
                    rankings=euro_ranking,
                    pars=pars)
  
  #-----------------------------------------------------------------------------
  # SIMULAZIONE QUARTI
  #-----------------------------------------------------------------------------
  quarti1 <- paste(ottavi1_winner, ottavi2_winner, sep = "-")
  quarti2 <- paste(ottavi3_winner, ottavi4_winner, sep = "-")
  quarti3 <- paste(ottavi5_winner, ottavi6_winner, sep = "-")
  quarti4 <- paste(ottavi7_winner, ottavi8_winner, sep = "-")
  
  # Quarti winner
  quarti1_winner=simulate_match(matchname=quarti1,
                            teams=euro_train_teams,
                            rankings=euro_ranking,
                            pars=pars)
  
  quarti2_winner=simulate_match(matchname=quarti2,
                            teams=euro_train_teams,
                            rankings=euro_ranking,
                            pars=pars)
  
  quarti3_winner=simulate_match(matchname=quarti3,
                            teams=euro_train_teams,
                            rankings=euro_ranking,
                            pars=pars)
  
  quarti4_winner=simulate_match(matchname=quarti4,
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

write.csv(winner_df, paste0(DATA_PATH,"winner_probs.csv"), row.names = FALSE)
winner_df=read.csv(paste0(DATA_PATH,"winner_probs.csv"))

                   