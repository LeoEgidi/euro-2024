initial_simulation <- function(N, K, team1, team2, ranking,
                               p_norm, nteams, nsims){
  #prior simulation
  att_raw <- matrix(NA, nsims, nteams )
  def_raw <- matrix(NA, nsims, nteams )
  att <- matrix(NA, nsims, nteams )
  def <- matrix(NA, nsims, nteams )
  mu_att <- c()
  mu_def <- c()
  sigma_att <- c()
  sigma_def <- c()
  phi <- c()
  beta <- c()
  eta <- array(NA, c(nsims, N, K-1))
  theta <- array(NA, c(nsims, N, K))
  clust <- c()
  exitus <- matrix(rep(c("1", "X", "2"),N), N, K, byrow=TRUE)
  y <- array(NA, c(nsims, N, K))
  prime = matrix(NA, 8,2)
  ottavi <- array(NA, c(nsims, 8,2))
  quarti <- array(NA, c(nsims, 4,2))
  semi <- array(NA, c(nsims, 2,2))
  final <- array(NA, c(nsims,2,2))
  win <- c()
  
  quarti_sq <- matrix(NA, nsims, 8)
  semi_sq <- matrix(NA, nsims, 4)
  final_sq <- matrix(NA, nsims, 2)
  
  
  
  
  
 
  
  
  
  for (i in 1:nsims){
    for (t in 1:nteams){
      att_raw[i, t] <- rnorm(1,mu_att[i]+beta[i]*
                               ranking[t], sigma_att[i])
      def_raw[i, t] <- rnorm(1,mu_def[i]-beta[i]*
                               ranking[t], sigma_def[i])
    }
    att[i,] <- att_raw[i,] - mean(att_raw[i, ])
    def[i,] <- def_raw[i,] - mean(def_raw[i, ])
    
    for(n in 1:N){
      eta[i,n,1] <- exp(att[i,team1[n]]+def[i,team2[n]]-
                          (gamma[i])*(1+1/abs(ranking[team1[n]])-
                                        ranking[team2[n]]))
      eta[i,n,2] <- exp(att[i,team2[n]]+def[i,team1[n]]-
                          (gamma[i])*(1+1/abs(ranking[team1[n]])-
                                        ranking[team2[n]]))
      theta[i,n,1] <- eta[i,n,1]/(1+eta[i,n,1]+eta[i,n,2])
      theta[i,n,3] <- eta[i,n,2]/(1+eta[i,n,1]+eta[i,n,2])
      theta[i,n,2] <- 1/(1+eta[i,n,1]+eta[i,n,2])
      
      
      clust[i] <- sample(1:2,1, prob=c(phi[i],1-phi[i]) )  
      macro_prob <- rbind(theta[i,n,], p_norm[n,])
      y[i,n,] <-  t(rmultinom(1, 1:3, 
                              prob=macro_prob[clust[i],])) 
    }
    
    ex_fin <- c()
    for (n in 1:N){
      ex_fin <- exitus[n, apply(y[i,,],1, 
                                function(x) which.max(x))]
    }
    
    
    prime = matrix(c("Netherlands", "Senegal",
              "England", "United States",
              "Argentina", "Poland",
              "France", "Denmark",
              "Spain", "Japan",
              "Croatia", "Morocco",
              "Brazil", "Serbia",
              "Portugal", "Ghana"),8,2, byrow=TRUE)
    
   
    ngames_ottavi <- 8
    ottavi[i,1,] <- c(prime[1,1], prime[2,2])
    ottavi[i,2,] <- c(prime[3,1], prime[4,2])
    ottavi[i,3,] <- c(prime[5,1], prime[6,2])
    ottavi[i,4,] <- c(prime[7,1], prime[8,2])
    ottavi[i,5,] <- c(prime[2,1], prime[1,2])
    ottavi[i,6,] <- c(prime[4,1], prime[3,2])
    ottavi[i,7,] <- c(prime[6,1], prime[5,2])
    ottavi[i,8,] <- c(prime[8,1], prime[7,2])
    
    source("base_code_whole_simulation.R")
    
    fit <- stan_foot(data = wc_data_stan[,-6],
                       model="diag_infl_biv_pois",
                       iter = n_iter, cores = 4,
                       predict= ngames_ottavi,
                       ranking = wc_ranking,
                       dynamic_type = "seasonal",
                       ind_home = "FALSE")
      qual <- c()
      p_elim = index= c()
      
      
      
      
    quarti_sq[i, ] <- 
      elim(8, team1_ottavi, team2_ottavi, 
           ottavi[i,,])$qual
    
    quarti[i,1, ] <- c(quarti_sq[i,1], quarti_sq[i,2])
    quarti[i,2, ] <- c(quarti_sq[i,3], quarti_sq[i,4])
    quarti[i,3, ] <- c(quarti_sq[i,5], quarti_sq[i,6])
    quarti[i,4, ] <- c(quarti_sq[i,7], quarti_sq[i,8])
    
    team1_quarti = team2_quarti <- c()
    for (g in 1:4){
      team1_quarti[g] <-(1:nteams)[teams==quarti[i,g,1]] 
      team2_quarti[g] <-(1:nteams)[teams==quarti[i,g,2]]
    }
    
    team1_quarti <- as.numeric(as.vector(team1_quarti))
    team2_quarti <- as.numeric(as.vector(team2_quarti))
    
    semi_sq[i, ] <- 
      elim(4, team1_quarti, team2_quarti, quarti[i,,])$qual
    
    semi[i,1,] <- c(semi_sq[i,1], semi_sq[i,2])
    semi[i,2,] <- c(semi_sq[i,3], semi_sq[i,4])
    
    team1_semi = team2_semi <- c()
    for (g in 1:2){
      team1_semi[g] <-(1:nteams)[teams==semi[i,g,1]] 
      team2_semi[g] <-(1:nteams)[teams==semi[i,g,2]]
    }
    
    team1_semi <- as.numeric(as.vector(team1_semi))
    team2_semi <- as.numeric(as.vector(team2_semi))
    
    final_sq[i, ] <- 
      elim(2, team1_semi, team2_semi, semi[i,,])$qual
    
    final[i,1,]<- c( final_sq[i,1], final_sq[i,2])
    
    
    
    team1_final <-(1:nteams)[teams==final[i,1,1]] 
    team2_final <-(1:nteams)[teams==final[i,1,2]]
    
    
    team1_final <- as.numeric(team1_final)
    team2_final <- as.numeric(team2_final)
    
    win[i] <- 
      elim(1, team1_final, team2_final, final[i,,])$qual
    
    
  } # i
  
  results<- apply(y, c(2,3), mean)
  max_prob_value <- apply(results,1, 
                          function(x) which.max(x))
  ex_fin <- c()
  for (n in 1:N){
    ex_fin <- exitus[n, max_prob_value]
  }
  tab <- cbind(teams[team1[1:N]],
               teams[team2[1:N]], results,ex_fin)
  
  
  gironi_class <- cbind(
    conta_punti_girone(ex_fin, girone_a)$class,
    conta_punti_girone(ex_fin, girone_b)$class,
    conta_punti_girone(ex_fin, girone_c)$class,
    conta_punti_girone(ex_fin, girone_d)$class,
    conta_punti_girone(ex_fin, girone_e)$class,
    conta_punti_girone(ex_fin, girone_f)$class,
    conta_punti_girone(ex_fin, girone_g)$class,
    conta_punti_girone(ex_fin, girone_h)$class)
  
  return(list(tab=tab, gironi_class=gironi_class, 
              win=win,
              att=att, def=def, gamma=gamma, phi=phi, y=y))
  
}


sim <-initial_simulation(N, K, team1, team2, ranking,
                         p_norm, nteams, nsims)