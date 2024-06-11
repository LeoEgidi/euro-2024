# Svizzera-Spagna
quote1 <- c(5.5, 3.75, 1.66)
prob1 <- c(0.351,0.252,0.397)
# Belgio-Italia
quote2 <- c(3.3, 3, 2.37)
prob2 <- c(0.544,0.262,0.195)
# Rep.Ceca-Danimarca
quote3 <- c(3.8, 3.2, 2.1)
prob3 <- c(0.260,0.277,0.463)
# Ucraina-Inghilterra
quote4 <- c(8.5, 4.5, 1.4)
prob4 <- c(0.187,0.254, 0.559)
# Italia-Spagna
quote5 <- c(2.45, 3.25, 3)
prob5 <- c(0.488, 0.269, 0.243)
# Inghilterra-Danimarca
quote6 <-c(1.72, 3.6, 5.25)
prob6 <-c(0.505, 0.269, 0.226)
# Inghilterra-Italia
quote7 <- c(2.55,3.05, 3.35)
prob7 <- c(0.415, 0.331, 0.254)
# vincente torneo

quotew <- c(2.5, 3.25, 3.75, 10)
probw<-c(0.294, 0.342, 0.223,0.14)
teamw <- c("Eng", "Ita", "Spain", "Denmark")


quotew <- c(2, 1.72)
probw <- c(prob7[3]+prob7[2]/2, prob7[1]+prob7[2]/2)


prob_bk <- function(quote_input){
  bk <- c()
  for (i in 1:length(quote_input))
      bk[i] <- (1/quote_input[i])/(sum(1/(quote_input)))
  return(bk)
}

prob_bk(quotew)

guadagno_atteso <- probw/prob_bk(quotew) -1

## quarti
quote <- rbind(quote1, quote2, quote3, quote4)
prob <- rbind(prob1, prob2, prob3, prob4)

tot_euro <- 100
tab<-round(quote*prob,2)
rownames(tab)<- c("Svi-Spa", "Bel-Ita", "Cec-Dan", "Ucr-Ing")
colnames(tab) <- c("1", "X", "2")
puntate <- c( (1.93-(1-prob[1,1]))/quote[1,1], 
              (1.8-(1-prob[2,1]))/quote[2,1],
              0, (1.59+1.14-(1-prob[4,1]-prob[4,2]))/2.75 )*tot_euro
puntate <- round(puntate*tot_euro/sum(puntate))

atteso <- c(puntate[1] *quote[1,1], puntate[2]*quote[2,1], 0, puntate[4]*2.75)
atteso <- as.numeric(as.vector(atteso))
puntate_secchi <- c("5e (1-1)", "5e (0-1)", "10e (0-1)", "10e (0-1)")
atteso_secchi <- c("50", "30", "65", "60" )

rule <- c("1", "1", "", "1X")
tab<-cbind(tab, rule, puntate, atteso, puntate_secchi, atteso_secchi)
tab


## semifinali

quote <- rbind(quote5, quote6)
prob <- rbind(prob5, prob6)
tot_euro <- 140
tab<-round(quote*prob,2)
rownames(tab)<- c("Ita-Spa", "Eng-Dan")
colnames(tab) <- c("1", "X", "2")
 # kelly algorithm
puntate <- c( (tab[1,1]-(1-prob[1,1]))/quote[1,1], 
              (tab[2,3]-(1-prob[2,3]-prob[2,2]))/2)*tot_euro
#puntate <- round(puntate*tot_euro/sum(puntate))


tabw <- round(quotew*probw,2)
tabw <- cbind( probw, tabw, rep("",4))
ordw<- sort.int(as.numeric(as.vector(tabw[,2])),
                index.return=TRUE,
         decreasing = TRUE)
tabw[,2] <- ordw$x
tabw[,1] <- tabw[,1][ordw$ix]
  rownames(tabw) <- teamw[ordw$ix]


tab <- rbind(tab, tabw)


 # kelly algorithm
puntatew <- c( (as.numeric(tabw[1,2])-(1-as.numeric(tabw[1,1])))/quotew[4],
               (as.numeric(tabw[2,2])-(1-as.numeric(tabw[2,1])))/quotew[2])*tot_euro



tot_puntate <-c(puntate, puntatew)
tot_puntate <- round(tot_puntate*tot_euro/sum(tot_puntate),0)

atteso <- c( tot_puntate[1] *quote[1,1], tot_puntate[2]*2,
             tot_puntate[3]*quotew[4], tot_puntate[4]*quotew[2])
atteso <- as.numeric(as.vector(atteso))
puntate_secchi <- c("10e (1-0)", "10 e (1-0)")
atteso_secchi <- c("80", "60")

rule <- c("1", "X2", "Den", "Ita", "", "")
tab<-cbind(tab, rule, c(tot_puntate, rep("",2)),
                      c(puntate_secchi, rep("",4)),
                        c(atteso, rep("",2)),
                          c(atteso_secchi, rep("",4)) )
tab


