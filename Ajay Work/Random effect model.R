library(lme4)

#Must take in an encoded csv, where value names are replaced by (-1,1) or (-1,0,1)
setwd("C:\\Users\\594305\\Documents\\GitHub\\Split_Plot_Design\\Ajay Work")
csvfile <- read.csv("LF21 DOE Encoded.csv") 
csvfile$day <- 4

runs=100
sim = 5
no_manuever <- csvfile[which(csvfile$Ship.Maneuver == 1),]
yes_manuever <- csvfile[which(csvfile$Ship.Maneuver == -1),]

#Decreasing variability
no_manuever <- 0.5*no_manuever
yes_manuever <- 0.5*yes_manuever

calc <- rep(0, 11)
labels <- c("(Intercept)","Target.Maneuver", "Target.Quantity",
            "Launch.Range", "Approach.Angle", "Ship.Maneuver",
            "Target.Maneuver:Target.Quantity",
            "Target.Maneuver:Launch.Range", "Target.Maneuver:Approach.Angle",
            "Target.Quantity:Launch.Range", "Target.Quantity:Ship.Maneuver")
power_df <- cbind.data.frame(labels, best_calc=rep(0,11))
best_power <- 0

#Outer Loop iterating the numbers of runs to sample
for (j in 1:sim){
  no_manuever$day[sample(1:20,8)] <- 1
  design <- rbind(no_manuever, yes_manuever)
  
    #Inner Loop specifying the number of times to test this sample
    for (i in 1:runs){
      rand_base <- rnorm(2)
      whole_plot_error <- c(rep(rand_base[1],8), rep(rand_base[2],21))
      design$wholeplot_error <- whole_plot_error
      design$subplot_error <- rnorm(29)
    
      #exclude the run column and the day column(6)
      response <- 0.5*(apply(design[,c(1:5,7:8)],1,sum)+
                         design$Target.Maneuver*design$Target.Quantity+
                         design$Target.Maneuver*design$Launch.Range+
                         design$Target.Maneuver*design$Approach.Angle+
                         design$Target.Quantity*design$Launch.Range+
                         design$Target.Quantity*design$Ship.Maneuver)
      #the whole plot effect is day
      rlm <- lmer(response~design$Target.Maneuver+design$Target.Quantity+
                    design$Launch.Range+design$Approach.Angle+
                    design$Ship.Maneuver+
                    design$Target.Maneuver*design$Target.Quantity+
                    design$Target.Maneuver*design$Launch.Range+
                    design$Target.Maneuver*design$Approach.Angle+
                    design$Target.Quantity*design$Launch.Range+
                    design$Target.Quantity*design$Ship.Maneuver+
                    (1|design$day))
      
      # extract coefficients
      #ray wants this object to go with the design
      coefs <- data.frame(coef(summary(rlm)))
      # use normal distribution to approximate p-value
      coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
      rows <- which(coefs$p.z < 0.2)
      calc[rows] <- calc[rows] + 1
      
      }
  calc <- calc/runs
  print(j)
  
  test_stat <- min(calc[-1] )#exclude the Intercept
  
  #Sorting out the best cases
  if (test_stat > best_power){
    best_power <- test_stat
    best_design <- design
    power_df$best_calc <- calc
    
  }
  print(calc)
  calc <- rep(0,11)
}
coefs$power <- power_df$best_calc
coefs$sims <- sim
print()
print('best_calc')
print(power_df$best_calc)

coefs#Reinstating value names to encoded identifiers
best_design[,1] <- sapply(best_design[,1], function(x){
 if(x == 0.5){"Weave"}
 else{"Radial"}
})

best_design[,2] <- sapply(best_design[,2], function(x){
 if(x == 0.5){9}
 else{3}
})

best_design[,3] <- sapply(best_design[,3], function(x){
 if(x == 0.5){2500}
 else if(x == 0){5000}
 else {7500}
})

best_design[,4] <- sapply(best_design[,4], function(x){
 if(x == 0.5){45}
 else{0}
})

best_design[,5] <- sapply(best_design[,5], function(x){
 if(x == 0.5){"No"}
 else{"Yes"}
})

write.csv(best_design, file="RandomEffectsModel_samples.csv")
write.csv(coefs, file="Coefficients_samples.csv")

