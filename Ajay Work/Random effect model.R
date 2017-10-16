library(lme4)

setwd("C:\\Users\\594305\\Documents\\GitHub\\Split_Plot_Design\\Ajay Work")
csvfile <- read.csv("LF21 DOE.csv")
csvfile$day <- 4

no_manuever <- csvfile[which(csvfile$Ship.Maneuver==-1),]
yes_manuever <- csvfile[which(csvfile$Ship.Maneuver==1),]

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

for (j in 1:10){
no_manuever$day[sample(8)] <- 1
design <- rbind(no_manuever, yes_manuever)

  for (i in 1:100){
  rand_base <- rnorm(2)
  whole_plot_error <- c(rep(rand_base[1],8), rep(rand_base[2],21))
  design$wholeplot_error <- whole_plot_error
  design$subplot_error <- rnorm(29)

  #exclude the run column(1) and the day column(7)
  response <- 0.5*(apply(design[,c(2:6,8:9)],1,sum)+
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
  coefs <- data.frame(coef(summary(rlm)))
  # use normal distribution to approximate p-value
  coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
  rows <- which(coefs$p.z < 0.2)
  calc[rows] <- calc[rows] + 1
  }
calc <- calc/i
print(j)
test_stat <- min(calc[-1])#exclude the Intercept
if (test_stat > best_power){
  best_power <- test_stat
  best_design <- design
  power_df$best_calc <- calc
}
calc <- rep(0, 11)
}
write.csv(best_design, file="RandomEffectsModel.csv")
