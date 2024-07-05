
vol_stat <- branch_df %>% 
  group_by(tree) %>% 
  select(-c(model,class_lower, class_upper)) %>% 
  summarise(length_cm = mean(length_cm),
            volume = mean(volume))
vol_stat$BAGB <- vol_stat$volume*0.547 #observed agb
vol_stat$DBH <- canopy_stat$DBH
plot(vol_stat$volume, vol_stat$BAGB)

#model
bmodel <- lm(vol_stat$BAGB ~ vol_stat$DBH + vol_stat$length_cm)
summary(bmodel)

bbranch.pred<-predict(bmodel,vol_stat)

obs.pred<-cbind(vol_stat$BAGB,bbranch.pred)          #column bind predicted and observed
colnames(obs.pred)<-c('observed','predicted')
head(obs.pred)

obs.pred<-data.frame(obs.pred)

plot(obs.pred$observed, obs.pred$predicted, main = 'Observed vs Predicted', xlab='Observed BranchAGB', ylab= 'Predicted BranchABG', 
     pch=21, col='black', bg='steelblue')     #plot
abline(lm(obs.pred$predicted~obs.pred$observed), lty=2, col='gray20', lwd=3)

bresidual<-bmodel$res
bresidual
bresidualplot<-plot(bbranch.pred, bresidual, xlab = 'Predicted Biomass', ylab = 'Model Residual')
abline(h=0)

# Create a line plot of observed volume vs DBH 
biomass_plot3 <- ggplot(vol_stat, aes(x = bbranch.pred, y = BAGB )) +
  stat_smooth(method = "lm", formula = y ~ poly(x, degree = 2), color = "steelblue", size = 1) +
  geom_point(color = "red", size = 1) +
  labs(x = "Observed biomass (g/cm3)", y = " Predicted biomass (g/cm3)") +
  theme_minimal()

# Print the plot
print(biomass_plot3)

#model1
bmodel1 <- lm(vol_stat$BAGB ~ vol_stat$DBH)
summary(bmodel1)

bbranch.pred1<-predict(bmodel1,vol_stat)
vol_stat$bbranchpred1 <- bbranch.pred1
obs.pred1<-cbind(vol_stat$BAGB,vol_stat$bbranchpred1)          #column bind predicted and observed
colnames(obs.pred1)<-c('observed','predicted')
head(obs.pred1)
obs.pred1<-data.frame(obs.pred1)

bresidual1<-bmodel1$res
bresidual1
bresidualplot1<-plot(bbranch.pred1, bresidual1, xlab = 'Predicted Biomass', ylab = 'Model Residual')
abline(h=0)

# Create a line plot of tree biomass from DBH
biomass_plot <- ggplot(vol_stat, aes(x = DBH, y = obs.pred1$observed)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, degree = 2), color = "steelblue", size = 1) +
  geom_point(color = "red", size = 1) +
  labs(x = "DBH (cm)", y = "Branch Biomass (g)") +
  theme_minimal()
plot(biomass_plot)
#model2
vol_stat$lgDBH <- canopy_stat$logDBH
vol_stat$lglenght <- log(vol_stat$length_cm)
vol_stat$lgBAGB <- log(vol_stat$BAGB)

bmodel2 <- lm (vol_stat$lgBAGB ~ vol_stat$lgDBH + vol_stat$lglenght)
summary(bmodel2)

bbranch.pred2<-predict(bmodel2,vol_stat)

vol_stat$expBAGB <- exp(vol_stat$lgBAGB)
vol_stat$expbbranchpred2 <- exp(bbranch.pred2)

obs.pred2<-cbind(vol_stat$expBAGB,vol_stat$expbbranchpred2)          #column bind predicted and observed
colnames(obs.pred2)<-c('observed','predicted')
head(obs.pred2)


obs.pred2<-data.frame(obs.pred2)

plot(obs.pred2$observed, obs.pred2$predicted, main = 'Observed vs Predicted', xlab='Observed BranchAGB', ylab= 'Predicted BranchABG', 
     pch=21, col='black', bg='steelblue')     #plot
abline(lm(obs.pred2$predicted~obs.pred2$observed), lty=2, col='gray20', lwd=3)

plot(vol_stat$DBH, obs.pred2$observed, xlab='DBH', ylab= 'observed BranchB', 
     pch=21, col='black', bg='steelblue')     #plot
abline(lm(obs.pred2$observed~vol_stat$DBH), lty=2, col='gray20', lwd=3)


bresidual2<-bmodel2$res
bresidual2
bresidualplot2<-plot(vol_stat$expbbranchpred2,bresidual2, xlab = 'Predicted Biomass', ylab = 'Model Residual')
abline(h=0)

# Create a line plot of observed vs predicted biomass 
biomass_plotln <- ggplot(vol_stat, aes(x = expbbranchpred2, y = expBAGB)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "steelblue", size = 1) +
  geom_point(color = "red", size = 1) +
  labs(x = "Observed Biomass (g/cm3)", y = "Predicted Biomass (g/cm3)") +
  theme_minimal()

plot(biomass_plotln)
#RMSE

rmse<- function(error){sqrt(mean(error^2))}
rmse(obs.pred$observed - obs.pred$predicted)

# Load necessary packages
library(ggplot2)

# Create a line plot of tree biomass from DBH
biomass_plot <- ggplot(vol_stat, aes(x = DBH, y = obs.pred$observed)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "steelblue", size = 1) +
  geom_point(color = "red", size = 1) +
  labs(x = "DBH (cm)", y = "Branch Biomass (kg)") +
  theme_minimal()
  
 
# Print the plot
print(biomass_plot)

# Create a line plot of observed vs predicted biomass 
biomass_plot1 <- ggplot(obs.pred, aes(x = obs.pred$predicted, y = obs.pred$observed)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "steelblue", size = 1) +
  geom_point(color = "red", size = 1) +
  labs(x = "observed (cm)", y = "Predicted Biomass (kg)") +
  theme_minimal()

# Print the plot
print(biomass_plot1)

# Create a line plot of observed volume vs DBH 
biomass_plot2 <- ggplot(vol_stat, aes(x = DBH, y = volume)) +
  stat_smooth(method = "lm", formula = y ~ poly(x, degree = 3), color = "steelblue", size = 1) +
  geom_point(color = "red", size = 1) +
  labs(x = "DBH (cm)", y = " Volume (m3)") +
  theme_minimal()

# Print the plot
print(biomass_plot2)

