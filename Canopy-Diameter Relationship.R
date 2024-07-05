################################################################################
# SUMMARIZE QSMs
################################################################################

# MAKE SURE THESE ARE BOTH INSTALLED

# load packages
library(R.matlab)
library(qsm2r)

# YOU NEED TO CHANGE THE PATHS TO YOUR DATA

# set paths & pattern
path_QSMs <- "C:/Users/olami/OneDrive - University of Eastern Finland/Documents/Second year/Thesis/Thesis Data/results/.mat"
path_out  <- "C:/Users/olami/OneDrive - University of Eastern Finland/Documents/Second year/Thesis/Thesis Data/R result"

# create output folder
dir.create(path_out, showWarnings = FALSE)

################################################################################

# THIS IS JUST A HELPER FUNCTION FOR FINDING THE CYLINDERS OF SUB-BRANCHES
# I THOUGH I WOULD ADD IT HERE BECAUSE YOU MIGHT NEED IT

# # get branch IDs if child branches
# find_childs_recursive_branch <- function(cylinder, branch_ID, include_self = TRUE) {
#   
#   # get cylinders of the branches
#   cyl_sub <- cylinder[cylinder$branch %in% branch_ID,]
#   
#   # get all cylinders which are children of the branches
#   cyl_childs <- cylinder[cylinder$parent %in% cyl_sub$cyl_id & !(cylinder$branch %in% branch_ID),]
#   
#   # return the branch IDs of the children
#   if (nrow(cyl_childs) == 0) {
#     if (include_self) {
#       return(branch_ID)
#     } else {
#       return(NULL)
#     }
#   } else {
#     id_childs <- unique(cyl_childs$branch)
#     id_childs_childs <- find_childs_recursive_branch(cylinder, id_childs)
#     if (include_self) {
#       return(c(branch_ID, id_childs, id_childs_childs))
#     } else {
#       return(c(id_childs, id_childs_childs))
#     }
#   }
# }

################################################################################

# get all relevant QSM paths in folder
all_paths_QSMs <- list.files(path_QSMs, pattern = paste0(
  "Final_QSMs_Optimized_", ".*[.]mat"), full.names = TRUE, recursive = TRUE)

# create empty variable for storage
branch_df4 <- c()

# loop through QSM paths
for (path in all_paths_QSMs) {
  
  # extract tree name
  tree_name <- sub("Final_QSMs_Optimized_ *(.*?) *[.]mat", "\\1", basename(path))
  
  # print progress
  print("----------")
  print(tree_name)
  
  # read mat file
  file <- readMat(path)
  
  # loop through QSMs
  n_qsm <- dim(file[["QSMs"]])[3]
  for (n in 1:n_qsm) {
    
    # load QSM
    qsm <- readQSM(path, qsm_idx = n)
    cyl <- qsm@cylinder
    
    # get height
    qsm_base_z <- cyl$start_Z[cyl$branch == 1 & cyl$PositionInBranch == 1]
    tree_height <- max(cyl$start_Z + cyl$length * cyl$axis_Z) - qsm_base_z
    
    # save data
        curr5 <- data.frame(
          "tree" = tree_name,
          "dbh_cm" = qsm@overview$DBHqsm * 100,
          "height_m" = tree_height,
          "model" = n,
          "diameter_cm" = cyl_main$radius ** 2 * 100,
          'canopy_cm' = qsm@overview$CrownDiamMax * 100)
        branch_df4 <- rbind(branch_df4, curr5)
      
    
  }
}
print("----------")

# save the data
write.csv(branch_df4, file = file.path(path_out, "final_qsms_branch_data.csv"), row.names = FALSE)

library(dplyr)
branch_df4$canopy_m <- branch_df4$canopy_cm/100

canopy_stat <- branch_df4 %>% 
  group_by(tree) %>% 
  select(-c(model, diameter_cm, canopy_cm)) %>% 
  summarise(DBH = mean(dbh_cm),
            height = mean(height_m),
            canopy = mean(canopy_m))
plot(canopy_stat$DBH, canopy_stat$canopy)

canopy_stat$DBH2 <- canopy_stat$DBH ** 2


canopy_stat$logDBH <- log(canopy_stat$DBH)

#model 1
model1 <- lm(canopy_stat$canopy ~ canopy_stat$DBH)
summary(model1)

cpred1<-predict(model1,canopy_stat)
canopy_stat$cpred1 <- cpred1
obs.pred1<-cbind(canopy_stat$canopy, canopy_stat$cpred1)          #column bind predicted and observed
colnames(obs.pred1)<-c('observed','predicted')
head(obs.pred1)
obs.pred1<-data.frame(obs.pred1)

cresidual1<-model1$res
cresidual1
cresidualplot1<-plot(cpred1, cresidual1, xlab = 'Predicted Canopy diameter', ylab = 'Model Residual')
abline(h=0)

# Create a line plot of  from DBH
canopy_plot1 <- ggplot(canopy_stat, aes(x = obs.pred1$observed, y = obs.pred1$predicted)) +
  stat_smooth(method = "lm", formula = y ~ x, color = "steelblue", size = 1, se = F) +
  geom_point(color = "red", size = 1) +
  labs(x = "Observed canopy diameter (m)", y = "Predicted canopy diameter (m)") +
  theme_minimal()
plot(canopy_plot1)

#model 2
model2 <- lm(canopy_stat$canopy ~ canopy_stat$DBH + canopy_stat$DBH2)

cpred2<-predict(model2,canopy_stat)
canopy_stat$cpred2 <- cpred2
obs.pred2<-cbind(canopy_stat$canopy, canopy_stat$cpred2)          #column bind predicted and observed
colnames(obs.pred2)<-c('observed','predicted')
head(obs.pred2)
obs.pred2<-data.frame(obs.pred2)

cresidual2<-model2$res
cresidual2
cresidualplot2<-plot(cpred2, cresidual2, xlab = 'Predicted Canopy diameter', ylab = 'Model Residual')
abline(h=0)

# Create a line plot of  from DBH
canopy_plot2 <- ggplot(canopy_stat, aes(x = obs.pred2$observed, y = obs.pred2$predicted)) +
  stat_smooth(method = "lm", formula = y ~ x, color = "steelblue", size = 1, se = F) +
  geom_point(color = "red", size = 1) +
  labs(x = "Observed canopy diameter (m)", y = "Predicted Canopy Diameter (m)") +
  theme_minimal()
plot(canopy_plot2)

#model3
model3 <- lm(canopy_stat$canopy ~ canopy_stat$DBH + canopy_stat$height)

cpred3<-predict(model3,canopy_stat)
canopy_stat$cpred3 <- cpred3
obs.pred3<-cbind(canopy_stat$canopy, canopy_stat$cpred3)          #column bind predicted and observed
colnames(obs.pred3)<-c('observed','predicted')
head(obs.pred3)
obs.pred3<-data.frame(obs.pred3)

cresidual3<-model3$res
cresidual3
cresidualplot3<-plot(cpred3, cresidual3, xlab = 'Predicted Canopy diameter', ylab = 'Model Residual')
abline(h=0)

# Create a line plot of  from DBH
canopy_plot3 <- ggplot(canopy_stat, aes(x = obs.pred3$observed, y = obs.pred3$predicted)) +
  stat_smooth(method = "lm", formula = y ~ x, color = "steelblue", size = 1, se = F) +
  geom_point(color = "red", size = 1) +
  labs(x = "Observed canopy diameter (m)", y = "Predicted Canopy Diameter (m)") +
  theme_minimal()
plot(canopy_plot3)

canopy_stat$logDBH <- log(canopy_stat$DBH)
canopy_stat$logcanopy <- log(canopy_stat$canopy)

model4 <- lm(canopy_stat$logcanopy ~ canopy_stat$logDBH)
summary(model4)

canopy_stat$expdbh <- exp(canopy_stat$logDBH)

cpred4<-predict(model4,canopy_stat)
canopy_stat$cpred4 <- cpred4
obs.pred4<-cbind(canopy_stat$logcanopy, canopy_stat$cpred4)          #column bind predicted and observed
colnames(obs.pred4)<-c('observed','predicted')
head(obs.pred4)
obs.pred4<-data.frame(obs.pred4)

cresidual4<-model4$res
cresidual4
cresidualplot4<-plot(cpred4, cresidual4, xlab = 'Predicted Canopy diameter', ylab = 'Model Residual')
abline(h=0)

# Create a line plot of  from DBH
canopy_plot4 <- ggplot(canopy_stat, aes(x = obs.pred4$observed, y = obs.pred4$predicted)) +
  stat_smooth(method = "lm", formula = y ~ x, color = "steelblue", size = 1, se = F) +
  geom_point(color = "red", size = 1) +
  labs(x = "log Observed canopy diameter (m)", y = "log Predicted Canopy Diameter (m)") +
  theme_minimal()
plot(canopy_plot4)


eqn_5<-model1$fit
plot(eqn_5, canopy_stat$canopy)
abline(h=0)
slope <- coef(model1)[2]
intercept <- coef(model1)[1]

ggplot(canopy_stat, aes(x = DBH, y = canopy)) +
  geom_point() +                  # Add data points to the plot
  geom_abline(intercept = intercept, slope = slope, color = "red") +   # Add the regression line
  labs(title = "Canopy vs DBH", x = "DBH", y = "Canopy")  # Add labels to the plot

modelresidual1<-model1$res
modelresidual1
residualplot1<-plot(canopy_stat$canopy,modelresidual1)
abline(h=0)

can.pred1<-predict(model1,canopy_stat)

obs.pred<-cbind(canopy_stat$canopy,can.pred1)          #column bind predicted and observed
colnames(obs.pred)<-c('observed','predicted')
head(obs.pred)


obs.pred<-data.frame(obs.pred)

plot(obs.pred$observed, obs.pred$predicted, main = 'Observed vs Predicted', xlab='Observed canopy', ylab= 'Predicted canopy', 
     pch=21, col='black', bg='steelblue')     #plot
abline(lm(obs.pred$predicted~obs.pred$observed), lty=2, col='gray20', lwd=3)

#RMSE

rmse<- function(error){sqrt(mean(error^2))}
rmse(obs.pred$observed - obs.pred$predicted)


# Check if there are any negative values
if (any(cpred4 < 0)) {
  print("data is not ok")
} else {
  print("data is ok")
}

canopy_stat$CL <- canopy1$CL
canopy_stat$CBH <- canopy1$CBH
canopy_stat$ProjArea <- canopy1$ProjArea
View(canopy_stat)
View(canopy1)

#model 5
model5 <- lm(canopy_stat$canopy ~ canopy_stat$DBH + canopy_stat$CL)
summary(model5)

cpred5<-predict(model5,canopy_stat)
canopy_stat$cpred5 <- cpred5
obs.pred5<-cbind(canopy_stat$canopy, canopy_stat$cpred5)          #column bind predicted and observed
colnames(obs.pred5)<-c('observed','predicted')
head(obs.pred5)
obs.pred5<-data.frame(obs.pred5)

cresidual5<-model5$res
cresidual5
cresidualplot5<-plot(cpred5, cresidual5, xlab = 'Predicted Canopy diameter', ylab = 'Model Residual')
abline(h=0)

# Create a line plot of  from DBH
canopy_plot5 <- ggplot(canopy_stat, aes(x = obs.pred5$observed, y = obs.pred5$predicted)) +
  stat_smooth(method = "lm", formula = y ~ x, color = "steelblue", size = 1, se = F) +
  geom_point(color = "red", size = 1) +
  labs(x = "Observed canopy diameter (m)", y = "Predicted canopy diameter (m)") +
  theme_minimal()
plot(canopy_plot5)

#model 6
model6 <- lm(canopy_stat$CBH ~ canopy_stat$DBH + canopy_stat$CL)
summary(model6)

cpred6<-predict(model6,canopy_stat)
canopy_stat$cpred6 <- cpred6
obs.pred6<-cbind(canopy_stat$CBH, canopy_stat$cpred6)          #column bind predicted and observed
colnames(obs.pred6)<-c('observed','predicted')
head(obs.pred6)
obs.pred6<-data.frame(obs.pred6)

cresidual6<-model6$res
cresidual6
cresidualplot6<-plot(cpred6, cresidual6, xlab = 'Predicted CBH', ylab = 'Model Residual')
abline(h=0)

# Create a line plot of  from DBH
canopy_plot6 <- ggplot(canopy_stat, aes(x = obs.pred6$observed, y = obs.pred6$predicted)) +
  stat_smooth(method = "lm", formula = y ~ x, color = "steelblue", size = 1, se = F) +
  geom_point(color = "red", size = 1) +
  labs(x = "Observed CBH (m)", y = "Predicted CBH (m)") +
  theme_minimal()
plot(canopy_plot6)
