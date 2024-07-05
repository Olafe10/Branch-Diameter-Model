# load packages
library(R.matlab)
library(qsm2r)

# YOU NEED TO CHANGE THE PATHS TO YOUR DATA

# set paths & pattern
path_QSMs <- "C:/Users/olami/OneDrive - University of Eastern Finland/Documents/Second year/Thesis/Thesis Data/results/.mat"
path_out  <- "C:/Users/olami/OneDrive - University of Eastern Finland/Documents/Second year/Thesis/Thesis Data/R result"

# create output folder
dir.create(path_out, showWarnings = FALSE)


# get all relevant QSM paths in folder
all_paths_QSMs <- list.files(path_QSMs, pattern = paste0(
  "Final_QSMs_Optimized_", ".*[.]mat"), full.names = TRUE, recursive = TRUE)

# create empty variable for storage
branch_df6 <- c()

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
    
   # check for branches
    if (length(unique(cyl$branch)) > 1) {
      
      # loop through branches
      for (branch_curr in unique(cyl$branch)) {
        
        # get main shoot length / volume
        cyl_main <- cyl[cyl$branch == branch_curr,]
        length_main <- sum(cyl_main$length)
        volume_main <- sum(cyl_main$length * pi * cyl_main$radius**2)
        
        # THIS PART BELOW COULD BE USEFUL TO YOU IF YOU WANT TO HAVE THE
        # CUMULATIVE LENGTH / VOLUME OF THE SUB-BRANCHES INCLUDED
        
        # # get cumulative length / volume
        # if (branch_curr == 1) {
        #   branch_child <- unique(cyl$branch) # for efficiency
        # } else {
        #   branch_child <- unique(find_childs_recursive_branch(cyl, branch_curr, TRUE))
        # }
        # cyl_cum <- cyl[cyl$branch %in% branch_child,]
        # length_cum <- sum(cyl_cum$length)
        # volume_cum <- sum(cyl_cum$length * pi * cyl_cum$radius**2)
        
        # save data
        curr7 <- data.frame(
          "tree" = tree_name,
          "Branch_vol_cm3" = qsm@overview$BranchVolume * 1000,
          "model" = n,
          'canopy_cm' = qsm@overview$CrownDiamAve * 100,
          "volume_cm3" = volume_main * 1000000,
          "Trunk_vol_cm3" = qsm@overview$TrunkVolume * 1000000)
        branch_df6 <- rbind(branch_df6, curr7)
      }
    }
  }
}
print("----------")

# save the data
write.csv(branch_df6, file = file.path(path_out, "final_qsms_branch_data.csv"), row.names = FALSE)

library(dplyr)
library(ggplot2)
library(ggpubr)
branch_df6$trunk_real <- branch_df6$Trunk_vol_cm3/1000000
branch_df6$trunk_new <- branch_df6$trunk_real * 1000
branch_df6$canopy_new <- branch_df6$canopy_cm / 100

bratru_stat <- branch_df6 %>% 
  group_by(tree) %>% 
  select(-c(model, canopy_cm, canopy_new, volume_cm3, trunk_real, Trunk_vol_cm3,)) %>% 
  summarise(bra_vol = mean(Branch_vol_cm3),
            tru_vol = mean(trunk_new))
ggbarplot(data = new_df, x='tree', y='volume_value',
          fill='volume_type', add = c('mean_sd'), position = position_dodge(0.7),
          width=0.5)+
  scale_fill_brewer(palette = 'Greens')

# Create a new dataframe with 'volume_type' and 'volume_value' columns
new_df <- bratru_stat %>%
  gather(key = volume_type, value = volume_value, bra_vol, tru_vol) %>%
  mutate(volume_type = gsub("_volume", "", volume_type))

print(new_df)

unique_trees <- unique(new_df$tree)

for (tree in unique_trees) {
  subset_df <- new_df[new_df$tree == tree, ]
  
  plot <- ggplot(subset_df, aes(x = volume_type, y = volume_value, fill = volume_type)) +
    geom_bar(stat = 'identity', add = c('mean_sd'), position = position_dodge(0.7), width = 0.5) +
    scale_fill_brewer(palette = 'Greens') +
    labs(x = 'Volume Type', y = 'Volume Value', fill = 'Volume Type') +
    ggtitle(paste(tree)) +
    theme_minimal()
  
  # Save or print the plot here (you can adjust this according to your needs)
  print(plot)
}


unique_trees <- unique(new_df$tree)

for (tree in unique_trees) {
  subset_df <- new_df[new_df$tree == tree, ]
  
  plot <- ggplot(subset_df, aes(x = volume_type, y = volume_value, fill = volume_type)) +
    geom_bar(stat = 'summary', fun = "mean", position = position_dodge(0.7), width = 0.5) +
    geom_errorbar(stat = 'summary', fun.data = "mean_se", position = position_dodge(0.7), width = 0.25) +
    scale_fill_brewer(palette = 'Greens') +
    labs(x = 'Volume Type', y = 'Volume Value', fill = 'Volume Type') +
    ggtitle(paste("Tree:", tree)) +
    theme_minimal()
  
  # Save or print the plot here (you can adjust this according to your needs)
  print(plot)
}

unique_trees <- unique(new_df$tree)

for (tree in unique_trees) {
  subset_df <- new_df[new_df$tree == tree, ]
  
  plot <- ggplot(subset_df, aes(x = volume_type, y = volume_value, fill = volume_type)) +
    geom_bar(stat = 'identity', position = position_dodge(0.7), width = 0.5) +
    stat_summary(aes(y = volume_value), fun = "mean", geom = "errorbar", position = position_dodge(0.7), width = 0.25) +
    scale_fill_brewer(palette = 'Greens') +
    labs(x = 'Volume Type', y = 'Volume Value', fill = 'Volume Type') +
    ggtitle(paste(tree)) +
    theme_minimal()
  
  # Save or print the plot here (you can adjust this according to your needs)
  print(plot)
}

library(dplyr)
library(tidyr)




# Create the clustered column chart
ggplot(bratru_stat, aes(x = tree, y = value, fill = variable)) +
  geom_col(position = "dodge", width = 0.7) +
  labs(title = "Clustered Column Chart",
       x = "tree",
       y = "Value") +
  scale_fill_manual(values = c("bra_vol" = "blue", "tru_vol" = "red")) +
  theme_minimal()

ggplot(bratru_stat, aes(x = tree, fill = variable)) +
  geom_col(aes(y = bra_vol), position = "dodge", width = 0.7, fill = "blue") +
  geom_col(aes(y = tru_vol), position = "dodge", width = 0.7, fill = "red") +
  labs(title = "Branch & Trunk Volume",
       x = "tree",
       y = "Value") +
  theme_minimal()


library(tidyverse)
bratru_stat$newbra <- bratru_stat$bra_vol/1000000
bratru_stat$newtru <- bratru_stat$tru_vol/1000000

#checking normality

hist(bratru_stat$newbra, breaks = 20, density = 20, freq = FALSE,
     col = 'darkgray', xlab = 'Branch Volume')
xfit<-seq(min(bratru_stat$newbra), max(bratru_stat$newbra), length = 100)
yfit<-dnorm(xfit, mean = mean(bratru_stat$newbra), sd=sd(bratru_stat$newbra))
lines(xfit, yfit, col='black', lwd=2)

hist(bratru_stat$newtru, breaks = 26, density = 20, freq = FALSE,
     col = 'darkgray', xlab = 'Trunk Volume')
xfit<-seq(min(bratru_stat$newtru), max(bratru_stat$newtru), length = 100)
yfit<-dnorm(xfit, mean = mean(bratru_stat$newtru), sd=sd(bratru_stat$newtru))
lines(xfit, yfit, col='black', lwd=2)



# Mann-Whitney U Test
wilcox.test(bratru_stat$bra_vol, bratru_stat$tru_vol, paired = TRUE)

test <- t.test(bratru_stat$bra_vol, bratru_stat$tru_vol, paired = TRUE)
print(test)
test$statistic   
test$estimate    
test$p.value     
test$conf.int

# Assuming you have a dataframe named 'df' with columns: 'Tree', 'Branch_Volume', and 'Trunk_Volume'

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Reshape the data to long format
df_long <- bratru_stat %>%
  pivot_longer(cols = c(bra_vol, tru_vol), names_to = "Volume_Type", values_to = "Volume")

# Plot the side-by-side bar plot
ggplot(df_long, aes(x = Tree, y = Volume, fill = Volume_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Tree", y = "Volume", title = "Branch and Trunk Volume for Each Tree") +
  theme_minimal()
barplot(bratru_stat)


brajoin <- inner_join(bratru_stat, canopy_stat, by = "tree")
#values greater than
result_table <- data.frame()

for (i in 1:nrow(brajoin)) {
  if (brajoin$tru_vol[i] > brajoin$bra_vol[i]) {
    result_table <- rbind(result_table, data.frame(tree = brajoin$tree[i], trunk_volume = brajoin$tru_vol[i], branch_volume = brajoin$bra_vol[i],
                                                   DBH = brajoin$DBH[i], Height = brajoin$height[i], canopy = brajoin$canopy[i]))  
  }
}

write.csv(result_table, 'result_table.csv', row.names = FALSE)
