# load packages
library(R.matlab)
library(qsm2r)

# YOU NEED TO CHANGE THE PATHS TO YOUR DATA

# set paths & pattern
path_QSMs <- "C:/Users/olami/OneDrive - University of Eastern Finland/Documents/Second year/Thesis/Thesis Data/results/.mat"
path_out  <- "C:/Users/olami/OneDrive - University of Eastern Finland/Documents/Second year/Thesis/Thesis Data/R result"

# create output folder
dir.create(path_out, showWarnings = FALSE)


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
branch_df <- c()

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
    
        # get starting diameter
        diameter_class <- unique(ceiling(cyl$radius*2*100))
        
        for (upper_cm in diameter_class) {
          
          lower_cm <- upper_cm - 1
          upper_rad_m <- upper_cm/100/2
          lower_rad_m <- lower_cm/100/2
          
          sub <- cyl[cyl$radius > lower_rad_m & cyl$radius <= upper_rad_m,]
          
          # get shoot length / volume
          
          length <- sum(sub$length)
          volume <- sum(sub$length * pi * sub$radius**2)
          
          curr <- data.frame(
            "tree" = tree_name,
            "model" = n,
            "class_upper" = upper_cm,
            "class_lower" = lower_cm,
            'length_cm' = length,
            'volume' = volume)
          branch_df <- rbind(branch_df, curr)
        }
      
    
  }
}
print("----------")

# save the data
write.csv(branch_df, file = file.path(path_out, "final_qsms_branch_data.csv"), row.names = FALSE)

library(dplyr)
library(ggplot2)

diam_stat <- branch_df %>% 
  group_by(tree, class_lower, class_upper) %>% 
  select(-model) %>% 
  summarise(length = mean(length_cm),
            volume = mean(volume),
            volume_relative = (volume / total_volume) * 100)

write.csv(diam_stat, 'diam_stat.csv')
ggplot(diam_stat, aes(x = class_lower, xend = class_upper, y = 0, yend = volume)) +
  geom_line() +
  labs(x = "Diameter Class", y = "Volume")
  

ggplot(diam_stat)+
  geom_rect(aes(xmin = class_lower, xmax = class_upper, ymin = 0, ymax = volume)) +
  geom_line(aes(x = (class_lower + class_upper) / 2, y = volume))+  
  labs(x = "Diameter Class", y = "Volume") +
  theme_minimal()


total_volume <- sum(diam_stat$volume)

plotall <- ggplot(diam_stat) +
  geom_rect(aes(xmin = class_lower, xmax = class_upper, ymin = 0, ymax = volume_relative)) +
  geom_line(aes(x = (class_lower + class_upper) / 2, y = volume_relative), stat = "identity") +
  labs(x = "Diameter Class", y = "Volume %")
plot(plotall)

write.csv(diam_stat, "diameter_stats.csv", row.names=FALSE)

# Get unique tree values
unique_trees <- unique(diam_stat$tree)

# Loop through each unique tree
for (tree in unique_trees) {
  # Subset the dataframe for the current tree
  tree_data <- subset(diam_stat, tree == tree)
  
  # Calculate the mean volume for each class within the current tree
  class_means <- aggregate(volume ~ class_lower + class_upper, data = tree_data, FUN = mean)
  
  cat("Tree", tree, ":\n")
  print(class_means)
  cat("\n")
}

write.csv(class_means, 'class_mean.csv', row.names = FALSE)
# Get unique tree values
Tree <- unique(diam_stat$tree)

# Create an empty list to store subsets for each tree
diam_sec_list <- list()

# Loop through each unique tree value
for (i in Tree) {
  # Subset the diam_stat data for the current tree value
  diam_sec <- diam_stat[diam_stat$tree == i, ]
  
  # Add the subset to the list, naming it based on the tree value
  diam_sec_list[[as.character(i)]] <- diam_sec
}

# Create an empty list to store the results for each tree
diam_relative_list <- list()

# Loop through each unique tree value and calculate diam_relative
for (i in Tree) {
  diam_sec <- diam_sec_list[[as.character(i)]]
  
  # Calculate diam_relative for the current tree value
  diam_relative <- diam_sec %>% 
    group_by(tree, class_lower, class_upper) %>% 
    summarise(length_relative = length / sum(diam_sec$length) * 100,
              volume_relative = volume / sum(diam_sec$volume) * 100,
              diamclass_relative = case_when(class_lower == 0 & class_upper == 1 ~ 3,
                                           class_lower == 1 & class_upper == 2 ~ 6,
                                           class_lower == 2 & class_upper == 3 ~ 9,
                                           class_lower == 3 & class_upper == 4 ~ 12,
                                           class_lower == 4 & class_upper == 5 ~ 15,
                                           class_lower == 5 & class_upper == 6 ~ 18,
                                           class_lower == 6 & class_upper == 7 ~ 21,
                                           class_lower == 7 & class_upper == 8 ~ 24,
                                           class_lower == 8 & class_upper == 9 ~ 27,
                                           class_lower == 9 & class_upper == 10 ~ 30,
                                           class_lower == 10 & class_upper == 11 ~ 33,
                                           class_lower == 11 & class_upper == 12 ~ 36,
                                           class_lower == 12 & class_upper == 13 ~ 39,
                                           class_lower == 13 & class_upper == 14 ~ 42,
                                           class_lower == 14 & class_upper == 15 ~ 45,
                                           class_lower == 15 & class_upper == 16 ~ 48,
                                           class_lower == 16 & class_upper == 17 ~ 51,
                                           class_lower == 17 & class_upper == 18 ~ 54,
                                           class_lower == 18 & class_upper == 19 ~ 57,
                                           class_lower == 19 & class_upper == 20 ~ 60,
                                           class_lower == 20 & class_upper == 21 ~ 63,
                                           class_lower == 21 & class_upper == 22 ~ 66,
                                           class_lower == 22 & class_upper == 23 ~ 69,
                                           class_lower == 23 & class_upper == 24 ~ 72,
                                           class_lower == 24 & class_upper == 25 ~ 75,
                                           class_lower == 25 & class_upper == 26 ~ 78,
                                           class_lower == 26 & class_upper == 27 ~ 81,
                                           class_lower == 27 & class_upper == 28 ~ 84,
                                           class_lower == 28 & class_upper == 29 ~ 87,
                                           class_lower == 29 & class_upper == 30 ~ 90,
                                           class_lower == 30 & class_upper == 31 ~ 93,
                                           class_lower == 31 & class_upper == 32 ~ 97,
                                           class_lower == 32 & class_upper == 33 ~ 100))
              

# Add the result to the diam_relative_list, naming it based on the tree value
  diam_relative_list[[as.character(i)]] <- diam_relative
}

# Create an empty list to store the plots for each tree
plots_list <- list()

# Loop through each unique tree value and create the plot
for (i in Tree) {
  # Check if the diam_relative_list contains a valid data frame for the current tree value
  if (!is.null(diam_relative_list[[as.character(i)]])) {
    diam_relative <- diam_relative_list[[as.character(i)]]
    
   # Create the plot for the current tree value of diam class relative
    diam_plot <- ggplot(diam_relative) +
      aes(x = diamclass_relative, y = volume_relative) +
      geom_bar(stat = "identity") +
      labs(x = "Diameter Class %", y = "Volume %", title = paste(i))
    
    # Add the plot to the plots_list, naming it based on the tree value
    plots_list[[as.character(i)]] <- diam_plot
    print(diam_plot)
    
    # Save the plot to the directory with the tree name as the filename
    file_name <- paste0('relative', i, ".png")  # You can change the file extension if needed
    file_path <- file.path("C:/Users/olami/OneDrive - University of Eastern Finland/Documents/Second year/Thesis/Thesis Data/plot", file_name)
    ggsave(filename = file_path, plot = diam_plot, width = 10, height = 6, unit = 'in', dpi = 300)
    
  } else {
    # Handle the case when diam_relative_list for the current tree value is NULL or invalid
    cat("Error: diam_relative_list for Tree", i, "is NULL or invalid.\n")
  }
}

#for diam class
# Create an empty list to store the plots for each tree
plots_list <- list()

# Loop through each unique tree value and create the plot
for (i in Tree) {
  # Check if the diam_relative_list contains a valid data frame for the current tree value
  if (!is.null(diam_relative_list[[as.character(i)]])) {
    diam_relative <- diam_relative_list[[as.character(i)]]

    #Create the plot for the current tree value
    diam_plot1 <- ggplot(diam_relative) +
      geom_rect(aes(xmin = class_lower, xmax = class_upper, ymin = 0, ymax = volume_relative),
                fill = "steelblue", color = 'black') +
      labs(x = "Diameter Class", y = "Volume %", title = paste(i))
    
# Add the plot to the plots_list, naming it based on the tree value
plots_list[[as.character(i)]] <- diam_plot1
print(diam_plot1)

# Save the plot to the directory with the tree name as the filename
file_name <- paste0(i, ".png")  # You can change the file extension if needed
file_path <- file.path("C:/Users/olami/OneDrive - University of Eastern Finland/Documents/Second year/Thesis/Thesis Data/plot", file_name)
ggsave(filename = file_path, plot = diam_plot1, width = 10, height = 6, unit = 'in', dpi = 300)

  } else {
    # Handle the case when diam_relative_list for the current tree value is NULL or invalid
    cat("Error: diam_relative_list for Tree", i, "is NULL or invalid.\n")
  }
}

