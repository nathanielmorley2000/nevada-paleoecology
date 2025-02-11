# load libraries
library("dplyr")
library("vegan")
library("ggplot2")

# overhead for creating Results subdirectory
current_time <- Sys.time()
formatted_time <- format(current_time, "%Y-%m-%d_%H-%M-%S")
folder_name <- paste("Results", formatted_time, sep = " ")
dir.create(folder_name)

# load data and replace NAs with 0s
rawData <- read.csv("Data/RelativeDataset.csv")
data <- rawData %>% replace(is.na(.), 0) # ensure NA values are replaced with 0

mann_whitney_test <- function(NMDS1, Early.Mid, data.scores) {
  test <- wilcox.test(NMDS1 ~ Early.Mid, data = data.scores)  # Use exact = FALSE for larger samples
  U_stat <- test$statistic
  p_value <- test$p.value
  return(c(U_stat, p_value))
}

success = 0

# create function for looping NMDS and producing plots
findNMDS <- function(r, nit, plotIndices) {
  
  # variables
  r <- 0.05 # proportion of data not culled
  nit <- 5  # number of iterations
  plotIndices <- 5 # number of plots produced
  
  
  # Initialize matrices to store U-statistics and p-values
  results <- matrix(NA, nrow = nit, ncol = 2)
  colnames(results) <- c("U_stat", "p_value")
  
  # Loop to perform the test nit times
  warning_count = 0
  i = 1
  for (i in 1:nit) {
    # randomly cull data
    culled_data = data %>%
      slice_sample(prop = r)
    
    #make community matrix - extract columns with relative abundance information
    com = culled_data[,4:ncol(culled_data)]
    m_com = as.matrix(com)
    
      # run NMDS
      nmds = metaMDS(m_com, distance = "bray")
      
      # extract NMDS scores (x and y coordinates) and bind to Early/Mid
      data.scores = as.data.frame(scores(nmds)$sites)
      data.scores$Early.Mid = culled_data$Early.Mid
      
      # make sure "Early" is on the left of the plot
      group_means <- aggregate(NMDS1 ~ Early.Mid, data = data.scores, mean)
      earlyMean <- group_means$NMDS1[group_means$Early.Mid == "Early"]
      midMean <- group_means$NMDS1[group_means$Early.Mid == "Middle"]
      if (earlyMean > midMean) {
        data.scores$NMDS1 <- -data.scores$NMDS1  # Flip the NMDS1 axis
      }
      
      write.csv(data.scores, paste0(folder_name, "/NMDSdata", i, ".csv"))
      
      # create plot using ggplot2 if iteration is less than or equal to plotIndices
      if (i <= plotIndices) {
        # Function to calculate the convex hull for each group
        find_hull = function(df) df[chull(df$NMDS1, df$NMDS2), ]
        
        # Apply the function to your NMDS scores grouped by your factor
        hulls = data.scores %>%
          group_by(Early.Mid) %>%
          do(find_hull(.))
        
        # NMDS plot with convex hulls
        p = ggplot() +
          geom_polygon(data = hulls, aes(x = NMDS1, y = NMDS2, fill = Early.Mid), alpha = 0.2) + # Convex hulls
          geom_point(data = data.scores, aes(x = NMDS1, y = NMDS2, color = Early.Mid), size = 4) + # Points
          theme_minimal() +
          labs(x = "NMDS1", y = "NMDS2") +
          ggtitle(paste("Plot for iteration", i)) +
          theme(plot.title = element_text(hjust = 0.5))
        
        
        
        # Construct the filename
        file_name = sprintf("NMDS.%d.png", i)
        file_path = file.path(folder_name, file_name)
        
        # Save the plot
        ggsave(filename = file_path, plot = p, bg = "white")
        
        
        
      }
      
      results[i, ] = mann_whitney_test(NMDS1, Early.Mid, data.scores)
      i = i + 1
  }
  
}

# variables
r <- 0.05 # proportion of data not culled
nit <- 30  # number of iterations
plotIndices <- 30 # number of plots produced

# call function
results <- findNMDS(r, nit, plotIndices)

# save individual results to the "Results" directory
file_name <- "individual.iterations.csv"
file_path <- file.path(folder_name, file_name)
write.csv(results, file = file_path, row.names = TRUE)

# Calculate the average U-statistic and p-value
average_U_stat <- mean(results[, "U_stat"])
average_p_value <- mean(results[, "p_value"])

# create matrix from summary results
names_vector <- c("Proportion of Data Remaining", "Number of Iterations", "Proportion of Plots",
                  "Average U-statistic", "Average p-value")
values_vector <- c(r, nit, plotIndices, average_U_stat, average_p_value)
matrix_data <- cbind(names_vector, values_vector)

# save summary results to the "Results" directory
file_name <- "summary.results.csv"
file_path <- file.path(folder_name, file_name)
write.csv(matrix_data, file = file_path, row.names = FALSE)
