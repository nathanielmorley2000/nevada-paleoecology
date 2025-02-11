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
  
  # Initialize matrices to store U-statistics and p-values
  results <- matrix(NA, nrow = nit, ncol = 1)
  colnames(results) <- c("Success/Fail")
  
  # Loop to perform the test nit times
  i = 1
  for (i in 1:nit) {
    
    # randomly cull data
    culled_data = data %>%
      slice_sample(prop = r)
    
    # CHECK 1: Make sure there are sufficient data (n = 5) in each sample to run U-test
    count_early = sum(culled_data$Early.Mid == "Early")
    count_mid = sum(culled_data$Early.Mid == "Middle")
    
    # if successful, run NMDS
    #if (count_early > 5 && count_mid > 5) {
      
      # make community matrix - extract columns with relative abundance information
      com = culled_data[,4:ncol(culled_data)]
      m_com = as.matrix(com)
      
      # run NMDS
      nmds = metaMDS(m_com, distance = "bray")
      
      # extract NMDS scores (x and y coordinates) and bind to Early/Mid
      data.scores = as.data.frame(scores(nmds)$sites)
      data.scores$Early.Mid = culled_data$Early.Mid
      
      #data.scores <- read.csv("Results 2025-02-11_11-25-17/NMDSdata1.csv")
    #}
      
    # CHECK 2: Make sure NMDS didn't return a 1-D or mostly 1-D solution
    z_scores = (data.scores$NMDS1 - mean(data.scores$NMDS1)) / sd(data.scores$NMDS1)
    outliers = abs(z_scores) > 3 # check for outliers outside 3 standard deviations
    
    # if solution is not 1-dimensional, conduct U-test
    #if (sum(outliers) != 1) {
      # make sure "Early" is on the left of the plot
      group_means <- aggregate(NMDS1 ~ Early.Mid, data = data.scores, mean)
      earlyMean <- group_means$NMDS1[group_means$Early.Mid == "Early"]
      midMean <- group_means$NMDS1[group_means$Early.Mid == "Middle"]
      if (earlyMean > midMean) {
        data.scores$NMDS1 <- -data.scores$NMDS1  # Flip the NMDS1 axis
      }
      
      write.csv(data.scores, paste0(folder_name, "/NMDSdata", i, ".csv"))
      
      # create plot using ggplot2 if iteration is less than or equal to plotIndices
      #if (i <= plotIndices) {
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
        
        u_test = mann_whitney_test(NMDS1, Early.Mid, data.scores)
    #}

      
        if (all(count_early >= 5 && count_mid >= 5, sum(outliers) != 1, u_test[2] < 0.05)) {
          print("success")
          results[i,] = "success"
          success = success + 1
          
        } else {
          print("fail")
          results[i,] = "fail"
        }
        
        i = i + 1
  }
  
return(success)
}
    
# call function
results <- findNMDS(r = 0.1, # proportion of data not culled
                    nit = 80, # number of iterations
                    plotIndices = 20) # number of plots produced
results
    
write.csv(results, paste0(folder_name, "/success.csv"))    
      
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
