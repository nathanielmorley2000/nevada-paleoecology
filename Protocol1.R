# load libraries
library("dplyr")
library("vegan")
library("ggplot2")

# create directory to store protocol 1 results
dir.create("Protocol1Results")

# load data and replace NAs with 0s
rawData <- read.csv("Data/RelativeDataset.csv")
data <- rawData %>% replace(is.na(.), 0) # ensure NA values are replaced with 0

# create function for running u-test on NMDS axis 1 scores and returning the U-statistic and p-value
mann_whitney_test <- function(NMDS1, Early.Mid, data.scores) {
  test <- wilcox.test(NMDS1 ~ Early.Mid, data = data.scores)
  U_stat <- test$statistic
  p_value <- test$p.value
  return(c(U_stat, p_value))
}

# create function for looping NMDS and producing plots
findNMDS <- function(r, nit, save, plotIndices) {
  
  # set successes to 0 to calculate success ratio
  success = 0
  
  # if you want to save individual results
  if (save == TRUE){
    
    # overhead for creating individual results subdirectory
    folder_name <- paste("Protocol1Results/IndividualResults", 1-r)
    dir.create(folder_name)
  }
  
  # loop to perform the test nit times
  i = 1
  for (i in 1:nit) {
    
    # randomly cull data
    culled_data = data %>%
      slice_sample(prop = r)
    
    # CHECK 1: Make sure there are sufficient data (n = 5) in each sample to run U-test
    count_early = sum(culled_data$Early.Mid == "Early")
    count_mid = sum(culled_data$Early.Mid == "Middle")
      
    # make community matrix - extract columns with relative abundance information
    com = culled_data[,4:ncol(culled_data)]
    m_com = as.matrix(com)
    
    # run NMDS
    nmds = metaMDS(m_com, distance = "bray")
    
    # extract NMDS scores (x and y coordinates) and bind to Early/Mid
    data.scores = as.data.frame(scores(nmds)$sites)
    data.scores$Early.Mid = culled_data$Early.Mid
      
    # CHECK 2: Make sure NMDS didn't return a 1-D or mostly 1-D solution
    z_scores = (data.scores$NMDS1 - mean(data.scores$NMDS1)) / sd(data.scores$NMDS1)
    outliers = abs(z_scores) > 4 # check for outliers outside 3 standard deviations
      
    # make sure "Early" is on the left of the plot
    group_means <- aggregate(NMDS1 ~ Early.Mid, data = data.scores, mean)
    earlyMean <- group_means$NMDS1[group_means$Early.Mid == "Early"]
    midMean <- group_means$NMDS1[group_means$Early.Mid == "Middle"]
    
    # flip the NMDS1 axis if "early" is not on left
    if (earlyMean > midMean) {
      data.scores$NMDS1 <- -data.scores$NMDS1 
    }
    
    # create plot and save axes scores if i < plotIndices 
    if (i <= plotIndices) {
      
      # create function to calculate the convex hull for each group
      find_hull = function(df) df[chull(df$NMDS1, df$NMDS2), ]
      
      # apply the function to your NMDS scores grouped by your factor
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
      
      # construct the filename
      file_name = sprintf("NMDS.%d.png", i)
      file_path = file.path(folder_name, file_name)

      # save the plot
      ggsave(filename = file_path, plot = p, bg = "white")
      
      # save axis scores
      write.csv(data.scores, paste0(folder_name, "/NMDSdata", i, ".csv"))
    }

    # CHECK 3: Make sure the p-value of the u-test is significant (p < 0.05)
    u_test = mann_whitney_test(NMDS1, Early.Mid, data.scores)
    
    # check that all 3 criteria are satisfied and mark as success
    if (all(count_early >= 5 && count_mid >= 5, sum(outliers) < 1, u_test[2] < 0.05)) {
      success = success + 1
    }
    
    # proceed to next iteration
    i = i + 1
    }
  
  # check that all iterations were performed correctly
  if (i == nit + 1) {
    
    # calculate success ratio
    successRatio = success/nit
    return(successRatio)
 
  } else {
    print("Failure")
  }
}

# define sequence and find number of rows
r_values <- seq(0.05, 1.00, by = 0.05)
n_rows <- length(r_values)

# initialize matrix
results_matrix <- matrix(NA, nrow = n_rows, ncol = 2)
results_matrix[, 1] <- 1 - r_values
colnames(results_matrix) <- c("Proportion of Data Culled", "Success Rate")

# loop to compute values and store in the second column
for (j in 1:n_rows) {
  results_matrix[j, 2] <- findNMDS(r = r_values[j], # proportion of data retained (loop through at 0.05 increments)
                                   nit = 5, # number of iterations
                                   save = TRUE, # if you want to save individual plots and axis scores
                                   plotIndices = 5) # number of plots produced (make sure save = TRUE)
}

# make matrix dataframe
results_dataframe <- data.frame(results_matrix)

# save matrix to Protocol1Results folder
write.csv(results_dataframe, "Protocol1Results/Summary.csv", row.names = FALSE)

# create a line plot
p <- ggplot(results_dataframe, aes(x = Proportion.of.Data.Culled, y = Success.Rate)) +
        geom_line(color = "black", size = 1) +
        labs(x = "Proportion of Data Culled", y = "Success Rate") +
        theme_classic()

# save line plot to Protocol1Results folder
ggsave("Protocol1Results/SummaryPlot.png", p, width = 3543, height = 2657, units = "px")
