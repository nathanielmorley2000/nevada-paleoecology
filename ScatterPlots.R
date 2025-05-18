# load required packages
library("dplyr")
library("rsdepth")
library("ggplot2")


# download data
fig4Data <- read.csv("Data/Fig4Data.csv") %>%
  select(Age, X, Y) %>%
  mutate(Age = recode(Age, Early = "Early Devonian", Middle = "Middle Devonian"))
fig5AData <- read.csv("Data/Fig5AData.csv")
fig5BData <- read.csv("Data/Fig5BData.csv")
fig6AData <- read.csv("Data/Fig6AData.csv") %>%
  rename(Age = Faunal.Interval)
fig6BData <- read.csv("Data/Fig6BData.csv") %>%
  rename(Age = Faunal.Interval)


# create function to generate line plots for figs 5A and 5B
plotLines <- function(data){
  fig <- ggplot(data=data, aes(x=Proportion.culled, y=Success.rate, group=1)) +
    geom_line() +
    geom_point() +
    xlab("Proportion of Data Culled") +
    ylab("Realized p-value") +
    theme_classic()
}

# generate line plot for fig. 5A
fig5A <- plotLines(fig5AData)
fig5A

# generate line plot for fig. 5B
fig5B <- plotLines(fig5BData)
fig5B


# create function to generate ordination plots for figs 4, 6A, 6B 
plotNMDS <- function(data, convexHulls) {
  
  if(convexHulls == TRUE) {
    # create function to calculate the convex hull for each group
    find_hull <- function(df) df[chull(df$X, df$Y), ]
    
    # apply the function to your NMDS scores grouped by your factor
    hulls <- data %>%
      group_by(Age) %>%
      do(find_hull(.))
    
    # find centroid of convex hulls
    # create empty dataframe
    centroids <- data.frame(Age = character(), 
                            X = double(), 
                            Y = double())
    # Get unique groups
    groups <- unique(data$Age)
    
    # for each Age category, find the centroid of the convex hulls and store in empty centroids dataframe
    for(g in groups) {
      group_data <- data[data$Age == g, ]
      centroid <- centroid(x = group_data$X, y = group_data$Y)
      centroids <- rbind(centroids, data.frame(Age = g, X = centroid[1], Y = centroid[2]))
    }
    
    
    # generate plot with convex hulls and centroid
    ggplot() +
      geom_polygon(data = hulls, aes(x = X, y = Y, fill = Age), alpha = 0.2) +
      geom_point(data=data, aes(x=X, y=Y, color=Age, shape=Age), size = 3) +
      #geom_point(data = centroids, aes(x = X, y = Y),  color = "blue", shape=3, size = 4) +
      xlab("NMDS 1") +
      ylab("NMDS 2") +
      theme_classic() +
      theme(panel.border = element_rect(linetype = "solid", fill = NA))
    
  } else {
    # generate scatterplot
    ggplot() +
      geom_point(data=data, aes(x=X, y=Y, color=Age, shape=Age), size = 3) +
      xlab("NMDS 1") +
      ylab("NMDS 2") +
      theme_classic() +
      theme(panel.border = element_rect(linetype = "solid", fill = NA))
  }

}

# generate ordination plot for Fig. 4
fig4 <- plotNMDS(data=fig4Data, convexHulls=FALSE)
fig4

# generate ordination plot for Fig. 6A
fig6A <- plotNMDS(data=fig6AData, convexHulls=TRUE)
fig6A

# generate ordination plot for Fig. 6B
fig6B <- plotNMDS(data=fig6BData, convexHulls=TRUE)
fig6B
