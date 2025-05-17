# load required packages
library("dplyr")
library("ggplot2")


# download data
fig4Data <- read.csv("Data/Fig4Data.csv") %>%
  select(Age, X, Y)
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
plotNMDS <- function(data) {
  # create function to calculate the convex hull for each group
  find_hull = function(df) df[chull(df$X, df$Y), ]
  
  # apply the function to your NMDS scores grouped by your factor
  hulls = data %>%
    group_by(Age) %>%
    do(find_hull(.))
  
  # generate plot
  ggplot() +
    geom_polygon(data = hulls, aes(x = X, y = Y, fill = Age), alpha = 0.2) +
    geom_point(data=data, aes(x=X, y=Y, color=Age, shape=Age)) +
    xlab("Axis 1") +
    ylab("Axis 2") +
    theme_classic()
}

# generate ordination plot for Fig. 4
fig4 <- plotNMDS(data=fig4Data)
fig4

# generate ordination plot for Fig. 6A
fig6A <- plotNMDS(data=fig6AData)
fig6A

# generate ordination plot for Fig. 6B
fig6B <- plotNMDS(data=fig6BData)
fig6B
