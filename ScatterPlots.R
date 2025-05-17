# load required packages
#library("dplyr")
library("ggplot2")


# download data
fig4Data <- read.csv("Data/Fig4Data.csv")
fig5AData <- read.csv("Data/Fig5AData.csv")
fig5BData <- read.csv("Data/Fig5BData.csv")
fig6AData <- read.csv("Data/Fig6AData.csv")
fig6BData <- read.csv("Data/Fig6BData.csv")


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


