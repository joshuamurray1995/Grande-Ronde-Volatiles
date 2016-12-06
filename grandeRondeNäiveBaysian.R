# Packages
library(RColorBrewer)
library(XLConnect)
library(ggplot2)
library(e1071)
library(plyr)
library(reshape2)

# Function that takes a given chemical data set and compares it
# with a member, passed by the client. 
memberProb <- function(given, memberMeans, memberStds){
  # Assert that the data are of the same length
  if(length(given) != length(memberMeans)) {
    stop("the given data are of different sizes")
  }

  # Define memberProb which will be updated with each chemical that
  #   is compared
  memberProb <- 1
  
  # Loop through and take the product of all the probabilities for each chemical  
  for (i in 1:length(given)) {
    memberProb <- memberProb * dnorm(given[i],memberMeans[i],memberStds[i])
  }
  return(memberProb)
}


# Function to create a vector of probabilities for each member
probVector <- function(given, prior, meanTable, stdTable){
  # number of members to calc. the probability for
  nMembers <- ncol(meanTable)
  
  #empty vector to populate with probabilities
  probVector <- numeric(nMembers)
  
  #loop through the members and compare to the given. Fill the vector with probabilities
  # for each member. 
  for (i in 1:nMembers) {
    probVector[i] <- memberProb(given, meanTable[,c(i)], stdTable[,c(i)])
  }
  
  # Make sure the prior is the correct length
  if (length(prior) != nMembers) {
    stop("prior is not of correct length")
  }
  
  # multiply by the prior
  probVector <- probVector * prior
  
  #normalise to add to 1
  probVector <- probVector / sum(probVector)
  
  return(probVector)
}

# Theme for plot
fte_theme <- function() {
  
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n=6)
  color.background = palette[1]
  color.grid.major = palette[2]
  color.axis.text = palette[4]
  color.axis.title = palette[5]
  color.title = palette[6]
  
  # Begin construction of chart
  theme_bw(base_size=9) +
    
    # Set the entire chart region to a light gray color
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    
    # Format the grid
    theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    
    # Format the legend
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    
    # Set title and axis labels, and format these and tick marks
    theme(plot.title=element_text(color=color.title, size=11, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=10,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=10,color=color.axis.title, vjust=1.25)) +
    
    # Plot margins
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

# Data-Set 
setwd("/Users/Josh/Documents/Princeton/Geoscience/VolatilesCRB_SmithNewton/Data")

grMeans <- readWorksheetFromFile("GrandeRondeMean.xlsx", sheet=1, startRow = 1, endCol = 40)
grMeans <- grMeans[-c(1),]
chemicals <- grMeans[,c(1)]
grMeans <- grMeans[-c(1)]
grMeans <- data.matrix(grMeans)

grStds <- readWorksheetFromFile("GrandeRondeStd.xlsx", sheet=1, startRow = 1, endCol = 40)
grStds <- grStds[-c(1),]
grStds <- grStds[-c(1)]
grStds <- data.matrix(grStds)

members <- colnames(grMeans)

#Now we should have 2 matrices: grStds and grMeans that each
# contain the standard deviations and means respectively 
# for each member of the grande ronde.

#Further, there exist string vectors, chemicals and members
# which each contain the list of chemicals and the list of members
# of the grande ronde. 

# Input Data here, format:
# SIO2  AL2O3 TIO2  FEO*  MNO   CAO   MGO   K2O   NA2O  P2O5  NI    CR    SC    V     BA    RB    SR    ZR    Y     NB    

#TEST (Smple 33 from pmoeroy map):
sampleX <- c(54.90, 14.55, 1.803, 10.22, 0.194, 8.89, 4.40, 1.66, 3.06, 0.321, 18, 50, 40, 302, 511, 34, 340, 153, 33, 12.4)
#Prior information, divided by MSU (N2, R2, N1, R1)
prior <- c(rep(1,15), rep(0,4), rep(1,4), rep(0,8))
probability <- probVector(sampleX, prior, grMeans, grStds)

# Create a plot of the result:
# Create a data frame then tell it that it's preordered
allData <- data.frame(members, probability)
allData$members <- factor(allData$members, levels = allData$members)
probPlot <- ggplot(data=allData, aes(x=members, y=probability)) +
  geom_bar(stat="identity", colour = "grey8", fill = "dodgerblue3") + 
  fte_theme() + 
  theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1)) +
  geom_hline(yintercept=0, size=0.4, color="grey8")
probPlot <- probPlot + labs(title = "Probability that Sample 33, listed as Stember Creek (Hooper, 1996),\nbelongs to members of the Grande Ronde with R2 prior")
probPlot <- probPlot + labs(x = "Member", y = "Probability")
  
#quartz()
print(probPlot)
