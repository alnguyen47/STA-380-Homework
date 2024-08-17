# Association Rule Mining Problem
library(tidyverse)
library(dplyr)
library(igraph)
library(arules)
library(arulesViz)

setwd('Homework')
groceries <- readLines('groceries.txt')

# Get the list of groceries
grocery_list <- strsplit(groceries, ',')

# Remove duplicates
grocery_list <- lapply(grocery_list, unique)

# Cast this variable as a special arules "transactions" class.
grocerytrans <- as(grocery_list, "transactions")
summary(grocerytrans)

# Run the apriori algorithm
groceryrules <- apriori(grocerytrans, parameter=list(support=.005, confidence=.1, maxlen=8))

# Check the output
inspect(groceryrules[1:10])

# plot all the rules in (support, confidence) space
# notice that high lift rules tend to have low support
plot(groceryrules)

# can swap the axes and color scales
plot(groceryrules, measure = c("support", "lift"), shading = "confidence")

# "two key" plot: coloring is by size (order) of item set
plot(groceryrules, method='two-key plot')

# can now look at subsets driven by the plot
inspect(subset(groceryrules, support > 0.035))
inspect(subset(groceryrules, confidence > 0.6))
plot(subset(groceryrules, lift > 2.5 & confidence > 0.6), measure = c("support", "lift"), shading = "confidence")

grocery_graph = associations2igraph(subset(groceryrules, lift > 2.5 & confidence > 0.6), associationsAsNodes = FALSE)
igraph::write_graph(grocery_graph, file='grocery.graphml', format = "graphml")

plot(grocery_graph)