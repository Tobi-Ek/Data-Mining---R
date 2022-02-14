# Association rule example

# ------- Load arules and Groceries data -------

install.packages("arules")
library(arules)
library(arulesViz)

data(Groceries)
View(Groceries)

length(Groceries)
size(Groceries)


# ------- Exploring items -------
inspect(Groceries[1:5])
inspect(Groceries) 
# or
head(as(Groceries, "list"), 5)

summary(Groceries)

# Plot the chart of the groceries dataset
itemFrequencyPlot(Groceries, topN = 10, type = "absolute")

itemFrequencyPlot(Groceries, support=0.1)

# Plotting a Sparse Matrix
image(Groceries[1:5])

image(sample(Groceries, 100))


# ------- Mining arules -------

apriori(Groceries)

groceryrules <- apriori(Groceries, parameter=list(support=0.006,confidence=0.25,minlen = 2))

# summary statistics
summary(groceryrules)

# rule inspection
inspect(groceryrules[1:5])

inspect(sort(groceryrules, by = 'lift')[1:10])







# ------- Find the frequent itemsets (Trial 1) -------
itemsets <- apriori(Groceries,parameter=list(confidence=0.0,support=0.02,minlen=1,maxlen=1,target="frequent itemsets"))
#  Prune 
inspect(head(sort(itemsets, by = "support"),10))

# ------- Find the frequent itemsets (Trial 2) -------
itemsets <- apriori(Groceries,parameter=list(confidence=0.0,support=0.02,minlen=2,maxlen=2,target="frequent itemsets"))
#  Prune 
inspect(head(sort(itemsets, by = "support"),10))

# Rules
rules <- apriori(Groceries,parameter=list(support=0.001,confidence=0.6,target="rules"))

inspect(head(sort(rules, by = "lift"),10))

strong_rules <- sort(rules, by="confidence", decreasing = T)
inspect(strong_rules)



# ------- Items with support greater than threshold -------

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=1,maxlen=1))
fit <- sort(fit,by="support")
inspect(fit)

# ------- Pairs of items with support greater than threshold -------

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=2,maxlen=2))
fit <- sort(fit,by="support")
inspect(fit)

# ------- Pairs of items with support greater than threshold (confidence threshold too) -------

fit <- apriori(Groceries,parameter=list(confidence=0.5,support=0.01,minlen=2,maxlen=2))
fit <- sort(fit,by="support")
inspect(fit)

# ------- Triples of items with support greater than threshold -------

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=3,maxlen=3))
fit <- sort(fit,by="support")
inspect(fit)

# ------- Triples of items with support greater than threshold (confidence threshold too) -------

fit <- apriori(Groceries,parameter=list(confidence=0.5,support=0.01,minlen=3,maxlen=3))
fit <- sort(fit,by="support")
inspect(fit)

# ------- Quadruples of items with support greater than threshold -------

fit <- apriori(Groceries,parameter=list(confidence=0.0,support=0.01,minlen=4,maxlen=4))
fit <- sort(fit,by="support")
inspect(fit)


