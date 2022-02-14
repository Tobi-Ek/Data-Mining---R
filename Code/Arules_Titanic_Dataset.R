
# -------- Titanic data set using rules --------

# Load libraries
library(arules)
library(arulesViz)  # Visualization

#Please change the folder path as per your local machine
load("titanic.raw.rdata")

titanic_r <- titanic.raw

# Explore
titanic_r[1:3,]

table(titanic_r$Age,titanic_r$Survived)

# Mining arules
rule <- apriori(titanic_r[2:4], 
                # min support & confidence
                parameter=list(minlen=2, supp=0.001, conf=0.001),  
                # specifying default appearance
                appearance = list(default = "lhs", rhs=c("Survived=Yes","Survived=No")))

inspect(rule)

sort.rule <- sort(rule, by="lift")

# Visualize
plot(sort.rule, method="graph", control=list(nodeCol="red", edgeCol="blue"))

# Mining arules - 1
rule1 <- apriori(titanic_r[2:4], 
                 # min support & confidence
                 parameter=list(minlen=2, supp=0.001, conf=0.001), 
                 # specifying default appearance explicitly
                 appearance = list(lhs=c("Age=Child","Age=Adult"), rhs=c("Survived=Yes")))

inspect(rule1)

sort.rule1 <- sort(rule1, by="lift")

# Visualize
plot(sort.rule1)

plot(sort.rule1, method="graph", control=list(nodeCol="red", edgeCol="blue"))

plot(sort.rule1, method="grouped", control=list(col=2))
