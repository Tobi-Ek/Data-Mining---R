# Large groceries data set using rules -------

# Import Required libraries ------ 

library(plyr)
library(arules)
library(arulesViz)


# --- Import the dataset --- 
groceries <- read.csv("LargeGroceries_dataset.csv")
class(groceries)

# Data Cleaning and Exploration ------ 

str(groceries)
head(groceries)

# --- Checking NA values --- 
sum(is.na(groceries))

# --- Sort groceries by member_number --- 
sorted <- groceries[order(groceries$Member_number),]

# --- Convert member_number to numeric --- 
sorted$Member_number <- as.numeric(sorted$Member_number)
str(sorted)

# --- Convert Date to categorical format --- 
sorted$Date <- as.factor(sorted$Date)
str(sorted)

# --- Convert item description to categorical format --- 
sorted$itemDescription <- as.factor(sorted$itemDescription)
str(sorted)

# --- Group grocery items together ---
# Group all the items that were bought together by 
# the same customer on the same date

itemList <- ddply(sorted, c("Member_number","Date"), 
                  function(df1)paste(df1$itemDescription,collapse = ","))
head(itemList,15)

# --- Remove member_number and date --- 
itemList$Member_number <- NULL
itemList$Date <- NULL

# --- Rename column name ---
colnames(itemList) <- c("itemList")

# --- Export Cleaned Item List to .csv file ---
write.csv(itemList,"ItemList.csv", quote = FALSE, row.names = TRUE)
head(itemList)

# --- Convert CSV file to Basket Format --- 
txn <- read.transactions(file="ItemList.csv", rm.duplicates= TRUE, 
                        format="basket",sep=",",cols=1)

print(txn)
str(txn)
# We can see a total of 14,964 transactions with 168 distinct products.

# --- View labels of the Transaction --- 
txn@itemInfo$labels 


# Mining the rules ------ 
# Using the apriori() we can generate the most relevant set of rules 
# from the large groceries transaction data. 

basket_rules <- apriori(txn, parameter = list(minlen=2, sup = 0.001,
                                              conf = 0.05, target="rules"))

# --- Total rules generated --- 
print(length(basket_rules))
summary(basket_rules)

# --- Inspecting the basket rules --- 
inspect(basket_rules[1:20])

# --- Most Frequent Products --- 
itemFrequencyPlot(txn, topN = 10, type = "absolute")

# --- Visualizations ---

plot(basket_rules, jitter=0)
plot(basket_rules[1:10], method="graph")
plot(basket_rules[1:10], method="paracoord")
plot(basket_rules[1:10], method="grouped", control=list(k=5))

# reference
# https://www.kaggle.com/heeraldedhia/groceries-dataset

