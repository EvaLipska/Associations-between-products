getwd()

library(arules)
library(arulesViz)

trans <- read.transactions("ElectronidexTransactions2017.csv", format = "basket", sep = ",", rm.duplicates = TRUE) 
# removing replicates is required

# data
summary(trans)
length (trans) # Number of transactions.
size(trans)
inspect(trans[1:3]) # first three transations, alphabetic order
inspect(trans[2]) # second transaction
LIST(trans) # lists the transactions by conversion
itemLabels(trans) # to see the item labels


# the most frequent products
itemFrequency(trans[ ,1]) 
9835*125*0.03506172 # amount of purcheses 
0.002745297*9835 # the item trans[,1] shows up in 27 transactions
0.101881037*9835 # shows in 1002 transactions
itemFrequency(trans) # trans[, 1:10])
sort(itemFrequency(trans, type = "absolute"), decreasing = F)

itemFrequencyPlot(trans, topN = 10, type = "absolute", main = "Item frequency", col = "turquoise")
itemFrequencyPlot(trans, support = 0.10)
plot(density(size (trans)), col = "turquoise", lwd = "3", main = "Number of items per transaction")
boxplot(size(trans))

# associating rules
rule1 <- apriori(trans, parameter = list(support = .009, confidence = .6))
inspect(rule1)
# rule2 <- apriori(trans, parameter = list(support = .01, confidence = .6, minlen = 2))
# inspect(rule2)
# rule3 <- apriori(trans, parameter = list(support = .005, confidence = .7, minlen = 2))
# inspect(rule3)

plot(rule2, "graph", engine = "interactive")

red <- is.redundant(rule1) # identifying duplicate rules
summary(red)

HPLap <- apriori(trans, parameter = list(supp = .001, conf = .2, minlen = 2), appearance = list(default = "rhs", lhs = "Acer Desktop"))
inspect(HPLap)

# m1 <- m1[!red] # removing redundand rules
top.support <- sort(m1, decreasing = TRUE, na.last = NA, by = "support")
inspect(top.support)

m2 <- apriori(trans, parameter = list(supp = .03, conf = .2, minlen = 2))
ts <- sort(m2, decreasing = T, na.last = NA, by = "support")
inspect(ts)
inspect(sort(m2, by = "lift")[1:9])
# ItemRules <- subset(RulesName, items %in% "item name")

image(trans, xlab = "Items", ylab = "Trasactions")
image(sample(trans, 50)

save.image()
