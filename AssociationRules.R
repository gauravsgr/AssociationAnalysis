#========================= An understanding of Apriori algorithm =====================================#
# If the basket contains beer and cheese, then it also contains honey [{beer, Cheese} => {honey}]     #
# 2/20 of the records contain both beer and cheese, so coverage/support                               #
#     is 10% [supp({beer, cheese} ??? {honey})/supp({beer, cheese})]                                  #   
# Of these 2, 1 contains honey, so confidence is 50%                                                  #
# Lift is a measure of if this Itemset is present not by just chance                                  #
# Lift = [supp({beer, cheese} ??? {honey})/supp({beer, cheese})x supp({honey})] > 1 is a good measure #
# if a and b are independent events p(a)*p(b) = p(a U b) i.e. the event of these happening together   #
#=====================================================================================================#

library(arules)    
library(arulesViz)

# Reading the transactions and creating the transactions object
# You can convert a dataframe to transactions as well; click transactions in ?read.transactions and check examples
# like Adult = as(AdultUCI, "transactions")
trns = read.transactions("C:\\R_Working_Dir\\InputData\\Transactions.txt", format="basket", sep=",")

# Getting the matrix image view of the table [rows as transaction and cols as items]
image(trns)

# Getting the frequency plot of the items whose support is atleast 20%
itemFrequencyPlot(trns, support = 0.2)

# Getting the association rules based on the min support and confidence threshold
rules <- apriori(trns, parameter = list(supp = 0.1, conf = 0.5))

# Get a visual representation of all the association rules
inspect(rules)

# Get the summary of the rules (5 pt. tukey summary + mean for each support, confidence and lift)
summary(rules)

# Print out the top 10 rules sorted by support
top.support <- sort(rules, decreasing = TRUE, na.last = NA, by = "support")
inspect(head(top.support, 10))  # or inspect(sort(top.support)[1:10])

# Print out the top 10 rules sorted by confidence
top.confidence <- sort(rules, decreasing = TRUE, na.last = NA, by = "confidence")
inspect(head(top.confidence, 10))

# Print out the top 10 rules sorted by lift
top.lift <- sort(rules, decreasing = TRUE, na.last = NA, by = "lift")
inspect(head(top.lift, 10))

# Other quality measures of the rules can be displayed with <check ?interestMeasure for definitions>
interestMeasure(rules, c("support", "chiSquare", "confidence", "conviction",
                         "cosine", "coverage", "leverage", "lift", "oddsRatio"), trns)

# Calculating a single measure and adding it to the quality slot
quality(rules) <- cbind(quality(rules), oddsRatio = interestMeasure(rules, method = "oddsRatio", trns))
inspect(head(SORT(rules, by = "oddsRatio")))

# Getting the various plots
plot(rules, measure=c("support","lift"), shading="confidence")
# Two-key plot, here order is the no. of items contained in the rule
plot(rules, shading="order", control=list(main ="Two-key plot"))

plot(rules, method="matrix", measure="lift")
plot(rules, method="matrix", measure="lift", control=list(reorder=TRUE))
plot(rules, method="matrix3D", measure="lift")
plot(rules, method="matrix3D", measure="lift", control = list(reorder=TRUE))
plot(rules, method="matrix", measure=c("lift", "confidence"))
plot(rules, method="matrix", measure=c("lift","confidence"), 
     control = list(reorder=TRUE))
plot(rules, method="grouped")
plot(rules, method="grouped", control=list(k=50))
# The most informative of all plots
sel = plot(rules, method="grouped", interactive=TRUE)

# Getting the rules into a text file (sink is just the direct '>' operator)
sink("sink-examp.txt")
inspect(head(SORT(rules, by = "support")))

