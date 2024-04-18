library(plyr)
library(arules)
library(arulesViz)

#User

#Data Preparation
df_user <- read.csv("C:/Users/princ/Downloads/userDistinctMilestoneDec15.csv")
df_user = ddply(df_user,c("id"),function(dfl)paste(dfl$milestone,collapse = ","))
df_user$id = NULL
write.table(df_user,"C:/Users/princ/Downloads/UserMilestone.csv",quote=FALSE,row.names = FALSE, col.names = FALSE)

#Generate Transactions and Understand the data
trUser = read.transactions("C:/Users/princ/Downloads/UserMilestone.csv", format = "basket", sep=",")
summary(trUser)

#Frequent Items
rfrequencies <- itemFrequency(trUser, type = "relative")
sortedRFrequencies <- sort(rfrequencies, decreasing = TRUE)
top10Items_RF <- sortedRFrequencies[1:10]
print(top10Items_RF)

afrequencies <- itemFrequency(trUser, type = "absolute")
sortedAFrequencies <- sort(afrequencies, decreasing = TRUE)
top10Items_AF <- sortedAFrequencies[1:10]
print(top10Items_AF)

itemFrequencyPlot(trUser,topN=10)

#Generate Rules
rulesUser = apriori(trUser, parameter = list(supp=0.4, conf=0.5))
rulesUser = apriori(trUser, parameter = list(supp=0.3, conf=0.5))

inspect(rulesUser)
inspect(sort(rulesUser,by='lift'))

plot(rulesUser, method = "paracoord", measure = "confidence", shading = "lift", control = list(reorder = TRUE))

#Generate Itemsets
itemsetsUser = unique(generatingItemsets(rulesUser))
itemsetsUser
inspect(itemsetsUser)

#Session

#Data Preparation
df_session <- read.csv("C:/Users/princ/Downloads/sessionDistinctMilestoneDec15.csv")
df_session = ddply(df_session,c("user_id","date"),function(dfl)paste(dfl$milestone_name,collapse = ","))
df_session$user_id = NULL
df_session$date = NULL
write.table(df_session,"C:/Users/princ/Downloads/SessionMilestone.csv",quote=FALSE,row.names = FALSE, col.names = FALSE)

#Generate Transactions and Understand the data
trSession = read.transactions("C:/Users/princ/Downloads/SessionMilestone.csv", format = "basket", sep=",")
summary(trSession)

#Frequent Items
rSfrequencies <- itemFrequency(trSession, type = "relative")
sortedRSFrequencies <- sort(rSfrequencies, decreasing = TRUE)
top10Items_SRF <- sortedRSFrequencies[1:10]
print(top10Items_SRF)

aSfrequencies <- itemFrequency(trSession, type = "absolute")
sortedASFrequencies <- sort(aSfrequencies, decreasing = TRUE)
top10Items_SAF <- sortedASFrequencies[1:10]
print(top10Items_SAF)

itemFrequencyPlot(trSession,topN=10)

#Generate Rules
rulesSession = apriori(trSession, parameter = list(supp=0.4, conf=0.5))
rulesSession = apriori(trSession, parameter = list(supp=0.3, conf=0.5))
rulesSession = apriori(trSession, parameter = list(supp=0.3, conf=0.4))
inspect(rulesSession)
inspect(sort(rulesSession,by='lift'))

plot(rulesSession, method = "paracoord", measure = "confidence", shading = "lift", control = list(reorder = TRUE))

#Generate Itemsets
itemsetsSession = unique(generatingItemsets(rulesSession))
itemsetsSession
inspect(itemsetsSession)