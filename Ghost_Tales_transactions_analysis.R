#Download CSV-file with data of paying transactions in game Ghost Tales.
setwd('~/source/R/ghost_tales_transactions_analysis')
download.file("https://dl.dropboxusercontent.com/u/1286637/2013-2014%20Ghost%20Tales%20transactions.csv",
              "2013-2014 Ghost Tales transactions.csv", method = "curl")
#Read data from file and convert dates
data <- read.csv2("2013-2014 Ghost Tales transactions.csv")
data$transaction_date <- as.POSIXct(strptime(data$transaction_date, "%d.%m.%Y"))
data$install_date <- as.POSIXct(strptime(data$install_date, "%d.%m.%Y"))

#Getting metrics for each player

library(dplyr)
library(ggplot2)

#Recency
#Filter last transaction_date for each ID
lastData <- summarise(group_by(data, id), last_transaction_date = max(transaction_date))
#Recency is calculated by subtracting last transaction date for each player from the end of the tracked period:
RFM <- mutate(lastData, recency = difftime(max(last_transaction_date),
                                           last_transaction_date, units = "days"))
rm(lastData)
ggplot(RFM, aes(as.numeric(recency))) + geom_histogram(color = "steelblue4", fill = "steelblue") +
    labs(title = "Historgam of recency", x = "Recency") +
    geom_vline (xintercept = c(30, 60, 120, 290), colour="slategrey", linetype = "longdash")

#Frequency
#Frequency is the number of transactions for each ID:
numData <- summarise(group_by(data, id), frequency = length(transaction_date))
RFM <- merge(RFM, numData, by = "id")
rm(numData)
ggplot(RFM, aes(frequency)) + geom_histogram(color = "green4", fill = "green3") +
    labs(title = "Historgam of frequency", x = "Frequency") +
    geom_vline (xintercept = c(2, 5, 25, 100), colour="slategrey", linetype = "longdash")

#Monetary Value
#Monetary value is the sum of values from all transactions for each ID divided by Frequency:
sumData <- summarise(group_by(data, id), gross_monetary = sum(value))
RFM <- merge(RFM, sumData, by = "id")
RFM <- mutate(RFM, monetary = gross_monetary / frequency)
rm(sumData)
ggplot(RFM, aes(monetary/100)) + geom_histogram(color = "chocolate4", fill = "chocolate3") +
    labs(title = "Historgam of monetary", x = "Monetary ($)") +
    geom_vline (xintercept = c(40, 60, 100, 200), colour="slategrey", linetype = "longdash")
#Head of the data frame with Recency, Frequency and Monetary Value calculated for each player:
head(RFM[, c(1,3,4,5,6)])

#Lifespan, Paying lifecycle and period to first transaction

#Let's see if there are players, that installed the game more that once:
nrow(unique(data[, c("id", "install_date")]))
nrow(RFM)
#We see, that some IDs have several install_date's. We get the first install_date in that case:
instData <- summarise(group_by(data, id), first_install_date = min(install_date))
fTranData <- summarise(group_by(data, id), first_transaction_date = min(transaction_date))
RFM <- merge(RFM, instData, by = "id")
RFM <- merge(RFM, fTranData, by = "id")
rm(instData)
rm(fTranData)
RFM <- mutate(RFM, lifespan =
                  difftime(last_transaction_date, first_install_date, units = "days"),
              paying_lifecycle =
                  difftime(last_transaction_date, first_transaction_date, units = "days"),
              period_to_first_transaction = lifespan - paying_lifecycle)
avgLifespan <- ceiling(mean(RFM$lifespan))
avgPayingLifecycle <- ceiling(mean(RFM$paying_lifecycle))
avgPeriodToFirstTrans <- ceiling(mean(RFM$period_to_first_transaction))

#Let's plot histograms and mean values for the 3 metrics we've calculated:
ggplot(RFM, aes(as.numeric(lifespan))) + geom_histogram(color = "snow4", fill = "snow3") +
    labs(title = "Historgam of lifespan", x = "Lifespan (days)") +
    geom_vline (xintercept = as.numeric(avgLifespan), colour="red")
ggplot(RFM, aes(as.numeric(paying_lifecycle))) + geom_histogram(color = "snow4", fill = "snow3") +
    labs(title = "Historgam of paying lifecycle", x = "Paying lifecycle (days)") +
    geom_vline (xintercept = as.numeric(avgPayingLifecycle), colour="red")
ggplot(RFM, aes(as.numeric(period_to_first_transaction))) + geom_histogram(color = "snow4", fill = "snow3") +
    labs(title = "Historgam of period to first transaction", x = "Period to first transaction (days)") +
    geom_vline (xintercept = as.numeric(avgPeriodToFirstTrans), colour="red")

#LTV (Life Time Value of a Customer)

#Calculating Retention Rate
#Average paying lifecycle ~ 93 days, calculate Retention Rate per month (Purches cycle):
min(data$transaction_date)
max(RFM$last_transaction_date)
#=> get period from sept.13 to jul.14 (we have hole month track only in this period)
retentionRate <- data.frame(beginning_of_period =
                                seq(as.POSIXct("2013/9/1"), by = "month", length.out = 11),
                            retention_rate = rep(NaN, 11))
for (i in 1:(nrow(retentionRate)-1)) {
    t <- retentionRate$beginning_of_period[i]
    tn <- retentionRate$beginning_of_period[i+1]
    CE <- nrow(RFM[(RFM$first_transaction_date < tn) & (RFM$last_transaction_date) >= tn, ])
    CN <- nrow(RFM[(RFM$first_transaction_date >= t) & (RFM$first_transaction_date < tn), ])
    CS <- nrow(RFM[(RFM$first_transaction_date < t) & (RFM$last_transaction_date >= t), ])
    retentionRate$retention_rate[i] <- (CE - CN)/CS
}
avgRetentionRate <- round(mean(retentionRate$retention_rate, na.rm = T)*100, 2)

#Calculating LTV
getLTV <- function(GM, r = 0.75, d = 0) {
    GM * r / (1 + d - r)
}
LTV <- getLTV(GM = mean(RFM$gross_monetary), r = avgRetentionRate/100)/100

#Average number of transactions by daily cohorts
dCohortsFPay <- summarise(group_by(RFM, first_transaction_date),
                          avg_number_of_transaction = mean(frequency))
dCohortsInst <- summarise(group_by(RFM, first_install_date),
                          avg_number_of_transaction = mean(frequency))

ggplot(dCohortsFPay, aes(as.Date(first_transaction_date), avg_number_of_transaction)) +
    geom_point(color = "steelblue", fill = "steelblue") +
    labs(x = "First transaction date", y = "Average number of transactions") +
    scale_x_date(breaks = pretty_breaks(12)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot(dCohortsInst, aes(as.Date(first_install_date), avg_number_of_transaction)) +
    geom_point(color = "green4", fill = "green4") +
    labs(x = "First transaction date", y = "Average number of transactions") +
    scale_x_date(breaks = pretty_breaks(12)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

#RFM-analisys
#Get a RFM score for all IDs:
RFM <- mutate(RFM, Rscore = factor(recency), Fscore = factor(frequency),
              Mscore = factor(ceiling(monetary)))
levels(RFM$Rscore) <- list(a = 0:30, b = 31:60, c = 61:120, d = 121:290,
                           e = 291:max(RFM$recency))
levels(RFM$Fscore) <- list(a = 1, b = 2:5, c = 6:25, d = 26:100,
                           e = 101:max(RFM$frequency))
levels(RFM$Mscore) <- list(a = 2000, b = 2001:4000, c = 4001:6000, d = 6001:10000,
                           e = 10000:max(ceiling(RFM$monetary)))

RFM <- mutate(RFM, Rscore = as.integer(Rscore), Fscore = as.integer(Fscore),
              Mscore = as.integer(Mscore))
RFM <- mutate(RFM, score = Rscore*100 + Fscore*10 + Mscore)
RFM <- mutate(RFM, Rscore = NULL, Fscore = NULL, Mscore = NULL)

#Generate user segments from RFM score
RFM <- mutate(RFM, segment = factor(score))
levels(RFM$segment) <- list(
    Best_repeat = c(555,554,545,544,535,534,525,524,455,454,445,444,435,434,425,424),
    Best_single = c(515,514,415,414),
    Good_repeat = c(553,543,533,523,453,443,433,423,353:355,343:345,333:335,323:325),
    Good_single = c(513,413,313:315),
    Opportunity_within_Recency_repeat = c(552,551,542,541,532,531,522,521,
                                          452,451,442,441,432,431,422,421,
                                          352,351,342,341,332,331,322,321),
    Opportunity_within_Recency_single = c(512,511,412,411,312,311),
    Opportunity_within_Monetary_repeat = c(253:255,243:245,233:235,223:225,
                                           153:155,143:145,133:135,123:125),
    Opportunity_within_Monetary_single = c(213:215,113:115),
    Dormant = c(251,252,241,242,231,232,221,222,211,212,151,152,141,142,131,132,121,122,111,112)
)
#Head of the data frame with RFM score and segment calculated for each player:
head(RFM[,c("id", "score", "segment")])

ggplot(RFM, aes(segment)) + geom_histogram(color = "steelblue", fill = "steelblue") +
    labs(title = "Number of customers in each segment") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))


