dataRBDA <- RP_GetAnalyticsDataLocalDB(user = "sduprey", region = "US", CAP = "R1000", Groups = "ALL",
                                       startDate = "2015-01-01", endDate = "2015-07-01",
                                       colSubset = c("DATE_CLOSECUT", "TIMESTAMP_REGION", "GROUP","CATEGORY",
                                                     "EVENT_SENTIMENT_SCORE"),
                                       version = "RBDA", CustomFilter = "", CustomSIMGAP = "CUSTOM")

dataRBDA <- dataRBDA[EVENT_SENTIMENT_SCORE != 0, ]

dataRBDA[, HOUR   := chron::hours(TIMESTAMP_REGION)]
dataRBDA[, MINUTE := chron::minutes(TIMESTAMP_REGION)]

dataNew <- dataRBDA
#dataNew <- dataRBDA[GROUP != "technical-analysis" & GROUP != "stock-prices"]
#dataNew <- dataRBDA[GROUP == "technical-analysis" | GROUP == "stock-prices"]

hour0000_0859 <- dataNew[HOUR <= 8]
hour0900_0929 <- dataNew[HOUR == 9 & MINUTE < 30]
hour0930_0959 <- dataNew[HOUR == 9 & MINUTE >= 30]
hour1000_1559 <- dataNew[HOUR >= 10 & HOUR <= 15]
hour1600_2359 <- dataNew[HOUR >= 16]

cat("Hours 00:00 - 08:59   ", NROW(hour0000_0859)/1e6)
cat("Hours 09:00 - 09:29   ", NROW(hour0900_0929)/1e6)
cat("Hours 09:31 - 09:59   ", NROW(hour0930_0959)/1e6)
cat("Hours 10:00 - 15:59   ", NROW(hour1000_1559)/1e6)
cat("Hours 16:00 - 23:59   ", NROW(hour1600_2359)/1e6)

# Check: the two below must be identical
identical(NROW(hour0000_0859) + NROW(hour0900_0929) + NROW(hour0930_0959) +
            NROW(hour1000_1559) + NROW(hour1600_2359), NROW(dataNew))

# Total, trading and non-trading hours
NROW(dataNew)/1e6
NROW(hour0930_0959)/1e6 + NROW(hour1000_1559)/1e6
NROW(hour0000_0859)/1e6 + NROW(hour0900_0929)/1e6 + NROW(hour1600_2359)/1e6