##### Fetching commodity data from RP database
######## Gold modeling
library("RPQuantUtils")
library("RPToolsDB")
require(ggplot2)
require("ppcor")
require(graphics)
require("TTR")
require(plyr)
# require(reshape)
require(reshape2)
require(RColorBrewer)
require(stats)
require(Rsolnp)
require(zoo)
require(xts)
require(vars)
require(Quandl)

source("./RCode/RP_Plotting_Utils.R")
source("./RCode/RP_Macro_Monthly_Utils.R")

user = 'sduprey'
# JIRA Code (e.g. NAR-#)
JIRACode = 'NAR-271'
repoPath = RP_GetSharedPath(user)
# Input Data Path
inputDataPath  = paste(repoPath,'InputData/', user,'/',JIRACode,'/',sep="")


# Getting data from Quandl
# CBOT 10-year US Treasury Note Futures #1 (TY1) - Unadjusted Prices, Roll On Last Trading Day, Continuous Contract History

US_10Y_TN_Rolled_Futures <- Quandl("SCF/CME_TY1_EN", authcode="zxRRshM1ve6mjrEckLYy")
SaveDataFrame(US_10Y_TN_Rolled_Futures,inputDataPath,"US_10Y_TN_Rolled_Futures")
US_10Y_TN_Rolled_Futures <- readRDS(paste(inputDataPath,"US_10Y_TN_Rolled_Futures.rds", sep = ""))
print(min(US_10Y_TN_Rolled_Futures$Date))

#CBOT 5-year US Treasury Note Futures #1 (FV1) - Unadjusted Prices, Roll On Last Trading Day, Continuous Contract History
#Underlying Futures Contract: CBOT 5-year US Treasury Note (FV) Depth: Continuous Futures Contract #1 (FV1) Roll Date: Roll on Last Trading Day. Contracts roll on the last trading day of the expiring or front contract. Thus the continuous contract history is a non-overlapping end-to-end concatenation of underlying individual contracts, spliced on successive expiry dates. Price Adjustment: Unadjusted. Prices are not adjusted in any way. Continuous contracts reflect raw prices from the underlying contracts. Methodology: To read more about the Stevens roll date and price adjustment methodology, see the Documentation tab on the Stevens Continuous Futures database home page. Contract Size: One U.S. Treasury note having a face value at maturity of $100,000. Deliverable Good: U.S. Treasury notes with an original term to maturity of not more than five years and three months and a remaining term to maturity of not less than four years and two months as of the first day of the delivery month. The invoice price equals the futures settlement price times a conversion factor, plus accrued interest. The conversion factor is the price of the delivered note ($1 par value) to yield 6 percent. Tick Size: One-quarter of one thirty-second (1/32) of one point ($7.8125, rounded up to the nearest cent per contract), including intermonth spreads. Pricing Unit: Points ($1,000) and quarters of 1/32 of a point. For example, 119-16 represents 119 16/32, 119-162 represents 119 16.25/32, 119-165 represents 119 16.5/32, and 119-167 represents 119 16.75/32. Par is on the basis of 100 points. Columns: Open, High, Low, Settle, Volume, Previous Day Open Interest. Note that Open Interest is always reported for the previous trading day, to avoid lookahead bias.
US_5Y_TN_Rolled_Futures <- Quandl("SCF/CME_FV1_EN", authcode="zxRRshM1ve6mjrEckLYy")
SaveDataFrame(US_5Y_TN_Rolled_Futures,inputDataPath,"US_5Y_TN_Rolled_Futures")
US_5Y_TN_Rolled_Futures <- readRDS(paste(inputDataPath,"US_5Y_TN_Rolled_Futures.rds", sep = ""))
print(min(US_5Y_TN_Rolled_Futures$Date))

#CBOT 2-year US Treasury Note Futures #1 (TU1) - Unadjusted Prices, Roll On Last Trading Day, Continuous Contract History
#Underlying Futures Contract: CBOT 2-year US Treasury Note (TU) Depth: Continuous Futures Contract #1 (TU1) Roll Date: Roll on Last Trading Day. Contracts roll on the last trading day of the expiring or front contract. Thus the continuous contract history is a non-overlapping end-to-end concatenation of underlying individual contracts, spliced on successive expiry dates. Price Adjustment: Unadjusted. Prices are not adjusted in any way. Continuous contracts reflect raw prices from the underlying contracts. Methodology: To read more about the Stevens roll date and price adjustment methodology, see the Documentation tab on the Stevens Continuous Futures database home page. Contract Size: One U.S. Treasury note having a face value at maturity of $200,000. Deliverable Good: .S. Treasury notes with an original term to maturity of not more than five years and three months and a remaining term to maturity of not less than one year and nine months from the first day of the delivery month and a remaining term to maturity of not more than two years from the last day of the delivery month. The invoice price equals the futures settlement price times a conversion factor, plus accrued interest. The conversion factor is the price of the delivered note ($1 par value) to yield 6 percent. Tick Size: One-quarter of one thirty-second (1/32) of one point ($15.625, rounded up to the nearest cent per contract), including intermonth spreads. Pricing Unit: Points ($2,000) and quarters of 1/32 of a point. For example, 109-16 represents 109 16/32, 109-162 represents 109 16.25/32, 109-165 represents 109 16.5/32, and 109-167 represents 109 16.75/32. Par is on the basis of 100 points. Columns: Open, High, Low, Settle, Volume, Previous Day Open Interest. Note that Open Interest is always reported for the previous trading day, to avoid lookahead bias.
US_2Y_TN_Rolled_Futures <- Quandl("SCF/CME_TU1_EN", authcode="zxRRshM1ve6mjrEckLYy")
SaveDataFrame(US_2Y_TN_Rolled_Futures,inputDataPath,"US_2Y_TN_Rolled_Futures")
US_2Y_TN_Rolled_Futures <- readRDS(paste(inputDataPath,"US_2Y_TN_Rolled_Futures.rds", sep = ""))
print(min(US_2Y_TN_Rolled_Futures$Date))


#CBOT 30-year US Treasury Bond Futures #1 (US1) - Unadjusted Prices, Roll on First of Month, Continuous Contract History
#Underlying Futures Contract: CBOT 30-year US Treasury Bond (US) Depth: Continuous Futures Contract #1 (US1) Roll Date: Roll on First Day of Delivery Month. Contracts roll on the first day of the delivery month of the expiring or front contract. If the front contract expires before the first day of the deliverty month, then contracts roll on the expiry date instead. Price Adjustment: Unadjusted. Prices are not adjusted in any way. Continuous contracts reflect raw prices from the underlying contracts. Methodology: To read more about the Stevens roll date and price adjustment methodology, see the Documentation tab on the Stevens Continuous Futures database home page. Contract Size: One U.S. Treasury bond having a face value at maturity of $100,000. Deliverable Good: U.S. Treasury bonds that, if callable, are not callable for at least 15 years from the first day of the delivery month or, if not callable, have a remaining term to maturity of at least 15 years from the first day of the delivery month. Note: Beginning with the March 2011 expiry, the deliverable grade for T-Bond futures will be bonds with remaining maturity of at least 15 years, but less than 25 years, from the first day of the delivery month. The invoice price equals the futures settlement price times a conversion factor, plus accrued interest. The conversion factor is the price of the delivered bond ($1 par value) to yield 6 percent. Tick Size: One thirty-second (1/32) of one point ($31.25), except for intermonth spreads, where the minimum price fluctuation shall be one-quarter of one thirty-second of one point ($7.8125 per contract). Pricing Unit: Points ($1,000) and 1/32 of a point. For example, 134-16 represents 134 16/32. Par is on the basis of 100 points. Columns: Open, High, Low, Settle, Volume, Previous Day Open Interest. Note that Open Interest is always reported for the previous trading day, to avoid lookahead bias.
US_30Y_TN_Rolled_Futures <- Quandl("SCF/CME_US1_FN", authcode="zxRRshM1ve6mjrEckLYy")
SaveDataFrame(US_30Y_TN_Rolled_Futures,inputDataPath,"US_30Y_TN_Rolled_Futures")
US_30Y_TN_Rolled_Futures <- readRDS(paste(inputDataPath,"US_30Y_TN_Rolled_Futures.rds", sep = ""))
print(min(US_30Y_TN_Rolled_Futures$Date))





#EUREX Euro-Bund Futures #1 (FGBL1) - Unadjusted Prices, Roll on First of Month, Continuous Contract History
#Deliverable Good:German debt security with remaining term of 8.5 to 10.5 years with a 6% coupon.
GER_10Y_BUND_Rolled_Futures <- Quandl("SCF/EUREX_FGBL1_FN", authcode="18dndgL4fDkTE-r7zMrV")
SaveDataFrame(GER_10Y_BUND_Rolled_Futures,inputDataPath,"GER_10Y_BUND_Rolled_Futures")
GER_10Y_BUND_Rolled_Futures <- readRDS(paste(inputDataPath,"GER_10Y_BUND_Rolled_Futures.rds", sep = ""))
print(min(GER_10Y_BUND_Rolled_Futures$Date))



# LIFFE Long Gilt Futures #1 (R1) - Unadjusted Prices, Roll on First of Month, Continuous Contract History
# Deliverable Good:Delivery may be made of any gilts on the List of Deliverable Gilts in respect of a delivery month of an Exchange Contract, as published by the Exchange on or before the tenth business day prior to the First Notice Day of such delivery month. Holders of long positions on any day within the Notice Period may be delivered against during the delivery month. All gilt issues included in the List will have the following characteristics: having terms as to redemption such as provide for redemption of the entire gilt issue in a single instalment such that the length of time to the maturity date from, and excluding, the first date of the relevant delivery month is within the maturity range for the relevant Exchange Contract specified by the Board in the Contract Details; having no terms permitting or requiring early redemption; bearing interest at a single fixed rate throughout the term of the issue payable in arrears semi-annually (except in the case of the first interest payment period which may be more or less than six months); being denominated and payable as to the principal and interest only in Pounds and pence; being fully paid or, in the event that the gilt issue is in its first period and is partly paid, being anticipated by the Board to be fully paid on or before the Last Notice Day of the relevant delivery month; not being convertible; not being in bearer form; having been admitted to the Official List of the UK Listing Authority; and being anticipated by the Board to have on one or more days in the delivery month an aggregate principal amount outstanding of not less than 1.5 billion which, by its terms and conditions, if issued in more than one tranche or tap or issue, is fungible. UK Government bonds eligible for the list of deliverable Gilts in the Long Gilt futures contract must have a coupon within a 1% - 7% coupon range inclusive.
UK_10Y_GILT_Rolled_Futures <- Quandl("SCF/LIFFE_R1_OR", authcode="18dndgL4fDkTE-r7zMrV")
SaveDataFrame(UK_10Y_GILT_Rolled_Futures,inputDataPath,"UK_10Y_GILT_Rolled_Futures")
UK_10Y_GILT_Rolled_Futures <- readRDS(paste(inputDataPath,"UK_10Y_GILT_Rolled_Futures.rds", sep = ""))
print(min(UK_10Y_GILT_Rolled_Futures$Date))

# CME Japanese Yen JPY Futures #2 (JY2) - Unadjusted Prices, Roll on First of Month, Continuous Contract History
# Underlying Futures Contract: CME Japanese Yen JPY (JY) Depth: Continuous Futures Contract #2 (JY2) Roll Date: Roll on First Day of Delivery Month. Contracts roll on the first day of the delivery month of the expiring or front contract. If the front contract expires before the first day of the deliverty month, then contracts roll on the expiry date instead. Price Adjustment: Unadjusted. Prices are not adjusted in any way. Continuous contracts reflect raw prices from the underlying contracts. Methodology: To read more about the Stevens roll date and price adjustment methodology, see the Documentation tab on the Stevens Continuous Futures database home page. Contract Size: 12,500,000 Japanese yen Deliverable Good: 12,500,000 Japanese yen Tick Size: $.000001 per Japanese yen increments ($12.50/contract). $.0000005 per Japanese yen increments ($6.25/contract) for JPY/USD futures intra-currency spreads executed on the trading floor and electronically, and for AON transactions. Pricing Unit: US dollars and cents Columns: Open, High, Low, Settle, Volume, Previous Day Open Interest. Note that Open Interest is always reported for the previous trading day, to avoid lookahead bias.
JP_10Y_YEN_Rolled_Futures <- Quandl("SCF/CME_JY2_FN", authcode="18dndgL4fDkTE-r7zMrV")
SaveDataFrame(JP_10Y_YEN_Rolled_Futures,inputDataPath,"JP_10Y_YEN_Rolled_Futures")
JP_10Y_YEN_Rolled_Futures <- readRDS(paste(inputDataPath,"JP_10Y_YEN_Rolled_Futures.rds", sep = ""))
print(min(JP_10Y_YEN_Rolled_Futures$Date))


# EUREX Euro-Bobl Futures #1 (FGBM1) - Unadjusted Prices, Roll on First of Month, Continuous Contract History
GER_5Y_BOBL_Rolled_Futures <- Quandl("SCF/EUREX_FGBM1_FN", authcode="18dndgL4fDkTE-r7zMrV")
SaveDataFrame(GER_5Y_BOBL_Rolled_Futures,inputDataPath,"GER_5Y_BOBL_Rolled_Futures")
GER_5Y_BOBL_Rolled_Futures <- readRDS(paste(inputDataPath,"GER_5Y_BOBL_Rolled_Futures.rds", sep = ""))
print(min(GER_5Y_BOBL_Rolled_Futures$Date))

# EUREX Euro-Schatz Futures #1 (FGBS1) - Unadjusted Prices, Roll on First of Month, Continuous Contract History
GER_2Y_SCHATZ_Rolled_Futures <- Quandl("SCF/EUREX_FGBS1_FN", authcode="18dndgL4fDkTE-r7zMrV")
SaveDataFrame(GER_2Y_SCHATZ_Rolled_Futures,inputDataPath,"GER_2Y_SCHATZ_Rolled_Futures")
GER_2Y_SCHATZ_Rolled_Futures <- readRDS(paste(inputDataPath,"GER_2Y_SCHATZ_Rolled_Futures.rds", sep = ""))
print(min(GER_2Y_SCHATZ_Rolled_Futures$Date))


# french OAT 10 Y
FR_10Y_OAT_Rolled_Futures <- Quandl("SCF/EUREX_FOAT1_FN", authcode="18dndgL4fDkTE-r7zMrV")
SaveDataFrame(FR_10Y_OAT_Rolled_Futures,inputDataPath,"FR_10Y_OAT_Rolled_Futures")
FR_10Y_OAT_Rolled_Futures <- readRDS(paste(inputDataPath,"FR_10Y_OAT_Rolled_Futures.rds", sep = ""))
################### drop them not enough historic
print(min(FR_10Y_OAT_Rolled_Futures$Date))


# EUREX Euro-BTP Futures #1 (FBTP1) - Unadjusted Prices, Roll on First of Month, Continuous Contract History
# short to long term for all Europe

EU_10Y_BTP_Rolled_Futures <- Quandl("SCF/EUREX_FBTP1_FN", authcode="18dndgL4fDkTE-r7zMrV")
SaveDataFrame(EU_10Y_BTP_Rolled_Futures,inputDataPath,"EU_10Y_BTP_Rolled_Futures")
EU_10Y_BTP_Rolled_Futures <- readRDS(paste(inputDataPath,"EU_10Y_BTP_Rolled_Futures.rds", sep = ""))
################### drop them not enough historic
print(min(EU_10Y_BTP_Rolled_Futures$Date))


# CANADA 10 Years
CA_10Y_Rolled_Futures <-Quandl("SCF/CME_CD1_OB", authcode="18dndgL4fDkTE-r7zMrV")
SaveDataFrame(CA_10Y_Rolled_Futures,inputDataPath,"CA_10Y_Rolled_Futures")
CA_10Y_Rolled_Futures <- readRDS(paste(inputDataPath,"CA_10Y_Rolled_Futures.rds", sep = ""))
print(min(CA_10Y_Rolled_Futures$Date))


# Euro Buxl 30 Years
EU_30Y_BUXL_Rolled_Futures <- Quandl("CHRIS/EUREX_FGBX1", authcode="18dndgL4fDkTE-r7zMrV", trim_start="2000-05-01")
SaveDataFrame(EU_30Y_BUXL_Rolled_Futures,inputDataPath,"EU_30Y_BUXL_Rolled_Futures")
EU_30Y_BUXL_Rolled_Futures <- readRDS(paste(inputDataPath,"EU_30Y_BUXL_Rolled_Futures.rds", sep = ""))
################### drop them not enough historic
print(min(EU_30Y_BUXL_Rolled_Futures$Date))

# EUREX BOBL
Quandl("SCF/EUREX_FGBM2_FN", authcode="18dndgL4fDkTE-r7zMrV")
Quandl("SCF/EUREX_FGBM1_FN", authcode="18dndgL4fDkTE-r7zMrV")
# MEDIUM LIFFE
# Quandl.get('CHRIS/LIFFE_H1', 'authcode', '18dndgL4fDkTE-r7zMrV')