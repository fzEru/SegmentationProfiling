
### when this program is first run, it appears that all of the recoding does not go through on the 1st attempt
### I usually run it a few times section by section and I see all of the NA blocks eventually fill in
### maybe it runs better without comments?


library(data.table)
library(lubridate)
library(bit64)
library(dplyr)
library(stringr)
library(anytime)
library(knitr)
library(tidyverse)
library(ggplot2)
library(broom)
library(janitor)
library(dlookr)
library(purrr)
library(psych)
library(openxlsx)
library(expss)
library(cluster)    
library(factoextra)
library(skimr)
library(cluster)    
library(e1071)      # to compute skewness

## read in customer data table

library(readr)
Customer_Dataset_File <- read_csv("/Users/mohammedkhan/Desktop/Data Science/Data Exploration/Customer_Dataset_File.csv")
Diagnose_Output <- read_csv("/Users/mohammedkhan/Desktop/Data Science/Data Exploration/Diagnose_Output.csv")
Operational_Documentation <- read_csv("/Users/mohammedkhan/Desktop/Data Science/Data Exploration/Operational_Documentation.csv")

#start~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## STEPS 
## identify continuous vs categorical variables 
  # change categorical variables to numeric

t1 <- Diagnose_Output %>% filter(Diagnose_Output$unique_count %in% c(2, 3, 5, 7) & Diagnose_Output$types == 'character')

# gender
Customer_Dataset_File$Gender[is.na(Customer_Dataset_File$Gender)] <- "Male"
# recoded 35 NA to males to reach closer to a 50/50 split
Customer_Dataset_File$Gender[Customer_Dataset_File$Gender == "Female"] <- 0
Customer_Dataset_File$Gender[Customer_Dataset_File$Gender == "Male"] <- 1
# recoded Female to 0 and Male to 1
Customer_Dataset_File$Gender <- as.numeric(Customer_Dataset_File$Gender)
# converting data type to numeric 

# jobCategory
Customer_Dataset_File$JobCategory[is.na(Customer_Dataset_File$JobCategory)] <- 4
# recoded 15 NA to Sales since it has the greatest frequency
Customer_Dataset_File$JobCategory[Customer_Dataset_File$JobCategory == "Agriculture"] <- 0
Customer_Dataset_File$JobCategory[Customer_Dataset_File$JobCategory == "Crafts"] <- 1
Customer_Dataset_File$JobCategory[Customer_Dataset_File$JobCategory == "Labor"] <- 2
Customer_Dataset_File$JobCategory[Customer_Dataset_File$JobCategory == "Professional"] <- 3
Customer_Dataset_File$JobCategory[Customer_Dataset_File$JobCategory == "Sales"] <- 4
Customer_Dataset_File$JobCategory[Customer_Dataset_File$JobCategory == "Service"] <- 5
# recoded Agriculture to 0, Crafts to 1, Labor to 2, Professional to 3, Sales to 4, and Service to 5
Customer_Dataset_File$JobCategory <- as.numeric(Customer_Dataset_File$JobCategory)
# converting data type to numeric

# unionMember
Customer_Dataset_File$UnionMember[Customer_Dataset_File$UnionMember == "No"] <- 0 
Customer_Dataset_File$UnionMember[Customer_Dataset_File$UnionMember == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$UnionMember <- as.numeric(Customer_Dataset_File$UnionMember)
# converting data type to numeric

# retired
Customer_Dataset_File$Retired[Customer_Dataset_File$Retired == "No"] <- 0 
Customer_Dataset_File$Retired[Customer_Dataset_File$Retired == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$Retired <- as.numeric(Customer_Dataset_File$Retired)
# converting data type to numeric 

# loanDefault 
Customer_Dataset_File$LoanDefault[Customer_Dataset_File$LoanDefault == "No"] <- 0 
Customer_Dataset_File$LoanDefault[Customer_Dataset_File$LoanDefault == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$LoanDefault <- as.numeric(Customer_Dataset_File$LoanDefault)
# converting data type to numeric 

# maritalStatus
Customer_Dataset_File$MaritalStatus[Customer_Dataset_File$MaritalStatus == "Married"] <- 0 
Customer_Dataset_File$MaritalStatus[Customer_Dataset_File$MaritalStatus == "Unmarried"] <- 1 
# recoded Married to 0 and Ummarried to 1 
Customer_Dataset_File$MaritalStatus <- as.numeric(Customer_Dataset_File$MaritalStatus)
# converting data type to numeric 

# carOwnership
Customer_Dataset_File$CarOwnership[Customer_Dataset_File$CarOwnership == "Lease"] <- 0 
Customer_Dataset_File$CarOwnership[Customer_Dataset_File$CarOwnership == "Own"] <- 1 
# recoded Lease to 0 and Own to 1 
# -1 remains at 497 
Customer_Dataset_File$CarOwnership <- as.numeric(Customer_Dataset_File$CarOwnership)
# converting data type to numeric 

# carBrand
Customer_Dataset_File$CarBrand[Customer_Dataset_File$CarBrand == "Domestic"] <- 0 
Customer_Dataset_File$CarBrand[Customer_Dataset_File$CarBrand == "Foreign"] <- 1 
# recoded Domestic to 0 and Foreign to 1 
# -1 remains at 497
Customer_Dataset_File$CarBrand <- as.numeric(Customer_Dataset_File$CarBrand)
# converting data type to numeric 

# politicalPartyMem
Customer_Dataset_File$PoliticalPartyMem[Customer_Dataset_File$PoliticalPartyMem== "No"] <- 0 
Customer_Dataset_File$PoliticalPartyMem[Customer_Dataset_File$PoliticalPartyMem == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$PoliticalPartyMem <- as.numeric(Customer_Dataset_File$PoliticalPartyMem)
# converting data type to numeric 

# vote
Customer_Dataset_File$Votes[Customer_Dataset_File$Votes == "No"] <- 0 
Customer_Dataset_File$Votes[Customer_Dataset_File$Votes == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$Votes<- as.numeric(Customer_Dataset_File$Votes)
# converting data type to numeric 

# creditCard
Customer_Dataset_File$CreditCard[Customer_Dataset_File$CreditCard == "AMEX"] <- 0
Customer_Dataset_File$CreditCard[Customer_Dataset_File$CreditCard == "Disc"] <- 1
Customer_Dataset_File$CreditCard[Customer_Dataset_File$CreditCard == "Mast"] <- 2
Customer_Dataset_File$CreditCard[Customer_Dataset_File$CreditCard == "Othe"] <- 3
Customer_Dataset_File$CreditCard[Customer_Dataset_File$CreditCard == "Visa"] <- 4
# recoded AMEX to 0, Disc to 1, Mast to 2, Othe to 3, and Visa to 4
Customer_Dataset_File$CreditCard <- as.numeric(Customer_Dataset_File$CreditCard)
# converting data type to numeric

# activeLifestyle
Customer_Dataset_File$ActiveLifestyle[Customer_Dataset_File$ActiveLifestyle == "No"] <- 0 
Customer_Dataset_File$ActiveLifestyle[Customer_Dataset_File$ActiveLifestyle == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$ActiveLifestyle<- as.numeric(Customer_Dataset_File$ActiveLifestyle)
# converting data type to numeric 

# equipmentRental
Customer_Dataset_File$EquipmentRental[Customer_Dataset_File$EquipmentRental == "No"] <- 0 
Customer_Dataset_File$EquipmentRental[Customer_Dataset_File$EquipmentRental == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$EquipmentRental<- as.numeric(Customer_Dataset_File$EquipmentRental)
# converting data type to numeric 

# callingCard
Customer_Dataset_File$CallingCard[Customer_Dataset_File$CallingCard == "No"] <- 0 
Customer_Dataset_File$CallingCard[Customer_Dataset_File$CallingCard == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$CallingCard<- as.numeric(Customer_Dataset_File$CallingCard)
# converting data type to numeric 

# wirelessData
Customer_Dataset_File$WirelessData[Customer_Dataset_File$WirelessData == "No"] <- 0 
Customer_Dataset_File$WirelessData[Customer_Dataset_File$WirelessData == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$WirelessData<- as.numeric(Customer_Dataset_File$WirelessData)
# converting data type to numeric 

# multiline
Customer_Dataset_File$Multiline[Customer_Dataset_File$Multiline == "No"] <- 0 
Customer_Dataset_File$Multiline[Customer_Dataset_File$Multiline == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$Multiline<- as.numeric(Customer_Dataset_File$Multiline)
# converting data type to numeric 

# VM
Customer_Dataset_File$VM[Customer_Dataset_File$VM == "No"] <- 0 
Customer_Dataset_File$VM[Customer_Dataset_File$VM == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$VM<- as.numeric(Customer_Dataset_File$VM)
# converting data type to numeric 

# pager
Customer_Dataset_File$Pager[Customer_Dataset_File$Pager == "No"] <- 0 
Customer_Dataset_File$Pager[Customer_Dataset_File$Pager== "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$Pager<- as.numeric(Customer_Dataset_File$Pager)
# converting data type to numeric 

# internet
Customer_Dataset_File$Internet[Customer_Dataset_File$Internet == "No"] <- 0 
Customer_Dataset_File$Internet[Customer_Dataset_File$Internet == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$Internet<- as.numeric(Customer_Dataset_File$Internet)
# converting data type to numeric 

# callerID
Customer_Dataset_File$CallerID[Customer_Dataset_File$CallerID == "No"] <- 0 
Customer_Dataset_File$CallerID[Customer_Dataset_File$CallerID == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$CallerID<- as.numeric(Customer_Dataset_File$CallerID)
# converting data type to numeric 

# callWait
Customer_Dataset_File$CallWait[Customer_Dataset_File$CallWait== "No"] <- 0 
Customer_Dataset_File$CallWait[Customer_Dataset_File$CallWait == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$CallWait<- as.numeric(Customer_Dataset_File$CallWait)
# converting data type to numeric 

# callForward
Customer_Dataset_File$CallForward[Customer_Dataset_File$CallForward == "No"] <- 0 
Customer_Dataset_File$CallForward[Customer_Dataset_File$CallForward == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$CallForward<- as.numeric(Customer_Dataset_File$CallForward)
# converting data type to numeric 

# threeWayCalling
Customer_Dataset_File$ThreeWayCalling[Customer_Dataset_File$ThreeWayCalling == "No"] <- 0 
Customer_Dataset_File$ThreeWayCalling[Customer_Dataset_File$ThreeWayCalling == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$ThreeWayCalling<- as.numeric(Customer_Dataset_File$ThreeWayCalling)
# converting data type to numeric 

# eBilling
Customer_Dataset_File$EBilling[Customer_Dataset_File$EBilling == "No"] <- 0 
Customer_Dataset_File$EBilling[Customer_Dataset_File$EBilling == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$EBilling<- as.numeric(Customer_Dataset_File$EBilling)
# converting data type to numeric 

# ownsPC
Customer_Dataset_File$OwnsPC[Customer_Dataset_File$OwnsPC== "No"] <- 0 
Customer_Dataset_File$OwnsPC[Customer_Dataset_File$OwnsPC == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$OwnsPC<- as.numeric(Customer_Dataset_File$OwnsPC)
# converting data type to numeric 

# ownsMobileDevice
Customer_Dataset_File$OwnsMobileDevice[Customer_Dataset_File$OwnsMobileDevice == "No"] <- 0 
Customer_Dataset_File$OwnsMobileDevice[Customer_Dataset_File$OwnsMobileDevice == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$OwnsMobileDevice<- as.numeric(Customer_Dataset_File$OwnsMobileDevice)
# converting data type to numeric 

# ownsGameSystem
Customer_Dataset_File$OwnsGameSystem[Customer_Dataset_File$OwnsGameSystem == "No"] <- 0 
Customer_Dataset_File$OwnsGameSystem[Customer_Dataset_File$OwnsGameSystem == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$OwnsGameSystem<- as.numeric(Customer_Dataset_File$OwnsGameSystem)
# converting data type to numeric 

# ownsFax
Customer_Dataset_File$OwnsFax[Customer_Dataset_File$OwnsFax == "No"] <- 0 
Customer_Dataset_File$OwnsFax[Customer_Dataset_File$OwnsFax == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$OwnsFax<- as.numeric(Customer_Dataset_File$OwnsFax)
# converting data type to numeric 

# newsSubscriber
Customer_Dataset_File$NewsSubscriber[Customer_Dataset_File$NewsSubscriber == "No"] <- 0 
Customer_Dataset_File$NewsSubscriber[Customer_Dataset_File$NewsSubscriber == "Yes"] <- 1 
# recoded No to 0 and Yes to 1 
Customer_Dataset_File$NewsSubscriber<- as.numeric(Customer_Dataset_File$NewsSubscriber)
# converting data type to numeric 

#feature engineering~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# creating new variables and removing $ signs and commas to convert to numeric
Customer_Dataset_File$HHIncome_2 = as.integer(gsub("[\\$,]", "", Customer_Dataset_File$HHIncome))
Customer_Dataset_File$CarValue_2 = as.integer(gsub("[\\$,]", "", Customer_Dataset_File$CarValue))
Customer_Dataset_File$CardSpendMonth_2 = as.integer(gsub("[\\$,]", "", Customer_Dataset_File$CardSpendMonth))
Customer_Dataset_File$VoiceLastMonth_2 = as.integer(gsub("[\\$,]", "", Customer_Dataset_File$VoiceLastMonth))
Customer_Dataset_File$VoiceOverTenure_2 = as.integer(gsub("[\\$,]", "", Customer_Dataset_File$VoiceOverTenure))
Customer_Dataset_File$EquipmentLastMonth_2 = as.integer(gsub("[\\$,]", "", Customer_Dataset_File$EquipmentLastMonth))
Customer_Dataset_File$EquipmentOverTenure_2 = as.integer(gsub("[\\$,]", "", Customer_Dataset_File$EquipmentOverTenure))
Customer_Dataset_File$DataLastMonth_2 = as.integer(gsub("[\\$,]", "", Customer_Dataset_File$DataLastMonth))
Customer_Dataset_File$DataOverTenure_2 = as.integer(gsub("[\\$,]", "", Customer_Dataset_File$DataOverTenure))

Customer_Dataset_File$TotalDebt = Customer_Dataset_File$CreditDebt + Customer_Dataset_File$OtherDebt
# finding total debt
Customer_Dataset_File$TotalLastMonth = Customer_Dataset_File$VoiceLastMonth_2 + Customer_Dataset_File$EquipmentLastMonth_2 + Customer_Dataset_File$DataLastMonth_2
# finding total spent last month between voice ,equipment and data
Customer_Dataset_File$TotalTenure = Customer_Dataset_File$VoiceOverTenure_2 + Customer_Dataset_File$EquipmentOverTenure_2 + Customer_Dataset_File$DataOverTenure_2
#finding total spent in tenure for voice, equipment and data
Customer_Dataset_File$TotalDebtLog = log(Customer_Dataset_File$TotalDebt)
Customer_Dataset_File$TotalLastMonthLog = log(Customer_Dataset_File$TotalLastMonth)
Customer_Dataset_File$TotalTenureLog = log(Customer_Dataset_File$TotalTenure)
# converting each of the three data above to logs for normal distribution

#find skewness and make data normally distributed~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

idx <- sapply(Customer_Dataset_File, class) == "numeric"
# create index of numeric classes
idx2 <- sapply(Customer_Dataset_File, class) == "integer"
# create index of integer classes
skewOutNumeric <- lapply(Customer_Dataset_File[, idx], skewness)
skewOutInteger <- lapply(Customer_Dataset_File[, idx2], skewness)
# return skewness for numeric and integer columns
Customer_Dataset_File$HHIncome_2Log = log(Customer_Dataset_File$HHIncome_2)
Customer_Dataset_File$CarValue_2Log = log(Customer_Dataset_File$CarValue_2)
Customer_Dataset_File$CardSpendMonth_2Log = log(Customer_Dataset_File$CardSpendMonth_2)
Customer_Dataset_File$VoiceLastMonth_2Log = log(Customer_Dataset_File$VoiceLastMonth_2)
Customer_Dataset_File$VoiceOverTenure_2Log = log(Customer_Dataset_File$VoiceOverTenure_2)
Customer_Dataset_File$CreditDebtLog = log(Customer_Dataset_File$CreditDebt)
Customer_Dataset_File$OtherDebtLog = log(Customer_Dataset_File$OtherDebt)
Customer_Dataset_File$EmploymentLengthLog = log(Customer_Dataset_File$EmploymentLength)
Customer_Dataset_File$DebtToIncomeRatioLog = log(Customer_Dataset_File$DebtToIncomeRatio)
Customer_Dataset_File$CardTenureLog = log(Customer_Dataset_File$CardTenure)
Customer_Dataset_File$EquipmentLastMonth_2Log = log(Customer_Dataset_File$EquipmentLastMonth_2)
Customer_Dataset_File$EquipmentOverTenure_2Log = log(Customer_Dataset_File$EquipmentOverTenure_2)
Customer_Dataset_File$DataLastMonth_2Log = log(Customer_Dataset_File$DataLastMonth_2)
Customer_Dataset_File$DataOverTenure_2Log = log(Customer_Dataset_File$DataOverTenure_2)
# made new columns with log values for more normally distributed

#document the extent of missing/bad data and diagnose data problems for categorical variables~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sum(is.na(Customer_Dataset_File$CarValue_2))
summary(Customer_Dataset_File$CarValue_2)
Customer_Dataset_File$CarValue_2[is.na(Customer_Dataset_File$CarValue_2)] <- median(Customer_Dataset_File$CarValue_2, na.rm = TRUE)
# 497 missing values
# replaced NA values with the median of the data
sum(is.na(Customer_Dataset_File$CardSpendMonth_2))
summary(Customer_Dataset_File$CardSpendMonth_2)
Customer_Dataset_File$CardSpendMonth_2[is.na(Customer_Dataset_File$CardSpendMonth_2)] <- median(Customer_Dataset_File$CardSpendMonth_2, na.rm = TRUE)
# 7 missing values
# replaced NA values with the median of the data
sum(is.na(Customer_Dataset_File$VoiceOverTenure_2))
summary(Customer_Dataset_File$VoiceOverTenure_2)
Customer_Dataset_File$VoiceOverTenure_2[is.na(Customer_Dataset_File$VoiceOverTenure_2)] <- median(Customer_Dataset_File$VoiceOverTenure_2, na.rm = TRUE)
# 3 missing values
# replaced NA values with the median of the data
sum(is.na(Customer_Dataset_File$EquipmentLastMonth_2))
summary(Customer_Dataset_File$EquipmentLastMonth_2)
Customer_Dataset_File$EquipmentLastMonth_2[is.na(Customer_Dataset_File$EquipmentLastMonth_2)] <- median(Customer_Dataset_File$EquipmentLastMonth_2, na.rm = TRUE)
# 3296 missing values 
# replaced NA values with the median of the data
sum(is.na(Customer_Dataset_File$EquipmentOverTenure_2))
summary(Customer_Dataset_File$EquipmentOverTenure_2)
Customer_Dataset_File$EquipmentOverTenure_2[is.na(Customer_Dataset_File$EquipmentOverTenure_2)] <- median(Customer_Dataset_File$EquipmentOverTenure_2, na.rm = TRUE)
# 3296 missing values
# replaced NA values with the median of the data
sum(is.na(Customer_Dataset_File$DataLastMonth_2))
summary(Customer_Dataset_File$DataLastMonth_2)
Customer_Dataset_File$DataLastMonth_2[is.na(Customer_Dataset_File$DataLastMonth_2)] <- median(Customer_Dataset_File$DataLastMonth_2, na.rm = TRUE)
# 3656 missing values 
# replaced NA values with the median of the data
sum(is.na(Customer_Dataset_File$DataOverTenure_2))
summary(Customer_Dataset_File$DataOverTenure_2)
Customer_Dataset_File$DataOverTenure_2[is.na(Customer_Dataset_File$DataOverTenure_2)] <- median(Customer_Dataset_File$DataOverTenure_2, na.rm = TRUE)
# 3656 missing values
# replaced NA values with the median of the data
sum(is.na(Customer_Dataset_File$NumberPets))
summary(Customer_Dataset_File$NumberPets)
Customer_Dataset_File$NumberPets[is.na(Customer_Dataset_File$NumberPets)] <- median(Customer_Dataset_File$NumberPets, na.rm = TRUE)
# 6 missing values
# replaced NA values with the median of the data
sum(is.na(Customer_Dataset_File$NumberBirds))
summary(Customer_Dataset_File$NumberBirds)
Customer_Dataset_File$NumberBirds[is.na(Customer_Dataset_File$NumberBirds)] <- median(Customer_Dataset_File$NumberBirds, na.rm = TRUE)
# 34 missing values
# replaced NA values with the median of the data
sum(is.na(Customer_Dataset_File$NumberCats))
summary(Customer_Dataset_File$NumberCats)
Customer_Dataset_File$NumberCats[is.na(Customer_Dataset_File$NumberCats)] <- median(Customer_Dataset_File$NumberCats, na.rm = TRUE)
# 7 missing values
# replaced NA values with the median of the data
sum(is.na(Customer_Dataset_File$NumberDogs))
summary(Customer_Dataset_File$NumberDogs)
Customer_Dataset_File$NumberDogs[is.na(Customer_Dataset_File$NumberDogs)] <- median(Customer_Dataset_File$NumberDogs, na.rm = TRUE)
# 8 missing values
# replaced NA values with the median of the data
sum(is.na(Customer_Dataset_File$HouseholdSize))
summary(Customer_Dataset_File$HouseholdSize)
Customer_Dataset_File$HouseholdSize[is.na(Customer_Dataset_File$HouseholdSize)] <- median(Customer_Dataset_File$HouseholdSize, na.rm = TRUE)
# 8 missing values
# replaced NA values with the median of the data

#finish recoding data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Customer_Dataset_File$TownSize<- as.numeric(Customer_Dataset_File$TownSize)
Customer_Dataset_File$CommuteTime<- as.numeric(Customer_Dataset_File$CommuteTime)
# converted character types to to numeric 
Customer_Dataset_File$TownSize[is.na(Customer_Dataset_File$TownSize)] <- 1
# recoded 2 NA to mode 
Customer_Dataset_File$CommuteTime[is.na(Customer_Dataset_File$CommuteTime)] <- 25
# recoded 2 NA to median 

Customer_Dataset_File$highValueTenure = (Customer_Dataset_File$TotalTenure > 2809)
# high value customers in this attribute are those who clock in above the median

Customer_Dataset_File$highValueTenure[Customer_Dataset_File$highValueTenure == FALSE] <- 0
Customer_Dataset_File$highValueTenure[Customer_Dataset_File$highValueTenure == TRUE] <- 1
# recoding high value customers vs low value customers

#setting up clusters~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# isolating variables for segmentation
Customer_Dataset_File.num <- subset(Customer_Dataset_File, select = c("TotalDebt",
                                                                      "EmploymentLength",
                                                                      "NewsSubscriber",
                                                                      "highValueTenure"))

# making sure the columns have been read into the df
c("highValueTenure") %in% colnames(Customer_Dataset_File.num)


# converts the columns in the df to numeric and calculates the groups means
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}

# utilizing the function to get a sense of the segmentation between the outputs of the chosen variable
names(Customer_Dataset_File.num)
seg.summ(Customer_Dataset_File.num, Customer_Dataset_File$highValueTenure)

# checking for metrics to compare categorical variables against each other and to help drive segmentation
t_test1 <- Customer_Dataset_File.num %>% 
  tabyl(highValueTenure, TotalDebt, show_missing_levels = FALSE) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title("combined")
t_test1

t_test2 <- Customer_Dataset_File.num %>% 
  tabyl(highValueTenure, EmploymentLength, show_missing_levels = FALSE) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title("combined")
t_test2

t_test3 <- Customer_Dataset_File.num %>% 
  tabyl(highValueTenure, NewsSubscriber, show_missing_levels = FALSE) %>% 
  adorn_totals("row") %>% 
  adorn_percentages("all") %>%
  adorn_pct_formatting(digits = 1) %>%
  adorn_ns %>%
  adorn_title("combined")
t_test3

# making sure all variables are numerical and there are no missing values present
summary(Customer_Dataset_File.num)
sapply(Customer_Dataset_File.num, class)
sum(is.na(Customer_Dataset_File.num))

# scaling values by  mean and standard deviation so the magnitude of variables do not affect the clustering 
Customer_Dataset_File.df <- as.data.frame(Customer_Dataset_File.num)
# confirming that the data type is a data frame
class(Customer_Dataset_File.df)

# setting up kmeans for 4 and 5 clusters
cus.seg.4<- kmeans(Customer_Dataset_File.df, centers = 4, nstart = 25, iter.max = 100)
cus.seg.4

cus.seg.5<- kmeans(Customer_Dataset_File.df, centers = 5, nstart = 25, iter.max = 100)
cus.seg.5

str(cus.seg.4)
str(cus.seg.5)

# displaying the group means by cluster
seg.summ(Customer_Dataset_File.df, cus.seg.4$cluster)
seg.summ(Customer_Dataset_File.df, cus.seg.5$cluster)

# a couple of cluster visualizations
pca.4 <-fviz_cluster(cus.seg.4, Customer_Dataset_File.df)
pca.4

pca.5 <-fviz_cluster(cus.seg.5, Customer_Dataset_File.df)
pca.5


#cluster analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# gap method 
set.seed(123)
gap_stat <- clusGap(Customer_Dataset_File.df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 10)
# Print the result
print(gap_stat, method = "firstmax")

#We can visualize the results with fviz_gap_stat function
fviz_gap_stat(gap_stat)

# gap method says 4 clusters would be best

# silhouette plot 
fviz_nbclust(Customer_Dataset_File.df, kmeans, method = "silhouette")

# silhouette plot also accepts 4 clusters as an appropriate choice, but 5 clusters is also a possibility


#profiling~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

customer.clusters.bind.4 <- cbind(Customer_Dataset_File.df, clusterNum = cus.seg.4$cluster)
customer.clusters.bind.5 <- cbind(Customer_Dataset_File.df, clusterNum = cus.seg.5$cluster)

customer.clusters.df.4.1 <- customer.clusters.bind.6 %>%
  filter(clusterNum == 1) %>%
  filter(highValueTenure == 1)

customer.clusters.df.4.2 <- customer.clusters.bind.6 %>%
  filter(clusterNum == 2) %>%
  filter(highValueTenure == 1)

customer.clusters.df.4.3 <- customer.clusters.bind.6 %>%
  filter(clusterNum == 3) %>%
  filter(highValueTenure == 1)

customer.clusters.df.4.4 <- customer.clusters.bind.6 %>%
  filter(clusterNum == 4) %>%
  filter(highValueTenure == 1)



customer.clusters.df.5.1 <- customer.clusters.bind.7 %>%
  filter(clusterNum == 1) %>%
  filter(highValueTenure == 1)

customer.clusters.df.5.2 <- customer.clusters.bind.7 %>%
  filter(clusterNum == 2) %>%
  filter(highValueTenure == 1)

customer.clusters.df.5.3 <- customer.clusters.bind.7 %>%
  filter(clusterNum == 3) %>%
  filter(highValueTenure == 1)

customer.clusters.df.5.4 <- customer.clusters.bind.7 %>%
  filter(clusterNum == 4) %>%
  filter(highValueTenure == 1)

customer.clusters.df.5.5 <- customer.clusters.bind.7 %>%
  filter(clusterNum == 5) %>%
  filter(highValueTenure == 1)














