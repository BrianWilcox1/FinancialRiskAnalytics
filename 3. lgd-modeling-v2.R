library(dplyr)
library(gmodels)
library(xtable)
library(knitr)
library(stargazer)
library(zoo)
library(ggplot2)

#### DATA PREPARATION ####
df <- read.csv("SBA_All_Extra_Data.csv", stringsAsFactors = FALSE)
pd <- read.csv("SBA_Data_with_Default_Probabilities_3_11_2018.csv", stringsAsFactors = FALSE)
# Exclude CANCLD and EXEMPT per teaching team instructions
df <- arrange(filter(df[,1:31], LoanStatus!="CANCLD", LoanStatus!="EXEMPT", LoanStatus!=""),X)
pd <- arrange(pd,Unnamed..0)
sum(pd$Unnamed..0==df$X)
pd$check <- pd$BorrName
df <- cbind(df,pd[c("Unnamed..0","default_probabilities_5_years","default_probabilities_1_year","check","unemp_Rate")])
sum(df$BorrName==df$check)
df$check <- NULL


# Remove Duplicate Records. There are only 2 of the 54,806 that are duplicated
# Removing these we have a dataset of size 54,804
# df <- df[!duplicated(df),]
# Convert Columns to correct formats.
# df$ApprovalDate <- as.Date(df$ApprovalDate,"%Y/%m/%d")
# df$ChargeOffDate <- as.Date(df$ChargeOffDate,"%Y/%m/%d")

## EXPLORE AND PREPARE INDEPENDENT VARIABLES ##
## INDEPENDENT VARIABLE PREPARATION ##
# BorrState:
table(df$BorrState)
# ProjectState: Equal to BorrState 99.5% of the time.
# Don't Include this var in model
sum(df$ProjectState==df$BorrState, na.rm = TRUE)/nrow(df)
df$BorrState <- as.factor(df$BorrState)
# ThirdPartyLender_State: In non-missing cases, 46% of the time 
# ThirdPartyLender_State is different from BorrState.
# Planning to fill missing values and keep this variable.
sum(df[df$ThirdPartyLender_State!="",]$ThirdPartyLender_State!=
  df[df$ThirdPartyLender_State!="",]$BorrState, na.rm = TRUE)/
  nrow(df[df$ThirdPartyLender_State!="",])

sum(df[!is.na(df$ThirdPartyLender_State),]$ThirdPartyLender_State!=
      df[!is.na(df$ThirdPartyLender_State),]$BorrState, na.rm = TRUE)/
  nrow(df[!is.na(df$ThirdPartyLender_State),])
table(df$ThirdPartyLender_State, useNA = "always")
df$ThirdPartyLender_State[is.na(df$ThirdPartyLender_State)] <- "MISSING"
table(df$ThirdPartyLender_State, useNA = "always")
df$ThirdPartyLender_State <- as.factor(df$ThirdPartyLender_State)
# DeliveryMethod:
table(df$DeliveryMethod)
df$DeliveryMethod <- as.factor(df$DeliveryMethod)
# subpgmdesc:
table(df$subpgmdesc)
table(df$subpgmdesc,df$DeliveryMethod)
df$subpgmdesc <- as.factor(df$subpgmdesc)
# TermInMonths: There are a bunch of oddball Terms that we set to OTHER.
# 99% of loans have Terms of 120 or 240 months.
table(df$TermInMonths)
df <- df %>%
  mutate(TermInMonths1 = 
          case_when(
            TermInMonths == 120 ~ "120",
            TermInMonths == 240 ~ "240",
            TermInMonths < 120 ~ "LT120",
            TermInMonths > 120 & TermInMonths < 240 ~ "BT120_240",
            TermInMonths > 240 ~ "GT240"))
df$TermInMonths1 <- as.factor(df$TermInMonths1)
table(df$TermInMonths1)
# NaicsCode: 
df$NaicsCode <- as.character(df$NaicsCode)
df$naics2 <- substr(df$NaicsCode,1,2)
table(df$naics2, useNA = "always")
df$naics2[is.na(df$NaicsCode)] <- "MISSING"
df$naics2 <- as.factor(df$naics2)
table(df$naics2)
# BusinessType: 
table(df$BusinessType, useNA = "always")
df$BusinessType[is.na(df$BusinessType)] <- "MISSING"
table(df$BusinessType)
df$BusinessType <- as.factor(df$BusinessType)

## DEAL WITH MISSING VALES OF ThirdPartyDollars ##
df$LoanBalRatio <- df$ThirdPartyDollars/df$GrossApproval
quantile(df$LoanBalRatio,c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1), na.rm = TRUE)
MedianLoanBalRatio <- median(df$LoanBalRatio, na.rm = TRUE)
# Interpolate missing values for ThirdPartyDollars using GrossApproval. 
# We will use this method.
df$ThirdPartyDollars0 <- ifelse(is.na(df$ThirdPartyDollars),
                                MedianLoanBalRatio*df$GrossApproval,
                                df$ThirdPartyDollars)
df$TotalLoan <- df$ThirdPartyDollars0 + df$GrossApproval
# Interpolate missing values for ThirdPartyDollars using average of
# ThirdPartyMissing values. For comparison only. Not used in modeling.
df$ThirdPartyDollars1 <- na.aggregate(df$ThirdPartyDollars)
# Create an indicator for when ThirdPartyDollars was missing so we can assess
# whether our interplated values are better or worse in prediction.
df$missingThirdPartyDollars <- ifelse(is.na(df$ThirdPartyDollars),1,0)

## DEFINE DIFFERENT VERSIONS OF LGD ##
df$lgd0 <- df$GrossChargeOffAmount/(df$ThirdPartyDollars0+df$GrossApproval)
df$lgd1 <- df$GrossChargeOffAmount/(df$ThirdPartyDollars1+df$GrossApproval)
# Cap LGD at 1. This is the version we will use.
df$lgd <- pmin(df$lgd0,1)

## ANALYZE LGD ##
# Create dataset containing only defaulted loans.
loss <- df[df$LoanStatus=="CHGOFF",]
quantile(loss$lgd0,c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1), na.rm = TRUE)
summary(loss$lgd0)
summary(loss$lgd1)
ggplot(loss, aes(x=lgd0)) + geom_histogram() + geom_histogram(binwidth=.01)
ggplot(loss, aes(x=lgd1)) + geom_histogram() + geom_histogram(binwidth=.01)
# This is the version of LGD we will use for modeling
summary(loss$lgd)
quantile(loss$lgd,c(0,.1,.2,.3,.4,.5,.6,.7,.8,.9,1), na.rm = TRUE)
ggplot(loss, aes(x=lgd)) + geom_histogram() + geom_histogram(binwidth=.01)

## CREATE TRAINING AND TEST DATA ##
baseline_features <- c("lgd","BorrState","ThirdPartyLender_State","DeliveryMethod","subpgmdesc",
                       "TermInMonths1","naics2","BusinessType","TotalLoan")
baseline_features <- c("lgd","BorrState","ThirdPartyLender_State","DeliveryMethod","subpgmdesc",
                   "TermInMonths1","naics2","BusinessType","TotalLoan","default_probabilities_5_years")
baseline_features <- c("lgd","BorrState","ThirdPartyLender_State","DeliveryMethod","subpgmdesc",
                       "TermInMonths1","naics2","BusinessType","TotalLoan","unemp_Rate","default_probabilities_5_years")

set.seed(1)
trainSet = sample(1:nrow(loss), 0.75*nrow(loss))
train = loss[trainSet,]
validation = loss[-trainSet,]

train_df <- as.data.frame(model.matrix(~.-1,train[,baseline_features]))
validation_df <- as.data.frame(model.matrix(~.-1,validation[,baseline_features]))
all_df <- as.data.frame(model.matrix(~.-1,df[,baseline_features]))



## FIT BASELINE MODEL ##
# Fractional logit from Papke and Wooldridge 1996
fr <- glm(lgd ~ . , data=train_df, family = quasibinomial('logit'))
preds_train <- data.frame("preds"=predict(fr,type='response'))
preds_validation <- data.frame("preds"=predict(fr,validation_df,type='response'))
train_rmse <- sqrt(mean((preds_train$preds-train_df$lgd)^2))
train_rmse
validation_rmse <- sqrt(mean((preds_validation$preds-validation_df$lgd)^2))
validation_rmse
# ggplot(preds, aes(x=preds)) + geom_histogram() + geom_histogram(binwidth=.01)

# Predict LGD for all 54K loans
preds_all <- data.frame("pred_lgd"=predict(fr,all_df,type='response'))
all_df1 <- cbind(df,preds_all)
# Overwrite Fitted LGD with Ground Truth for loans that defaulted
all_df1$lgd_final <- ifelse(is.na(all_df1$ChargeOffDate),
                            all_df1$pred_lgd,
                            all_df1$lgd)
write.csv(all_df1,"/Users/scottmurff/Google Drive/Stanford/MS&E 246/MS&E 246/Project/raw_data_files/SBA_PD_LGD.csv")



## REFINED MODEL ##
# Derive additional variables to include in the model.
# age, balance, 