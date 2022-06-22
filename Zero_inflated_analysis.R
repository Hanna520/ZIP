##############################################################################
# This script performs the Zero-inflated Poisson Analysis
##############################################################################

source("//ochca.com/hcashares/chs/Data/data_science/rscripts/source/master.R")
library(ggplot2)
library(pscl)

setwd('Z:/rscripts/users/interns/hlu')

##########################################################
# Load data
##########################################################
pos <- read.csv("data/model_zero_inflated_pos_335_release.csv")
neg <- read.csv("data/model_zero_inflated_neg_release.csv")

neg$num_attempts <- 0 # Assign number of attempts to be 0 for negative cases

neg <- neg[neg$BookingDate >= as.Date("2014-01-01"),]

pos_model <- pos[,c("BookingNumber",'Sex','Race','MaritalStatus','age', 'duration','ab109', 'num_attempts')]
neg_model <- neg[,c('BookingNumber','Sex','Race','MaritalStatus','age', 'duration','ab109', 'num_attempts')]

df <- rbind(pos_model,neg_model)

##########################################################
# Clean data
##########################################################
df[df$Sex=="Male",'Sex'] <- "M"
df[df$Sex=="Female",'Sex'] <- "F"
# df[is.na(df$MaritalStatus)|df$MaritalStatus=="W", "MaritalStatus"] <- "U" # IF NA or W, then "Unknown"

df = df[df$MaritalStatus!="W",]
df$MaritalStatus <- ifelse(df$MaritalStatus == "Single", "S", df$MaritalStatus)

df$Race <- case_when(
  df$Race %in% c("A","F","K","V","Asian","C","L","Z","D","J","American Indian or Alaska Native","I") ~ "A",
  df$Race %in% c("Hispanic or Latino", "H")                   ~ "H",
  # df$Race %in% c("American Indian or Alaska Native","I")      ~ "I",
  # df$Race == "O"                                              ~ "O",# Combine other with unknown or na
  # df$Race %in% c("P","G","S","U")                             ~ "P",# Combine other with unknown or na
  df$Race %in% c("White","W")                                 ~ "W",# Combine other with unknown or na
  df$Race == "B"                                              ~ "B",
  TRUE                                                        ~ "U")

df = df[df$Race !='U',]

df <- within(df, {
  Sex <- factor(Sex)
  Race <- factor(Race)
  MaritalStatus <- factor(MaritalStatus)
  AB109 <- factor(ab109)
})

df <- df[df$age < 100,]

##########################################################
# Visualize data
##########################################################

# Plot Number of Attempts
ggplot(df) +
  geom_histogram(aes(num_attempts),col="blue")

table(df$num_attempts)

# Plot Age
ggplot(df) +
  geom_boxplot(aes(num_attempts, age, group=num_attempts),col="blue")

ggplot(df) +
  geom_boxplot(aes(Sex, age),col="blue")

ggplot(df) +
  geom_boxplot(aes(Race, age),col="blue")

#################################################
# Model 1 (Zero-inflated Poisson Regression)
#################################################

model_Ps <- zeroinfl(num_attempts ~ Sex + Race + MaritalStatus + age + AB109, 
                     data = df, dist = "poisson")
summary(model_Ps)


# Check the dispersion
# Overdispersion can produce false significant relationships
# Underdispersion can mask truly significant relationships
res <- resid(model_Ps, type = "pearson")
dispersion_Ps <- sum(res^2)/(nrow(df) - length(coef(model_Ps)))

#################################################
# Model 2 (Zero-inflated Negative Binomial Regression)
#################################################

model_NB <- zeroinfl(num_attempts ~ Sex + Race + MaritalStatus + age,
                   data = df, dist = "negbin")
summary(model_NB)

res <- resid(model_NB, type = "pearson")
dispersion_NB <- sum(res^2)/(nrow(df) - length(coef(model_NB)) - 1)# "-1" for variance parameter in NB


# Compare the two models (likelihood ratio test):
lrtest(model_Ps, model_NB)


####################################################################################
# Group age
####################################################################################
df$age_cat <- case_when(
  df$age < 25     ~ "lt_24",
  df$age < 35     ~ "25-34",
  df$age < 45     ~ "35-44",
  df$age < 55     ~ "45-54",
  TRUE            ~ "gt_55")

df$age_cat <- factor(df$age_cat, levels = c('lt_24',"25-34",'35-44','45-54','gt_55'))
df$duration <- as.numeric(df$duration)

df$Race <- factor(df$Race, levels = c('W', 'H', 'B', 'A'))

####################################################################################
# No Offset
# The same variables in both Logistic and Poisson part of the model
####################################################################################
model_Ps2 <- zeroinfl(num_attempts ~ Sex + Race + MaritalStatus + age_cat + AB109,
                      data = df, dist = "poisson")
summary(model_Ps2)

####################################################################################
# With Offset 
# Different variables in the Logistic and Poisson part of the model
# Removing insignificant variables one at a time
####################################################################################

model_Ps3 <- zeroinfl(num_attempts ~ Race + age_cat, 
                      offset = log(duration+1), data = df, dist = "poisson")
summary(model_Ps3)


model_Ps4 <- zeroinfl(num_attempts ~ Race | Race + age_cat, 
                      offset = log(duration+1), data = df, dist = "poisson")
summary(model_Ps4)

model_Ps4 <- zeroinfl(num_attempts ~ Race + MaritalStatus + age_cat + AB109| Race + age_cat + MaritalStatus, 
                      offset = log(duration+1), data = df, dist = "poisson")
summary(model_Ps4)

model_Ps4 <- zeroinfl(num_attempts ~ Race + age_cat + AB109| Race + age_cat, 
                      offset = log(duration+1), data = df, dist = "poisson")
summary(model_Ps4)

model_Ps4 <- zeroinfl(num_attempts ~ Race + AB109| Race + age_cat, 
                      offset = log(duration+1), data = df, dist = "poisson")
summary(model_Ps4)

#################################################
# Add predictions from notes data

pred_text <- read.csv("other/prediction_for_ZIP3.csv")

df2 <- merge(df, pred_text, by = 'BookingNumber')

model_Ps5 <- zeroinfl(num_attempts ~ Race + Sex + age_cat + MaritalStatus + AB109 + text_pred, 
                      offset = log(duration+1), data = df2, dist = "poisson")
summary(model_Ps5)
