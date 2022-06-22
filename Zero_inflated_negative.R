##############################################################################
# This script extracts and cleans the data for the negative cases
##############################################################################

source("//ochca.com/hcashares/chs/Data/data_science/rscripts/source/master.R")


# library(tidyverse)
library(tidyr)
library(stringr)
library(lubridate)
library(dplyr)

setwd('Z:/rscripts/users/interns/hlu')


# Read the data for the positive cases
positives <- read.csv("data/model_zero_inflated_pos_335_release.csv")

##############################################################################

load2("iat_book")# collapsed version of the above iat_book_L

# Find the Inmate IDs for the positive cases 
u_pos_InmateIDs <- as.character(unique(positives$InmateID))

# Exclude the positive InmateIDs and those that start with T or X (temporary IDs)
negatives <-  .s(INMATES,ID,BookingNumber,ID.I = u_pos_InmateIDs,ID.K=c("T","X"))
negatives <- negatives[negatives$BookingNumber!="?",]

iat_book2 <- iat_book
rownames(iat_book2) <- iat_book2$BookingNumber

neg_booking <- iat_book2[negatives$BookingNumber,
                                   c("BookingNumber", "InmateID","DateOfBirth",
                                     "Sex","Race","MaritalStatus",'BookingDate','ReleaseDate','EVENT')]
neg_booking <- neg_booking[!is.na(neg_booking$BookingNumber),]

# Remove duplicated InmateIDs
neg_booking <- neg_booking %>%
  group_by(InmateID) %>%
  dplyr::summarise(BookingNumber = first(BookingNumber),
                   DateOfBirth = first(DateOfBirth),
                   Sex = first(Sex),
                   Race = first(Race),
                   MaritalStatus = first(MaritalStatus),
                   BookingDate = first(BookingDate),
                   ReleaseDate = last(ReleaseDate)) %>% 
  dplyr::select(InmateID,BookingNumber,DateOfBirth,Sex,Race,
                MaritalStatus,BookingDate,ReleaseDate)

# Calculate the age at release date (if active, )
neg_booking$age <- (as.Date(neg_booking$ReleaseDate) - as.Date(neg_booking$DateOfBirth))/365

neg_booking$duration <- (as.Date(neg_booking$ReleaseDate) - as.Date(neg_booking$BookingDate))

# Remove the ones that have an error in the DOB column (negative age)
neg_booking <- neg_booking[neg_booking$age > 0,]


# find AB109 data
ab109 = .s(INMATE_ADDITIONAL_INFORMATION,InmateID,FieldValue,LastUpdateDateTime, FieldName="BookStatus",FieldValue = c("SPOC","FLSH","REVH","3056","3455"))
neg_ab109_id = ab109$InmateID

neg_booking$ab109 = neg_booking$InmateID %in% neg_ab109_id

write.csv(neg_booking, "Z:/rscripts/users/interns/hlu/data/model_zero_inflated_neg_release.csv")

#########################################################################
