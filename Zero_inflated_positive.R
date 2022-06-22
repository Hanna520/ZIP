##############################################################################
# This script extracts and cleans the data for the positives cases
##############################################################################

source("//ochca.com/hcashares/chs/Data/data_science/rscripts/source/master.R")


library(xml2)
# library(tidyverse)
library(tidyr)
library(stringr)
library(lubridate)
library(dplyr)
library(readxl)

setwd('Z:/rscripts/users/interns/hlu')

##############################################################################
########################### Load Self-harm Data from Excel ###################
##############################################################################
# # Load the self-harm data
suicide <- as.data.frame(read_excel("data/Suicides_combined.xlsx"))
names(suicide) <- c("Inmate_Name", "BookingNumber", "Date_Attempt", "Location",
                    "Method", "Outcome", "Comments", "Entered_by", "ICE_Yes_No")
suicide$BookingNumber = as.character(suicide$BookingNumber)

######
# Some inmates attempted multiple times in the same booking.
suicide <- suicide[order(suicide$BookingNumber, suicide$Date_Attempt),]
suicide <- suicide %>%
  group_by(BookingNumber) %>%
  dplyr::summarise(stamp_attempt = first(Date_Attempt), # Keep the earliest attempt stamp
                   num_attempts = n()) %>% # Count the number of attempts
  dplyr::select(BookingNumber,stamp_attempt,num_attempts)

##############################################################################

load2("iat_book")# collapsed version of the above iat_book_L

rownames(iat_book)= iat_book$BookingNumber
u_pos_InmateIDs =unique(iat_book[suicide$BookingNumber,"InmateID"])

# Find the InmateID for suicide table
suicide_booking_inmate <- iat_book[suicide$BookingNumber,
                                   c("BookingNumber", "InmateID","DateOfBirth",
                                     "Sex","Race","MaritalStatus",'BookingDate','ReleaseDate','EVENT')]

suicide_inmate <- merge(suicide, suicide_booking_inmate, by = "BookingNumber")

# Find the BookingDate based on Attempt Date
suicide_booking_iat = addsbn(suicide_inmate,ciatL2 = iat_book_L)

suicide_booking_iat <- suicide_booking_iat %>%
  group_by(InmateID) %>%
  dplyr::summarise(stamp_attempt = first(stamp_attempt), # Keep the earliest attempt stamp
                   num_attempts = sum(num_attempts),
                   BookingNumber = first(BookingNumber),
                   DateOfBirth = first(DateOfBirth),
                   Sex = first(Sex),
                   Race = first(Race),
                   MaritalStatus = first(MaritalStatus),
                   BookingDate = first(BookingDate),
                   ReleaseDate = last(ReleaseDate),
                   EVENT_loc = first(EVENT)) %>% # Count the number of attempts for each InmateID
  dplyr::select(InmateID,stamp_attempt,num_attempts,BookingNumber,DateOfBirth,Sex,Race,
                MaritalStatus,BookingDate,ReleaseDate,EVENT_loc)


# Calculate age at attempt
suicide_booking_iat$age <- (as.Date(suicide_booking_iat$stamp_attempt) - as.Date(suicide_booking_iat$DateOfBirth))/365
suicide_booking_iat$duration <- (as.Date(suicide_booking_iat$stamp_attempt) - as.Date(suicide_booking_iat$BookingDate))

###########################
suicide_inmate = suicide_inmate[c("BookingNumber", "stamp_attempt", "InmateID")]
positives = merge(suicide_inmate, suicide_booking_iat, by = 'BookingNumber', how = 'left')

positives = positives[c("BookingNumber","InmateID.x","stamp_attempt.x",   
                       "num_attempts", 'Sex',"Race","MaritalStatus",    
                        "age","duration",'EVENT_loc')]
names(positives) = c("BookingNumber" ,"InmateID", 'stamp_attempt',   
                     "num_attempts", 'Sex',"Race","MaritalStatus",    
                     "age","duration",'EVENT_loc')

# find AB109 data
ab109 = .s(INMATE_ADDITIONAL_INFORMATION,InmateID,FieldValue, LastUpdateDateTime, FieldName="BookStatus",FieldValue = c("SPOC","FLSH","REVH","3056","3455"))
pos_ab109_id = ab109$InmateID

positives$ab109 = positives$InmateID %in% pos_ab109_id

write.csv(positives, "data/model_zero_inflated_pos_335_release.csv")
  
#########################################################################


