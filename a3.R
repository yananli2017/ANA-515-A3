library(tidyverse)
library(dplyr)
st <- read_csv("/Users/liyanan/Downloads/StormEvents_details-ftp_v1.0_d2001_c20220425.csv")
st_sub <- select(st,BEGIN_YEARMONTH, EPISODE_ID, STATE, STATE_FIPS, CZ_NAME, CZ_TYPE, CZ_FIPS, EVENT_TYPE)

#3.	Arrange the data by the state name (STATE) (5 points) 
st_sorted <- arrange(st_sub, STATE)

# 4.	Change state and county names to title case (e.g., “New Jersey” instead of “NEW JERSEY”) (5 points) 
library(stringr)
st_sorted$STATE = str_to_title(st_sorted$STATE)
st_sorted$CZ_NAME = str_to_title(st_sorted$CZ_NAME)
head(st_sorted)

# 5.	Limit to the events listed by county FIPS (CZ_TYPE of “C”) and then 
# remove the CZ_TYPE column (5 points) 
st_c <- filter(st_sorted,CZ_TYPE == "C")
stc <- st_c %>% select(-c('CZ_TYPE'))
head(stc)


# 6.	Pad the state and county FIPS with a “0” at the beginning (hint: there’s a function in stringr to do this) and then unite the 
# two columns to make one FIPS column with the new state-county FIPS code (5 points) 
stc$STATE_FIPS <- str_pad(stc$STATE_FIPS, width=3, side = 'left', pad = '0')
stc$CZ_FIPS <- str_pad(stc$CZ_FIPS, width=3, side = 'left', pad = '0')
stc <- stc %>% unite("FIPS", STATE_FIPS,CZ_FIPS, remove = FALSE)

#7.	Change all the column names to lower case (you may want to try the rename_all function for this) (5 points) 
stc <-  rename_all(stc, tolower)

#8.	There is data that comes with base R on U.S. states (data("state")). Use that to create 
# a dataframe with these three columns: state name, area, and region (5 points)
head(data("state"))
us_state_info<-data.frame(state=state.name, region=state.region, area=state.area)
head(us_state_info)

#9.	Create a dataframe with the number of events per state in the year of your birth. Merge in the state information dataframe you just created in step 8. 
# Remove any states that are not in the state information dataframe. (5 points) 
newset <- data.frame(table(stc$state))
newset1 <- rename(newset, c('state' = 'Var1'))
head(newset1)

merged <- merge(x=newset1, y=us_state_info, by.x= 'state',by.y = 'state')
head(merged)


#10.	Create the following plot (10 points): 
library(ggplot2)
st_plot <- ggplot(merged, aes(x=area, y=Freq)) + geom_point(aes(color = region)) +
  labs( x = 'Land area (square miles)',
        y = '# of storm events in 2001')

st_plot
