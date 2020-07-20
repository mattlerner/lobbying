library(tidyverse)

working.directory.name <- Sys.getenv("HOME_DIR")
setwd(working.directory.name)

# background https://www.propublica.org/nerds/lobbying-registration-database-reporting-recipe
# disclosure search http://disclosures.house.gov/ld/ldsearch.aspx

# focusing on 115th congress

# Notes
# Party codes https://voteview.com/articles/data_help_parties

###### Insheet

# SOURCE https://voteview.com/data
rollcalls <- read.csv(paste0(working.directory.name,"/","data/bill_data/HSall_rollcalls.csv"))

###### Some formatting
rollcalls$date.clean <- strptime(as.character(rollcalls$date), format="%Y-%m-%d")
rollcalls$year <- format(rollcalls$date.clean, '%Y')

###### How far does a given bill make it?
rollcalls.post.2018 <- rollcalls %>% subset(year >= 2018)

# Unique bills
unique.bills.post.2018 <- rollcalls.post.2018 %>% select(bill_number) %>% unique()
write.csv(unique.bills.post.2018, "output/unique_bills.csv", row.names=FALSE)

###### Get only the latest vote regarding passage of each bill
# This implicitly restricts the dataset to all bills since 1990; before that, vote_question is blank
rollcalls <- rollcalls %>% subset(chamber == "House" & vote_question == "On Passage") %>% group_by(congress, year, bill_number) %>% dplyr::mutate(latest = (date.clean == max(date.clean)), earliest=(date.clean == min(date.clean)))
bill.passages <- rollcalls %>% subset(latest == TRUE)
bill.firstvotes <- rollcalls %>% subset(earliest == TRUE)
write.csv(bill.passages, "output/bill_passages.csv", row.names=FALSE)
write.csv(bill.firstvotes, "output/bill_firstvotes.csv", row.names=FALSE)

