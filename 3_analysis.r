library(XML)
library(scales)
library(tidyverse)
library(ggpubr)

########## Notes ##########
# LDA Guidance
# https://lobbyingdisclosure.house.gov/ldaguidance.pdf

working.directory.name <- Sys.getenv("HOME_DIR")
setwd(working.directory.name)

##########################################################################
#### Attribute amounts spent supporting/opposing a given HR or S bill ####
##########################################################################

########## Function: Calculate bill-level statistics ##########

billstats <- function(frame_) {

  # pare down by year
  hr.final <- frame_ %>% subset(Year == 2018)
  
  # add support.amount and oppose.amount
  hr.final[hr.final$support.match == 1, 'support.amount'] <- hr.final[hr.final$support.match == 1, 'issue.amount']
  hr.final[hr.final$oppose.match == 1, 'oppose.amount'] <- hr.final[hr.final$oppose.match == 1, 'issue.amount']
  
  # support/oppose gap
  hr.summary <- hr.final %>% group_by(bill.name) %>% dplyr::summarise(total.spent = sum(issue.amount, na.rm = TRUE), total.supporting = sum(support.amount, na.rm = TRUE), total.opposing = sum(oppose.amount, na.rm = TRUE))
  hr.summary$support.oppose.gap <- hr.summary$total.supporting - hr.summary$total.opposing
  
  return(hr.summary)
  
}

########## Amount calculations: House ##########

# we break up amount such that if I = # of issues and B = # of bills, with A = amount of funding
# then each issue, including bill-associated ones, gets A/(I + B)

house.associated.bills.file <- "output/hr_attributable.csv"
temp.bill.summary <- "temp/temp_bill_summary.csv"

chunksize <- 10000 # this many rows at a time
insheet <- file(description = house.associated.bills.file, open="r")
data <- read.csv(insheet, nrows=chunksize, header=TRUE)

colnames <- colnames(data)
append = FALSE

# please note that this function produces a ~20gb CSV
repeat {
  
  # if the data is empty, break
  if (nrow(data) == 0) {
    break
  }
  
  #################################
  ###### Begin process data #######
  #################################
  
  stats <- billstats(data)
  write.csv(stats, temp.bill.summary, append=append, col.names = colnames(stats)) # this will throw warnings every loop after the first; this is intended
  append <- TRUE
  
  ###############################
  ###### End process data #######
  ########################### ###
  
  # if we've reached the end, break
  if (nrow(data) != chunksize) {
    break
  }
  
  # read in the next chunk
  data <- read.csv(insheet, nrows=chunksize, col.names=colnames)
  
}

close(insheet)

###############################################
###### Combine temp file and graph stats ######
###############################################

temp.bills <- read.csv(temp.bill.summary, header=T)
bills.allocated.final <- temp.bills %>% group_by(bill.name) %>% dplyr::summarise(total.spent = sum(total.spent), total.supporting=sum(total.supporting), total.opposing=sum(total.opposing), support.oppose.gap=sum(support.oppose.gap))

write.csv(bills.allocated.final, "output/bill_level_statistics.csv")

#############################################
#### Combine with bills voted on in 2018 ####
#############################################

# import bills
bills.allocated.final <- read.csv("output/bill_level_statistics.csv")
bill.passages <- read.csv("output/bill_passages.csv") %>% select(year, vote_result, bill_number, yea_count, nay_count) %>% subset(year >= 2018)
unique.bills <- read.csv("output/unique_bills.csv")

########## Statistics about funding ##########

# What is the per-bill spending distribution?
bill.merge <- merge(bills.allocated.final, unique.bills, by.x="bill.name", by.y="bill_number", all=TRUE)
bill.merge <- bill.merge %>% subset(grepl("HR[0-9]+",bill.name)) # remove e.g. continuing resolutions
bill.merge[is.na(bill.merge$total.spent), 'total.spent'] <- 0

mean(bill.merge$total.spent == 0) # so roughly 99% of the bills that came up for a vote saw some lobbying
nrow(bill.merge) # how many bills were voted on?


# bills that made it to the floor in 2018
passages.merge <- merge(bills.allocated.final, bill.passages, by.x="bill.name", by.y="bill_number", all=FALSE) # the inner merge leaves us only with HRs


#### PLOT: Density of per-bill spending

# What is the per-issue spending distribution?
issue.amounts <- read.csv("output/issues_only.csv", colClasses=c("NULL","NULL","NULL","NULL","NULL",NA)) # reading in only one column to save memory
issue.amounts.only <- as.numeric(issue.amounts$issue.amount)

# As of first vote, is there a relationship between voting characteristics and lobbying expenditure?

# total spending versus vote difference
bill.firstvotes <- read.csv("output/bill_firstvotes.csv") %>% subset(year == 2018)
firstvote.merge <- merge(bills.allocated.final, bill.firstvotes, by.x="bill.name", by.y="bill_number", all=FALSE)
firstvote.merge$votegap = abs(firstvote.merge$yea_count - firstvote.merge$nay_count)

# plot against nominate spread
plot(firstvote.merge$votegap, firstvote.merge$total.spent)


########## Viz ##########

margins <- unit(c(1,1,1,1),"cm")

# spending density: bills that made it to the floor
rollcall.plot <- ggplot(passages.merge) + geom_density(aes(x=total.spent), fill="green", alpha=0.5) + scale_x_continuous(trans="log10", name="\nDollars", labels=comma) + theme_minimal() + ggtitle("Amount spent per roll-call vote\n(estimated)") + ylab("Density\n") + geom_vline(xintercept=median(passages.merge$total.spent), linetype="dotted") + theme(text=element_text(size=12,  family="Helvetica"), axis.text.x = element_text(size=8)) + theme(plot.margin=margins, panel.background = element_blank(), panel.grid = element_line(colour="#eeeeee"))

# spending density: all bills with lobbying
allbills.plot <- ggplot(bills.allocated.final) + geom_density(aes(x=total.spent), fill="red", alpha=0.5) + scale_x_continuous(trans="log10", name="\nDollars", labels=comma) + theme_minimal() + ggtitle("Amount spent per bill\n(estimated)") + ylab("Density\n") + geom_vline(xintercept=median(bills.allocated.final$total.spent), linetype="dotted") + theme(plot.margin=margins, text=element_text(size=12,  family="Helvetica"), axis.text.x = element_text(size=8))

# spending density: all issues
#### PLOT: Density of per-issue spending
issue.plot <- ggplot(issue.amounts) + geom_density(aes(x=issue.amount), fill="blue", alpha=0.5, bw=0.08) + scale_x_continuous(trans="log10", name="\nDollars", labels=comma) + theme_minimal() + ggtitle("Amount spent per issue-lobbyist-quarter\n(estimated)") + ylab("Density\n") + geom_vline(xintercept=median(issue.amounts$issue.amount, na.rm=TRUE), linetype="dotted") + theme(plot.margin=margins, text=element_text(size=12,  family="Helvetica"), axis.text.x = element_text(size=8))# + annotate("text", x = median(issue.amounts$issue.amount, na.rm=TRUE, size=8)+260, y=1, label= paste("Median:",round(median(issue.amounts$issue.amount, na.rm=TRUE))))

full.plot <- ggarrange(rollcall.plot, allbills.plot, issue.plot, ncol=2, nrow=2)

# output all together
ggsave("spending_plots.png", full.plot, width=9, height=9)
