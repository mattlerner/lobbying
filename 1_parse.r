#############################
# Setup
#############################

working.directory.name <- Sys.getenv("HOME_DIR")

lobbying.directory.name <- "data/lobbying_data"                      # name the folder that contains XML files downloaded from the government.

setwd(working.directory.name)

library(plyr)
source('easyxml.r')
library(ff)

###################################################################################
########## Read in lobbying files; send filings and issues to dataframes ##########
###################################################################################

# source: https://www.senate.gov/legislative/Public_Disclosure/LDA_reports.htm
# see also: https://lobbyingdisclosure.house.gov/ldaguidance.pdf

########## Loop over files in lobbying data directory, bind to one dataframe

all.filings <- c()
all.issues <- c()
quarters <- list.files(lobbying.directory.name)
for (i in 1:length(quarters)) {
  files <- list.files(paste0(lobbying.directory.name,"/",quarters[[i]]))
  for (j in 1:length(files)) {
    frame <- read.file(paste0(lobbying.directory.name,"/",quarters[[i]],"/",files[[j]]))
    
    all.filings <- rbind.fill(all.filings, frame$Filing) 
    all.issues <- rbind.fill(all.issues, frame$Issue)
  }
}

########## Formatting: Create flatfiles of interest, in particular issue/filing amounts

# need to change filing IDs to strings in order to merge
all.filings$xml_key_Filing <- as.character(all.filings$xml_key_Filing)
all.issues$xml_key_Filing <- as.character(all.issues$xml_key_Filing)

# merge filings and issues by filing ID
filing.issues <- data.frame(merge(all.filings, all.issues, by="xml_key_Filing"))

# turn all columns except for Amount into all characters
for (i in 1:length(colnames(filing.issues))) {
  if (colnames(filing.issues)[[i]]  == "Amount") {
    filing.issues[,colnames(filing.issues)[[i]]] <- as.numeric(filing.issues[,colnames(filing.issues)[[i]]])
  } else {
    filing.issues[,colnames(filing.issues)[[i]]] <- as.character(filing.issues[,colnames(filing.issues)[[i]]])
  }
}

# to csv
write.csv(filing.issues, "output/filing_issues.csv")

###################################################################
########## Identify and allocate bill-level expenditures ##########
###################################################################

#######################################
#### House bills analysis function ####
#######################################

# This function takes in a dataframe of all issue objects from
# disclosure XML (by merging the Filing and Issue tables extracted)
# from the XML, separates them out into frames for attributable
# and nonattributable funding, and writes them out
hr.analysis <- function(filing.issues) {
  
  # separate out into those with  specific bill match and those without
  hr.match.indices <- grepl("H\\.* *R\\.* *[0-9]+", filing.issues[,'SpecificIssue'])
  has.hr.matches <- filing.issues[hr.match.indices,]
  no.hr.matches <- filing.issues[!hr.match.indices,]
  
  # a data frame with # columns = max number of regex matches across the dataframe
  # colnames will be X123 etc.
  hr.matches.only <- str_extract_all(has.hr.matches$SpecificIssue, "H\\.* *R\\.* *[0-9]+", simplify=F)
  
  # instead of melting, work iteratively
  hr.matches.melted <- data.frame()
  for (i in 1:length(hr.matches.only)) {
    bill.name <- hr.matches.only[[i]]
    num.bills <- length(bill.name)
    analogous.row <- has.hr.matches[i,]
    duplicated.rows <- has.hr.matches[rep(i, num.bills),]
    result <- cbind(duplicated.rows,bill.name)
    hr.matches.melted <- bind_rows(hr.matches.melted, result)
  }
  
  hr.matches.melted <- hr.matches.melted %>% subset(bill.name != "") %>% mutate(raw.bill.name = bill.name, bill.name = gsub("\\.| ","", bill.name))
  
  # recombine. So now we have the original filing.issues dataset but with the duplicated rows commented about above.
  hr.final <- rbind.fill(no.hr.matches, hr.matches.melted) %>% arrange(ID) %>% group_by(ID) %>% dplyr::mutate(divisor=n())

  # this is without the bill matches but with the divisor
  hr.final.unmelted <- filing.issues %>% arrange(ID) %>% group_by(ID) %>% dplyr::mutate(divisor=n())
    
  # flag support/oppose spending
  support.match.function <- function(x, y) {  return(grepl(paste0("[Ss]upport[A-Za-z]* ", y), x)) }
  oppose.match.function <- function(x, y) {  return(grepl(paste0("[Oo]ppos[A-Za-z]* ", y), x)) }
  
  support.matches <- mapply(support.match.function, hr.final$SpecificIssue, hr.final$raw.bill.name)
  oppose.matches <- mapply(oppose.match.function, hr.final$SpecificIssue, hr.final$raw.bill.name)
  
  # indicator for funding was either supporting or opposing
  hr.final$support.match <- as.numeric(support.matches)
  hr.final$oppose.match <- as.numeric(oppose.matches)
  
  hr.final <- hr.final %>% mutate(issue.amount = Amount / divisor)
  
  return(list(hr.final = hr.final, hr.final.unmelted=hr.final.unmelted))
}


###########################
#### Insheet in chunks ####
###########################

# the data is small enough to be stored in CSV, but too big to be worked on in memory
# We split it up and operate on it in chunks

filing.issues.filename <- "output/filing_issues.csv"
hr.associated.filename <- "output/hr_attributable.csv"
issues.only.filename <- "output/issues_only.csv"

chunksize <- 10000 # this many rows at a time
insheet <- file(description = filing.issues.filename, open="r")
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
  

  results <- hr.analysis(data)
  hr.final <- results$hr.final
  hr.final.bill.associated <- hr.final %>% subset(!is.na(bill.name))
  hr.final.bill.unassociated <- hr.final %>% subset(is.na(bill.name))
  
  write.csv(hr.final.bill.associated, hr.associated.filename, append=append, col.names = colnames(hr.associated)) # this will throw warnings every loop after the first; this is intended
  
  # issues and amounts only
  # First we re-aggregate HR-level amounts back into issues. Ugly, but it works
  # Ten write to CSV
  issues.only <- results$hr.final.unmelted %>% mutate(issue.amount = Amount/divisor) %>% select(Year, Code, SpecificIssue, issue.amount)
  write.csv(issues.only, issues.only.filename, append=append, col.names = colnames(issues.only)) # this will throw warnings every loop after the first; this is intended
  
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

