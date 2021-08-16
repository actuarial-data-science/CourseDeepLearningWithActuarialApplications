# Code to clean the freMTPL2 data
# used for https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3900350

library(OpenML)
library(farff)

WorkersComp <- getOMLDataSet(data.id = 42876)$data

names0 <- c("ClaimNumber","DateTimeOfAccident","DateReported","Age","Gender","MaritalStatus","DependentChildren","DependentsOther","WeeklyPay","PartTimeFullTime","HoursWorkedPerWeek","DaysWorkedPerWeek","ClaimDescription","InitialCaseEstimate","Claim")
names(WorkersComp) <- names0

WorkersComp$Gender <- as.factor(WorkersComp$Gender)
WorkersComp$MaritalStatus <- as.factor(WorkersComp$MaritalStatus)
WorkersComp$PartTimeFullTime <- as.factor(WorkersComp$PartTimeFullTime)
WorkersComp$Claim <- as.integer(WorkersComp$Claim)

str(WorkersComp)    

Sys.setlocale("LC_ALL", "English")
WorkersComp$AccDate <- as.Date(WorkersComp$DateTimeOfAccident)
WorkersComp$AccDay <- as.integer(difftime(as.Date(WorkersComp$AccDate), as.Date("1987-12-28"), units="days"))
WorkersComp$AccYear <- as.integer(substr(WorkersComp$AccDate,1,4))
WorkersComp$AccMonth <- as.integer(substr(WorkersComp$AccDate,6,7))
WorkersComp$AccWeekday <- 1+round(7*((WorkersComp$AccDay)/7-floor((WorkersComp$AccDay)/7)))
WorkersComp$AccTime <- as.integer(substr(WorkersComp$DateTimeOfAccident, start=12, stop=13))
WorkersComp$RepDate <- as.Date(WorkersComp$DateReported)
WorkersComp$RepDay <- as.integer(difftime(as.Date(WorkersComp$RepDate), as.Date("1987-12-28"), units="days"))
WorkersComp$RepYear <- as.integer(substr(WorkersComp$RepDate,1,4))
WorkersComp$RepMonth <- as.integer(substr(WorkersComp$RepDate,6,7))
WorkersComp$RepWeekday <- 1+round(7*((WorkersComp$RepDay)/7-floor((WorkersComp$RepDay)/7)))
WorkersComp$RepDelay <- WorkersComp$RepDay - WorkersComp$AccDay

WorkersComp <- WorkersComp[,-c(2,3)]

str(WorkersComp)    

save(WorkersComp,file="C://Users/juerg/AktuarDataScience/Blockkurs/2021/Exercises/Development/WorkersComp.RData")

