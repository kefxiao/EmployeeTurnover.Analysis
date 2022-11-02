library(readxl)
library(janitor)
library(dplyr)

rm(list = ls())
EEdata2020 <- read_excel("Retention (DVDC) for UIowa Project_Master.xlsx", 
                        sheet = "EE Data of 1.1.2020")
EEdata2020 = clean_names(EEdata2020)

EEdata2020 = EEdata2020[which(EEdata2020$employment_status=="Yes"),]
EEUniverse = unique(EEdata2020$employee_id)
EEdata2020$Year = 2020
EEdata2020$hire_date = as.Date(EEdata2020$hire_date, format = "Y%-m%-d") 

EEdata2021 <- read_excel("Retention (DVDC) for UIowa Project_Master.xlsx", 
                        sheet = "EE Data as of 1.1.2021")
EEdata2021 = clean_names(EEdata2021)
EEdata2021$Year = 2021
EEdata2021$hire_date = as.Date(EEdata2021$hire_date, format = "Y%-m%-d")
`%!in%`<- Negate(`%in%`)
EEdata2021 = EEdata2021[which(EEdata2021$employee_id %in% EEUniverse | EEdata2021$hire_date > "2020-01-01"),]
EEUniverse = unique(EEdata2021$employee_id)


EEdata2022 <- read_excel("Retention (DVDC) for UIowa Project_Master.xlsx", 
                        sheet = "EE Data as of 1.1.2022")
EEdata2022 = clean_names(EEdata2022)
EEdata2022$Year = 2022
EEdata2022$hire_date = as.Date(EEdata2022$hire_date, format = "Y%-m%-d") 
EEdata2022 = EEdata2022[which(EEdata2022$employee_id %in% EEUniverse | EEdata2022$hire_date > "2021-01-01"),]
EEUniverse = unique(EEdata2022$employee_id)

Uptodate <- read_excel("Retention (DVDC) for UIowa Project_Master.xlsx", 
                        sheet = "EE Data as of 9.26.2022")
Uptodate = clean_names(Uptodate)
Uptodate$Year = 2023
Uptodate$hire_date = as.Date(Uptodate$hire_date, format = "Y%-m%-d") 
Uptodate = Uptodate[which(Uptodate$employee_id %in% EEUniverse | Uptodate$hire_date > "2022-01-01"),]
EEUniverse = unique(Uptodate$employee_id)

Retention = rbind(EEdata2020,EEdata2021)
Retention = rbind(Retention,EEdata2022)
Retention = rbind(Retention,Uptodate)


rm(EEdata2020,EEdata2021, EEdata2022, Uptodate)


Changes <- read_excel("Comp Changes, Job Changes, Terminations 2020 - Present 2022-09-26 10_46 CDT.xlsx")
Changes = clean_names(Changes)
Changes = Changes[which(Changes$employee_id %in% EEUniverse),]

EE_summary = Changes %>% group_by(employee_id) %>% summarise(count = n())

#############get a list of True Terminations
EE_Terms = Changes %>% filter(Changes$business_process_type == "Termination")
RIF = grep("Workforce Reduction", EE_Terms$business_process_reason)
EE_Terms = EE_Terms[-RIF,]
Conver = grep("Conversion", EE_Terms$business_process_reason_category)
EE_Terms = EE_Terms[-Conver,]
PE = grep("Position Elimination", EE_Terms$business_process_reason)
EE_Terms = EE_Terms[-PE,]
GM = grep("Gross Misconduct", EE_Terms$business_process_reason)
EE_Terms = EE_Terms[-GM,]
Retire = grep("Retirement", EE_Terms$business_process_reason)
EE_Terms = EE_Terms[-Retire,]
EoA = grep("End of Assignment", EE_Terms$business_process_reason)
EE_Terms = EE_Terms[-EoA,]
PJP = grep("Poor Job Performance", EE_Terms$business_process_reason)
EE_Terms = EE_Terms[-PJP,]
Death = grep("Death", EE_Terms$business_process_reason)
EE_Terms = EE_Terms[-Death,]
EA = grep("Excessive Absenteeism", EE_Terms$business_process_reason)
EE_Terms = EE_Terms[-EA,]
PV = grep("Policy Violation", EE_Terms$business_process_reason)
EE_Terms = EE_Terms[-PV,]

Terms = EE_Terms$employee_id

rm("Conver", "Death", "EA", "EoA", "GM", "PE", "PJP", "PV", "Retire", "RIF")
####################
##Remove these employees from Changes
EE_IVTerms = Changes %>% filter(Changes$business_process_type == "Termination")
RIF = grep("Workforce Reduction", EE_IVTerms$business_process_reason)
REMOVE = RIF
#Conver = grep("Conversion", EE_IVTerms$business_process_reason_category)
#REMOVE = append(REMOVE,Conver)
PE = grep("Position Elimination", EE_IVTerms$business_process_reason)
REMOVE = append(REMOVE,PE)
GM = grep("Gross Misconduct", EE_IVTerms$business_process_reason)
REMOVE = append(REMOVE,GM)
Retire = grep("Retirement", EE_IVTerms$business_process_reason)
REMOVE = append(REMOVE,Retire)
EoA = grep("End of Assignment", EE_IVTerms$business_process_reason)
REMOVE = append(REMOVE,EoA)
PJP = grep("Poor Job Performance", EE_IVTerms$business_process_reason)
REMOVE = append(REMOVE,PJP)
Death = grep("Death", EE_IVTerms$business_process_reason)
REMOVE = append(REMOVE,Death)
EA = grep("Excessive Absenteeism", EE_IVTerms$business_process_reason)
REMOVE = append(REMOVE,EA)
PV = grep("Policy Violation", EE_IVTerms$business_process_reason)
REMOVE = append(REMOVE,PV)
rm("Death", "EA", "EoA", "GM", "PE", "PJP", "PV", "Retire", "RIF")


EE_IVTerms = EE_IVTerms[REMOVE,]

EEtoRemove = unique(EE_IVTerms$employee_id)

`%notin%` <- Negate(`%in%`)
Changes = Changes[which(Changes$employee_id %notin% EEtoRemove),]

Changes = Changes %>% arrange(employee_id, date_and_time_initiated)

Retention = Retention[which(Retention$employee_id %notin% EEtoRemove),]

Retention = Retention %>% arrange(employee_id, Year)

###########################
#Manager Metrics
MgrMet = Changes %>% group_by(employee_id, manager) %>% summarise()

MgrMet = MgrMet %>% group_by(manager) %>% summarise(NumEE = n())

MgrTerm = MgrMet %>% left_join(EE_Terms, by = "manager") 
MgrTerm$Term = 1
MgrTerm$Term[which(is.na(MgrTerm$effective_date))]= 0
MgrTerm = MgrTerm %>% filter(Term==1) %>% group_by(manager) %>% summarise(terms = sum(Term))

MgrMet = MgrMet %>% left_join(MgrTerm, by = "manager") 
MgrMet$terms[which(is.na(MgrMet$terms))]= 0
MgrMet$TO = 0
MgrMet$TO[which(MgrMet$terms!=0)] = MgrMet$terms[which(MgrMet$terms!=0)]/MgrMet$NumEE[which(MgrMet$terms!=0)]

######################
#Employeemetrics

EEMet = Changes %>% group_by(employee_id, manager) %>% summarise()

EEMet = EEMet %>% group_by(employee_id) %>% summarise(NumMgr = n())

EEInc = Changes  %>% group_by(employee_id) %>%
  summarise(AvgInc = mean(base_pay_increase_percent), num_inc = n(), #Average Increase in base pay percent
            first_pay_amt_a = first(total_base_pay_amount_after),   
            last_pay_amt_a = last(total_base_pay_amount_after),
            first_pay_amt_b = first(total_base_pay_amount_before),
            last_pay_amt_b = last(total_base_pay_amount_before))


Changes = clean_names(Changes)
library(stringr)


One = as.data.frame(str_extract(Changes$business_process_reason_category, "\\w+"))
names(One)[1] = "ReasonOne"
One$ReasonOne[is.na(One$ReasonOne)] = ""
Two = as.data.frame(str_extract(Changes$business_process_reason_category, "\\w+\\ \\w+"))
names(Two)[1] = "ReasonTwo"
Two$ReasonTwo[is.na(Two$ReasonTwo)] = ""
Three_a = as.data.frame(str_extract(Changes$business_process_reason_category, "\\>\\s\\w+"))
names(Three_a)[1] = "ReasonThree_a"
Three_a$ReasonThree_a[!is.na(Three_a$ReasonThree_a)] = substr(Three_a$ReasonThree_a[!is.na(Three_a$ReasonThree_a)],
                                                              3,
                                                              nchar(Three_a$ReasonThree_a[!is.na(Three_a$ReasonThree_a)]))
Three_a$ReasonThree_a[is.na(Three_a$ReasonThree_a)] = ""


Three_b = as.data.frame(str_extract(Changes$business_process_reason_category, "\\>\\ \\w+\\ \\w+\\ \\w+"))
names(Three_b)[1] = "ReasonThree_b"
Three_b$ReasonThree_b[!is.na(Three_b$ReasonThree_b)] = substr(Three_b$ReasonThree_b[!is.na(Three_b$ReasonThree_b)],
                                                              3,
                                                              nchar(Three_b$ReasonThree_b[!is.na(Three_b$ReasonThree_b)]))

Three_b$ReasonThree_b[is.na(Three_b$ReasonThree_b)] = ""


Changes = cbind(Changes, One)
Changes = cbind(Changes, Two)
Changes = cbind(Changes, Three_a)
Changes = cbind(Changes, Three_b)


Changes$extract_reason = ""

for (i in 1:nrow(Changes)){
  print(i)
  if (Changes$ReasonThree_b[i]!=""){
    print("reasonThree_b")
    Changes$extract_reason[i] = Changes$ReasonThree_b[i]
  }
  if (Changes$ReasonThree_a[i]!="" & Changes$extract_reason[i]==""){
    print("reasonThree_a")
    Changes$extract_reason[i] = Changes$ReasonThree_a[i]
  }
  if (Changes$ReasonTwo[i]!="" & Changes$extract_reason[i]==""){
    Changes$extract_reason[i] = Changes$ReasonTwo[i]
    print("reasonTwo")
  }
  if (Changes$extract_reason[i]==""){
    Changes$extract_reason[i] = Changes$ReasonOne[i]
    print("reasonOne")
  }  
}

#CLean up a few that had no reasons
Changes$extract_reason[Changes$extract_reason==""] = Changes$business_process_type[Changes$extract_reason==""]



Changes$ReasonOne = NULL
Changes$ReasonTwo = NULL
Changes$ReasonThree_a = NULL
Changes$ReasonThree_b = NULL

rm("One", "Two", "Three_a", "Three_b")


library(tidyr)
Changes$Test = 1
Changes = Changes %>% pivot_wider(names_from = extract_reason,
                                          values_from = Test,
                                          values_fill = 0)
Changes = clean_names(Changes)

Changes_sum = Changes %>% group_by(employee_id) %>% summarise(conversion = sum(conversion),
                                                              variable_compensation_change = sum(variable_compensation_change),
                                                              transfer = sum(transfer),                        
                                                              base_pay_change = sum(base_pay_change),                              
                                                              promotion = sum(promotion),
                                                              lateral_move = sum(lateral_move),                                 
                                                              data_changes = sum(data_changes),
                                                              demotion = sum(demotion),                                     
                                                              request_compensation_change = sum(request_compensation_change)              )


###############################

EEMet = EEMet %>% left_join(EEInc, by = "employee_id")
EEMet = EEMet %>% left_join(Changes_sum, by = "employee_id")
EEMet$Term = ""

EEMet$Term[which(EEMet$employee_id %in% Terms)] = 1
EEMet$Term[which(EEMet$employee_id %notin% Terms)] = 0

EEMet$Term = as.integer(EEMet$Term)
sum(EEMet$Term)

write.csv(EEMet, "EEMetrics_v2.csv", row.names = FALSE)


Retention_df <- subset(Retention, select = -c(employment_status, race_ethnicity_visual_survey_multiple_allowed,
                                                 hispanic_or_latino_visual_survey, hispanic_or_latino_y_n,
                                                 job_level_primary_position, job_history_skills_and_experience,
                                                 cost_center_3, manager,organization,position,job_profile_change,
                                                 job_family,job_profile,job_family_group,original_hire_date,
                                                 continuous_service_date, time_in_position,union_start_date,
                                                 union_membership,union_seniority_date,currency_49,currency_51,
                                              total_base_pay_reporting_currency,total_base_pay_employee_currency,
                                              period_salary_plan,employee_type_5,exempt,comp_grade_profile_job_profile,
                                              comp_grade_job_profile,bonus_plan_effective_date,sales_incentive_plan,
                                              cost_center_66,position_id))

#View(Retention_df)


Cleaned_Retention_df <- subset(Retention_df,(Retention_df$work_address_country == "United States of America" &
                                               Retention_df$home_address_country == "United States of America"))
#View(Cleaned_Retention_df)


## move bonus related to a different file 
TurnOver = unique(left_join(EEMet, Cleaned_Retention_df, by="employee_id")) 
Bonus_plan_df <- subset(TurnOver,select = c(allowance_plan,merit_plan,bonus_plan,pay_group))

## drop bonus related columns and create new file 
TurnOver_bonus_out <- subset(TurnOver,select = -c(allowance_plan,merit_plan,bonus_plan,pay_group))

##combine salary and hourly column  into one column 
TurnOver_bonus_out $salary_plan_new <- paste(TurnOver_bonus_out $salary_plan, TurnOver_bonus_out $hourly_plan)
TurnOver_bonus_out $salary_plan <- NULL
TurnOver_bonus_out $hourly_plan <- NULL

##Clean race ethnicity 
TurnOver_bonus_out$race_ethnicity <- gsub("\\s*\\([^\\)]+\\)", "",TurnOver_bonus_out$race_ethnicity)


##clean work location: two new columns - city, state 
TurnOver_bonus_out$work_state <- sub("^.*?,", "", TurnOver_bonus_out$work_location)
TurnOver_bonus_out$work_state_new <- sub("^.*?,", "", TurnOver_bonus_out$work_state)

##remove one country column and make country column all U.S.
TurnOver_bonus_out$home_address_country <- NULL
TurnOver_bonus_out$work_address_country <- 'United States of America'

##extract numeric values from salary_plan
library(stringr) 
TurnOver_bonus_out$salary_plan_new <- str_extract_all(TurnOver_bonus_out$salary_plan_new,"\\(?[0-9,.]+\\)?")
new <-unnest(TurnOver_bonus_out,cols=salary_plan_new )
new[48] <- NULL
new$salary <- NULL
value <- new$salary_plan_new[1]
new <- new %>% filter(salary_plan_new!=value)
new <- select(new,c(employee_id,Year,salary_plan_new))
new$salary_plan_new <- as.numeric(gsub(",", "", as.character(new$salary_plan_new)))
TurnOver_bonus_out <- TurnOver_bonus_out %>% left_join(new,by=c('employee_id','Year'))
TurnOver_bonus_out$salary_plan_new.x <- NULL

##cost_center_table, max = 3
turnover <- TurnOver_bonus_out
cost_center_table <- turnover %>% group_by(employee_id) %>% summarise(count_cost_center = n_distinct(cost_center_id,na.rm = TRUE)) 
cost_center_table [order(cost_center_table $count_cost_center, decreasing = TRUE), ] 
cost_center_table[which(cost_center_table $count_cost_center >1),]

##Set a limit so that employees salaires are <= 125,000
##TurnOver_bonus_out <- subset(TurnOver_bonus_out, (TurnOver_bonus_out$salary_plan_new.y <= "150,000.00"))

##capturing cost_center_id changes

cost_center <- select(turnover,employee_id,cost_center_id)
cost_center <- unique(cost_center,na.rm=TRUE)
cost_center <- na.omit(cost_center)

library(tidyverse)
cost_center_convert <- cost_center  %>%
  group_by(employee_id) %>%
  mutate(n = paste0("cost_center_id", 1:n())) %>%
  spread(n, cost_center_id)

cost_center_changes <- left_join(cost_center_convert, cost_center_table, by = 'employee_id')
TurnOver_bonus_out <- TurnOver_bonus_out %>% left_join(cost_center_changes,by = "employee_id")

##ccompany table, max = 3
turnover <- TurnOver_bonus_out
company_table <- turnover %>% group_by(employee_id) %>% summarise(count_company = n_distinct(company,na.rm = TRUE)) 
company_table [order(company_table $count_company, decreasing = TRUE), ] 
company_table[which(company_table $count_company >1),]

##capturing company changes
company<- select(turnover,employee_id,company)
company <- unique(company,na.rm=TRUE)
company <- na.omit(company)

library(tidyverse)
company_convert <- company %>%
  group_by(employee_id) %>%
  mutate(n = paste0("company", 1:n())) %>%
  spread(n, company)

company_changes <- left_join(company_convert, company_table, by = 'employee_id')
turnover <- turnover %>% left_join(company_changes,by="employee_id")

##cleanning termination date
term_date <- select(turnover,employee_id,termination_date)
term_date <- unique(term_date,na.rm=TRUE)
term_date <- na.omit(term_date)
count <- count(term_date,employee_id)
ids <- count[which(count$n>1),]
mpre_than_1_term <- term_date[term_date$employee_id %in% ids$employee_id, ]

termdate_convert <- term_date %>%
  group_by(employee_id) %>%
  mutate(n = paste0("term_date", 1:n())) %>%
  spread(n, termination_date)

termination_times <- left_join(termdate_convert, count, by = 'employee_id')
turnover <- turnover %>% left_join(termination_times,by="employee_id")

##cleaning length_of_service_worker
length_of_service <- select(turnover,employee_id,length_of_service_worker)
max_service <- length_of_service %>% group_by(employee_id) %>% summarise(cmax = max(length_of_service_worker,na.rm = TRUE)) 
turnover <- turnover %>% left_join(max_service,by="employee_id")

## Manager id
manager_ever_been <- select(turnover,employee_id,is_manager,Year)

manager_ever_been <- manager_ever_been %>% group_by(employee_id) %>%
  mutate(yes_ratio = sum(is_manager =="Yes",na.rm=TRUE)/n()) %>% unique()

uptodate_manager <- select(filter(manager_ever_been, Year == 2023), is_manager)
manager_ever_been <- manager_ever_been %>% left_join(uptodate_manager,by="employee_id")
manager_ever_been$is_manager.x <- NULL
manager_ever_been$Year <- NULL
manager_ever_been <- unique(manager_ever_been)
manager_ever_been$is_manager.y[is.na(manager_ever_been$is_manager.y)] <- 0

manager_cal <- function(x,y) { 
  if(x == 1) z <- "Always a manager"
  if(x < 1 & y == 'Yes') z <- "Became/Still a Manager"
  if(x < 1 & y != 'Yes') z <- "Was a manager"
  return(z)
}

manager_ever_been$manager_history <- mapply(manager_cal, manager_ever_been$yes_ratio, manager_ever_been$is_manager.y)
manager_ever_been$yes_ratio <- NULL
manager_ever_been$is_manager.y <- NULL

turnover <- turnover %>% left_join(manager_ever_been,by="employee_id")
turnover$is_manager <- NULL

##drop out some columns from turnover
turnover_before <- select(turnover,-c("cost_center_id","company","termination_date","length_of_service_worker","Year","fte_percent"))
turnover_before <- unique(turnover_before) 

## Clean salary 
salary <- select(turnover,employee_id,salary_plan_new.y)
salary <- unique(salary,na.rm=TRUE)
salary <- na.omit(salary)

salary_convert <- salary %>%
  group_by(employee_id) %>%
  mutate(n = paste0("salary", 1:n())) %>%
  spread(n, salary_plan_new.y)

salary_count <- count(salary,employee_id)

salary_times <- left_join(salary_convert, salary_count, by = 'employee_id')

## delete salary_plan_new and incorporate changes of salary
turnover_before$salary_plan_new.y <- NULL
turnover_before <- unique(turnover_before) 
turnover_before <- turnover_before %>% left_join(salary_times,by="employee_id")

## delete work_location 
copy_work_location <- turnover_before
turnover_before$work_location <- NULL
turnover_before <- unique(turnover_before) 

##Convert direct reports 
direct_reports <- select(turnover_before,employee_id,number_of_direct_reports_as_of_current_moment_will_not_reflect_future_dated_changes)
direct_reports_table <- direct_reports %>% group_by(employee_id) %>% 
  summarise(num_of_direct_reports_uptodate = max(number_of_direct_reports_as_of_current_moment_will_not_reflect_future_dated_changes)) %>%unique()
turnover_before$number_of_direct_reports_as_of_current_moment_will_not_reflect_future_dated_changes <- NULL

turnover_before <- turnover_before %>% left_join(direct_reports_table,by="employee_id") %>% unique()

##clean business title 
business_title <- select(turnover_before,employee_id,business_title)
business_title <- unique(business_title,na.rm=TRUE)
business_title <- na.omit(business_title)
title_convert <- business_title %>%
  group_by(employee_id) %>%
  mutate(n = paste0("business_title", 1:n())) %>%
  spread(n, business_title)

title_count <- count(business_title,employee_id)
title_changes <- left_join(title_convert,title_count,by="employee_id")
colnames(title_changes)[6] <- "count_of_title"

turnover_before$business_title <- NULL
turnover_before <- unique(turnover_before) 
turnover_before <- turnover_before %>% left_join(title_changes,by="employee_id") %>% unique()

##Clean manager id
manager_id <- select(turnover_before,employee_id,manager_id)
manager_id <- unique(manager_id,na.rm=TRUE)
manager_id <- na.omit(manager_id)
manager_id_convert <- manager_id %>%
  group_by(employee_id) %>%
  mutate(n = paste0("manager_id", 1:n())) %>%
  spread(n, manager_id)

turnover_before$manager_id <- NULL
turnover_before <- unique(turnover_before) 
turnover_before <- turnover_before %>% left_join(manager_id_convert,by="employee_id") %>% unique()

##clean group 
group <- select(turnover_before,employee_id,group)
group  <- unique(group,na.rm=TRUE)
group  <- na.omit(group)
group_convert <- group  %>%
  group_by(employee_id) %>%
  mutate(n = paste0("group", 1:n())) %>%
  spread(n, group)

turnover_before$group <- NULL
turnover_before <- unique(turnover_before) 
turnover_before <- turnover_before %>% left_join(group_convert,by="employee_id") %>% unique()

##clean tables 
rm(business_title,company,company_changes,company_convert,company_table,cost_center,cost_center_changes,
   cost_center_table,count,direct_reports,direct_reports_table,group,group_convert,ids,length_of_service,manager_id,
   manager_id_convert,max_service,mpre_than_1_term,new,salary,salary_convert,salary_count,salary_times,term_date,
   termdate_convert,termination_times,title_convert,title_changes,title_count,TurnOver,TurnOver_bonus_out,
   uptodate_manager)

##clean work state
turnover_before$work_state <- NULL

## Clean work state new
state <- select(turnover_before,employee_id,work_state_new)
state  <- unique(state,na.rm=TRUE)
state  <- na.omit(state)
state_convert <- state  %>%
  group_by(employee_id) %>%
  mutate(n = paste0("state", 1:n())) %>%
  spread(n, work_state_new)

turnover_before$work_state_new <- NULL
turnover_before <- unique(turnover_before) 
turnover_before <- turnover_before %>% left_join(state_convert,by="employee_id") %>% unique()
turnover_before$state4 <- NULL

## clean management level and union status
turnover_before$management_level <-sub("(^[^-]+)-.*", "\\1", turnover_before$management_level)
turnover_before$union_membership_type <- NULL
turnover_before$scheduled_weekly_hours <- NULL

##Rearrange management level 
ma_level <- select(turnover_before,employee_id,management_level)
ma_level  <- unique(ma_level,na.rm=TRUE)
ma_level  <- na.omit(ma_level)
ma_level_convert <- ma_level  %>%
  group_by(employee_id) %>%
  mutate(n = paste0("ma_level", 1:n())) %>%
  spread(n, management_level)

turnover_before$management_level<- NULL
turnover_before <- unique(turnover_before) 
turnover_before <- turnover_before %>% left_join(ma_level_convert,by="employee_id") %>% unique()

ma_level <-cbind(turnover_before$ma_level1, turnover_before$ma_level2, turnover_before$ma_level3)
turnover_before$manage_level<-apply(ma_level,1,function(x){paste(x[!(is.na(x)|x==""|x=="NA")],collapse=";")})
turnover_before <- select(turnover_before,-c(ma_level1,ma_level2,ma_level3))

##clean pay frequency
turnover_before$pay_frequency[which(turnover_before$pay_frequency=="Semimonthly")] <- "Biweekly"

## location work hours
work_hours <- select(turnover_before,employee_id,location_weekly_hours)
max_hours <- work_hours %>% group_by(employee_id) %>% summarise(work_hours = max(location_weekly_hours,na.rm = TRUE)) 
turnover_before <- turnover_before %>% left_join(max_hours,by="employee_id")
turnover_before$location_weekly_hours <- NULL

##Rearrange hourly or salary 
pay_type <- select(turnover_before,employee_id,pay_rate_type_job_profile)
pay_type  <- unique(pay_type,na.rm=TRUE)
pay_type  <- na.omit(pay_type)
pay_type_convert <- pay_type  %>%
  group_by(employee_id) %>%
  mutate(n = paste0("pay_type", 1:n())) %>%
  spread(n, pay_rate_type_job_profile)

turnover_before$pay_rate_type_job_profile<- NULL
turnover_before <- unique(turnover_before) 
turnover_before <- turnover_before %>% left_join(pay_type_convert,by="employee_id") %>% unique()

pay_type_combine<-cbind(turnover_before$pay_type1, turnover_before$pay_type2)
turnover_before$pay_type<-apply(pay_type_combine,1,function(x){paste(x[!(is.na(x)|x==""|x=="NA")],collapse=" to ")})
turnover_before <- select(turnover_before,-c(pay_type1,pay_type2))

##Rearrange pay frequency
pay_freq <- select(turnover_before,employee_id,pay_frequency)
pay_freq <- unique(pay_freq,na.rm=TRUE)
pay_freq  <- na.omit(pay_freq)
pay_freq_convert <- pay_freq  %>%
  group_by(employee_id) %>%
  mutate(n = paste0("pay_freq", 1:n())) %>%
  spread(n, pay_frequency)

turnover_before$pay_frequency<- NULL
turnover_before <- unique(turnover_before) 
turnover_before <- turnover_before %>% left_join(pay_freq_convert,by="employee_id") %>% unique()

pay_freq_combine<-cbind(turnover_before$pay_freq1, turnover_before$pay_freq2)
turnover_before$pay_freq<-apply(pay_freq_combine,1,function(x){paste(x[!(is.na(x)|x==""|x=="NA")],collapse=" to ")})
turnover_before <- select(turnover_before,-c(pay_freq1,pay_freq2))

##Rearrange full time or part time
time_type <- select(turnover_before,employee_id,time_type)
time_type <- unique(time_type,na.rm=TRUE)
time_type  <- na.omit(time_type)
time_type_convert <- time_type  %>%
  group_by(employee_id) %>%
  mutate(n = paste0("time_type", 1:n())) %>%
  spread(n, time_type)

turnover_before$time_type<- NULL
turnover_before <- unique(turnover_before) 
turnover_before <- turnover_before %>% left_join(time_type_convert,by="employee_id") %>% unique()

time_type_combine<-cbind(turnover_before$time_type1, turnover_before$time_type2)
turnover_before$time_type<-apply(time_type_combine,1,function(x){paste(x[!(is.na(x)|x==""|x=="NA")],collapse=" to ")})
turnover_before <- select(turnover_before,-c(time_type1,time_type2))

##clean union name 
turnover_before$union_name <- gsub("\\:.*", "", turnover_before$union_name)

## Clean union name
union <- select(turnover_before,employee_id,union_name)
union  <- unique(union,na.rm=TRUE)
union  <- na.omit(union)
union_convert <- union  %>%
  group_by(employee_id) %>%
  mutate(n = paste0("union", 1:n())) %>%
  spread(n, union_name)

turnover_before$union_name <- NULL
turnover_before <- unique(turnover_before) 
turnover_before <- turnover_before %>% left_join(union_convert,by="employee_id") %>% unique()

union_combine<-cbind(turnover_before$union1, turnover_before$union2)
turnover_before$union_name<-apply(union_combine,1,function(x){paste(x[!(is.na(x)|x==""|x=="NA")],collapse=" to ")})
turnover_before <- select(turnover_before,-c(union1,union2))

## clean cmax
turnover_before$cmax <- gregexpr('[0-9]+',turnover_before$cmax)


## box plot 
table(turnover_before$Term,turnover_before$gender)
 table(turnover_before$Term,turnover_before$race_ethnicity) %>% 
  barplot(legend.text = TRUE)

boxplot(Term~race_ethnicity,
        data=turnover_before,
        main="Different boxplots for each month",
        xlab="Month Number",
        ylab="Degree Fahrenheit",
        col="orange",
        border="brown"
)


















