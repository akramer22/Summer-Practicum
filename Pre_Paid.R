library(Hmisc)
library(lubridate)
library(Hmisc)
library(lubridate)
require(dplyr)
require(tidyverse)
require(purrr)
require(haven)
require(lubridate)
library(gmodels)
library(DescTools)
library(vcdExtra)
library(plotly)
# Load all libraries
# When adding code to this script, please add any libraries not already
# on this list




"Training data"
#Filter out Fully Paid Loans 
train_master_processed <- train_master_processed %>%
 dplyr::filter(loan_status=="Fully Paid")

#Remove "Months" from rows
train_master_processed$term = substr(train_master_processed$term, 1,2)
train_master_processed$term = as.numeric(train_master_processed$term)

#Convert actual term length to months
train_master_processed$term_len = round(as.numeric(difftime(train_master_processed$last_pymnt_d, train_master_processed$issue_d, units ="days"))/(365.25/12))

#calculate Revenue gains and losses
train_master_processed$LC_Revenue = if_else(train_master_processed$term_len <= train_master_processed$term , train_master_processed$term_len*train_master_processed$installment*.01 , train_master_processed$term*train_master_processed$installment*.01)
train_master_processed$LC_Rev_wlate = train_master_processed$LC_Revenue + train_master_processed$total_rec_late_fee
train_master_processed$expected_LC_Rev = train_master_processed$term*train_master_processed$installment*.01
train_master_processed$LC_rev_loss = train_master_processed$expected_LC_Rev - train_master_processed$LC_Revenue

#Create prepay flag and calculate prepay percentage of fully paid loans
train_master_processed$prepay =  if_else(train_master_processed$term_len < train_master_processed$term, 1,0)
sum(train_master_processed$prepay)
sum(train_master_processed$prepay)/ length(train_master_processed$term_len)



"Testing data"
#Filter out Fully Paid Loans 
test_master_processed <- test_master_processed %>%
  dplyr::filter(loan_status=="Fully Paid")
  

#Remove "Months" from rows
test_master_processed$term = substr(test_master_processed$term, 1,2)
test_master_processed$term = as.numeric(test_master_processed$term)

#Convert actual term length to months
test_master_processed$term_len = round(as.numeric(difftime(test_master_processed$last_pymnt_d, test_master_processed$issue_d, units ="days"))/(365.25/12))

#calculate Revenue gains and losses
test_master_processed$LC_Revenue = if_else(test_master_processed$term_len <= test_master_processed$term , test_master_processed$term_len*test_master_processed$installment*.01 , test_master_processed$term*test_master_processed$installment*.01)
test_master_processed$LC_Rev_wlate = test_master_processed$LC_Revenue + test_master_processed$total_rec_late_fee
test_master_processed$expected_LC_Rev = test_master_processed$term*test_master_processed$installment*.01
test_master_processed$LC_rev_loss = test_master_processed$expected_LC_Rev - test_master_processed$LC_Revenue

#Create prepay flag and calculate prepay percentage of fully paid loans
test_master_processed$prepay =  if_else(test_master_processed$term_len < test_master_processed$term, 1,0)
sum(train_master_processed$prepay)
sum(train_master_processed$prepay)/ length(train_master_processed$term_len)









#create stacked bar graph of revenue and total loss 
total = rbind(train_master_processed, test_master_processed)
h = total %>%group_by(issue_d_year) %>% sum(total$LC_Revenue)
k = sum(total$LC_rev_loss)
data = c(h,k)
plot_ly(data=total, x = ~ issue_d_year, y = ~ LC_Revenue, type = 'bar', name = 'Revenue') %>%
  add_trace(y = ~ LC_rev_loss, name = 'Potential Revenue') %>%
  layout(yaxis = list(title = 'US Dollars'), barmode = 'stack') %>%
  layout(xaxis = list(title = "Loan Issue Year"))






car = total %>% filter(purpose == "car") %>%
  group_by(prepay)
prepay_pct = (sum(car$prepay)/length(car$prepay))
prepay_pct

credit = total %>% filter(purpose == "credit_card")
sum(credit$prepay)/length(credit$prepay)

library(gmodels)
CrossTable(total$purpose, total$prepay)

#run model selection
run_selection_experiemnt_logistic(train_master_processed, default ~., experiment_name = "prepay1", criterion = 2, step = T, forward = T, back = T, train_split = .2, print = F)
run_selection_experiemnt_logistic(train_master_processed, default ~.^2, experiment_name = "prepay1", criterion = 2, step = T, forward = T, back = T, train_split = .2, print = F)
