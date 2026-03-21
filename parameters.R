library(readxl)
SIPRI <- read_excel("SIPRI.xlsx")

#regress changes in Canadian military spending on changes in USA military spending and on Canadian military expenditure
lm(SIPRI$Canada_change~SIPRI$USA_change+SIPRI$Canada)

#regress changes in USA military spending on changes in Canadian military spending and on USA military expenditure 
lm(SIPRI$USA_change~SIPRI$Canada_change+SIPRI$USA)
