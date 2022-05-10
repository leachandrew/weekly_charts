library(tidyverse)
library(readxl)
library(janitor)

members<-read_csv("Subscription List - Weekly v2.csv")%>%
  select(name=Name,email=Email)

adds<-read_csv("Subscription List - Form responses 1.csv")%>%
  clean_names()%>%select(name=enter_your_name,email=enter_your_email_address,company_school)

all<-bind_rows(members,adds)%>%
  mutate(dupl=duplicated(email))%>%
  group_by(email)%>%
  arrange(email,name,company_school,dupl)%>%
  mutate(n=row_number())%>%
  ungroup()%>%
  filter(n==1,!is.na(email))
  

write_csv(all,"weekly.csv")