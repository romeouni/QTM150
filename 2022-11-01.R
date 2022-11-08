library(dplyr)
library(forcats)
library(ggplot2)

?gss_cat

relig_summary <- gss_cat %>% 
  group_by(relig) %>% 
  summarize(avg=mean(tvhours, na.rm=T)) %>%
  arrange(desc(avg))

relig_summary

ggplot(relig_summary,aes(x=relig, y=avg))+
  geom_col()

ggplot(relig_summary,aes(x=relig, y=avg))+
  geom_col() +
  coord_flip()

ggplot(relig_summary, aes(x=avg, y=fct_reorder(relig,avg)))+
  geom_col()

ggplot(relig_summary, aes(x=avg, y=fct_reorder(relig,avg) %>% fct_rev())) +
  geom_col()

ggplot(relig_summary, aes(x=avg, y=fct_relevel(relig,"Don't know", after=0)))+ 
  geom_col()

ggplot(relig_summary, aes(x=avg, y=fct_relevel(fct_rev(fct_reorder(relig,avg)),"Don't know", after=0)))+
  geom_col()

ggplot(relig_summary, aes(x=avg, y=fct_reorder(relig,avg) %>% fct_rev() %>% fct_relevel("Don't know", after=0)))+
  geom_col()

gss_cat %>%
  ggplot(aes(x=marital))+
  geom_bar()

barplot(sort(table(gss_cat$marital)))

gss_cat %>%
  ggplot(aes(x=fct_infreq(marital)))+
  geom_bar()

gss_cat %>%
  ggplot(aes(x=fct_rev(fct_infreq(marital))))+
  geom_bar()

gss_cat %>%
  ggplot(aes(x=fct_infreq(marital) %>% fct_rev()))+
  geom_bar()

gss_cat %>%
  group_by(partyid) %>%
  summarize(avg=mean(tvhours,na.rm=T))

gss_cat %>%
  mutate(partyid1 = fct_recode(partyid,
                               "Republican, strong" = "Strong republican",
                               "Republican, weak" = "Not str republican",
                               "Independent, near rep" = "Ind,near rep",
                               "Independent, near dem" = "Ind,near dem",
                               "Democrat, weak" = "Not str democrat",
                               "Democrat, strong" = "Strong democrat"
  )) %>%
  count(partyid1)

gss_cat %>%
  mutate(partyid1 = fct_recode(partyid,
                               "Republican, strong" = "Strong republican",
                               "Republican, weak" = "Not str republican",
                               "Independent, near rep" = "Ind,near rep",
                               "Independent, near dem" = "Ind,near dem",
                               "Democrat, weak" = "Not str democrat",
                               "Democrat, strong" = "Strong democrat",
                               "Other" = "No answer",
                               "Other" = "Don't know",
                               "Other" = "Other party"
  )) %>%
  count(partyid1)

gss_cat %>%
  filter(!is.na(tvhours)) %>%
  mutate(partyidnew = fct_recode(partyid,
                                 "Republican, strong"    = "Strong republican",
                                 "Republican, weak"      = "Not str republican",
                                 "Independent, near rep" = "Ind,near rep",
                                 "Independent, near dem" = "Ind,near dem",
                                 "Democrat, weak"        = "Not str democrat",
                                 "Democrat, strong"      = "Strong democrat",
                                 "Other"                 = "No answer",
                                 "Other"                 = "Don't know",
                                 "Other"                 = "Other party")) %>%
  group_by(partyidnew) %>%
  summarize(avg=mean(tvhours))

gss_cat %>%
  mutate(partyidnew = fct_collapse(partyid,
                                   other = c("No answer", "Don't know", "Other party"),
                                   rep = c("Strong republican", "Not str republican"),
                                   ind = c("Ind,near rep", "Independent", "Ind,near dem"),
                                   dem = c("Not str democrat", "Strong democrat")
  )) %>%
  count(partyidnew)

gss_cat %>%
  mutate(relig = fct_lump(relig)) %>%
  count(relig)

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig)  

gss_cat %>%
  mutate(relig = fct_lump(relig, n = 10)) %>%
  count(relig, sort = TRUE) %>%
  print(n=5)
