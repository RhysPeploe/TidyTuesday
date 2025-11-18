library(tidyverse)
library(gt)
library(gtsummary)
library(gtExtras)

lead = tidytuesdayR::tt_load(2025, week = 44)

flint_mdeq <- lead$flint_mdeq
flint_vt <- lead$flint_vt

flint_fives = flint_vt %>% 
  filter(sample %% 5 == 0) %>% 
  mutate(week = row_number()) %>% 
  select(week, sample, lead) %>% 
  mutate(change = lead - lag(lead),
         percentchange = round(change/lag(lead),3)*100)

flint_fives %>% 
  gt() %>% 
  gt_plt_bar_pct(
    lead,
    labels = TRUE
  ) 


ggplot(flint_fives, aes(x = week, y = percentchange)) +
  geom_bar(stat = "identity", fill = "#435C6D") +
  labs(x = "Sample (Every 5th Sample)",
       y = "Percentage Change from Previous",
       title = "Lead level (parts per billion) have increased constantly in Michigan in 2015") +
  theme_minimal() + 
  geom_hline(mapping = aes(yintercept = mean(percentchange, na.rm = T)),
             linetype = "dashed") +
  annotate("text", x = 22, y = 14, 
           label = "mean percentage change")+
  geom_bar(stat = "identity", 
           data = flint_fives %>% 
             filter(percentchange >= 12.17547),
           fill = "#f54242") 

