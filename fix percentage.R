
library(tidyverse)

d <- read_csv("data/leaders_cz.csv")

d <- d %>% 
  mutate(cz_label = str_replace_all(cz_label, "Counties: ", "")) %>% 
  mutate(cz_label = str_replace_all(cz_label, "County: ", ""))
  
d %>% 
  arrange(desc(mean_pct))

d %>% 
  write_csv("data/leaders_cz.csv")
              