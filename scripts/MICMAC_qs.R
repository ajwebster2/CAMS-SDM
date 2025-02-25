library(tidyverse)
 
dat = read.csv("data_raw/top_tier_themes.csv")

dat_combo = expand.grid(rep(list(dat$Theme), 2))
dat_combo <- dat_combo[dat_combo$Var1 != dat_combo$Var2,]

datz = list()
for(i in c(1:420)){
  datz[[i]]  = 
    paste("If", dat_combo[i,1],"changed, what would be its direct influence on", dat_combo[i,2], "and how confident are you in your answer?")
}

qs = data.frame("Questions" = unlist(datz))
View(qs)

write.csv(qs, "top_tier_MICMAC_qs.csv", row.names = F)