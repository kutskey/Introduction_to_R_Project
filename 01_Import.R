# Load Libraries --------------------------------------------------------------------------------------------------

library(tidyverse)
library(haven)


# Import Data -----------------------------------------------------------------------------------------------------

ESS11 <- read_dta("data/ESS11.dta")

# ESS11[1,]
# colnames(ESS11)
# unique(ESS11$proddate)



# Prepare the data --------------------------------------------------------------------------------------------------------


ESS_prepared <- ESS11 |> 
  select(cntry, gndr, agea, # demographics
         lrscale, # political orientation
         trstprl, # trust in political institutions
         ccnthum) |> # human caused climate change belief
  mutate(
    gndr = factor(gndr, levels = c(1, 2), labels = c("Male", "Female"))
  ) |>
  filter(
    ccnthum <= 5
  ) |> 
  drop_na()


# glimpse(ESS_prepared)
# unique(ESS_prepared$ccnthum)




# Save RDS --------------------------------------------------------------------------------------------------------

write_rds(ESS_prepared, "repo/ESS_prepared.rds")
