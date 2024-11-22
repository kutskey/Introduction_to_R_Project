# Load Libraries --------------------------------------------------------------------------------------------------

library(tidyverse)


# Load Data -------------------------------------------------------------------------------------------------------

df <- read_rds("repo/ESS.rds")

# df_ESS |> summary()

# Prepare the data --------------------------------------------------------------------------------------------------------


df_cleaned <- df |> 
  select(idno, cntry, gndr, agea, region) |> 
  filter(cntry %in% c("CH", "DE", "AT")) |> 
  mutate(
    gndr = as_factor(gndr)
  )



# Create useful Visualizations ------------------------------------------------------------------------------------------------------

# mean age per country
df_mean_age_by_country <- 
  df_cleaned |> 
    group_by(cntry) |> 
    summarize(mean = mean(agea, na.rm = TRUE)) |> 
    mutate(
      y_lab_positions = mean + 3
    )
df_mean_age_by_country

barplot_mean_age_by_country <- 
  ggplot(
    df_mean_age_by_country,
    aes(x = cntry,
        y = mean)
  ) +
  geom_col() +
  geom_text(
    aes(
      y = y_lab_positions,
      label = mean)
  )
barplot_mean_age_by_country

scatterplot_mean_age_by_country <- 
  ggplot(
    df_mean_of_age_by_country,
    aes(x = cntry,
        y = mean)
  ) +
  geom_point() +
  geom_label(
    mapping = aes(
      label = round(mean, digits = 1)
    )
  ) 
scatterplot_mean_age_by_country

#proportion of women by country
df_proportion_of_women_by_country <- 
  df_cleaned |> 
    group_by(cntry) |> 
    summarize(
      proportion_of_women = sum(gndr == "Female", na.rm = TRUE) / n()
    ) |> 
  mutate(
    proportion_of_women_in_pct = paste(round(proportion_of_women * 100, digits = 1), "%")
  )
df_proportion_of_women_by_country

ggplot(
  df_proportion_of_women_by_country,
  aes(x = cntry,
      y = proportion_of_women)
) +
  geom_point() +
  geom_label(
    mapping = aes(
      label = proportion_of_women_in_pct)
  )
  

# More Plots ----------------------------------------------------------------------------------------------------

# boxplot
ggplot(
  df_ESS,
  aes(
    x = cntry,
    y = agea)
) +
  geom_boxplot()

# violinplot
ggplot(
  df_ESS,
  aes(
    x = cntry,
    y = agea)
) +
  geom_violin()

# scatterplot
ggplot(
  df_ESS,
  aes(
    x = cntry,
    y = agea)
) +
  geom_jitter(color = "darkblue")

# histograms
ggplot(
  df_ESS,
  aes(
    x = agea,
    )
) +
  geom_histogram(
    # add useful bins
  )

ggplot(
  df_ESS,
  aes(
    x = gndr,
  )
) +
  geom_histogram(stat = "count")

ggplot(
  df_ESS,
  aes(
    x = cntry
  )
) +
  geom_histogram(stat = "count")









