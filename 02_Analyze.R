# Load Libraries --------------------------------------------------------------------------------------------------

library(tidyverse)
library(haven)

# Load Data -------------------------------------------------------------------------------------------------------

ESS_prepared <- read_rds("repo/ESS_prepared.rds")



# Analyze Demographics --------------------------------------------------------------------------------------------

# age

df_mean_age_by_country <- 
  ESS_prepared |> 
  group_by(cntry ) |> 
  summarize(
    mean = mean(agea, na.rm = TRUE)
  ) |> 
  mutate(
    mean = round(mean, 1),
    y_lab_positions = mean + 3
  ) |> 
  arrange(desc(mean)) |> 
  mutate(
    cntry = factor(cntry, levels = cntry)  # Reorder factor levels
  )



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


ggplot(
  ESS_prepared,
  aes(
    x = cntry,
    y = agea)
) +
  geom_violin()

ggplot(
  ESS_prepared,
  aes(
    x = agea,
  )
) +
  geom_histogram(
    binwidth = 5
  )


# gender


df_proportion_of_women_by_country <- 
  ESS_prepared |> 
  group_by(cntry) |> 
  summarize(
    proportion_of_women = sum(gndr == "Female", na.rm = TRUE) / n()
  ) |> 
  mutate(
    proportion_of_women_in_pct = paste(round(proportion_of_women * 100, digits = 1), "%"),
    y_lab_positions = proportion_of_women + 0.03
  ) |> 
  arrange(desc(proportion_of_women_in_pct)) |> 
  mutate(
    cntry = factor(cntry, levels = cntry)  # Reorder factor levels
  )


ggplot(
  df_proportion_of_women_by_country,
  aes(x = cntry,
      y = proportion_of_women)
) +
  geom_col() +
  geom_text(
    aes(
      y = y_lab_positions,
      label = proportion_of_women_in_pct),
    size = 3
  )

ggplot(
  ESS_prepared,
  aes(
    x = gndr,
  )
) +
  geom_histogram(stat = "count")



# countries

df_participants_per_country <- 
  ESS_prepared |> 
    group_by(cntry) |> 
    summarise(count = n()) |> 
    arrange(desc(count)) |> 
    mutate(
      cntry = factor(cntry, levels = cntry)  # Reorder factor levels
    )


ggplot(
  df_participants_per_country,
  aes(
    x = cntry,
    y = count
  )
) +
  geom_col()



# Visualize trust and climate beliefs across demographics -------------------------------------------------------------------

#trust by age groups and countries

df_age_groups <- 
  ESS_prepared |> 
    mutate(
      age_group = cut(
        agea,
        breaks = seq(0, 90, by = 5), # Breaks every 5 years
        labels = paste(seq(0, 85, by = 5), seq(5, 90, by = 5) - 1, sep = "-"),
        right = FALSE
      )
    ) |> 
  group_by(age_group, cntry) |> 
  summarise(
    mean_trust = mean(trstprl),
    mean_cc_belief = mean(ccnthum),
    mean_lr_position = mean(lrscale)
  ) |> 
  drop_na()

create_facet_plot <- function(data, y_variable, y_limits = NULL, title, ylab) {
  ggplot(
    data,
    aes(
      x = age_group,
      y = !!ensym(y_variable) # Dynamically set the y aesthetic
    )
  ) + 
    geom_col(
      fill = "steelblue"
    ) +
    facet_wrap(
      facets = ~cntry
    ) +
    theme(
      axis.text.x = 
        element_text(
          angle = 90,
          vjust = 1,
          hjust = 1
        ),
      axis.title.y =
        element_text(
          hjust = 1
        ), 
      panel.background = element_blank(),
      panel.grid = element_blank(),
      panel.grid.major.y = element_line(
        colour = "black", 
        size = 0.3
      ),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    coord_cartesian(
      ylim = if (is.null(y_limits)) range(data[[deparse(substitute(y_variable))]], na.rm = TRUE) else y_limits
    ) +
    labs(
      title = title,
      subtitle = "Grouped by Country",
      x = "Age Groups", 
      y = ylab, 
      caption = "Source: ESS11"
    )
}

barplot_trust <- 
  create_facet_plot(
    df_age_groups,
    y_variable = mean_trust,
    title = "Average Trust in a Governments Parliament by Age Groups",
    ylab = "Average Trust in a Governments Parliament"
  )

barplot_climate_change <- 
  create_facet_plot(
    df_age_groups,
    y_variable = mean_cc_belief,
    title = "Average Belief in Human Made Climate Change by Age Groups",
    ylab = "Average Belief in Human Made Climate Change"
  )

barplot_lr_self <- 
  create_facet_plot(
    df_age_groups,
    y_variable = mean_lr_position,
    title = "Average Self-declared left-right Position by Age Groups",
    ylab = "Average Self-declared left-right Position (0 = left, 10 = right)"
  )




# Save Plots ------------------------------------------------------------------------------------------------------


write_rds(
  x = barplot_trust,
  file = "repo/barplot_trust.rds"
  )

write_rds(
  x = barplot_climate_change,
  file = "repo/barplot_climate_change.rds"
)

write_rds(
  x = barplot_lr_self,
  file = "repo/barplot_lr_self.rds"
)



