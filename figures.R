#==== PLOT FUNCTION ====#
#estimate event study model
make_event_plot <- function(data, figure_title) {

  #estimate effects of being in a ban state; control for year and school fixed effects  
  model <- feols(
    female_share ~ i(year, repeal, ref = 2021) | UNITID + year,
    cluster = ~ UNITID,
    data = data
  )
 #find coefficients, confidence intervals 
  estimates <- coef(model)
  intervals <- confint(model)
  # build dataframe; one estimate and confidence interval 95%
  plot_data <- tibble(
    term = names(estimates),
    estimate = unname(estimates),
    lower_ci = intervals[names(estimates), 1],
    upper_ci = intervals[names(estimates), 2]
  ) |>
    #extract year from coefficient
    mutate(
      year = as.integer(stringr::str_extract(term, "\\d{4}"))
    ) |>
    # add omitted reference year 2021, effect of 0
    bind_rows(
      tibble(
        term = "2021 reference",
        estimate = 0,
        lower_ci = 0,
        upper_ci = 0,
        year = 2021
      )
    ) |>
    arrange(year)
  
  # create the event study figure
  ggplot(plot_data, aes(x = year, y = estimate)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_errorbar(
      aes(ymin = lower_ci, ymax = upper_ci),
      width = 0.08
    ) +
    geom_point(shape = 15, size = 3) +
    scale_x_continuous(breaks = 2018:2024) +
    labs(
      title = figure_title,
      x = NULL,
      y = "Change in share of female applicants"
    ) +
    theme_classic()
}

#==== FIGURE 1 ====#
# FIGURE 1: any ban vs control
figure_1 <- make_event_plot(
  analysis_df,
  "Figure 1. Change in Share of Female Applications, Any Ban vs. Control States"
)

#==== FIGURE A1 ====#
# FIGURE A1: ranks 1-50
figure_a1 <- make_event_plot(
  analysis_df |> filter(usnews_rank <= 50),
  "Figure A1. Change in Share of Female Applications, Universities Ranked 1-50"
)

#==== FIGURE A2 ====#
# FIGURE A2: 50%+ out of state
df_a2 <- analysis_df |>
  filter(outstate_share >= 0.50)

figure_a2 <- make_event_plot(
  df_a2,
  "Figure A2. Change in Share of Female Applications, 50%+ Out-of-State Enrollment"
)
#==== FIGURE A3 ====#
# FIGURE A3: ranks 51-100
figure_a3 <- make_event_plot(
  analysis_df |> filter(usnews_rank >= 51, usnews_rank <= 100),
  "Figure A3. Change in Share of Female Applications, Universities Ranked 51-100"
)

#==== FIGURE A4 ====#
# FIGURE A4: <50% out of state
df_a4 <- analysis_df |>
  filter(outstate_share < 0.50)

figure_a4 <- make_event_plot(
  df_a4,
  "Figure A4. Change in Share of Female Applications, <50% Out-of-State Enrollment"
)

#==== DISPLAY FIGURES ====#

figure_1
figure_a1
figure_a2
figure_a3
figure_a4

#==== FIND COEFFICIENT ====#
figure_1$data |>
  filter(year == 2024) |>
  select(year, estimate, lower_ci, upper_ci)

#==== FOLDER TREE ====#
fs::dir_tree(here::here(), recurse = 3)






