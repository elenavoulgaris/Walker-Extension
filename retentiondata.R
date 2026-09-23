#==== IMPORT AND PREPARE EF-D RETENTION DATA ====#

pacman::p_load(here, tidyverse)

ef_d_files <- list.files(
  here("EF-D"),
  pattern = "\\.csv$",
  full.names = TRUE
)

df.efd <- purrr::map_dfr(
  ef_d_files,
  function(file) {
    
    report_year <- stringr::str_extract(
      basename(file),
      "\\d{4}"
    ) |>
      as.numeric()
    
    readr::read_csv(
      file,
      show_col_types = FALSE
    ) |>
      transmute(
        UNITID = as.numeric(UNITID),
        report_year = report_year,
        cohort_year = report_year - 1,
        retention_rate = as.numeric(RET_PCF) / 100
      )
  }
)

#==== CHECK EF-D DATA ====#

df.efd |>
  summarise(
    rows = n(),
    schools = n_distinct(UNITID),
    minimum_retention = min(retention_rate, na.rm = TRUE),
    maximum_retention = max(retention_rate, na.rm = TRUE),
    missing_retention = sum(is.na(retention_rate))
  )

df.efd |>
  count(UNITID, cohort_year) |>
  filter(n > 1)