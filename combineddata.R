source(here("retentiondata.R"))

#==== JOINING DATA ====#
#make year numeric so its possible to join
df.adm <- df.adm |>
  mutate(year = as.numeric(year))

df.hd <- df.hd |>
  mutate(year = as.numeric(year))

#create one data frame by adding directory info to admissions data
df <- df.adm |>
  left_join(df.hd, join_by(UNITID, year))

#==== POLICY VARIABLE ====#

# Create the abortion-policy treatment indicator
policy_for_join <- df.policy |>
  transmute(
    STABBR = state_abbr,
    repeal = if_else(
      tolower(trimws(abortion_status)) == "banned",
      1L,
      0L
    )
  )

# Match university to state policy indicator
df <- df |>
  left_join(policy_for_join, by = "STABBR")

#===== OUTCOME VARIABLE ====#
# calculate female share
df |> mutate(wshare = APPLCNW/(APPLCNW + APPLCNM)) 

#==== ADD US NEWS RANK, DEFINE ANALYSIS SAMPLE ====#
# make rankings long format: one row per year
df.rank.long <- df.rank |>
  pivot_longer(
    cols = `2026`:`1984`,  
    names_to = "year",
    values_to = "rank")

# keep 2023 ranking as the fixed measure to define the sample
ranking_for_join <- df.rank.long |>
  filter(year == 2023) |>
  transmute(
    UNITID = as.numeric(IPEDS),
    usnews_rank = as.numeric(rank)
  ) |>
  filter(!is.na(UNITID), !is.na(usnews_rank)) |>
  distinct(UNITID, .keep_all = TRUE)

# check that every university was assigned only one ranking
ranking_for_join |>
  count(UNITID) |>
  filter(n > 1)

# add rankings to the combined data
df <- df |>
  select(-any_of("usnews_rank")) |>
  left_join(ranking_for_join, by = "UNITID")

# restrict sample to top 100 ranked schools; construct female share
analysis_df <- df |>
  filter(
    year %in% 2018:2024,
    !is.na(repeal),
    !is.na(usnews_rank),
    usnews_rank <= 100
  ) |>
  transmute(
    UNITID,
    year,
    repeal,
    female_share = APPLCNW / (APPLCNW + APPLCNM),
    usnews_rank
  ) |>
  filter(!is.na(female_share))

# check number of observations, number of schools 
analysis_df |>
  summarise(
    school_years = n(),
    schools = n_distinct(UNITID)
  )
#==== CREATE RETENTION ANALYSIS DATA ====#

retention_df <- analysis_df |>
  inner_join(
    df.efd |>
      select(UNITID, cohort_year, report_year, retention_rate),
    by = c("UNITID", "year" = "cohort_year")
  ) |>
  select(
    UNITID,
    year,
    report_year,
    repeal,
    retention_rate,
    usnews_rank
  ) |>
  filter(!is.na(retention_rate))

# check retention analysis sample
retention_df |>
  summarise(
    school_years = n(),
    schools = n_distinct(UNITID),
    first_cohort_year = min(year),
    last_cohort_year = max(year)
  )

retention_df |>
  count(year)


#==== CREATE OUT OF STATE ENROLLMENT MEASURE ====#
#identify each home state
school_state_check <- df.hd |>
  semi_join(
    analysis_df |> distinct(UNITID),
    by = "UNITID"
  ) |>
  transmute(
    UNITID = as.numeric(UNITID),
    FIPS = as.numeric(FIPS)
  ) |>
  filter(!is.na(UNITID), !is.na(FIPS)) |>
  group_by(UNITID) |>
  summarise(
    number_of_fips_values = n_distinct(FIPS),
    FIPS = first(FIPS),
    .groups = "drop"
  )

#keep one school state code per university
school_state <- school_state_check |>
  select(UNITID, FIPS)

#==== OUT OF STATE ENROLLMENT SHARE FROM EF-C ====#
# FIPS codes for the 50 states and Washington, DC
state_fips <- c(
  1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18,
  19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31,
  32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45,
  46, 47, 48, 49, 50, 51, 53, 54, 55, 56
)

# calculate share of out of state enrollment
df.outstate <- df.efc |>
  transmute(
    UNITID = as.numeric(UNITID),
    residence_fips = as.numeric(EFCSTATE),
    enrolled_first_time = as.numeric(EFRES01)
  ) |>
  filter(
    residence_fips %in% state_fips,
    enrolled_first_time >= 0
  ) |>
  left_join(school_state, by = "UNITID") |>
  group_by(UNITID) |>
  summarise(
    total_enrolled = sum(enrolled_first_time),
    outstate_enrolled = sum(enrolled_first_time[residence_fips != FIPS]),
    outstate_share = outstate_enrolled / total_enrolled,
    .groups = "drop"
  )

#==== ADD OUT OF STATE SHARE TO ANALYSIS SAMPLE ====#
# add enrollment share for every university to every year
analysis_df <- analysis_df |>
  select(-any_of("outstate_share")) |>
  left_join(
    df.outstate |> select(UNITID, outstate_share),
    by = "UNITID"
  )