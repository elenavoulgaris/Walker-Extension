#===== LOAD DATAFRAMES =====#
source(here("importdata.R"))

#==== JOINING DATA ====#
df.adm <- df.adm |>
  mutate(year = as.numeric(year))

df.hd <- df.hd |>
  mutate(year = as.numeric(year))

df <- df.adm |>
  left_join(df.hd, join_by(UNITID, year))

#==== POLICY VARIABLE ====#

df <-  df.policy |>
  select(STABBR = state_abbr, status = `abortion_status`) |>
  mutate(repeal = if_else(status == "Banned",1,0)) |>
  right_join(df,join_by(STABBR))
df |> count(repeal)

# Recreate combined school-year data without prior policy columns
df <- df.adm |>
  select(-any_of(c("status", "repeal", "repeal.x", "repeal.y"))) |>
  left_join(df.hd, join_by(UNITID, year))

# Add one clean repeal column
df <- df |>
  left_join(policy_for_join, by = "STABBR")


# Create the main school-year data frame
df <- df.adm |>
  left_join(df.hd, join_by(UNITID, year))

# Add policy status
policy_for_join <- df.policy |>
  transmute(
    STABBR = state_abbr,
    repeal = if_else(
      tolower(trimws(abortion_status)) == "banned",
      1L,
      0L
    )
  )

df <- df |>
  left_join(policy_for_join, by = "STABBR")

#===== OUTCOME VARIABLE ====#

df |> mutate(wshare = APPLCNW/(APPLCNW + APPLCNM)) 

#==== HETEROGENOUS VARIABLES ====#

names(df.rank)

df.rank.long <- df.rank |>
  pivot_longer(
    cols = `2026`:`1984`,  
    names_to = "year",
    values_to = "rank")

df <- df |>
  left_join(
    #format df.rank.long to have numerical year and UNITID cols
    df.rank.long |> mutate(year = as.numeric(year),UNITID = IPEDS),
    join_by(UNITID,year)
  )

df <- df.rank |> select(UNITID = IPEDS,rank2023 = `2023`) |> 
  right_join(df,join_by(UNITID))

df |> select(UNITID,rank2023) |> datasummary_skim()

df |> filter(!is.na(rank2023)) |> distinct(UNITID) |> nrow()

#==== ADD US NEWS RANK, DEFINE ANALYSIS SAMPLE ====#
ranking_for_join <- df.rank.long |>
  filter(year == 2023) |>
  transmute(
    UNITID = as.numeric(IPEDS),
    usnews_rank = as.numeric(rank)
  ) |>
  filter(!is.na(UNITID), !is.na(usnews_rank)) |>
  distinct(UNITID, .keep_all = TRUE)

ranking_for_join |>
  count(UNITID) |>
  filter(n > 1)

df <- df |>
  select(-any_of("usnews_rank")) |>
  left_join(ranking_for_join, by = "UNITID")

analysis_df <- df |>
  filter(
    year %in% 2018:2022,
    !is.na(repeal),
    !is.na(usnews_rank),
    usnews_rank <= 100
  ) |>
  transmute(
    UNITID,
    year,
    repeal,
    female_share = APPLCNW / APPLCN,
    usnews_rank
  ) |>
  filter(!is.na(female_share))

analysis_df |>
  summarise(
    school_years = n(),
    schools = n_distinct(UNITID)
  )

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

analysis_df <- analysis_df |>
  select(-any_of("outstate_share")) |>
  left_join(
    df.outstate |> select(UNITID, outstate_share),
    by = "UNITID"
  )