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
