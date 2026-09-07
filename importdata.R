#==== LOAD PACKAGES ====#
suppressMessages(install.packages("pacman",quiet=TRUE))
library (pacman)
pacman::p_load(here, readxl, readr, tidyverse, modelsummary)

#==== IMPORT RANKINGS DATA ====#

#importing university rankings data
df.rank <-read_excel(here("US-News-National-University-Rankings-Top-150-Through-2026.xlsx"))

#==== IMPORT ADMISSIONS DATA ====#
#build admissions folder path
folder.path <- here("admissions/")

#list files in admissions folder
lf <- list.files(folder.path)

#read files and add year variable
##initialize dataframe
ll <- 1
df <- read_csv(here(folder.path, lf[ll])) |>
  mutate(year = lf[ll] |> str_extract("\\d{4}"))

#loop over remaining files
for (ll in 2:length(lf)){
  #read file and add year variable
  df2 <- read_csv(here(folder.path, lf[ll])) |>
    mutate(year = lf[ll] |> str_extract("\\d{4}+"))
  #bind to existing dataframe
  df <- df |> bind_rows(df2)
  }
df.adm <- df
rm(folder.path, lf,ll,df,df2)

df.adm |> select(UNITID,year,APPLCNM,APPLCNW) |> datasummary_skim()

#==== IMPORT HD DATA ====#
#build HD folder path
folder.path <- here("HD/")

#list files in HD folder
lf <- list.files(folder.path)

#read files and add year variable
##initialize dataframe
ll <- 1
df <- read_csv(here(folder.path, lf[ll])) |>
  mutate(year = lf[ll] |> str_extract("\\d{4}"))

#loop over remaining files
for (ll in 2:length(lf)){
  #read file and add year variable
  df2 <- read_csv(here(folder.path, lf[ll])) |>
    mutate(year = lf[ll] |> str_extract("\\d{4}+"))
  #bind to existing dataframe
  df <- df |> bind_rows(df2)
}
df.hd <-df
rm(folder.path, lf, ll, df, df2)

df.hd |> select(UNITID,year,STABBR) |> datasummary_skim()

#==== IMPORTING BAN STATUS DATA ====#
df.policy <- read_excel(here("ban status data.xlsx"))


