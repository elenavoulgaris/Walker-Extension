#==== LOAD PACKAGES ====#
#suppressMessages(install.packages("pacman",quiet=TRUE))
library (pacman)
pacman::p_load(here, readxl, readr, tidyverse, modelsummary, dplyr, fixest, ggplot2, stringr, tinytex)

#==== IMPORT RANKINGS DATA ====#

#importing university rankings data
df.rank <-read_excel(here("additional data", "US-News-National-University-Rankings-Top-150-Through-2026.xlsx"))

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
#add to dataframe, remove temporary objects
df.adm <- df
rm(folder.path, lf,ll,df,df2)
# quick summary of the 4 admissions variables
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
# add to dataframe, remove temporary objects
df.hd <-df
rm(folder.path, lf, ll, df, df2)
# quick summary of the three variables
df.hd |> select(UNITID,year,STABBR) |> datasummary_skim()

#==== IMPORT EFC DATA ====#
#build EF-C folder path
folder.path <- here("EF-C/")
#read out of state enroll. for 2021 only- fixed characteristic
df.efc <- read_csv(
  here("EF-C", "ef2021c_rv.csv"),
  show_col_types = FALSE
)

#==== IMPORTING BAN STATUS DATA ====#
df.policy <- read_excel(here("additional data", "ban status data.xlsx"))


