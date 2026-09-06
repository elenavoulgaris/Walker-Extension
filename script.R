#==== LOAD PACKAGES ====#
suppressMessages(install.packages("pacman",quiet=TRUE))
library (pacman)
pacman::p_load(here, readxl, readr, tidyverse)

#==== IMPORT RANKINGS AND BAN STATUS DATA ====#

#importing ban status data
ban_status <- read_excel(here("ban status data.xlsx"))
View(ban_status)

#importing university rankings data
df.rank <-read_excel(here("US-News-National-University-Rankings-Top-150-Through-2026.xlsx"))

#==== IMPORT ADMISSIONS DATA ====#
#build admissions folder path
folder.path <- here("admissions")

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

library(modelsummary)

df |> select(UNITID,year,APPLCNM,APPLCNW) |> datasummary_skim()

datasummary_skim(df)
