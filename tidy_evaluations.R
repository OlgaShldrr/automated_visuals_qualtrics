library(qualtRics)
library(dplyr)
library(tidyr)
library(janitor)

#evaluations <- read_survey(paste0(setwd(here::here()), "/evaluations1.csv"))

evaluations_tidy <- evaluations %>%
  pivot_longer(
    -(StartDate:`Q1.1`), 
    names_to = c("meeting_number", "question"),
    # Use Q(number).(number) pattern to extract values
    names_sep = "\\.",
    values_to = "response", 
    values_drop_na = TRUE
  ) %>%
  mutate(
    meeting_number = as.numeric(gsub(x=meeting_number, pattern = "Q", replacement = ""))) %>% 
  # Remove last optional question
  filter(meeting_number!=max(meeting_number)) %>% 
  pivot_wider(names_from = "question", 
              values_from = "response", 
              names_prefix = "q") %>% 
  clean_names() %>% 
  rename(meeting = q1_1) %>% 
  arrange(meeting_number) %>% 
  mutate(meeting = gsub("&", "and", meeting))

assign("evaluations_tidy", evaluations_tidy, envir= .GlobalEnv)

#readr::write_csv(evaluations_tidy, "evaluations_tidy.csv")
  