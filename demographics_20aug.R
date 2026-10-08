#sample characteristics (summarise in text)
library(readr)
  data <- read_csv("/RDS79338-ArchibaldARCIF/Madi-Masters/Data/20-AUGUST-26/landholder_preferences_final_cleaned.csv")

#average age (what year are you born) (number)
mean(data$age, na.rm = TRUE)
sd(data$age, na.rm = TRUE)
max(data$age, na.rm = TRUE)
min(data$age, na.rm = TRUE)

#gender split (%)
table(data$gender)
prop.table(table(data$gender)) * 100 #%

#average household income split (%)?
table(data$income)
prop.table(table(data$income)) * 100 #%

income_counts <- table(data$income)

income_summary <- data.frame(
  Income = names(income_counts),
  Count = as.vector(income_counts),
  Percentage = as.vector(prop.table(income_counts) * 100)
)

income_summary

#education level - split (%)
table(data$education)
prop.table(table(data$education)) * 100 #%

#employment - split (%)
table(data$employment)
prop.table(table(data$employment)) * 100 #%

#CONSERVATION participation
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

conservationactivity <- data %>%
  mutate(respondent = row_number()) %>%
  separate_rows(conservationscheme, sep = ",") %>%
  mutate(conservationscheme = str_squish(conservationscheme)) %>%
  filter(!is.na(conservationscheme), conservationscheme != "")

conservationparticipation <- conservationactivity %>%
  count(conservationscheme, name = "respondents") %>%
  mutate(
    percentage = respondents / n_distinct(conservationactivity$respondent) * 100
  ) %>%
  arrange(desc(respondents))

conservationparticipation

sum(!is.na(data$conservationscheme)) #check matches number of responses!

ggplot(conservationparticipation, 
       aes(x = reorder(conservationscheme, percentage), 
           y = percentage)) +
  geom_col(fill = "darkturquoise") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Percentage of respondents",
    title = "Participation in different activities"
  ) +
  theme_minimal()

ggplot(conservationparticipation, 
       aes(x = reorder(conservationscheme, percentage), 
           y = percentage,
           fill = conservationscheme == "None of these")) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("TRUE" = "gray", "FALSE" = "darkturquoise"),
    guide = "none"
  ) +
  labs(
    x = NULL,
    y = "Percentage of respondents",
    title = "Participation in different activities"
  ) +
  theme_minimal()

#MULtiPLE actioNS
# Count number of conservation actions per respondent
action_counts <- conservationactivity %>%
  count(respondent, name = "n_actions")

# Summarise into useful groups
action_summary <- action_counts %>%
  mutate(
    action_group = case_when(
      n_actions == 1 ~ "1 action",
      n_actions >= 2 & n_actions <= 5 ~ "2–5 actions",
      n_actions >= 6 ~ "6+ actions"
    )
  ) %>%
  count(action_group) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

action_summary

#SUMMARY
one_action <- action_summary %>%
  filter(action_group == "1 action") %>%
  pull(percentage)

two_five <- action_summary %>%
  filter(action_group == "2–5 actions") %>%
  pull(percentage)

six_plus <- action_summary %>%
  filter(action_group == "6+ actions") %>%
  pull(percentage)

#PART A SUMMARY
#homeowner?
table(data$homeowner)
prop.table(table(data$homeowner)) * 100 #%

#householdcomposition
table(data$housecomp)
prop.table(table(data$housecomp)) * 100 #%

library(dplyr)

homeowner_summary <- data %>%
  mutate(
    homeownership_group = ifelse(
      homeowner %in% c(
        "I own my home outright",
        "I own my home and have a mortgage"
      ),
      "Own home (outright or mortgage)",
      "Other housing arrangement"
    )
  ) %>%
  count(homeownership_group) %>%
  mutate(
    percentage = n / sum(n) * 100
  )

homeowner_summary
#people with detatched homes
yes_count <- sum(data$detached == "Yes", na.rm = TRUE)
total_responses <- sum(!is.na(data$detached))
yes_percent <- yes_count / total_responses * 100

yes_percent
yes_count

#land size 
land_split <- data %>%
  filter(!is.na(landsize)) %>%
  count(landsize) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n))

land_split

#NO NAMES?! 
ggplot(land_split,
       aes(x = reorder(landsize, percentage), y = percentage)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Percentage of respondents",
    title = "Land size of respondents"
  ) +
  theme_minimal()

#POSTCODE
postcode_summary <- data %>%
  filter(!is.na(postcode)) %>%
  count(postcode, name = "respondents") %>%
  mutate(
    percentage = respondents / sum(respondents) * 100
  ) %>%
  arrange(desc(respondents))

postcode_summary
print(postcode_summary, n =35)

#POSTCODE BY STATE:
postcode_summary <- data %>%
  filter(!is.na(postcode)) %>%
  mutate(
    postcode_prefix = substr(as.character(postcode), 1, 1)
  ) %>%
  count(postcode_prefix, name = "respondents") %>%
  mutate(
    percentage = respondents / sum(respondents) * 100
  ) %>%
  arrange(postcode_prefix)

postcode_summary

#NOTE: one answer changed from worded answer to postcode answer DRUMMOND COVE to 6532

#RENAME TO STATES
postcode_summary <- data %>%
  filter(!is.na(postcode)) %>%
  mutate(
    postcode_prefix = substr(as.character(postcode), 1, 1),
    state = case_when(
      postcode_prefix == "2" ~ "NSW",
      postcode_prefix == "3" ~ "VIC",
      postcode_prefix == "4" ~ "QLD",
      postcode_prefix == "5" ~ "SA",
      postcode_prefix == "6" ~ "WA",
      postcode_prefix == "7" ~ "TAS",
      postcode_prefix == "8" ~ "NT",
      postcode_prefix == "0" ~ "NT",
      postcode_prefix == "9" ~ "ACT",
      TRUE ~ "Unknown"
    )
  ) %>%
  count(state, name = "respondents") %>%
  mutate(
    percentage = respondents / sum(respondents) * 100
  ) %>%
  arrange(desc(respondents))

postcode_summary

#plantopurchase?
plantopurchase <- data %>%
  filter(!is.na(plantopurchase)) %>%
  count(plantopurchase) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n))

plantopurchase

plantopurchase_yes <- data %>%
  filter(!is.na(plantopurchase)) %>%
  summarise(
    yes = sum(str_detect(plantopurchase, regex("^yes", ignore_case = TRUE))),
    total = n(),
    percentage = yes / total * 100
  )

plantopurchase_yes

#firsthome vs investment
first_home <- data %>%
  filter(!is.na(firsthome)) %>%
  count(firsthome) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n))

first_home 

#regional area?
regional_area <- data %>%
  filter(!is.na(regional)) %>%
  count(regional) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n))

regional_area

#landsize future
landsize_future <- data %>%
  filter(!is.na(landsize_future)) %>%
  count(landsize_future) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n))

landsize_future

ggplot(landsize_future,
       aes(x = reorder(landsize_future, percentage), y = percentage)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    x = NULL,
    y = "Percentage of respondents",
    title = "Land size of respondents"
  ) +
  theme_minimal()

#compare landsize
comparison_landsize <- bind_rows(
  data %>%
    filter(!is.na(landsize)) %>%
    count(activity = landsize) %>%
    mutate(
      percentage = n / sum(n) * 100,
      timeframe = "Current"
    ),
  
  data %>%
    filter(!is.na(landsize_future)) %>%
    count(activity = landsize_future) %>%
    mutate(
      percentage = n / sum(n) * 100,
      timeframe = "Future"
    )
)

comparison_landsize

ggplot(comparison_landsize,
       aes(x = activity, y = percentage, fill = timeframe)) +
  geom_col(position = "dodge") +
  labs(
    x = NULL,
    y = "Percentage of respondents",
    fill = NULL,
    title = "Current and future land use"
  ) +
  theme_minimal() +
  coord_flip()

#landuse-current
landuse_current <- data %>%
  filter(!is.na(landuse_current)) %>%
  count(landuse_current) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n))

landuse_current

#landuse-future
landuse_future <- data %>%
  filter(!is.na(landuse_future)) %>%
  count(landuse_future) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n))

landuse_future

#compare current and future land use
comparison <- bind_rows(
  data %>%
    filter(!is.na(landuse_current)) %>%
    count(activity = landuse_current) %>%
    mutate(
      percentage = n / sum(n) * 100,
      timeframe = "Current"
    ),
  
  data %>%
    filter(!is.na(landuse_future)) %>%
    count(activity = landuse_future) %>%
    mutate(
      percentage = n / sum(n) * 100,
      timeframe = "Future"
    )
)

comparison

ggplot(comparison,
       aes(x = activity, y = percentage, fill = timeframe)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("violet", "darkturquoise"))+
  labs(
    x = NULL,
    y = "Percentage of respondents",
    fill = NULL,
    title = "Current and future land use"
  ) +
  theme_minimal() +
  coord_flip()

library(ggplot2)
#mtheme_minimal()#mortgage value & interest
mortgage_value <- data %>%
  filter(!is.na(mortgagevalue)) %>%
  count(mortgagevalue) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n))

mortgage_value

mortgage_interest <- data %>%
  filter(!is.na(mortgageinterest)) %>%
  count(mortgageinterest) %>%
  mutate(
    percentage = n / sum(n) * 100
  ) %>%
  arrange(desc(n))

mortgage_interest

#would they want to take out a nal?
table(c(data$nal_transition, data$nal_newloan))
table(c(data$nal_newloan))
table(c(data$nal_transition))

library(dplyr)
library(tidyr)
library(ggplot2)

data %>%
  select(nal_transition, nal_newloan) %>%
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response"
  ) %>%
  filter(!is.na(response), response != "") %>%
  mutate(
    question = recode(
      question,
      nal_transition = "Transition existing loan",
      nal_newloan = "New loan"
    )
  ) %>%
  count(question, response) %>%
  ggplot(aes(x = response, y = n, fill = question)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("violet", "darkturquoise"))+
  labs(
    x = "Response",
    y = "Number of people",
    fill = "Question"
  ) +
  theme_minimal()

#as percent
data %>%
  select(nal_transition, nal_newloan) %>%
  pivot_longer(
    cols = everything(),
    names_to = "question",
    values_to = "response"
  ) %>%
  filter(!is.na(response), response != "") %>%
  mutate(
    question = recode(
      question,
      nal_transition = "Transition existing loan",
      nal_newloan = "New loan"
    )
  ) %>%
  count(question, response) %>%
  group_by(question) %>%
  mutate(percent = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = response, y = percent, fill = question)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("darkolivegreen4", "darkolivegreen3")) +
  labs(
    x = "Response",
    y = "Percent of people",
    fill = "Question"
  ) +
  theme_minimal()
#OVERALL IMPORTANCE OF EACH ATTRIBUTE
library(dplyr)
library(tidyr)

anchor_vars <- paste0("QID34_", 1:10)

anchor_labels <- c(
  "Easy ways to check conservation requirements",
  "Application as simple as a regular loan",
  "Little time spent on monitoring or paperwork",
  "Works with existing land management",
  "Can switch back to a regular loan",
  "Can try the loan before committing long-term",
  "Noticeably lower interest rate",
  "Other people have had good experiences",
  "Can see a difference being made to nature",
  "Choice over conservation actions"
)

anchor_summary <- data %>%
  summarise(
    across(
      all_of(anchor_vars),
      list(
        important = ~ sum(. == "Important", na.rm = TRUE),
        not_important = ~ sum(. == "Not important", na.rm = TRUE),
        valid = ~ sum(!is.na(.))
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", ".value"),
    names_pattern = "(QID34_\\d+)_(important|not_important|valid)"
  ) %>%
  mutate(
    feature = anchor_labels[match(variable, anchor_vars)],
    percent_important = round(important / valid * 100, 1),
    percent_not_important = round(not_important / valid * 100, 1)
  ) %>%
  select(
    feature,
    important,
    percent_important,
    not_important,
    percent_not_important,
    valid
  ) %>%
  arrange(desc(percent_important))

anchor_summary

