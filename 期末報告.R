usethis::create_github_token(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)

# Load the tidyverse package
library(tidyverse)

# Read the data
tidy_data <- read_csv("Taipei_Learning_Statistics.csv")

# Data cleaning
tidy_data <- tidy_data %>%
  mutate(
    Female = as.numeric(str_replace_all(女, ",", "")),
    Male = as.numeric(str_replace_all(男, ",", "")),
    Total_Participants = as.numeric(str_replace_all(總人次, ",", "")),
    Female_Rate = as.numeric(str_replace_all(女比率, "%", "")) / 100,
    Male_Rate = as.numeric(str_replace_all(男比率, "%", "")) / 100
  )

# Gender participation analysis
tidy_gender_summary <- tidy_data %>%
  select(Name = 名稱, Female, Male) %>%
  pivot_longer(cols = c(Female, Male), names_to = "Gender", values_to = "Participants")

# Plot gender participation as a bar chart
tidy_gender_summary %>%
  ggplot(aes(x = Name, y = Participants, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Gender Participation Across Learning Centers",
    x = "Learning Center",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
