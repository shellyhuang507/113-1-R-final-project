usethis::create_github_token()

# Read the data
tidy_data <- read_csv("臺北市樂齡學習統計資料.csv")

# Clean and transform data
tidy_data <- tidy_data %>%
  mutate(
    `女` = as.numeric(str_replace_all(`女`, ",", "")),
    `男` = as.numeric(str_replace_all(`男`, ",", "")),
    `總人次` = as.numeric(str_replace_all(`總人次`, ",", "")),
    `場次` = as.numeric(`場次`),
    `女比率` = as.numeric(str_replace_all(`女比率`, "%", "")) / 100,
    `男比率` = as.numeric(str_replace_all(`男比率`, "%", "")) / 100
  ) %>%
  mutate(平均參與人數 = `總人次` / `場次`)

# Gender participation analysis: reshape to long format
gender_summary <- tidy_data %>%
  select(`名稱`, `女`, `男`) %>%
  pivot_longer(cols = c(`女`, `男`), names_to = "性別", values_to = "參與人數")

# Visualization: Gender participation
gender_summary %>%
  ggplot(aes(x = `名稱`, y = 參與人數, fill = 性別)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Gender Participation by Learning Center",
    x = "Learning Center",
    y = "Number of Participants"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visualization: Average participation per session
tidy_data %>%
  ggplot(aes(x = reorder(`名稱`, 平均參與人數), y = 平均參與人數)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Participants per Session by Learning Center",
    x = "Learning Center",
    y = "Average Participants per Session"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 計算性別比例與總參與人次的關係
tidy_data <- tidy_data %>%
  mutate(
    女性比例 = `女` / `總人次`,
    男性比例 = `男` / `總人次`
  )

# 視覺化：性別比例與總參與人次
tidy_data %>%
  ggplot(aes(x = 女性比例, y = `總人次`)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    title = "Relationship Between Female Proportion and Total Participation",
    x = "Female Proportion",
    y = "Total Participation"
  ) +
  theme_minimal()

# 計算相關係數
correlation <- tidy_data %>%
  summarise(correlation = cor(女性比例, `總人次`, use = "complete.obs"))
print(correlation)

# 建立線性迴歸模型
model <- lm(`總人次` ~ 女性比例, data = tidy_data)
summary(model)




