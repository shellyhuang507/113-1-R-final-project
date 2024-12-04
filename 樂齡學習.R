usethis::use_git_config(user.name = "shellyhuang507", user.email = "s411373018@gm.ntpu.edu.tw")

library(tidyverse)

# 讀取資料
tidy_data <- read_csv("臺北市樂齡學習統計資料.csv")

# 清理資料：移除千位分隔符與百分比符號，並轉換數字類型
tidy_data <- tidy_data %>%
  mutate(
    `女` = as.numeric(str_replace_all(`女`, ",", "")),
    `男` = as.numeric(str_replace_all(`男`, ",", "")),
    `總人次` = as.numeric(str_replace_all(`總人次`, ",", "")),
    `場次` = as.numeric(`場次`),  # 確保場次是數字
    `女比率` = as.numeric(str_replace_all(`女比率`, "%", "")) / 100,
    `男比率` = as.numeric(str_replace_all(`男比率`, "%", "")) / 100
  )

# 計算每場次平均參與人數
tidy_data <- tidy_data %>%
  mutate(平均參與人數 = `總人次` / `場次`)

# 性別參與分析：轉換為長格式
gender_summary <- tidy_data %>%
  select(`名稱`, `女`, `男`) %>%
  pivot_longer(cols = c(`女`, `男`), names_to = "性別", values_to = "參與人數")

# 視覺化：性別分布
gender_summary %>%
  ggplot(aes(x = `名稱`, y = 參與人數, fill = 性別)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "各學習中心的性別參與分布",
    x = "學習中心",
    y = "參與人數"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 視覺化：平均參與人數效能比較
tidy_data %>%
  ggplot(aes(x = reorder(`名稱`, 平均參與人數), y = 平均參與人數)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "各學習中心的每場次平均參與人數",
    x = "學習中心",
    y = "平均參與人數"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

