
usethis::create_github_token()

# 讀取數據
tidy_data <- read_csv("臺北市樂齡學習統計資料.csv")

# 數據清理與轉換
tidy_data <- tidy_data %>%
  mutate(
    `女` = as.numeric(str_replace_all(`女`, ",", "")), # 移除女性數據中的逗號並轉換為數字
    `男` = as.numeric(str_replace_all(`男`, ",", "")), # 移除男性數據中的逗號並轉換為數字
    `總人次` = as.numeric(str_replace_all(`總人次`, ",", "")), # 移除總人次中的逗號並轉換為數字
    `場次` = as.numeric(`場次`), # 將場次轉換為數字
    `女比率` = as.numeric(str_replace_all(`女比率`, "%", "")) / 100, # 將女性比率的百分比轉換為小數
    `男比率` = as.numeric(str_replace_all(`男比率`, "%", "")) / 100  # 將男性比率的百分比轉換為小數
  ) %>%
  mutate(平均參與人數 = `總人次` / `場次`) # 計算每場的平均參與人數

# 性別參與分析：將數據轉為長格式
gender_summary <- tidy_data %>%
  select(`名稱`, `女`, `男`) %>%
  pivot_longer(cols = c(`女`, `男`), names_to = "性別", values_to = "參與人數") # 將性別數據展平，轉為長格式

# 視覺化：各學習中心的性別參與情況
gender_summary %>%
  ggplot(aes(x = `名稱`, y = 參與人數, fill = 性別)) +
  geom_bar(stat = "identity", position = "dodge") + # 繪製分組柱狀圖
  labs(
    title = "各學習中心的性別參與情況",
    x = "學習中心名稱",
    y = "參與人數"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 將X軸文字旋轉45度以防重疊

# 視覺化：每場的平均參與人數
tidy_data %>%
  ggplot(aes(x = reorder(`名稱`, 平均參與人數), y = 平均參與人數)) +
  geom_bar(stat = "identity", fill = "steelblue") + # 繪製柱狀圖
  labs(
    title = "各學習中心每場的平均參與人數",
    x = "學習中心名稱",
    y = "每場平均參與人數"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 將X軸文字旋轉45度以防重疊

# 計算性別比例與總參與人次的關係
tidy_data <- tidy_data %>%
  mutate(
    女性比例 = `女` / `總人次`, # 計算女性比例
    男性比例 = `男` / `總人次`  # 計算男性比例
  )

# 視覺化：女性比例與總參與人次的關係
tidy_data %>%
  ggplot(aes(x = 女性比例, y = `總人次`)) +
  geom_point(color = "blue", size = 3, alpha = 0.6) + # 繪製散點圖
  geom_smooth(method = "lm", color = "red", se = TRUE) + # 添加線性回歸線
  labs(
    title = "女性比例與總參與人次的關係",
    x = "女性比例",
    y = "總參與人次"
  ) +
  theme_minimal()

# 計算相關係數
correlation <- tidy_data %>%
  summarise(correlation = cor(女性比例, `總人次`, use = "complete.obs")) # 計算女性比例與總參與人次之間的相關係數
print(correlation)

# 建立線性迴歸模型
model <- lm(`總人次` ~ 女性比例, data = tidy_data) # 建立以女性比例為自變量的線性回歸模型
summary(model) # 查看回歸模型的摘要





