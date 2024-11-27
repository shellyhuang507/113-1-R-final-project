library(remotes)
install.packages("remotes")
remotes::install_github("tpemartin/ntpuR")

library(ntpuR)

setup_github_personal_access_token()

usethis::create_github_token()

# Load the tidyverse package if not already loaded
library(tidyverse)

# Import the CSV file
tidy_higher_suspended <- read_csv("98_104-Higheruspended.csv")

# View the structure of the data
glimpse(tidy_higher_suspended)

# Rename the columns into English
native <- native %>%
  rename(
    Academic_Year = 學年,
    Leave_of_Absence_Due_to_Illness = 新增辦理休學人數_因病,
    Leave_of_Absence_Due_to_Economic_Hardship = 新增辦理休學人數_因經濟困難,
    Leave_of_Absence_Due_to_Academic_Interest = 新增辦理休學人數_因學業志趣,
    Leave_of_Absence_Due_to_Work_Demand = 新增辦理休學人數_因工作需求
  )

# Check the updated column names
glimpse(native)
