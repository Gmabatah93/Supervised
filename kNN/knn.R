library(class)
library(readr)
library(dplyr)

# Road Signs ----

# data
signs <- read_csv("https://assets.datacamp.com/production/repositories/718/datasets/c274ea22cc3d7e12d7bb9fdc9c2bdabe9ab025f4/knn_traffic_signs.csv")

# eda
signs$sign_type %>% table()
# - avg red color
signs %>% 
  group_by(sign_type) %>% 
  summarise(avg_r10 = mean(r10))

# model
# - train
signs_train <- signs %>% 
  filter(sample == "train") %>% 
  select(-id, -sample)
sign_types_train <- signs_train$sign_type
# - test
signs_test <- signs %>% 
  filter(sample == "test") %>% 
  select(-id, -sample)
sign_types_test <- signs_test$sign_type

mod_sign <- knn(train = signs_train[-1], 
                test = signs_test[-1],
                cl = sign_types_train)

mod_7_sign <- knn(train = signs_train[-1], 
                  test = signs_test[-1],
                  cl = sign_types_train,
                  k = 7,
                  prob = TRUE)
mod_7_sign %>% head()
mod_7_sign %>% attr(which = "prob") %>% head()

mod_15_sign <- knn(train = signs_train[-1], 
                   test = signs_test[-1],
                   cl = sign_types_train,
                   k = 15)

# diagnostics
signs_actual <- signs_test$sign_type
# - cm
table(mod_sign, signs_actual)
# - accuracy
mean(mod_sign == signs_actual)
mean(mod_7_sign == signs_actual)
mean(mod_15_sign == signs_actual)
