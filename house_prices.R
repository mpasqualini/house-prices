library(dplyr)
library(tidyr)
library(ggplot2)
library(ggcorrplot)
library(scales)
library(FactoMineR)

# Leitura dos arquivos----
train <- read.csv(file = "./data/train.csv", stringsAsFactors = FALSE)
test <- read.csv(file = "./data/test.csv", stringsAsFactors = FALSE)

dim(train) # dimensao do conjunto de treino
str(train) # estrutura do conjunto de treino

train <- train %>% 
          select(-Id)

# Analise exploratoria----
train %>% 
    ggplot(aes(x = SalePrice)) +
      geom_histogram(binwidth = 10000) +
        scale_x_continuous(labels = comma) +
          theme_bw() +
            theme(panel.grid = element_blank())

train %>% 
  ggplot(aes(x = as.factor(Neighborhood), y = SalePrice)) +
    geom_boxplot() +
      scale_y_continuous(labels = comma) +
        theme_bw() +
          theme(panel.grid = element_blank())

train %>% 
  ggplot(aes(x = as.factor(Neighborhood))) +
    geom_bar()

correl <- train %>% 
            select_if(is.numeric) %>% 
              cor()

ggcorrplot(correl, type="lower")

# Inputation----

train <- train %>% 
          mutate(PoolQC = case_when(is.na(PoolQC) ~ "No pool"),
                 MiscFeature = case_when(is.na(MiscFeature) ~ "None"),
                 Fence = case_when(is.na(Fence) ~ "No fence"),
                 FireplaceQu = case_when(is.na(FireplaceQu) ~ "No fireplace"),
                 Alley = case_when(is.na(Alley) ~ "No alley access"))

train <- train %>% 
  mutate(PoolQC = ifelse(is.na(PoolQC), "No pool", PoolQC),
         MiscFeature = ifelse(is.na(MiscFeature), "None", MiscFeature),
         Fence = ifelse(is.na(Fence), "No fence", Fence),
         FireplaceQu = ifelse(is.na(FireplaceQu), "No fireplace", FireplaceQu),
         Alley = ifelse(is.na(Alley), "No alley access", Alley))
