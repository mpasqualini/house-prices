library(dplyr)
library(tidyr)
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(scales)
library(FactoMineR)

# Leitura dos arquivos----
train <- read.csv(file = "./data/train.csv", stringsAsFactors = FALSE)
test <- read.csv(file = "./data/test.csv", stringsAsFactors = FALSE)

train <- train %>% 
          select(-Id)

dim(train) # dimensao do conjunto de treino
str(train) # estrutura do conjunto de treino

# Missing values----

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

train_wt_na <- train %>% 
  drop_na()

train_wt_na %>% 
  lapply(class) 

train_wt_na <- train_wt_na %>% 
  mutate_if(is.character, as.factor)

train_wt_na <- train_wt_na %>% 
                  mutate(YearBuilt = as.factor(YearBuilt),
                         YrSold = as.factor(YrSold),
                         YearRemodAdd = as.factor(YearRemodAdd),
                         MoSold = as.factor(MoSold),
                         MSSubClass = as.factor(MSSubClass))
# it is a factor:
## mssubclass
## overralqual - ordinal
## overralcond - ordinal
## yearbuilt
## yearremodadd
## garageyrblt
## mosold
## yrsold

# EDA----
train %>% 
    ggplot(aes(x = SalePrice)) +
      geom_histogram() +
        scale_x_continuous(labels = comma) +
          theme_bw() +
            theme(panel.grid = element_blank())

train %>% 
  ggplot(aes(x = as.factor(Neighborhood), y = SalePrice)) +
    geom_col() +
      scale_y_continuous(labels = comma) +
        theme_bw() +
          theme(panel.grid = element_blank())

train %>% 
  ggplot(aes(x = LotArea, y = SalePrice)) +
    geom_point()
  
correl <- train %>% 
            select_if(is.numeric) %>% 
              cor()

correl <- train %>% 
  select_if(is.numeric) %>% 
    cor()

ggcorrplot(correl, type="lower")

train %>% 
  ggplot(aes(x = as.factor(OverallQual), y = SalePrice)) +
    geom_boxplot()

train %>% 
  ggplot(aes(x = as.factor(MSSubClass), y = SalePrice)) +
    geom_boxplot()

# tendencia de aumento ao longo dos anos de yearbuilt e saleprice
train %>% 
  ggplot(aes(x = YearBuilt, y = SalePrice)) +
    geom_line()

train %>% 
  ggplot(aes(x = as.factor(MSSubClass))) +
    geom_bar()
