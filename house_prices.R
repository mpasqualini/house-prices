library(dplyr)
library(ggplot2)
library(scales)

# Leitura dos arquivos----
train <- read.csv(file = "./data/train.csv", stringsAsFactors = FALSE)
test <- read.csv(file = "./data/test.csv", stringsAsFactors = FALSE)

dim(train) # dimensao do conjunto de treino
str(train) # estrutura do conjunto de treino

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

cor(train$SalePrice, train$LotArea)
cor(train$SalePrice, train$X1stFlrSF)
cor(train$SalePrice, train$X2ndFlrSF)
cor(train$SalePrice, train$GarageCars)
cor(train$SalePrice, train$GarageArea)
cor(train$SalePrice, train$TotalBsmtSF)
