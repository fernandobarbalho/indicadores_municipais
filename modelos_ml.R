# Load libraries
library(caret)
library(randomForest)
library(xgboost)
library(tidyverse)

indicadores_municipios_export <- read_excel("indicadores_municipios.xlsx")


dados_modelo<-
indicadores_municipios_export %>%
  select(sigla_uf,normalizado_0_100_sdg3_27_dsp_sau:normalizado_0_100_indice_qualidade_informacao_contabil) %>%
  readr::write_csv("sample_indicadores.csv")

# Remove any rows with missing values (if necessary)
data <- na.omit(dados_modelo)

# Convert 'sigla_uf' to a factor (if it's not already)
data$sigla_uf <- as.factor(data$sigla_uf)

# Separate features and target variable
features <- data %>% select(-sigla_uf)
target <- data$sigla_uf


# Train a decision tree model
set.seed(123)  # For reproducibility
dt_model <- train(sigla_uf ~ ., data = data, method = "rpart", trControl = trainControl(method = "cv", number = 5))

# Extract feature importance
dt_importance <- varImp(dt_model)


# Plot feature importance
ggplot(dt_importance, aes(x = reorder(rownames(dt_importance), y = Overall)) +
         geom_bar(stat = "identity") +
         coord_flip() +
         labs(title = "Feature Importance - Decision Tree", x = "Features", y = "Importance")