# Load libraries
library(caret)
library(randomForest)
library(xgboost)
library(tidyverse)
library(readxl)
library(rattle)

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

rattle::fancyRpartPlot(dt_model$finalModel)

# Extract feature importance
dt_importance <- varImp(dt_model)


# Plot feature importance
ggplot(dt_importance, aes(x = reorder(rownames(dt_importance), y = Overall))) +
         geom_bar(stat = "identity") +
         coord_flip() +
         labs(title = "Feature Importance - Decision Tree", x = "Features", y = "Importance")


indicadores_municipios_export %>%
  ggplot(aes(x=sigla_uf,y= normalizado_0_100_quantidade_desastres_2023)) +
  geom_jitter(color= "gray")+
  geom_boxplot(fill=NA, outlier.shape = NA)


modelo_lm_desastre<- lm(normalizado_0_100_quantidade_desastres_2023~sigla_uf, data = indicadores_municipios_export )

summary(modelo_lm_desastre)


modelo_lm_info_contabil<- lm(normalizado_0_100_indice_qualidade_informacao_contabil~sigla_uf, data = indicadores_municipios_export )

summary(modelo_lm_info_contabil)

indicadores_municipios_export %>%
  ggplot(aes(x=sigla_uf,y= normalizado_0_100_indice_qualidade_informacao_contabil)) +
  geom_jitter(color= "gray")+
  geom_boxplot(fill=NA, outlier.shape = NA)


modelo_lm_sdg_32_ubs<- lm(normalizado_0_100_sdg3_32_ubs~sigla_uf, data = indicadores_municipios_export )

summary(modelo_lm_sdg_32_ubs)

indicadores_municipios_export %>%
  ggplot(aes(x=sigla_uf,y= normalizado_0_100_sdg3_32_ubs)) +
  geom_jitter(color= "gray")+
  geom_boxplot(fill=NA, outlier.shape = NA)


modelo_lm_servidores_per_capita<- lm(normalizado_0_100_servidores_per_capita~sigla_uf, data = indicadores_municipios_export )

summary(modelo_lm_servidores_per_capita)

indicadores_municipios_export %>%
  ggplot(aes(x=sigla_uf,y= normalizado_0_100_servidores_per_capita)) +
  geom_jitter(color= "gray")+
  geom_boxplot(fill=NA, outlier.shape = NA)


