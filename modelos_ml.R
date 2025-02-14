# Load libraries
library(caret)
library(randomForest)
library(xgboost)
library(tidyverse)
library(readxl)
library(rattle)
library(FactoMineR)  # For PCA
library(factoextra)  # For visualization


indicadores_municipios_export <- read_excel("indicadores_municipios.xlsx")


dados_modelo<-
indicadores_municipios_export %>%
  select(sigla_uf,normalizado_0_100_sdg3_27_dsp_sau:normalizado_0_100_indice_qualidade_informacao_contabil) 

dados_modelo_pca<-
  indicadores_municipios_export %>%
  select(nome,sigla_uf,normalizado_0_100_sdg3_27_dsp_sau:normalizado_0_100_indice_qualidade_informacao_contabil) 


######################### Decision Tree

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



#### Random Forest

data$sigla_uf <- as.factor(data$sigla_uf)

set.seed(123)
rf_model <- randomForest(sigla_uf ~ ., data = data, importance = TRUE, ntree = 500)

# Extract feature importance
rf_importance <- randomForest::importance(rf_model)

# Convert to a data frame for plotting
rf_importance_df <- data.frame(Feature = rownames(rf_importance), Importance = rf_importance[, "MeanDecreaseGini"])

# Plot feature importance
ggplot(rf_importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Feature Importance - Random Forest", x = "Features", y = "Importance")


modelo_lm_proporcao_gestao_publica_pib<- lm(normalizado_0_100_proporcao_gestao_publica_pib~sigla_uf, data = indicadores_municipios_export )

summary(modelo_lm_proporcao_gestao_publica_pib)

indicadores_municipios_export %>%
  ggplot(aes(x=sigla_uf,y= normalizado_0_100_proporcao_gestao_publica_pib)) +
  geom_jitter(color= "gray")+
  geom_boxplot(fill=NA, outlier.shape = NA)


mun_sel<- 
  indicadores_municipios_export %>%
  mutate(escopo = "todos municípios") %>%
  filter(sigla_uf=="SP")      


mun_sel_gray<- 
  indicadores_municipios_export %>%
  mutate(escopo = "todos municípios") %>%
  filter(sigla_uf!="SP")      



indicadores_municipios_export %>%
  mutate(escopo = "todos municípios") %>%
  ggplot(aes(x=escopo,y= normalizado_0_100_proporcao_gestao_publica_pib)) +
  geom_jitter(data= mun_sel_gray, color= "gray")+
  geom_jitter(data= mun_sel, color= "red")+
  geom_boxplot(fill=NA, outlier.shape = NA)





modelo_lm_sdg3_27_dsp_sau<- lm(normalizado_0_100_sdg3_27_dsp_sau~sigla_uf, data = indicadores_municipios_export )

summary(modelo_lm_sdg3_27_dsp_sau)

indicadores_municipios_export %>%
  ggplot(aes(x=sigla_uf,y= normalizado_0_100_sdg3_27_dsp_sau)) +
  geom_jitter(color= "gray")+
  geom_boxplot(fill=NA, outlier.shape = NA)



##### análise PCA

# Remove any rows with missing values (if necessary)
data <- na.omit(dados_modelo_pca)


# Separate features and target variable
features <- data %>% select(-c(nome,sigla_uf))
#target <- data$sigla_uf



# Perform PCA
pca_result <- PCA(features, scale.unit = TRUE, ncp = 5, graph = FALSE)

# Summary of PCA results
summary(pca_result)

# Visualize the eigenvalues (scree plot)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Visualize the contributions of variables to the principal components
fviz_contrib(pca_result, choice = "var", axes = 1, top = 10)  # Contributions to PC1
fviz_contrib(pca_result, choice = "var", axes = 2, top = 10)  # Contributions to PC2

# Biplot of individuals and variables
fviz_pca_biplot(pca_result, 
                col.ind = data$sigla_uf,  # Color by state
                palette = "jco",          # Color palette
                addEllipses = TRUE,       # Add ellipses around groups
                repel = TRUE              # Avoid label overlap
)

# Extract the principal components
pca_components <- pca_result$ind$coord

# Add the principal components to the original dataset
data_with_pca <- cbind(data, pca_components)

# View the dataset with PCA components
head(data_with_pca)
