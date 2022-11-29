library(tidyverse)
library(factoextra)
library(cptcity)
library(ggview)

# Reading spatial data ----------------------------------------------------
data <- read_csv("../processed_data/db_variables.csv") %>% 
  select(-lat,-lon,-village) %>% 
  as.data.frame()
rownames(data) <- data$codigo

# PCA analysis  -----------------------------------------------------------
datafinal <- data %>% select(-codigo) %>% 
  scale(center = T,scale = T) 
PCA <- prcomp(datafinal,center = FALSE,scale. = FALSE)
summary(PCA)

p1 <- fviz_pca_var(
  PCA,
  col.var = "contrib", # Color by contributions to the PC
  gradient.cols = cpt(pal = "mpl_viridis",rev = 1),
  repel = TRUE     # Avoid text overlapping
)

ggsave(
  filename = "../graphics/pca_plot.png",
  plot = last_plot(),
  width = 8,
  height = 8,bg = "white")

dbPCA  <- PCA$x[,1:3] %>% 
  as.data.frame() %>%  
  mutate(
    codigo = data$codigo,
    index = PC1 + PC2 + PC3
    )
original_data <- read_csv("../processed_data/db_variables.csv")
final_data <- left_join(
  original_data,
  dbPCA,
  "codigo"
  ) %>% 
  mutate(
    quartile = ntile(index, 4),
    class = case_when(
      quartile == 1 ~ "bajo",
      quartile == 2 ~ "medio",
      quartile == 3 ~ "alto",
      quartile == 4 ~ "muy alto"
    )
    )
# Save original data 
write_csv(final_data,"../processed_data/db_variables_with_PCA.csv")
# Random selection 
set.seed(2022)
moderate <- sample_n(final_data %>% filter(class == "bajo"),5)
proximate <- sample_n(final_data %>% filter(class == "medio"),5)
distant <- sample_n(final_data %>% filter(class == "alto"),5)
extradistant <- sample_n(final_data %>% filter(class == "muy alto"),5)

# Final dataset of villages
villages_sample <- bind_rows(moderate,proximate,distant,extradistant)
write_csv(villages_sample,"../processed_data/villages_selected_sample.csv")
