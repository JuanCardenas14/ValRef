library(readxl)
library(ggplot2)
library(reshape2)
library(tibble)
library(tidyr)
library(dplyr)

# ---- 1. GRÁFICAS HOMBRES -----

Hombres <- read_excel("D:/Informacion/Escritorio/Valref_males.xlsx")
Comparaciones_hombres<- read_excel("Comparaciones_hombres_todos.xlsx", sheet = 2)
Correlaciones_hombres <- read_excel("Correlaciones_hombres_todos.xlsx", sheet = 2)


# ---- 1.1 VO2 absoluto -----
Comparaciones_hombres_1<- Comparaciones_hombres[c(1:27),] %>% 
  mutate(p_value = if_else(pvalue_N < 0.001, "p < 0.001",
                           if_else(pvalue_N < 0.01, "p < 0.01",
                                   if_else(pvalue_N < 0.05, "p < 0.05",
                                           if_else(pvalue_N >= 0.05, "NS", "NA"))))) %>% 
  filter(p_value == "NS", variable != "vo2_20s_x_pkr_dx")


Correlaciones_hombres_1<- Correlaciones_hombres[c(1:27),] %>% 
  filter(pvalue < 0.05, r_value > 0.5) %>% 
  filter(variable != "vo2_20s_x_pkr_dx")


Datos_hombres_VO2 <- melt(Hombres[,c(124, 166:184, 186:192)])

Datos_hombres_VO2_1 <- Datos_hombres_VO2 %>%
  mutate (real = if_else(variable == "vo2_20s_x_pkr_dx", "Measured VO2pk", 
                         "Variable"))


Datos_hombres_VO2_1$variable<- recode(Datos_hombres_VO2_1$variable, 
                             "vo2_20s_x_pkr_dx"= "Measured peak VO2")
  
plot_VO2_males<- Datos_hombres_VO2_1 %>% 
  ggplot(aes(variable, value, fill = real)) + 
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "none") +
  geom_text(data = Comparaciones_hombres_1, 
            label = Comparaciones_hombres_1$p_value, 
            size = 3, col = "darkblue", fontface = "bold") +
  geom_text(data = Correlaciones_hombres_1, 
            label = round (Correlaciones_hombres_1$r_value, 2), size = 3,
            col = "darkgreen", fontface = "bold") +
  annotate(geom = "label", y = 1600, x = 1, label = "Difference", 
           size = 2, fontface = "bold", fill = "darkblue", colour = "white")+
  annotate(geom = "label", y = 1400, x = 1, label = "Correlation", 
           size = 2, fontface = "bold", fill = "darkgreen", colour = "white") +
  labs(title = expression(paste("Measured vs. predicted ", dot(V), "O2"["pk"], ", males")),
       y = expression(paste(dot(V), "O2"["pk"], " [mL"%.%"min"^{"-1"},"]")),
       x = " ")

ggsave(plot_VO2_males, filename = "./Plots/plot_VO2_males.png",
       dpi = "retina", width = 12, height = 6)

# ----- 1.2 VO2 por kilo ------

Comparaciones_hombres_2<- Comparaciones_hombres[c(28:36),] %>% 
  mutate(p_value = if_else(pvalue_N < 0.001, "p < 0.001",
                           if_else(pvalue_N < 0.01, "p < 0.01",
                                   if_else(pvalue_N < 0.05, "p < 0.05",
                                           if_else(pvalue_N >= 0.05, "NS", "NA"))))) %>% 
  filter(p_value == "NS", variable != "vo2k_20s_x_pkr_dx")

Correlaciones_hombres_2<- Correlaciones_hombres[c(28:36),] %>% 
  filter(pvalue < 0.05, r_value > 0.5) %>% 
  filter(variable != "vo2k_20s_x_pkr_dx")

Datos_hombres_VO2_kg <- melt(Hombres[,c(125, 193, 194, 196:201)])

Datos_hombres_VO2_kg_1 <- Datos_hombres_VO2_kg %>%
  mutate (real = if_else(variable == "vo2k_20s_x_pkr_dx", "Measured VO2pk", 
                         "Variable"))


Datos_hombres_VO2_kg_1$variable<- recode(Datos_hombres_VO2_kg_1$variable, 
                                      "vo2k_20s_x_pkr_dx"="Measured VO2pk-kg")

plot_VO2_kg_males<- Datos_hombres_VO2_kg_1 %>% 
  ggplot(aes(variable, value, fill = real)) + 
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "none") +
  geom_text(data = Comparaciones_hombres_2, 
            label = Comparaciones_hombres_2$p_value, 
            size = 3, col = "darkblue", fontface = "bold") +
  geom_text(data = Correlaciones_hombres_2, 
            label = round (Correlaciones_hombres_2$r_value, 2), size = 3,
            col = "darkgreen", fontface = "bold") +
  annotate(geom = "label", y = 5, x = 1, label = "Difference", 
           size = 2, fontface = "bold", fill = "darkblue", colour = "white")+
  annotate(geom = "label", y = 0, x = 1, label = "Correlation", 
           size = 2, fontface = "bold", fill = "darkgreen", colour = "white") +
  labs(title = expression(paste("Measured vs. predicted ", dot(V), "O2"["pk-kg"], ", males")),
       y = expression(paste(dot(V), "O2"["pk-kg"], " [mL"%.%"min"^{"-1"}%.%"kg"^{"-1"},"]")),
       x = " ")

ggsave(plot_VO2_kg_males, filename = "./Plots/plot_VO2_kg_males.png",
       dpi = "retina", width = 12, height = 6)


# ----- 2. GRÁFICAS MUJERES -------

Mujeres <- read_excel("D:/Informacion/Escritorio/Valref_females.xlsx")
Comparaciones_mujeres<- read_excel("Comparaciones_mujeres_todos.xlsx", sheet = 2)
Correlaciones_mujeres <- read_excel("Correlaraciones_mujeres_todos.xlsx", sheet = 2)

# ---- 1.1 VO2 absoluto -----

Comparaciones_mujeres_1<- Comparaciones_mujeres[c(1:27),] %>% 
  mutate(p_value = if_else(pvalue_N < 0.001, "p < 0.001",
                           if_else(pvalue_N < 0.01, "p < 0.01",
                                   if_else(pvalue_N < 0.05, "p < 0.05",
                                           if_else(pvalue_N >= 0.05, "NS", "NA"))))) %>% 
  filter(p_value == "NS", variable != "vo2_20s_x_pkr_dx")


Correlaciones_mujeres_1<- Correlaciones_mujeres[c(1:27),] %>% 
  filter(pvalue < 0.05, r_value > 0.5) %>% 
  filter(variable != "vo2_20s_x_pkr_dx")

Comparaciones_mujeres_1$value <- as.numeric (Comparaciones_mujeres_1$value)

Datos_mujeres_VO2 <- melt(Mujeres[,c(124, 166:184, 186:192)])

Datos_mujeres_VO2_1 <- Datos_mujeres_VO2 %>%
  mutate (real = if_else(variable == "vo2_20s_x_pkr_dx", "Measured VO2pk", 
                         "Variable"))


Datos_mujeres_VO2_1$variable<- recode(Datos_mujeres_VO2_1$variable, 
                                      "vo2_20s_x_pkr_dx"= "Measured peak VO2")

plot_VO2_females<- Datos_mujeres_VO2_1 %>% 
  ggplot(aes(variable, value, fill = real)) + 
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "none") +
  geom_text(data = Comparaciones_mujeres_1, 
            label = Comparaciones_mujeres_1$p_value, 
            size = 3, col = "darkblue", fontface = "bold") +
  geom_text(data = Correlaciones_mujeres_1, 
            label = round (Correlaciones_mujeres_1$r_value, 2), size = 3,
            col = "darkgreen", fontface = "bold") +
  annotate(geom = "label", y = 500, x = 1, label = "Difference", 
           size = 2, fontface = "bold", fill = "darkblue", colour = "white")+
  annotate(geom = "label", y = 300, x = 1, label = "Correlation", 
           size = 2, fontface = "bold", fill = "darkgreen", colour = "white") +
  labs(title = expression(paste("Measured vs. predicted ", dot(V), "O2"["pk"], ", females")),
       y = expression(paste(dot(V), "O2"["pk"], " [mL"%.%"min"^{"-1"},"]")),
       x = " ")

ggsave(plot_VO2_females, filename = "./Plots/plot_VO2_females.png",
       dpi = "retina", width = 12, height = 6)

# ----- 1.2 VO2 por kilo ------

Comparaciones_mujeres_2<- Comparaciones_mujeres[c(28:34),] %>% 
  mutate(p_value = if_else(pvalue_N < 0.001, "p < 0.001",
                           if_else(pvalue_N < 0.01, "p < 0.01",
                                   if_else(pvalue_N < 0.05, "p < 0.05",
                                           if_else(pvalue_N >= 0.05, "NS", "NA"))))) %>% 
  filter(p_value == "NS", variable != "vo2k_20s_x_pkr_dx")

Comparaciones_mujeres_2$value <- as.numeric (Comparaciones_mujeres_2$value)

Correlaciones_mujeres_2<- Correlaciones_mujeres[c(28:34),] %>% 
  filter(pvalue < 0.05, r_value > 0.5) %>% 
  filter(variable != "vo2k_20s_x_pkr_dx")


Datos_mujeres_VO2_kg <- melt(Mujeres[,c(125, 193, 194, 196:199)])

Datos_mujeres_VO2_kg_1 <- Datos_mujeres_VO2_kg %>%
  mutate (real = if_else(variable == "vo2k_20s_x_pkr_dx", "Measured VO2pk", 
                         "Variable"))


Datos_mujeres_VO2_kg_1$variable<- recode(Datos_mujeres_VO2_kg_1$variable, 
                                         "vo2k_20s_x_pkr_dx"="Measured VO2pk-kg")

plot_VO2_kg_females<- Datos_mujeres_VO2_kg_1 %>% 
  ggplot(aes(variable, value, fill = real)) + 
  geom_boxplot() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "none") +
  geom_text(data = Comparaciones_mujeres_2, 
            label = Comparaciones_mujeres_2$p_value, 
            size = 3, col = "darkblue", fontface = "bold") +
  geom_text(data = Correlaciones_mujeres_2, 
            label = round (Correlaciones_mujeres_2$r_value, 2), size = 3,
            col = "darkgreen", fontface = "bold") +
  annotate(geom = "label", y = 5, x = 1, label = "Difference", 
           size = 2, fontface = "bold", fill = "darkblue", colour = "white")+
  annotate(geom = "label", y = 0, x = 1, label = "Correlation", 
           size = 2, fontface = "bold", fill = "darkgreen", colour = "white") +
  labs(title = expression(paste("Measured vs. predicted ", dot(V), "O2"["pk-kg"], ", females")),
       y = expression(paste(dot(V), "O2"["PK-KG"], " [mL"%.%"min"^{"-1"}%.%"kg"^{"-1"},"]")),
       x = " ")

ggsave(plot_VO2_kg_females, filename = "./Plots/plot_VO2_kg_females.png",
       dpi = "retina", width = 12, height = 6)

