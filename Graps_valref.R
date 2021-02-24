library(readxl)
library(ggplot2)
library(reshape2)
library(tibble)
library(tidyr)
library(dplyr)

Hombres <- read_excel("D:/Informacion/Escritorio/Valref_males.xlsx")
Comparaciones_hombres<- read_excel("Comparaciones_hombres_todos.xlsx", sheet = 2)

Comparaciones_hombres_1<- Comparaciones_hombres[c(1:27),] %>% 
  mutate(p_value = if_else(pvalue_N < 0.001, "p < 0.001",
                           if_else(pvalue_N < 0.01, "p < 0.01",
                                   if_else(pvalue_N < 0.05, "p < 0.05",
                                           if_else(pvalue_N >= 0.05, "NS", "NA"))))) %>% 
  filter(p_value == "NS", variable != "vo2_20s_x_pkr_dx")


Correlaciones_hombres <- read_excel("Correlaciones_hombres_todos.xlsx", sheet = 2)

Correlaciones_hombres_1<- Correlaciones_hombres[c(1:27),] %>% 
  filter(pvalue < 0.05, r_value > 0.5) %>% 
  filter(variable != "vo2_20s_x_pkr_dx")

Datos_hombres_VO2 <- melt(Hombres[,c(124, 166:184, 186:192)])

Datos_hombres_VO2_1 <- Datos_hombres_VO2 %>%
  mutate (real = if_else(variable == "vo2_20s_x_pkr_dx", "Measured VO2pk", 
                         "Variable"))


Datos_hombres_VO2_1$variable<- recode(Datos_hombres_VO2_1$variable, 
                             "vo2_20s_x_pkr_dx"="Measured VO2pk")
  
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
  labs(title = expression(paste("Measured vs. predicted ", dot(V), "O2"["pk"], ", males")))

ggsave(plot_VO2_males, filename = "./Plots/plot_VO2_males.png",
       dpi = "retina", width = 12, height = 6)
