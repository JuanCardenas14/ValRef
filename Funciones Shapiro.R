library(tidyr)
library(dplyr)
library(readxl)

Hombres <- read_excel("D:/Informacion/Escritorio/Valref_males.xlsx")
Mujeres <- read_excel("D:/Informacion/Escritorio/Valref_females.xlsx")


# ----- 1. CREAR LOS GRUPOS ----  
Nativos <- Hombres %>%
  filter(sex_sub_e1 == 1, alt_ada_e1 == 1)

Aclimatados <- Hombres %>%
  filter(sex_sub_e1 == 1, alt_ada_e1 == 0)


Nativas <- Mujeres %>%
  filter(sex_sub_e1 == 0, alt_ada_e1 == 1)

Aclimatadas <- Mujeres %>%
  filter(sex_sub_e1 == 0, alt_ada_e1 == 0)


# ----- 2.  SHAPIRO ------

# 2.1 Hombres

# 2.1.1 Todos los hombres

Datos_hombres <- as.list(Hombres[,c(11:17, 20:23, 27:30,32:38, 40:33, 46:49, 166:184,
                                    186:194, 196:201)])


Normalidad_hombres <- lapply(names(Datos_hombres), function (x)
  shapiro.test(Datos_hombres[[x]]))

names(Normalidad_hombres) <- names(Datos_hombres)

Normalidad_males_p<- lapply(names(Normalidad_hombres), function(x)
  Normalidad_hombres[[x]]$p.value)

names(Normalidad_males_p) <- names(Datos_hombres)

# 2.1.2 Nativos

Datos_nativos<- as.list(Nativos[,c(11:17, 20:23, 27:30,32:38, 40:33, 46:49, 166:184,
                                           186:194, 196:201)])

Normalidad_nativos <- lapply(names(Datos_nativos), function (x)
       shapiro.test(Datos_nativos[[x]]))

names(Normalidad_nativos) <- names(Datos_nativos)

Normalidad_nat_p<- lapply(names(Normalidad_nativos), function(x)
  Normalidad_nativos[[x]]$p.value)

names(Normalidad_nat_p) <- names(Datos_nativos)


# 2.1.3 Aclimatados

Datos_aclimatados<- as.list(Aclimatados[,c(11:17, 20:23, 27:30,32:38, 40:33, 46:49, 166:184,
                                           186:194, 196:201)])

Normalidad_aclimatados <- lapply(names(Datos_aclimatados), function (x)
  shapiro.test(Datos_aclimatados[[x]]))

names(Normalidad_aclimatados) <- names(Datos_aclimatados)

Normalidad_acl_p<- lapply(names(Normalidad_aclimatados), function(x)
  Normalidad_aclimatados[[x]]$p.value)

names(Normalidad_acl_p) <- names(Datos_aclimatados)

# 2.2 Mujeres

# 2.2.1 Todas las mujeres

Datos_mujeres <- as.list(Mujeres[,c(11:17, 20:23, 27:30,32:38, 40:33, 46:49, 166:184,
                                    186:194, 196:199)])

Normalidad_mujeres <- lapply(names(Datos_mujeres), function (x)
  shapiro.test(Datos_mujeres[[x]]))

names(Normalidad_mujeres) <- names(Datos_mujeres)

Normalidad_females_p<- lapply(names(Normalidad_mujeres), function(x)
  Normalidad_mujeres[[x]]$p.value)

names(Normalidad_females_p) <- names(Datos_mujeres)

# 2.2.2 Nativas


Datos_nativas<- as.list(Nativas[,c(11:17, 20:23, 27:30,32:38, 40:33, 46:49, 166:184,
                                    186:194, 196:199)])

Normalidad_nativas <- lapply(names(Datos_nativas), function (x)
  shapiro.test(Datos_nativas[[x]]))

names(Normalidad_nativas) <- names(Datos_nativas)

Normalidad_nata_p<- lapply(names(Normalidad_nativas), function(x)
  Normalidad_nativas[[x]]$p.value)

names(Normalidad_nata_p) <- names(Datos_nativas)


# 2.2.3 Aclimatadas

Datos_aclimatadas<- as.list(Aclimatadas[,c(11:17, 20:23, 27:30,32:38, 40:33, 46:49, 166:184,
                                           186:194, 196:199)])

Normalidad_aclimatadas <- lapply(names(Datos_aclimatadas), function (x)
  shapiro.test(Datos_aclimatadas[[x]]))

names(Normalidad_aclimatadas) <- names(Datos_aclimatadas)

Normalidad_acla_p<- lapply(names(Normalidad_aclimatadas), function(x)
  Normalidad_aclimatadas[[x]]$p.value)

names(Normalidad_acla_p) <- names(Datos_aclimatadas)

# ---- 3. DATA FRAMES ------

# 3.1 Hombres

# 3.1.1 Todos los hombres

nombres_males <- names(Datos_hombres)

Shap_males<- data.frame(t(matrix(unlist(Normalidad_males_p))))

Shap_males <- Shap_males %>%
  setNames(nombres_males)


# 3.1.2 Nativos
nombres_nat <- names(Datos_nativos)

Shap_nativos<- data.frame(t(matrix(unlist(Normalidad_nat_p))))

Shap_nativos <- Shap_nativos %>%
  setNames(nombres_nat)


# 3.1.3 Aclimatados

nombres_acl <- names(Datos_aclimatados)

Shap_aclimatados<- data.frame(t(matrix(unlist(Normalidad_acl_p))))

Shap_aclimatados <- Shap_aclimatados %>%
  setNames(nombres_acl)

# 3.2 Mujeres

# 3.2.1 Todas las mujeres

nombres_females <- names(Datos_mujeres)

Shap_females<- data.frame(t(matrix(unlist(Normalidad_females_p))))

Shap_females <- Shap_females %>%
  setNames(nombres_females)

# 3.2.2 Aclimatadas

nombres_acla <- names(Datos_aclimatadas)

Shap_aclimatadas<- data.frame(t(matrix(unlist(Normalidad_acla_p))))

Shap_aclimatadas <- Shap_aclimatadas %>%
  setNames(nombres_acla)

# 3.2.3 Nativas

nombres_nata <- names(Datos_nativas)

Shap_nativas<- data.frame(t(matrix(unlist(Normalidad_nata_p))))

Shap_nativas <- Shap_nativas %>%
  setNames(nombres_nata)

# ---- 4. EXṔORTAR EN CSV ------

Males <- rbind(Shap_males, Shap_nativos, Shap_aclimatados)
rownames(Males) <- c("Todos","Nativos", "Aclimatados")
write.csv(Males, "Shapiro_hombres.csv")

Females <- rbind(Shap_females, Shap_nativas, Shap_aclimatadas)
rownames(Females) <- c("Todas", "Nativas", "Aclimatadas")
write.csv(Females, "Shapiro_mujeres.csv")

