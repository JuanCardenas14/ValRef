library(tidyr)
library(dplyr)

# Grupos

Nativos <- ValoresRev %>%
  filter(sex_sub_e1 == 1, alt_ada_e1 == 1)

Aclimatados <- ValoresRev %>%
  filter(sex_sub_e1 == 1, alt_ada_e1 == 0)


Nativas <- ValoresRev %>%
  filter(sex_sub_e1 == 0, alt_ada_e1 == 1)

Aclimatadas <- ValoresRev %>%
  filter(sex_sub_e1 == 0, alt_ada_e1 == 0)

# Shapiro

# Nativos

Datos_nativos<- as.list(Nativos[,c(11:30,32:38, 40:49)])

Normalidad_nativos <- lapply(names(Datos_nativos), function (x)
       shapiro.test(Datos_nativos[[x]]))

names(Normalidad_nativos) <- names(Datos_nativos)

Normalidad_nat_p<- lapply(names(Normalidad_nativos), function(x)
  Normalidad_nativos[[x]]$p.value)

names(Normalidad_nat_p) <- names(Datos_nativos)


# Aclimatados

Datos_aclimatados<- as.list(Aclimatados[,c(11:30,32:38, 40:49)])

Normalidad_aclimatados <- lapply(names(Datos_aclimatados), function (x)
  shapiro.test(Datos_aclimatados[[x]]))

names(Normalidad_aclimatados) <- names(Datos_aclimatados)

Normalidad_acl_p<- lapply(names(Normalidad_aclimatados), function(x)
  Normalidad_aclimatados[[x]]$p.value)

names(Normalidad_acl_p) <- names(Datos_aclimatados)


# Nativas


Datos_nativas<- as.list(Nativas[,c(11:30,32:39, 41:45, 48:49)])

Normalidad_nativas <- lapply(names(Datos_nativas), function (x)
  shapiro.test(Datos_nativas[[x]]))

names(Normalidad_nativas) <- names(Datos_nativas)

Normalidad_nata_p<- lapply(names(Normalidad_nativas), function(x)
  Normalidad_nativas[[x]]$p.value)

names(Normalidad_nata_p) <- names(Datos_nativas)


# Aclimatadas

Datos_aclimatadas<- as.list(Aclimatadas[,c(11:30,32:39, 41:45, 48:49)])

Normalidad_aclimatadas <- lapply(names(Datos_aclimatadas), function (x)
  shapiro.test(Datos_aclimatadas[[x]]))

names(Normalidad_aclimatadas) <- names(Datos_aclimatadas)

Normalidad_acla_p<- lapply(names(Normalidad_aclimatadas), function(x)
  Normalidad_aclimatadas[[x]]$p.value)

names(Normalidad_acla_p) <- names(Datos_aclimatadas)

### Crear los data frames

# Nativos
nombres_nat <- names(Datos_nativos)

Shap_nativos<- data.frame(t(matrix(unlist(Normalidad_nat_p))))

Shap_nativos <- Shap_nativos %>%
  setNames(nombres_nat)


# Aclimatados

nombres_acl <- names(Datos_aclimatados)

Shap_aclimatados<- data.frame(t(matrix(unlist(Normalidad_acl_p))))

Shap_aclimatados <- Shap_aclimatados %>%
  setNames(nombres_acl)


#Aclimatadas

nombres_acla <- names(Datos_aclimatadas)

Shap_aclimatadas<- data.frame(t(matrix(unlist(Normalidad_acla_p))))

Shap_aclimatadas <- Shap_aclimatadas %>%
  setNames(nombres_acla)

# Nativas

nombres_nata <- names(Datos_nativas)

Shap_nativas<- data.frame(t(matrix(unlist(Normalidad_nata_p))))

Shap_nativas <- Shap_nativas %>%
  setNames(nombres_nata)

## crear los excel

Hombres <- rbind(Shap_nativos, Shap_aclimatados)
rownames(Hombres) <- c("Nativos", "Aclimatados")
write.csv(Hombres, "Shapiro_hombres.csv")

Mujeres <- rbind(Shap_nativas, Shap_aclimatadas)
rownames(Mujeres) <- c("Nativas", "Aclimatadas")
write.csv(Mujeres, "Shapiro_mujeres.csv")

