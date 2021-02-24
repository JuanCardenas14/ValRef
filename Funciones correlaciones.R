library(tidyr)
library(dplyr)

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

# ----- 2. CORRELACIONES --------

Correlaciones <- function (x) {
    r_value <- cor.test(lista_datos[[1]], lista_datos[[x]], method = "pearson")$estimate[[1]]
    p_value <- cor.test(lista_datos[[1]], lista_datos[[x]], method = "pearson")$p.value
  return(c(r_value, p_value))
}

# ----- 3. APLICAR CORRELACIONES ---------

# 3.1 Hombres

# 3.1.1 Todos los hombres

# 3.1.1.1 VO2 absoluto

Datos_hombres_VO2 <- as.list(Hombres[,c(124, 166:184, 186:192)])

lista_datos <- Datos_hombres_VO2

Correlaciones_males_VO2 <- lapply(names(Datos_hombres_VO2), Correlaciones)

# 3.1.1.2 VO2 por kg

Datos_hombres_VO2_kg <- as.list(Hombres [,c(125, 193, 194, 196:201)])

lista_datos <- Datos_hombres_VO2_kg

Correlaciones_males_VO2_kg <- lapply(names(Datos_hombres_VO2_kg), Correlaciones)

# 3.1.2 Nativos

# 3.1.2.1 VO2 absoluto

Datos_nativos_VO2<- as.list(Nativos[,c(124, 166:184, 186:192)])

lista_datos <- Datos_nativos_VO2

Correlaciones_nativos_VO2 <- lapply(names(Datos_nativos_VO2), Correlaciones)

# 3.1.2.1 VO2 por kg

Datos_nativos_VO2_kg <- as.list(Nativos [,c(125, 193, 194, 196:201)])

lista_datos <- Datos_nativos_VO2_kg

Correlaciones_nativos_VO2_kg <- lapply(names(Datos_nativos_VO2_kg), Correlaciones)

# 3.1.3 Aclimatados

# 3.1.3.1 VO2 absoluto

Datos_aclimatados_VO2<- as.list(Aclimatados[,c(124, 166:184, 186:192)])

lista_datos <- Datos_aclimatados_VO2

Correlaciones_aclimatados_VO2 <- lapply(names(Datos_aclimatados_VO2), Correlaciones)


# 3.1.2.1 VO2 por kg

Datos_aclimatados_VO2_kg <- as.list(Aclimatados [,c(125, 193, 194, 196:201)])

lista_datos <- Datos_aclimatados_VO2_kg

Correlaciones_aclimatados_VO2_kg <- lapply(names(Datos_aclimatados_VO2_kg), Correlaciones)

# 3.2 Mujeres

# 3.2.1 Todas las mujeres

# 3.2.1.1 VO2 absoluto

Datos_mujeres_VO2 <- as.list(Mujeres[,c(124, 166:184, 186:192)])

lista_datos <- Datos_mujeres_VO2

Correlaciones_females_VO2 <- lapply(names(Datos_mujeres_VO2), Correlaciones)

# 3.2.1.1 VO2 por kg

Datos_mujeres_VO2_kg <- as.list(Mujeres [,c(125, 193, 194, 196:199)])

lista_datos <- Datos_mujeres_VO2_kg

Correlaciones_females_VO2_kg <- lapply(names(Datos_mujeres_VO2_kg), Correlaciones)

# 3.2.2 Nativas

# 3.2.2.1 VO2 absoluto

Datos_nativas_VO2<- as.list(Nativas[,c(124, 166:184, 186:192)])

lista_datos <- Datos_nativas_VO2

Correlaciones_nativas_VO2 <- lapply(names(Datos_nativas_VO2), Correlaciones)

# 3.2.2.1 VO2 por kg

Datos_nativas_VO2_kg <- as.list(Nativas [,c(125, 193, 194, 196:199)])

lista_datos <- Datos_nativas_VO2_kg

Correlaciones_nativas_VO2_kg <- lapply(names(Datos_nativas_VO2_kg), Correlaciones)

# 3.2.3 Aclimatadas

# 3.2.2.1 VO2 absoluto

Datos_aclimatadas_VO2 <- as.list(Aclimatadas[,c(124, 166:184, 186:192)])

lista_datos <- Datos_aclimatadas_VO2

Correlaciones_aclimatadas_VO2 <- lapply(names(Datos_aclimatadas_VO2), Correlaciones)

# 3.2.2.1 VO2 por kg

Datos_aclimatadas_VO2_kg <- as.list(Aclimatadas [,c(125, 193, 194, 196:199)])

lista_datos <- Datos_aclimatadas_VO2_kg

Correlaciones_aclimatadas_VO2_kg <- lapply(names(Datos_aclimatadas_VO2_kg), Correlaciones)


# ---- 4. CREAR LOS DATA FRAMES -----

# 4.1 Hombres

# 4.1.1 Todos los hombres

nombres_males_VO2 <- names(Datos_hombres_VO2)

Correl_males<- data.frame(simplify2array(Correlaciones_males_VO2))

Correl_males <- Correl_males %>%
  setNames(nombres_males_VO2)

rownames(Correl_males) <- c("r_value", "pvalue")


nombres_males_VO2_kg <- names(Datos_hombres_VO2_kg)

Correl_males_kg<- data.frame(simplify2array(Correlaciones_males_VO2_kg))

Correl_males_kg <- Correl_males_kg %>%
  setNames(nombres_males_VO2_kg)

rownames(Correl_males_kg) <- c("r_value", "pvalue")

Correl_males_all<- bind_cols(Correl_males, Correl_males_kg)

# 4.1.2 Nativos
nombres_nat_VO2 <- names(Datos_nativos_VO2)

Correl_nat<- data.frame(simplify2array(Correlaciones_nativos_VO2))

Correl_nat <- Correl_nat %>%
  setNames(nombres_nat_VO2)

rownames(Correl_nat) <- c("r_value", "pvalue")

nombres_nat_VO2_kg <- names(Datos_nativos_VO2_kg)

Correl_nat_kg<- data.frame(simplify2array(Correlaciones_nativos_VO2_kg))

Correl_nat_kg <- Correl_nat_kg %>%
  setNames(nombres_nat_VO2_kg)

rownames(Correl_nat_kg) <- c("r_value", "pvalue")

Correl_nat_all<- bind_cols(Correl_nat, Correl_nat_kg)

# 4.1.3 Aclimatados

nombres_acl_VO2 <- names(Datos_aclimatados_VO2)

Correl_acl<- data.frame(simplify2array(Correlaciones_aclimatados_VO2))

Correl_acl <- Correl_acl %>%
  setNames(nombres_nat_VO2)

rownames(Correl_acl) <- c("r_value", "pvalue")

nombres_acl_VO2_kg <- names(Datos_aclimatados_VO2_kg)

Correl_acl_kg<- data.frame(simplify2array(Correlaciones_aclimatados_VO2_kg))

Correl_acl_kg <- Correl_acl_kg %>%
  setNames(nombres_nat_VO2_kg)

rownames(Correl_acl_kg) <-c("r_value", "pvalue")

Correl_acl_all <- bind_cols(Correl_acl, Correl_acl_kg)

# 4.2 Mujeres

# 4.2.1 Todas las mujeres

nombres_females_VO2 <- names(Datos_mujeres_VO2)

Correl_females<- data.frame(simplify2array(Correlaciones_females_VO2))

Correl_females <- Correl_females %>%
  setNames(nombres_females_VO2)

rownames(Correl_females) <- c("r_value", "pvalue")

nombres_females_VO2_kg <- names(Datos_mujeres_VO2_kg)

Correl_females_kg<- data.frame(simplify2array(Correlaciones_females_VO2_kg))

Correl_females_kg <- Correl_females_kg %>%
  setNames(nombres_females_VO2_kg)

rownames(Correl_females_kg) <-c("r_value", "pvalue")

Correl_females_all<- bind_cols(Correl_females, Correl_females_kg)


# 4.2.2 Nativas

nombres_nata_VO2 <- names(Datos_nativas_VO2)

Correl_nata<- data.frame(simplify2array(Correlaciones_nativas_VO2))

Correl_nata <- Correl_nata %>%
  setNames(nombres_nata_VO2)

rownames(Correl_nata) <- c("r_value", "pvalue")

nombres_nata_VO2_kg <- names(Datos_nativas_VO2_kg)

Correl_nata_kg<- data.frame(simplify2array(Correlaciones_nativas_VO2_kg))

Correl_nata_kg <- Correl_nata_kg %>%
  setNames(nombres_nata_VO2_kg)

rownames(Correl_nata_kg) <- c("r_value", "pvalue")

Correl_nata_all<- bind_cols(Correl_nata, Correl_nata_kg)


# 4.2.3 Aclimatadas

nombres_acla_VO2 <- names(Datos_aclimatadas_VO2)

Correl_acla<- data.frame(simplify2array(Correlaciones_aclimatadas_VO2))

Correl_acla <- Correl_acla %>%
  setNames(nombres_acla_VO2)

rownames(Correl_acla)  <- c("r_value", "pvalue")

nombres_acla_VO2_kg <- names(Datos_aclimatadas_VO2_kg)

Correl_acla_kg<- data.frame(simplify2array(Correlaciones_aclimatadas_VO2_kg))

Correl_acla_kg <- Correl_acla_kg %>%
  setNames(nombres_acla_VO2_kg)

rownames(Correl_acla_kg)  <-c("r_value", "pvalue")

Correl_acla_all<- bind_cols(Correl_acla, Correl_acla_kg)


# ------ 5. CREAR CSV ------
write.csv(Correl_males_all, "Correlaciones_hombres_todos.csv")
write.csv(Correl_nat_all, "Correlaciones_hombres_nativos.csv")
write.csv(Correl_acl_all, "Correlaciones_hombres_aclimatados.csv")

write.csv(Correl_females_all, "Correlaraciones_mujeres_todos.csv")
write.csv(Correl_nata_all, "Correlaraciones_mujueres_nativos.csv")
write.csv(Correl_acla_all, "Correlaraciones_mujeres_aclimatados.csv")
