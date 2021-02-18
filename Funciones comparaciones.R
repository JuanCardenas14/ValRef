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


#---- 2. COMPARAFUNCTION ------

Comparafunction <- function (x) {
  if (df_normalidad[[x]] < 0.05) {
    Method <- "Wilcoxon"
    Varianzas <- "No aplica"
    Test <- wilcox.test(lista_datos[[1]], lista_datos[[x]], conf.int = TRUE)
    diferencia <- Test$estimate[[1]]
  }
  else {
    Fish <- var.test(x = lista_datos[[1]],  y = lista_datos[[x]])
    if (Fish$p.value < 0.05) {
      Method <- "t-Student"
      Varianzas <- "diferentes"
      Test <- t.test(lista_datos[[1]], lista_datos[[x]], 
                     paired = FALSE, var.equal = FALSE)
      diferencia <- Test$parameter[[1]]
    }
    else {
      Method <- "t-Student"
      Varianzas <- "iguales"
      Test <- t.test(lista_datos[[1]], lista_datos[[x]], 
                     paired = FALSE, var.equal = TRUE)
      diferencia <- Test$parameter[[1]]
    }
  }
  Conf_int_1 <- Test$conf.int[[1]]
  Conf_int_2 <- Test$conf.int[[2]]
  p_value <- Test$p.value[[1]]
  return(c(Varianzas, Method, diferencia, Conf_int_1, Conf_int_2, p_value))
}


# ----- 3. APLICAR COMPARAFUNCTION ---------
## Acá se requiere haber corrido el script de "Funciones Shapiro"

# 3.1 Hombres

# 3.1.1 Todos los hombres

# 3.1.1.1 VO2 absoluto

Datos_hombres_VO2 <- as.list(Hombres[,c(124, 166:184, 186:192)])

lista_datos <- Datos_hombres_VO2
df_normalidad <- Shap_males[,c(1,3:28)] # Esta lista viene del script "Funciones Shapiro"

Comparaciones_males_VO2 <- lapply(names(Datos_hombres_VO2), Comparafunction)

# 3.1.1.2 VO2 por kg

Datos_hombres_VO2_kg <- as.list(Hombres [,c(125, 193, 194, 196:201)])

lista_datos <- Datos_hombres_VO2_kg
df_normalidad <- Shap_males[,c(2, 29:36)]

Comparaciones_males_VO2_kg <- lapply(names(Datos_hombres_VO2_kg), Comparafunction)

# 3.1.2 Nativos

# 3.1.2.1 VO2 absoluto

Datos_nativos_VO2<- as.list(Nativos[,c(124, 166:184, 186:192)])

lista_datos <- Datos_nativos_VO2
df_normalidad <- Shap_nativos[,c(1,3:28)]

Comparaciones_nativos_VO2 <- lapply(names(Datos_nativos_VO2), Comparafunction)

# 3.1.2.1 VO2 por kg

Datos_nativos_VO2_kg <- as.list(Nativos [,c(125, 193, 194, 196:201)])

lista_datos <- Datos_nativos_VO2_kg
df_normalidad <- Shap_nativos[,c(2, 29:36)]

Comparaciones_nativos_VO2_kg <- lapply(names(Datos_nativos_VO2_kg), Comparafunction)

# 3.1.3 Aclimatados

# 3.1.3.1 VO2 absoluto

Datos_aclimatados_VO2<- as.list(Aclimatados[,c(124, 166:184, 186:192)])

lista_datos <- Datos_aclimatados_VO2
df_normalidad <- Shap_aclimatados[,c(1,3:28)]


Comparaciones_aclimatados_VO2 <- lapply(names(Datos_aclimatados_VO2), Comparafunction)

# 3.1.2.1 VO2 por kg

Datos_aclimatados_VO2_kg <- as.list(Aclimatados [,c(125, 193, 194, 196:201)])

lista_datos <- Datos_aclimatados_VO2_kg
df_normalidad <- Shap_aclimatados[,c(2, 29:36)]

Comparaciones_aclimatados_VO2_kg <- lapply(names(Datos_aclimatados_VO2_kg), Comparafunction)

# 3.2 Mujeres

# 3.2.1 Todas las mujeres

# 3.2.1.1 VO2 absoluto

Datos_mujeres_VO2 <- as.list(Mujeres[,c(124, 166:184, 186:192)])

lista_datos <- Datos_mujeres_VO2
df_normalidad <- Shap_females[,c(1,3:28)]

Comparaciones_females_VO2 <- lapply(names(Datos_mujeres_VO2), Comparafunction)

# 3.2.1.1 VO2 por kg

Datos_mujeres_VO2_kg <- as.list(Mujeres [,c(125, 193, 194, 196:199)])

lista_datos <- Datos_mujeres_VO2_kg
df_normalidad <- Shap_females[,c(2, 29:34)]

Comparaciones_females_VO2_kg <- lapply(names(Datos_mujeres_VO2_kg), Comparafunction)


# 3.2.2 Nativas

# 3.2.2.1 VO2 absoluto

Datos_nativas_VO2<- as.list(Nativas[,c(124, 166:184, 186:192)])

lista_datos <- Datos_nativas_VO2
df_normalidad <- Shap_nativas[,c(1,3:28)]

Comparaciones_nativas_VO2 <- lapply(names(Datos_nativas_VO2), Comparafunction)

# 3.2.2.1 VO2 por kg

Datos_nativas_VO2_kg <- as.list(Nativas [,c(125, 193, 194, 196:199)])

lista_datos <- Datos_nativas_VO2_kg
df_normalidad <- Shap_nativas[,c(2, 29:34)]

Comparaciones_nativas_VO2_kg <- lapply(names(Datos_nativas_VO2_kg), Comparafunction)

# 3.2.3 Aclimatadas

# 3.2.2.1 VO2 absoluto

Datos_aclimatadas_VO2 <- as.list(Aclimatadas[,c(124, 166:184, 186:192)])

lista_datos <- Datos_aclimatadas_VO2
df_normalidad <- Shap_aclimatadas[,c(1,3:28)]

Comparaciones_aclimatadas_VO2 <- lapply(names(Datos_aclimatadas_VO2), Comparafunction)

# 3.2.2.1 VO2 por kg

Datos_aclimatadas_VO2_kg <- as.list(Aclimatadas [,c(125, 193, 194, 196:199)])

lista_datos <- Datos_aclimatadas_VO2_kg
df_normalidad <- Shap_aclimatadas[,c(2, 29:34)]

Comparaciones_aclimatadas_VO2_kg <- lapply(names(Datos_aclimatadas_VO2_kg), Comparafunction)


# ---- 4. CREAR LOS DATA FRAMES -----

# 4.1 Hombres

# 4.1.1 Todos los hombres

nombres_males_VO2 <- names(Datos_hombres_VO2)

Comp_males<- data.frame(simplify2array(Comparaciones_males_VO2))

Comp_males <- Comp_males %>%
  setNames(nombres_males_VO2)

rownames(Comp_males) <- c("Varianza_N", "Método_N", "Diferencia_N", 
                        "CI1_N", "CI2_N", "pvalue_N")


nombres_males_VO2_kg <- names(Datos_hombres_VO2_kg)

Comp_males_kg<- data.frame(simplify2array(Comparaciones_males_VO2_kg))

Comp_males_kg <- Comp_males_kg %>%
  setNames(nombres_males_VO2_kg)

rownames(Comp_males_kg) <- c("Varianza_N", "Método_N", "Diferencia_N", 
                          "CI1_N", "CI2_N", "pvalue_N")

Comp_males_all<- bind_cols(Comp_males, Comp_males_kg)

# 4.1.2 Nativos
nombres_nat_VO2 <- names(Datos_nativos_VO2)

Comp_nat<- data.frame(simplify2array(Comparaciones_nativos_VO2))

Comp_nat <- Comp_nat %>%
  setNames(nombres_nat_VO2)

rownames(Comp_nat) <- c("Varianza_N", "Método_N", "Diferencia_N", 
                        "CI1_N", "CI2_N", "pvalue_N")

nombres_nat_VO2_kg <- names(Datos_nativos_VO2_kg)

Comp_nat_kg<- data.frame(simplify2array(Comparaciones_nativos_VO2_kg))

Comp_nat_kg <- Comp_nat_kg %>%
  setNames(nombres_nat_VO2_kg)

rownames(Comp_nat_kg) <- c("Varianza_N", "Método_N", "Diferencia_N", 
                        "CI1_N", "CI2_N", "pvalue_N")

Comp_nat_all<- bind_cols(Comp_nat, Comp_nat_kg)

# 4.1.3 Aclimatados

nombres_acl_VO2 <- names(Datos_aclimatados_VO2)

Comp_acl<- data.frame(simplify2array(Comparaciones_aclimatados_VO2))

Comp_acl <- Comp_acl %>%
  setNames(nombres_nat_VO2)

rownames(Comp_acl) <- c("Varianza_A", "Método_A", "Diferencia_A", "CI1_A", "CI2_A", "pvalue_A")


nombres_acl_VO2_kg <- names(Datos_aclimatados_VO2_kg)

Comp_acl_kg<- data.frame(simplify2array(Comparaciones_aclimatados_VO2_kg))

Comp_acl_kg <- Comp_acl_kg %>%
  setNames(nombres_nat_VO2_kg)

rownames(Comp_acl_kg) <- c("Varianza_A", "Método_A", "Diferencia_A", 
                           "CI1_A", "CI2_A", "pvalue_A")

Comp_acl_all <- bind_cols(Comp_acl, Comp_acl_kg)

# 4.2 Mujeres

# 4.2.1 Todas las mujeres

nombres_females_VO2 <- names(Datos_mujeres_VO2)

Comp_females<- data.frame(simplify2array(Comparaciones_females_VO2))

Comp_females <- Comp_females %>%
  setNames(nombres_females_VO2)

rownames(Comp_females) <- c("Varianza_N", "Método_N", "Diferencia_N", 
                          "CI1_N", "CI2_N", "pvalue_N")


nombres_females_VO2_kg <- names(Datos_mujeres_VO2_kg)

Comp_females_kg<- data.frame(simplify2array(Comparaciones_females_VO2_kg))

Comp_females_kg <- Comp_females_kg %>%
  setNames(nombres_females_VO2_kg)

rownames(Comp_females_kg) <- c("Varianza_N", "Método_N", "Diferencia_N", 
                             "CI1_N", "CI2_N", "pvalue_N")

Comp_females_all<- bind_cols(Comp_females, Comp_females_kg)


# 4.2.2 Nativas

nombres_nata_VO2 <- names(Datos_nativas_VO2)

Comp_nata<- data.frame(simplify2array(Comparaciones_nativas_VO2))

Comp_nata <- Comp_nata %>%
  setNames(nombres_nata_VO2)

rownames(Comp_nata) <- c("Varianza_N", "Método_N", "Diferencia_N", "CI1_N", "CI2_N", "pvalue_N")

nombres_nata_VO2_kg <- names(Datos_nativas_VO2_kg)

Comp_nata_kg<- data.frame(simplify2array(Comparaciones_nativas_VO2_kg))

Comp_nata_kg <- Comp_nata_kg %>%
  setNames(nombres_nata_VO2_kg)

rownames(Comp_nata_kg) <- c("Varianza_N", "Método_N", "Diferencia_N", "CI1_N", "CI2_N", "pvalue_N")

Comp_nata_all<- bind_cols(Comp_nata, Comp_nata_kg)


# 4.2.3 Aclimatadas

nombres_acla_VO2 <- names(Datos_aclimatadas_VO2)

Comp_acla<- data.frame(simplify2array(Comparaciones_aclimatadas_VO2))

Comp_acla <- Comp_acla %>%
  setNames(nombres_acla_VO2)

rownames(Comp_acla_VO2)  <- c("Varianza_A", "Método_A", "Diferencia_A", 
                              "CI1_A", "CI2_A", "pvalue_A")


nombres_acla_VO2_kg <- names(Datos_aclimatadas_VO2_kg)

Comp_acla_kg<- data.frame(simplify2array(Comparaciones_aclimatadas_VO2_kg))

Comp_acla_kg <- Comp_acla_kg %>%
  setNames(nombres_acla_VO2_kg)

rownames(Comp_acla_kg)  <- c("Varianza_A", "Método_A", 
                                 "Diferencia_A", "CI1_A", "CI2_A", "pvalue_A")

Comp_acla_all<- bind_cols(Comp_acla, Comp_acla_kg)


# ------ 5. CREAR CSV ------
write.csv(Comp_males_all, "Comparaciones_hombres_todos.csv")
write.csv(Comp_nat_all, "Comparaciones_hombres_nativos.csv")
write.csv(Comp_acl_all, "Comparaciones_hombres_aclimatados.csv")

write.csv(Comp_females_all, "Comparaciones_mujeres_todos.csv")
write.csv(Comp_nata_all, "Comparaciones_mujueres_nativos.csv")
write.csv(Comp_acla_all, "Comparaciones_mujeres_aclimatados.csv")


View(Hombres)
View(Mujeres)
