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

# Comparafunction

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

# Nativos

Datos_nativos<- as.list(Nativos[,c(11:30,32:38,40:49)])

lista_datos <- Datos_nativos
df_normalidad <- Shap_nativos

Comparaciones_nativos <- lapply(names(Datos_nativos), Comparafunction)

Comparaciones_nativos


# Aclimatados

Datos_aclimatados<- as.list(Aclimatados[,c(11:30,32:38,40:49)])

lista_datos <- Datos_aclimatados
df_normalidad <- Shap_aclimatados


Comparaciones_aclimatados <- lapply(names(Datos_aclimatados), Comparafunction)

# Nativas


Datos_nativas<- as.list(Nativas[,c(11:30,32:39, 41:45, 48:49)])

lista_datos <- Datos_nativas
df_normalidad <- Shap_nativas

Comparaciones_nativas <- lapply(names(Datos_nativas), Comparafunction)


# Aclimatadas

Datos_aclimatadas<- as.list(Aclimatadas[,c(11:30,32:39, 41:45, 48:49)])

lista_datos <- Datos_aclimatadas
df_normalidad <- Shap_aclimatadas

Comparaciones_aclimatadas <- lapply(names(Datos_aclimatadas), Comparafunction)

### Crear los data frames

# Nativos
nombres_nat <- names(Datos_nativos)

Comp_nat<- data.frame(simplify2array(Comparaciones_nativos))

Comp_nat <- Comp_nat %>%
  setNames(nombres_nat)

rownames(Comp_nat) <- c("Varianza_N", "Método_N", "Diferencia_N", "CI1_N", "CI2_N", "pvalue_N")


# Aclimatados

nombres_acl <- names(Datos_aclimatados)

Comp_acl<- data.frame(simplify2array(Comparaciones_aclimatados))

Comp_acl <- Comp_acl %>%
  setNames(nombres_nat)

rownames(Comp_acl) <- c("Varianza_A", "Método_A", "Diferencia_A", "CI1_A", "CI2_A", "pvalue_A")


# Nativas

nombres_nata <- names(Datos_nativas)

Comp_nata<- data.frame(simplify2array(Comparaciones_nativas))

Comp_nata <- Comp_nata %>%
  setNames(nombres_nata)

rownames(Comp_nata) <- c("Varianza_N", "Método_N", "Diferencia_N", "CI1_N", "CI2_N", "pvalue_N")

#Aclimatadas

nombres_acla <- names(Datos_aclimatadas)

Comp_acla<- data.frame(simplify2array(Comparaciones_aclimatadas))

Comp_acla <- Comp_acla %>%
  setNames(nombres_acla)

rownames(Comp_acla)  <- c("Varianza_A", "Método_A", "Diferencia_A", "CI1_A", "CI2_A", "pvalue_A")

##

Hombres <- rbind(Comp_nat, Comp_acl)
write.csv(Hombres, "Comparaciones_hombres.csv")

Mujeres <- rbind(Comp_nata, Comp_acla)
write.csv(Mujeres, "Comparaciones_mujeres.csv")


View(Hombres)
View(Mujeres)
