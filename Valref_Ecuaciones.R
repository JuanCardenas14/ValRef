library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(reshape2)

MOM1<- read_excel("C://Users/Manuel Cardenas/Downloads/The_MOM_1.xlsx")

ECG_rep_EM <- read_excel("E:/Informaciòn/Pictures/Downloads/CPET_max.xlsx", 
                         skip = 1)



#---- 1. VO2 MÁXIMO--------

#------ 1.1 Hombres-------------------------

Hombres <- MOM1 %>%
  filter(sex_sub_e1 == 1)

Ecuaciones_hombres <- Hombres %>%
  rowwise %>%
  mutate(Jones_1_85 = ((-3.76 + (-0.028 * edad_ca_em) + 
                          0.034 * estatur_ea +
                          0.022 * pes_tot_ea) * 1000),
         Jones_3_85 = ((-4.31 + (-0.624 * 0) + 
                          (-0.021 * edad_ca_em) +
                          (0.046*estatur_ea))* 1000),
         Jones_4_85 = ((-3.17 + (-0.492*0) +
                          (-0.024*edad_ca_em)+
                          (0.032*estatur_ea)+
                          (0.019*pes_tot_ea))*1000),
         Blackie_1_89 = (3.015 + (-0.0494*edad_ca_em) +
                           (0.0142*estatur_ea)+
                           (0.00257*pes_tot_ea))*1000,
         Blackie_3_89 = (1.095 + (-0.666*0)+
                           (-0.0235*edad_ca_em) +
                           (0.0126*estatur_ea)+
                           (0.00927*pes_tot_ea))*1000,
         Singh_1_89 = (1.99 + (-0.04*edad_ca_em)+
                         (0.035*pes_tot_ea)*1000),
         Storer_1_90 = (519.3 + (-10.49*edad_ca_em) +
                          (6.35*pes_tot_ea)+
                          (10.51*wl_bxb_pk_dx)),
         Storer_3_90 = (403.4 + (-252.2 * 0) +
                          (-7.91*edad_ca_em) +
                          (7.15*pes_tot_ea)+
                          (10.22*wl_bxb_pk_dx)),
         Fairbarn_1_94 = (-0.332 + (-0.031*edad_ca_em) +
                            (0.023*estatur_ea)+
                            (0.0117*pes_tot_ea))*1000,
         Neder_1A_99 = (702 + (-24.3*edad_ca_em) +
                          (9.8*estatur_ea) +
                          (12.5*pes_tot_ea)),
         Neder_1C_99 = (1125 + (-24.3*edad_ca_em) +
                          (8.3*estatur_ea) +
                          (10.2*pes_tot_ea)),
         Neder_2A_99 = (207 + (-22.8*edad_ca_em)+
                          (17.9*estatur_ea)),
         Neder_2C_99 = (570 + (-23*edad_ca_em)+
                          (15.8*estatur_ea)),
         Neder_3A_99 = (2267 + (-25.2*edad_ca_em)+
                          (14.3*pes_tot_ea)),
         Neder_3C_99 = (2431 + (-25.1*edad_ca_em) +
                          (12.1*pes_tot_ea)),
         Davis_1_01 = ((0.32 + (-0.0282*edad_ca_em) +
                          (0.0205*estatur_ea))*1000),
         Davis_3_01 = ((2.6538 + (-0.0296*edad_ca_em) +
                          (0.0167*pes_tot_ea))*1000),
         Davis_5_01 = (2.1154 + (-0.0262*edad_ca_em) +
                         (0.0266*pe_lg_bio_ea))*1000,
         Ong_1_02 = exp(7.6929 + (-0.3522* 1) +
                          (-0.006*edad_ca_em) +
                          (0.0009*estatur_ea)+
                          (0.0052*pes_tot_ea)),
         Koch_1_09 = (4307 - 241*1 +
                        (-20*1^2) +
                        (-1281*1) +
                        (133*1*1)),
         Magrani_1_10 = (0.518 + (-0.0292*edad_ca_em) +
                           (0.01482*pes_tot_ea)+
                           (0.01016*wl_bxb_pk_dx))*1000,
         Gläser_1_10 = (-69 + (1.48*edad_ca_em) + 
                          (14.02*estatur_ea) +
                          (7.44*pes_tot_ea)+
                          (-0.2256*(edad_ca_em^2))),
         Gläser_1_13 = (254.761 + (-22.695*edad_ca_em) +
                          (17.246*estatur_ea) + 
                          (4.414*pes_tot_ea)),
         Grigaliuniene_1_13 = (3.599 + (-0.016*edad_ca_em))*1000,
         Mylius_1_19 = (-3039.1+ (634.32*1)+
                          (-16.5*edad_ca_em)+
                          (29.22*estatur_ea)+
                          (16.17*pes_tot_ea)),
         Mylius_2_19 = (-1469 + (673*1)+
                          (16.87*edad_ca_em)+
                          (39.7*pes_tot_ea)+
                          (-0.47*(edad_ca_em^2)+
                             (0.07*estatur_ea^2)+
                             (-0.16*pes_tot_ea^2))),
         Mylius_3_19 = (-2537.29 + (743.35*1) +
                          (4.263*edad_ca_em)+
                          (24.3*estatur_ea)+
                          (12.57*pes_tot_ea)),
         Tammelin_1_04_kg = (90 + (-0.16*fc_bxb_pk_dx)),
         Tammelin_2_04_kg = (77.69 + (-0.23*fc_bxb_pk_dx) +
                               (-0.9*imc_cal_ea)),
         Koch_1_09_kg = (47.7565 + (-0.988*1) +
                           (-8.8697*1) +
                           (2.3597*1)+
                           (-0.2356*1^2)+
                           (-3.7405*1*1)+
                           (0.2512*1*1)+
                           (1.3797*1*1)),
         Grigaliuniene_1_13_kg = (48.77 + (-0.342*edad_ca_em)),
         Itoh_1_13_kg = (42.05 + (-0.268*edad_ca_em)+
                           (-7.22*0)),
         Shim_1_1986_kg = 46.25 + (-0.26*edad_ca_em),
         Singh_1_89_kg = 67.7 + (-0.77*edad_ca_em),
         Kokkinos_1_18_kg = (3.5 + (1.76*(wl_bxb_pk_dx*6.12/pes_tot_ea))),
         Kokkinos_3_18_kg = (3.5 + (1.74*(wl_bxb_pk_dx*6.12/pes_tot_ea))))

write.csv(Ecuaciones_hombres, "Valref_males.csv")

VO2_abs_hombres <- Ecuaciones_hombres %>%
  select(vo2_20s_x_pkr_dx, Jones_1_85, Jones_3_85, Jones_4_85, 
         Blackie_1_89, Blackie_3_89, Singh_1_89, Storer_1_90, Storer_3_90,
         Fairbarn_1_94, Neder_1A_99, Neder_1C_99, Neder_2A_99, Neder_2C_99,
         Neder_3A_99, Neder_3C_99, Davis_1_01, Davis_3_01, Davis_5_01, Ong_1_02,
         Koch_1_09, Magrani_1_10, Gläser_1_10, Gläser_1_13, Grigaliuniene_1_13, Mylius_1_19,
         Mylius_2_19, Mylius_3_19)


plot_VO2_abs_hombres<- melt(VO2_abs_hombres)

plot_VO2_abs_hombres %>%
  ggplot(aes(variable, value)) + geom_boxplot()


#---------- 1.2 Mujeres--------

Mujeres <- MOM1 %>%
  filter(sex_sub_e1 == 0)


Ecuaciones_mujeres <- Mujeres %>%
  rowwise %>%
  mutate(Jones_2_85 = ((-2.26 + (-0.018 * edad_ca_em) + 
                          0.025 * estatur_ea +
                          0.01 * pes_tot_ea) * 1000),
         Jones_3_85 = ((-4.31 + (-0.624 * 1) + 
                          (-0.021 * edad_ca_em) +
                          (0.046*estatur_ea))* 1000),
         Jones_4_85 = ((-3.17 + (-0.492*1) +
                          (-0.024*edad_ca_em)+
                          (0.032*estatur_ea)+
                          (0.019*pes_tot_ea))*1000),
         Blackie_2_89 = (0.651 + (-0.0115*edad_ca_em) +
                           (0.0142*estatur_ea)+
                           (0.00974*pes_tot_ea))*1000,
         Blackie_3_89 = (1.095 + (-0.666*1)+
                           (-0.0235*edad_ca_em) +
                           (0.0126*estatur_ea)+
                           (0.00927*pes_tot_ea))*1000,
         Storer_2_90 = (136 + (-5.88*edad_ca_em) +
                          (7.71*pes_tot_ea)+
                          (9.39*wl_bxb_pk_dx)),
         Storer_3_90 = (403.4 + (-252.2 * 1) +
                          (-7.91*edad_ca_em) +
                          (7.15*pes_tot_ea)+
                          (10.22*wl_bxb_pk_dx)),
         Fairbarn_2_94 = (0.207 + (-0.027*edad_ca_em) +
                            (0.0158*estatur_ea)+
                            (0.00899*pes_tot_ea))*1000,
         Neder_1B_99 = (372 + (-13.7*edad_ca_em) +
                          (7.4*estatur_ea) +
                          (7.5*pes_tot_ea)),
         Neder_1C_99 = (60 + (-13.7*edad_ca_em) +
                          (8.3*estatur_ea) +
                          (10.2*pes_tot_ea)),
         Neder_2B_99 = (-170 + (-12.7*edad_ca_em)+
                          (13.6*estatur_ea)),
         Neder_2C_99 = (-527 + (-12.5*edad_ca_em)+
                          (15.8*estatur_ea)),
         Neder_3B_99 = (1470 + (-14.7*edad_ca_em)+
                          (9.5*pes_tot_ea)),
         Neder_3C_99 = (1310 + (-14.7*edad_ca_em) +
                          (12.1*pes_tot_ea)),
         Davis_2_01 = ((-0.274 + (-0.0171*edad_ca_em) +
                          (0.016*estatur_ea))*1000),
         Davis_4_01 = ((1.6267 + (-0.0199*edad_ca_em) +
                          (0.0135*pes_tot_ea))*1000),
         Davis_6_01 = (1.3405 + (-0.0183*edad_ca_em) +
                         (0.023*pe_lg_bio_ea))*1000,
         Ong_1_02 = exp(7.6929 + (-0.3522* 2) +
                          (-0.006*edad_ca_em) +
                          (0.0009*estatur_ea)+
                          (0.0052*pes_tot_ea)),
         Wisen_1_04 = (1993 + (2.355*edad_ca_em)),
         Koch_1_09 = (4307 - 241*1 +
                        (-20*1^2) +
                        (-1281*2) +
                        (133*1*2)),
         Magrani_2_10 = (-0.461 + (-0.01006*edad_ca_em) +
                           (0.007096*pes_tot_ea)+
                           (0.01043*wl_bxb_pk_dx))*1000,
         Gläser_2_10 = (-588 + (-11.33*edad_ca_em) + 
                          (9.13*estatur_ea) +
                          (26.88*pes_tot_ea)+
                          (-0.12*(pes_tot_ea^2))),
         Gläser_2_13 = (-54.739 + (-9.805*edad_ca_em) +
                          (9.9172*estatur_ea) + 
                          (8.0557*pes_tot_ea)),
         Grigaliuniene_2_13 = (2.31 + (-0.011*edad_ca_em))*1000,
         Mylius_1_19 = (-3039.1+ (634.32*0)+
                          (-16.5*edad_ca_em)+
                          (29.22*estatur_ea)+
                          (16.17*pes_tot_ea)),
         Mylius_2_19 = (-1469 + (673*0)+
                          (16.87*edad_ca_em)+
                          (39.7*pes_tot_ea)+
                          (-0.47*(edad_ca_em^2)+
                             (0.07*estatur_ea^2)+
                             (-0.16*pes_tot_ea^2))),
         Mylius_4_19 = (-2537.29 + (743.35*0) +
                          (7.391*edad_ca_em)+
                          (24.3*estatur_ea)+
                          (12.57*pes_tot_ea)),
         Tammelin_1_04_kg = (70.06 + (-0.16*fc_bxb_pk_dx)),
         Tammelin_2_04_kg = (65.67 + (-0.21*fc_bxb_pk_dx) +
                               (-0.53*imc_cal_ea)),
         Koch_1_09_kg = (47.7565 + (-0.988*1) +
                           (-8.8697*2) +
                           (2.3597*1)+
                           (-0.2356*1^2)+
                           (-3.7405*2*1)+
                           (0.2512*2*1)+
                           (1.3797*2*1)),
         Grigaliuniene_1_13_kg = (40.99 + (-0.319*edad_ca_em)),
         Itoh_1_13_kg = (42.05 + (-0.268*edad_ca_em)+
                           (-7.22*1)),
         Kokkinos_1_18_kg = (3.5 + (1.65*(wl_bxb_pk_dx*6.12/pes_tot_ea))),
         Kokkinos_3_18_kg = (3.5 + (1.74*(wl_bxb_pk_dx*6.12/pes_tot_ea))))

write.csv(Ecuaciones_mujeres, "Valref_females.csv")


a <- Ecuaciones_mujeres %>%
  select(vo2_20s_x_pkr_dx, Jones_2_85, Jones_3_85, Jones_4_85, 
         Blackie_2_89, Blackie_3_89, Storer_2_90, Storer_3_90,
         Fairbarn_2_94, Neder_1B_99, Neder_1C_99, Neder_2B_99, Neder_2C_99,
         Neder_3B_99, Neder_3C_99, Davis_2_01, Davis_4_01, Davis_6_01, Ong_1_02,
         Wisen_1_04, Koch_1_09, Magrani_2_10, Gläser_2_10, Gläser_2_13, Grigaliuniene_2_13, Mylius_1_19,
         Mylius_2_19, Mylius_4_19)

plot_VO2_abs_mujeres<- melt(a)

plot_VO2_abs_mujeres %>%
  ggplot(aes(variable, value)) + geom_boxplot()

#------ 2. VO2 POR KG------

#------2.1 Hombres-----------

VO2_kg_hombres<- Ecuaciones_hombres %>%
  select(vo2k_20s_x_pkr_dx, ends_with("kg"))

plot_VO2_kg_hombres <- melt(VO2_kg_hombres)

plot_VO2_kg_hombres %>%
  ggplot(aes(variable, value)) + geom_boxplot()

#------2.2 Mujeres-----------


VO2_kg_mujeres<- Ecuaciones_mujeres %>%
  select(vo2k_20s_x_pkr_dx, ends_with("kg"))

plot_VO2_kg_mujeres <- melt(VO2_kg_mujeres)

plot_VO2_kg_mujeres %>%
  ggplot(aes(variable, value)) + geom_boxplot()
