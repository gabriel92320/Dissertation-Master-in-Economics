
# Ce script R réalise des cartes par ZE sur les différents outcomes calculés:
# en niveau 2022;
# en différence première 2021-2022;

data_econo_carto <- ze %>%
  inner_join(data_econo,by=c("ze2020"="zempt"))

# cartographie:
# réalisation de 6 cartes sur les outcomes suivants:
# Carte 1: taux de séparation moyen par ZE, en % (outcome 1);
# Carte 2: taux moyen d'embauche par ZE, en % (outcome 2);
# Carte 3: taux moyen de CDD court par ZE, en % (outcome 3);
# Carte 4: taux moyen d'embauches en CDI par ZE, en % (outcome 4);
# Carte 5: niveau de l'emploi total en eqtp par ZE (outcome 5);
# Carte 6: niveau du salaire moyen par eqtp dans la ZE (outcome 6);

# 1) en niveau 2022:

carte_outcome1_22 <-ggplot()+
  geom_sf(data=data_econo_carto %>% filter(annee=="2022"),aes(fill=tx_sep_all)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %") +
  labs(title = "outcome 1") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome1_22

carte_outcome2_22 <-ggplot()+
  geom_sf(data=data_econo_carto %>% filter(annee=="2022"),aes(fill=tx_emb_all)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %")+
  labs(title = "outcome 2") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome2_22

carte_outcome3_22 <-ggplot()+
  geom_sf(data=data_econo_carto %>% filter(annee=="2022"),aes(fill=tx_emb_CDD_court_et)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %")+
  labs(title = "outcome 3") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome3_22

carte_outcome4_22 <-ggplot()+
  geom_sf(data=data_econo_carto %>% filter(annee=="2022"),aes(fill=tx_emb_CDI_et)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %")+
  labs(title = "outcome 4") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome4_22

carte_outcome5_22 <-ggplot()+
  geom_sf(data=data_econo_carto %>% filter(annee=="2022"),aes(fill=log_eff_ZE_et_all)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En log")+
  labs(title = "outcome 5") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome5_22

carte_outcome6_22 <-ggplot()+
  geom_sf(data=data_econo_carto %>% filter(annee=="2022"),aes(fill=log_sal_ZE_et_all)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En log")+
  labs(title = "outcome 6") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome6_22

cartes_all_outcomes_1_4_22<-ggarrange(carte_outcome1_22,carte_outcome2_22,carte_outcome3_22,carte_outcome4_22,ncol = 2,nrow = 2)
cartes_all_outcomes_5_6_22<-ggarrange(carte_outcome5_22,carte_outcome6_22,ncol = 2,nrow = 1)



# 2) En différence première 2021-2022:

evol_outcomes_21_22 <- data_econo %>%
  filter(annee %in% c("2021","2022")) %>%
  pivot_wider(
    id_cols = zempt,
    names_from = annee,
    values_from = c(tx_sep_all,tx_emb_all,tx_emb_CDD_court_et,tx_emb_CDI_et,log_eff_ZE_et_all,log_sal_ZE_et_all)
  ) %>%
  mutate(delta_tx_sep_all_21_22=tx_sep_all_2022-tx_sep_all_2021,
         delta_tx_emb_all_21_22=tx_emb_all_2022-tx_emb_all_2021,
         delta_tx_emb_CDD_court_et_21_22=tx_emb_CDD_court_et_2022-tx_emb_CDD_court_et_2021,
         delta_tx_emb_CDI_et_21_22=tx_emb_CDI_et_2022-tx_emb_CDI_et_2021,
         delta_log_eff_ZE_et_all_21_22=(log_eff_ZE_et_all_2022-log_eff_ZE_et_all_2021)*100,
         delta_log_sal_ZE_et_all_21_22=(log_sal_ZE_et_all_2022-log_sal_ZE_et_all_2021)*100) %>%
  dplyr::select(zempt,delta_tx_sep_all_21_22,delta_tx_emb_all_21_22,delta_tx_emb_CDD_court_et_21_22,
                delta_tx_emb_CDI_et_21_22,delta_log_eff_ZE_et_all_21_22,delta_log_sal_ZE_et_all_21_22)


data_econo_carto_2 <- ze %>%
  inner_join(evol_outcomes_21_22,by=c("ze2020"="zempt"))


carte_outcome1_21_22 <-ggplot()+
  geom_sf(data=data_econo_carto_2,aes(fill=delta_tx_sep_all_21_22)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En pp") +
  labs(title = "outcome 1") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome1_21_22


carte_outcome2_21_22 <-ggplot()+
  geom_sf(data=data_econo_carto_2,aes(fill=delta_tx_emb_all_21_22)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En pp") +
  labs(title = "outcome 2") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome2_21_22


carte_outcome3_21_22 <-ggplot()+
  geom_sf(data=data_econo_carto_2,aes(fill=delta_tx_emb_CDD_court_et_21_22)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En pp") +
  labs(title = "outcome 3") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome3_21_22

carte_outcome4_21_22 <-ggplot()+
  geom_sf(data=data_econo_carto_2,aes(fill=delta_tx_emb_CDI_et_21_22)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En pp") +
  labs(title = "outcome 4") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome4_21_22

carte_outcome5_21_22 <-ggplot()+
  geom_sf(data=data_econo_carto_2,aes(fill=delta_log_eff_ZE_et_all_21_22)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En pp") +
  labs(title = "outcome 5") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome5_21_22

# on rep?re 3 ZE qui affiche des taux de croissance extr?mes...
# on les exclue de la carte bis:

ZE_outlier <- data_econo_carto_2 %>% slice_max(delta_log_eff_ZE_et_all_21_22,n=10)
carte_outcome5_21_22_2 <-ggplot()+
  geom_sf(data=data_econo_carto_2 %>% mutate(
    delta_log_eff_ZE_et_all_21_22=ifelse(ze2020 %in% c("8413","9302","8418","8411"),NA,delta_log_eff_ZE_et_all_21_22)
  ),aes(fill=delta_log_eff_ZE_et_all_21_22)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %") +
  labs(title = "outcome 5") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 
carte_outcome5_21_22_2

carte_outcome6_21_22 <-ggplot()+
  geom_sf(data=data_econo_carto_2,aes(fill=delta_log_sal_ZE_et_all_21_22)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %") +
  labs(title = "outcome 6") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte_outcome6_21_22

cartes_all_outcomes_1_4_21_22<-ggarrange(carte_outcome1_21_22,carte_outcome2_21_22,carte_outcome3_21_22,carte_outcome4_21_22,ncol = 2,nrow = 2)
cartes_all_outcomes_5_6_21_22<-ggarrange(carte_outcome5_21_22_2,carte_outcome6_21_22,ncol = 2,nrow = 1)




