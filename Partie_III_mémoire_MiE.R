
# Ce script R microsimule des variantes du bonus-malus français:

# Variant 1: Extend the French bonus-malus scheme to all employers

# Variant 2: modification of the French schedule for calculating UI tax rates

# Variant 3: microsimulation of the American experience-rating "reserve ratio" on French data


# Variant 1: Extend the French bonus-malus scheme to all employers

# Création du tableau_simulation1
data_tableau_simulation1<- base_etude_ent_BM %>% dplyr::select(a38,type_modulation_ref=type_modulation,tx_contribution_ref=tx_contribution,tx_contrib_non_borne) %>%
  mutate(modulation_ref=tx_contribution_ref-4.05) %>%
  mutate(tx_contribution_ext_ref=case_when(
    tx_contrib_non_borne<3.00 ~ 3.00,
    tx_contrib_non_borne>=3.00 & tx_contrib_non_borne<=5.05 ~ tx_contrib_non_borne,
    tx_contrib_non_borne>5.05 ~ 5.05
  )) %>%
  mutate(modulation_ext_ref=tx_contribution_ext_ref-4.05) 

tableau_simulation1<- data_tableau_simulation1 %>%
  group_by(a38) %>%
  summarise(
    nb_ent=n(),
    moyenne=mean(tx_contribution_ext_ref,na.rm = T),
    ecart_type=sd(tx_contribution_ext_ref,na.rm=T),
    cv=cv(tx_contribution_ext_ref,na.rm = T),
    p10=quantile(tx_contribution_ext_ref,
                 probs=0.10,na.rm = T),
    p25=quantile(tx_contribution_ext_ref,
                 probs=0.25,na.rm = T),
    p50=quantile(tx_contribution_ext_ref,
                 probs=0.50,na.rm = T),
    p75=quantile(tx_contribution_ext_ref,
                 probs=0.75,na.rm = T),
    p90=quantile(tx_contribution_ext_ref,
                 probs=0.90,na.rm = T),
    moy_modulation=mean(modulation_ext_ref,na.rm = T),
    med_modulation=median(modulation_ext_ref,na.rm = T),
    part_ent_malus=round(sum(tx_contribution_ext_ref>4.05)/n()*100,1),
    part_ent_bonus=round(sum(tx_contribution_ext_ref<4.05)/n()*100,1),
    part_ent_tx_max=round(sum(tx_contribution_ext_ref==5.05)/n()*100,1),
    part_ent_tx_min=round(sum(tx_contribution_ext_ref==3.00)/n()*100,1),
    p25_modul=quantile(modulation_ext_ref,
                       probs=0.25,na.rm = T),
    p75_modul=quantile(modulation_ext_ref,
                       probs=0.75,na.rm = T)
  ) %>%
  mutate(rapport_interdécile=p90/p10,
         écart_interquartile=p75-p25,
         écart_interquartile_modul=p75_modul-p25_modul) %>%
  mutate_if(is.numeric,~round(.,2)) %>%
  rename(groupe_ent=a38) %>%
  filter(nb_ent>=200) %>%
  na.omit() %>%
  dplyr::select(groupe_ent,nb_ent,moy_modulation,med_modulation,écart_interquartile_modul,part_ent_malus,part_ent_tx_max,part_ent_tx_min) 


# Graphique pour illustrer: part des entreprises en malus par secteur
Graph_simulation1 <-tableau_simulation1 %>% arrange(desc(part_ent_malus)) %>%
  ggplot(aes(x=groupe_ent,y=part_ent_malus)) +
  geom_col(fill="blue")+ 
  xlab("Secteur (A38)")+
  ylab("En %")
Graph_simulation1


# Variant 2: modification of the French schedule for calculating UI tax rates

data_tableau_simulation2<- base_etude_ent_BM %>% dplyr::select(siren,a38,eligibilité_BM_modulation1,
                                                               type_modulation_ref=type_modulation,tx_contribution_ref=tx_contribution,tx_contrib_non_borne) %>%
  mutate(modulation_ref=tx_contribution_ref-4.05) %>%
  mutate(tx_contribution_2=case_when(
    eligibilité_BM_modulation1=="entreprise éligible" &  tx_contrib_non_borne<2.50 ~ 2.50,
    eligibilité_BM_modulation1=="entreprise éligible" &  tx_contrib_non_borne>=2.50 &
      tx_contrib_non_borne<=7.00 ~ tx_contrib_non_borne,
    eligibilité_BM_modulation1=="entreprise éligible" &  tx_contrib_non_borne>7.00 ~ 7.00,
    eligibilité_BM_modulation1=="entreprise non éligible" ~ 4.05
  )
  ) %>%
  mutate(modulation_2=tx_contribution_2-4.05) 

# Réplication du tableau 2:

tableau2_1_simul2<- data_tableau_simulation2 %>% 
  filter(eligibilité_BM_modulation1=="entreprise éligible" & !a38=="IZ") %>%
  group_by(a38) %>%
  summarise(
    moyenne=mean(tx_contribution_2,na.rm = T),
    ecart_type=sd(tx_contribution_2,na.rm=T),
    cv=cv(tx_contribution_2,na.rm = T),
    p10=quantile(tx_contribution_2,
                 probs=0.10,na.rm = T),
    p25=quantile(tx_contribution_2,
                 probs=0.25,na.rm = T),
    p50=quantile(tx_contribution_2,
                 probs=0.50,na.rm = T),
    p75=quantile(tx_contribution_2,
                 probs=0.75,na.rm = T),
    p90=quantile(tx_contribution_2,
                 probs=0.90,na.rm = T),
    moy_modulation=mean(modulation_2,na.rm = T),
    med_modulation=median(modulation_2,na.rm = T),
    part_ent_malus=round(sum(tx_contribution_2>4.05)/n()*100,1),
    part_ent_bonus=round(sum(tx_contribution_2<4.05)/n()*100,1),
    part_ent_tx_max=round(sum(tx_contribution_2==7.00)/n()*100,1),
    part_ent_tx_min=round(sum(tx_contribution_2==2.50)/n()*100,1),
    p25_modul=quantile(modulation_2,
                       probs=0.25,na.rm = T),
    p75_modul=quantile(modulation_2,
                       probs=0.75,na.rm = T)
  ) %>%
  mutate(rapport_interdécile=p90/p10,
         écart_interquartile=p75-p25,
         écart_interquartile_modul=p75_modul-p25_modul) %>%
  mutate_if(is.numeric,~round(.,2)) %>%
  rename(groupe_ent=a38) %>%
  dplyr::select(groupe_ent,moy_modulation,med_modulation,écart_interquartile_modul,part_ent_malus,part_ent_tx_max,part_ent_tx_min)


tableau2_2_simul2<- data_tableau_simulation2 %>%
  filter(eligibilité_BM_modulation1=="entreprise éligible" & !a38=="IZ") %>%
  summarise(
    moyenne=mean(tx_contribution_2,na.rm = T),
    ecart_type=sd(tx_contribution_2,na.rm=T),
    cv=cv(tx_contribution_2,na.rm = T),
    p10=quantile(tx_contribution_2,
                 probs=0.10,na.rm = T),
    p25=quantile(tx_contribution_2,
                 probs=0.25,na.rm = T),
    p50=quantile(tx_contribution_2,
                 probs=0.50,na.rm = T),
    p75=quantile(tx_contribution_2,
                 probs=0.75,na.rm = T),
    p90=quantile(tx_contribution_2,
                 probs=0.90,na.rm = T),
    moy_modulation=mean(modulation_2,na.rm = T),
    med_modulation=median(modulation_2,na.rm = T),
    part_ent_malus=round(sum(tx_contribution_2>4.05)/n()*100,1),
    part_ent_bonus=round(sum(tx_contribution_2<4.05)/n()*100,1),
    part_ent_tx_max=round(sum(tx_contribution_2==7.00)/n()*100,1),
    part_ent_tx_min=round(sum(tx_contribution_2==2.50)/n()*100,1),
    p25_modul=quantile(modulation_2,
                       probs=0.25,na.rm = T),
    p75_modul=quantile(modulation_2,
                       probs=0.75,na.rm = T)
  ) %>%
  mutate(rapport_interdécile=p90/p10,
         écart_interquartile=p75-p25,
         écart_interquartile_modul=p75_modul-p25_modul) %>%
  mutate_if(is.numeric,~round(.,2)) %>%
  mutate(groupe_ent="entreprise éligible") %>%
  dplyr::select(groupe_ent,moy_modulation,med_modulation,écart_interquartile_modul,part_ent_malus,part_ent_tx_max,part_ent_tx_min)


tableau2_simul2 <- bind_rows(tableau2_2_simul2,tableau2_1_simul2)


# Distribution des modulations de taux pour les deux barêmes:

data_graph_simul2 <-data_tableau_simulation2 %>%
  filter(eligibilité_BM_modulation1=="entreprise éligible") %>%
  dplyr::select(siren,modulation_ref,modulation_2) %>%
  pivot_longer(cols = starts_with("modul"),
               names_to = "type_bareme",
               values_to = "tx_modulation") %>%
  mutate(type_bareme2=recode(type_bareme,
                             "modulation_ref" = "barême officiel",
                             "modulation_2"="barême modifié"))

Graph_simulation2 <-data_graph_simul2 %>%
  ggplot(aes(x=tx_modulation,color=type_bareme2)) +
  geom_density()

Graph_simulation2 + labs(color="Type de barême")  +
  xlab("Modulation, en point de pourcentage") 


# Variant 3: microsimulation of the American experience-rating "reserve ratio" on French data

# Récupération des montants d'allocation-chômage versés par entreprise à ses ex-salariés sur la période 
# d'observation 2021 S2 - 2022S1, à partir des données FNA de MIDAS;


# TODO!!







