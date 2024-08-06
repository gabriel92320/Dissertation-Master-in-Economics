
# Figures de la partie 1 du Mémoire:

# Microsimulation of the French bonus-malus system (first modulation)

# A) Analyse descriptive des entreprises exposées à la première modulation du dispositif bonus-malus:
# Angle principal d'analyse: le secteur;
# Mais on mesure l'influence du secteur en contrôlant d'autres caractéristiques.., modèle Logit.

# 1) Le taux moyen de séparation et le taux moyen de contribution:
# on pondère par l'EMA mesuré sur la première période d'observation.

# Tableau 1: Taux de séparation des entreprises concernées/non concernées par la première modulation
# du bonus-malus (rapportés au taux de séparation médian de leur secteur respectif):

tableau1_1<- base_etude_ent_BM %>% 
  filter(eligibilité_BM_modulation1=="entreprise éligible" & !a38=="IZ") %>%
  group_by(a38) %>%
  summarise(nb_ent=n(),
            EMA_2122=sum(EMA_2122,na.rm = T)/1000,
            taux_interim_2122=mean(tx_recours_interim_2122,na.rm=T),
            moyenne=mean(ratio_tx_sep_2122,na.rm = T),
            ecart_type=sd(ratio_tx_sep_2122,na.rm=T),
            cv=cv(ratio_tx_sep_2122,na.rm = T),
            p10=quantile(ratio_tx_sep_2122,
                         probs=0.10,na.rm = T),
            p25=quantile(ratio_tx_sep_2122,
                         probs=0.25,na.rm = T),
            p50=quantile(ratio_tx_sep_2122,
                         probs=0.50,na.rm = T),
            p75=quantile(ratio_tx_sep_2122,
                         probs=0.75,na.rm = T),
            p90=quantile(ratio_tx_sep_2122,
                         probs=0.90,na.rm = T),
  ) %>%
  mutate(écart_interdécile=p90-p10,
         écart_interquartile=p75-p25) %>%
  mutate_if(is.numeric,~round(.,1)) %>%
  rename(groupe_ent=a38)

tableau1_2<- base_etude_ent_BM %>% 
  group_by(eligibilité_BM_modulation1) %>%
  summarise(nb_ent=n(),
            EMA_2122=sum(EMA_2122,na.rm = T)/1000,
            taux_interim_2122=mean(tx_recours_interim_2122,na.rm=T),
            moyenne=mean(ratio_tx_sep_2122,na.rm = T),
            ecart_type=sd(ratio_tx_sep_2122,na.rm=T),
            cv=cv(ratio_tx_sep_2122,na.rm = T),
            p10=quantile(ratio_tx_sep_2122,
                         probs=0.10,na.rm = T),
            p25=quantile(ratio_tx_sep_2122,
                         probs=0.25,na.rm = T),
            p50=quantile(ratio_tx_sep_2122,
                         probs=0.50,na.rm = T),
            p75=quantile(ratio_tx_sep_2122,
                         probs=0.75,na.rm = T),
            p90=quantile(ratio_tx_sep_2122,
                         probs=0.90,na.rm = T),
  ) %>%
  mutate(écart_interdécile=p90-p10,
         écart_interquartile=p75-p25) %>%
  mutate_if(is.numeric,~round(.,1)) %>%
  arrange(desc(eligibilité_BM_modulation1)) %>%
  rename(groupe_ent=eligibilité_BM_modulation1)

tableau1_3<- base_etude_ent_BM %>% 
  filter(eligibilité_BM_modulation1=="entreprise non éligible" & a38 %in% c("CA","CC","CG","EZ","HZ","MC")) %>%
  summarise(nb_ent=n(),
            EMA_2122=sum(EMA_2122,na.rm = T)/1000,
            taux_interim_2122=mean(tx_recours_interim_2122,na.rm=T),
            moyenne=mean(ratio_tx_sep_2122,na.rm = T),
            ecart_type=sd(ratio_tx_sep_2122,na.rm=T),
            cv=cv(ratio_tx_sep_2122,na.rm = T),
            p10=quantile(ratio_tx_sep_2122,
                         probs=0.10,na.rm = T),
            p25=quantile(ratio_tx_sep_2122,
                         probs=0.25,na.rm = T),
            p50=quantile(ratio_tx_sep_2122,
                         probs=0.50,na.rm = T),
            p75=quantile(ratio_tx_sep_2122,
                         probs=0.75,na.rm = T),
            p90=quantile(ratio_tx_sep_2122,
                         probs=0.90,na.rm = T),
  ) %>%
  mutate(écart_interdécile=p90-p10,
         écart_interquartile=p75-p25) %>%
  mutate_if(is.numeric,~round(.,1)) %>%
  mutate(groupe_ent="entreprise non éligible (secteurs éligibles)") %>%
  relocate(groupe_ent)

tableau1_4<- base_etude_ent_BM %>% 
  filter(eligibilité_BM_modulation1=="entreprise non éligible" & !a38 %in% c("CA","CC","CG","EZ","HZ","MC")) %>%
  summarise(nb_ent=n(),
            EMA_2122=sum(EMA_2122,na.rm = T)/1000,
            taux_interim_2122=mean(tx_recours_interim_2122,na.rm=T),
            moyenne=mean(ratio_tx_sep_2122,na.rm = T),
            ecart_type=sd(ratio_tx_sep_2122,na.rm=T),
            cv=cv(ratio_tx_sep_2122,na.rm = T),
            p10=quantile(ratio_tx_sep_2122,
                         probs=0.10,na.rm = T),
            p25=quantile(ratio_tx_sep_2122,
                         probs=0.25,na.rm = T),
            p50=quantile(ratio_tx_sep_2122,
                         probs=0.50,na.rm = T),
            p75=quantile(ratio_tx_sep_2122,
                         probs=0.75,na.rm = T),
            p90=quantile(ratio_tx_sep_2122,
                         probs=0.90,na.rm = T),
  ) %>%
  mutate(écart_interdécile=p90-p10,
         écart_interquartile=p75-p25) %>%
  mutate_if(is.numeric,~round(.,1)) %>%
  mutate(groupe_ent="entreprise non éligible (secteurs non éligibles)") %>%
  relocate(groupe_ent)

tableau1_5<- base_etude_ent_BM %>% 
  summarise(nb_ent=n(),
            EMA_2122=sum(EMA_2122,na.rm = T)/1000,
            taux_interim_2122=mean(tx_recours_interim_2122,na.rm=T),
            moyenne=mean(ratio_tx_sep_2122,na.rm = T),
            ecart_type=sd(ratio_tx_sep_2122,na.rm=T),
            cv=cv(ratio_tx_sep_2122,na.rm = T),
            p10=quantile(ratio_tx_sep_2122,
                         probs=0.10,na.rm = T),
            p25=quantile(ratio_tx_sep_2122,
                         probs=0.25,na.rm = T),
            p50=quantile(ratio_tx_sep_2122,
                         probs=0.50,na.rm = T),
            p75=quantile(ratio_tx_sep_2122,
                         probs=0.75,na.rm = T),
            p90=quantile(ratio_tx_sep_2122,
                         probs=0.90,na.rm = T),
  ) %>%
  mutate(écart_interdécile=p90-p10,
         écart_interquartile=p75-p25) %>%
  mutate_if(is.numeric,~round(.,1)) %>%
  mutate(groupe_ent="Ensemble") %>%
  relocate(groupe_ent)

tableau1 <- bind_rows(tableau1_1,tableau1_2,tableau1_3,tableau1_4,tableau1_5)


# Tableau 2: Taux de contribution des entreprises concernées/non concernées par la première modulation
# du bonus-malus (en écart au taux de référence pré-réforme: 4,05%):

tableau2_1<- base_etude_ent_BM %>% 
  filter(eligibilité_BM_modulation1=="entreprise éligible" & !a38=="IZ") %>%
  mutate(modulation=tx_contribution-4,05) %>%
  group_by(a38) %>%
  summarise(
    moyenne=mean(tx_contribution,na.rm = T),
    ecart_type=sd(tx_contribution,na.rm=T),
    cv=cv(tx_contribution,na.rm = T),
    p10=quantile(tx_contribution,
                 probs=0.10,na.rm = T),
    p25=quantile(tx_contribution,
                 probs=0.25,na.rm = T),
    p50=quantile(tx_contribution,
                 probs=0.50,na.rm = T),
    p75=quantile(tx_contribution,
                 probs=0.75,na.rm = T),
    p90=quantile(tx_contribution,
                 probs=0.90,na.rm = T),
    moy_modulation=mean(modulation,na.rm = T),
    med_modulation=median(modulation,na.rm = T),
    part_ent_malus=round(sum(tx_contribution>4.05)/n()*100,1),
    part_ent_bonus=round(sum(tx_contribution<4.05)/n()*100,1),
    part_ent_tx_max=round(sum(tx_contribution==5.05)/n()*100,1),
    part_ent_tx_min=round(sum(tx_contribution==3.00)/n()*100,1),
    p25_modul=quantile(modulation,
                       probs=0.25,na.rm = T),
    p75_modul=quantile(modulation,
                       probs=0.75,na.rm = T)
  ) %>%
  mutate(rapport_interdécile=p90/p10,
         écart_interquartile=p75-p25,
         écart_interquartile_modul=p75_modul-p25_modul) %>%
  mutate_if(is.numeric,~round(.,2)) %>%
  rename(groupe_ent=a38) %>%
  dplyr::select(groupe_ent,moy_modulation,med_modulation,écart_interquartile_modul,part_ent_malus,part_ent_tx_max,part_ent_tx_min)


tableau2_2<- base_etude_ent_BM %>%
  filter(eligibilité_BM_modulation1=="entreprise éligible" & !a38=="IZ") %>%
  mutate(modulation=tx_contribution-4,05) %>%
  summarise(
    moyenne=mean(tx_contribution,na.rm = T),
    ecart_type=sd(tx_contribution,na.rm=T),
    cv=cv(tx_contribution,na.rm = T),
    p10=quantile(tx_contribution,
                 probs=0.10,na.rm = T),
    p25=quantile(tx_contribution,
                 probs=0.25,na.rm = T),
    p50=quantile(tx_contribution,
                 probs=0.50,na.rm = T),
    p75=quantile(tx_contribution,
                 probs=0.75,na.rm = T),
    p90=quantile(tx_contribution,
                 probs=0.90,na.rm = T),
    moy_modulation=mean(modulation,na.rm = T),
    med_modulation=median(modulation,na.rm = T),
    part_ent_malus=round(sum(tx_contribution>4.05)/n()*100,1),
    part_ent_bonus=round(sum(tx_contribution<4.05)/n()*100,1),
    part_ent_tx_max=round(sum(tx_contribution==5.05)/n()*100,1),
    part_ent_tx_min=round(sum(tx_contribution==3.00)/n()*100,1),
    p25_modul=quantile(modulation,
                       probs=0.25,na.rm = T),
    p75_modul=quantile(modulation,
                       probs=0.75,na.rm = T)
  ) %>%
  mutate(rapport_interdécile=p90/p10,
         écart_interquartile=p75-p25,
         écart_interquartile_modul=p75_modul-p25_modul) %>%
  mutate_if(is.numeric,~round(.,2)) %>%
  mutate(groupe_ent="entreprise éligible") %>%
  dplyr::select(groupe_ent,moy_modulation,med_modulation,écart_interquartile_modul,part_ent_malus,part_ent_tx_max,part_ent_tx_min)


tableau2 <- bind_rows(tableau2_2,tableau2_1)



# 2) Un modèle logit pour mesurer l'influence des principales caractéristiques des entreprises éligibles sur leur
# probabilité de subir un malus: 

# champ: 16 969 entreprises exposées à la première modulation du bonus-malus:
data_pour_Logit <- base_etude_ent_BM %>%
  filter(eligibilité_BM_modulation1=="entreprise éligible" & !a38=="IZ") %>%
  dplyr::select(siren,denom,a38,age_0721,id_groupe,treffen_21,caht_exp_21,vaht_21,eff_eqtp_21_fare,ebe_21,
                tx_contribution,tx_recours_interim_2122) %>%
  mutate(ent_en_malus=as_factor(ifelse(tx_contribution>4.05,"oui","non"))) %>%
  mutate(categorie_age=as_factor(case_when(
    (age_0721<7) ~ "UL jeunes (moins de 6 ans)",
    (age_0721<11) ~ "UL matures (7 à 10 ans)",
    TRUE ~ "UL de plus de 10 ans"
  ))) %>%
  mutate(UL_groupe= as_factor(case_when(
    (id_groupe =="") ~ "UL indépendante",
    TRUE ~ "UL dans un groupe"
  ))) %>%
  mutate(tranche_eff=fct_collapse(treffen_21,
                                  "Moins de 20 postes" = c("00","01","02","03"),
                                  "Entre 20 et 50 postes" = "04",
                                  "Entre 50 et 250 postes" = c("05","06"),
                                  "Entre 250 et 1000 postes" =c("07","08"),
                                  other_level = "Plus de 1000 postes")) %>%
  mutate(UL_export= as_factor(case_when((caht_exp_21>1) ~ "UL exportatrice",
                                        TRUE ~ "UL non exportatrice"
  )))  %>% # Quartiles de productivité du travail (VAHT/effectif) et de taux de marge (EBE/VAHT):
  mutate(productivite=vaht_21/eff_eqtp_21_fare,
         tx_marge=ebe_21/vaht_21) %>%
  mutate(quartiles_produc=quantcut(productivite,q=4),
         quartiles_tx_marge=quantcut(tx_marge,q=4),
         quartiles_tx_interim=quantcut(tx_recours_interim_2122,q=4)) 

levels(data_pour_Logit$quartiles_produc) <-c("Q1","Q2","Q3","Q4")
levels(data_pour_Logit$quartiles_tx_marge) <-c("Q1","Q2","Q3","Q4")
levels(data_pour_Logit$quartiles_tx_interim) <-c("Q1","Q2","Q3","Q4")

data_pour_Logit %>% look_for("ent_en_malus")

# La modalité de référence est "Bonus".

# Régression logistique:
model_logit1 <- glm(ent_en_malus ~ a38 + tranche_eff + categorie_age + UL_groupe + UL_export + quartiles_produc,
                    family = binomial,data=data_pour_Logit)
model_logit1

res.model_logit1 <- model_logit1 %>%
  tbl_regression(intercept = T) %>%
  bold_labels()

res.model_logit1

# Affichage des odds ratio (OR): 

OR.model_logit1 <- model_logit1 %>%
  tbl_regression(exponentiate = T) %>%
  bold_labels()

OR.model_logit1

# Visualisation des OR:

graph1_OR.model_logit1 <- model_logit1 %>%
  ggstats::ggcoef_model(exponentiate = T)

graph1_OR.model_logit1

graph2_OR.model_logit1 <- model_logit1 %>%
  ggstats::ggcoef_table(exponentiate = T)

graph2_OR.model_logit1


# B) Analyse descriptive des entreprises exposées à la première modulation du dispositif bonus-malus:
# Angle principal d'analyse: la zone d'emploi (zonage géographique);

variable_traitements <- base_etude_etab_BM_tb %>%
  mutate(modulation=tx_contribution-4.05) %>%
  group_by(zempt) %>%
  summarise(part_ent_concernées_ZE=round(sum(eligibilité_BM_modulation1=="entreprise éligible")/n()*100,1),
            part_ent_concernées_ZE_p=round(sum(eff_eqtp_et * (eligibilité_BM_modulation1=="entreprise éligible"),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
            part_ent_malus_ZE=round(sum(tx_contribution>4.05)/n()*100,1),
            part_ent_malus_ZE_p=round(sum(eff_eqtp_et * (tx_contribution>4.05),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
            part_ent_tx_max_ZE=round(sum(tx_contribution==5.05)/n()*100,1),
            part_ent_tx_max_ZE_p=round(sum(eff_eqtp_et * (tx_contribution==5.05),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
            tx_moy_modulation_ZE=mean(modulation,na.rm = T),
            tx_moy_modulation_ZE_p=weighted.mean(modulation,eff_eqtp_et/sum(eff_eqtp_et),na.rm = T),
            tx_moy_contrib_ZE=mean(tx_contribution,na.rm = T),
            tx_moy_contrib_ZE_p=weighted.mean(tx_contribution,eff_eqtp_et/sum(eff_eqtp_et),na.rm = T))

variable_traitements_carto <- ze %>%
  inner_join(variable_traitements,by=c("ze2020"="zempt"))

# cartographie:
# réalisation de 4 cartes sur les indicateurs suivants:
# Carte 1: part d'établissements soumis au BM dans la ZE;
# Carte 2: part d'établissements subissant un malus dans la ZE;
# Carte 3: part d'établissements subissant un taux plafond (5,05%);
# Carte 4: taux moyen de modulation (en pp)


carte1 <-ggplot()+
  geom_sf(data=variable_traitements_carto,aes(fill=part_ent_concernées_ZE)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %") +
  labs(title = "Entreprises concernées") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte1

carte2 <-ggplot()+
  geom_sf(data=variable_traitements_carto,aes(fill=part_ent_malus_ZE)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %")+
  labs(title = "Entreprises en malus") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte2

carte3 <-ggplot()+
  geom_sf(data=variable_traitements_carto,aes(fill=part_ent_tx_max_ZE)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %")+
  labs(title = "Entreprises au taux plafond") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 


carte3

carte4 <-ggplot()+
  geom_sf(data=variable_traitements_carto,aes(fill=tx_moy_modulation_ZE)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En pp")+
  labs(title = "Modulation moyenne") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte4

library(egg)
cartes_all<-ggarrange(carte1,carte2,carte3,carte4,ncol = 2,nrow = 2)


# En Annexe, on peut insérer les mêmes cartes mais en pondérant par les effectifs des établissements:


carte1_annexe <-ggplot()+
  geom_sf(data=variable_traitements_carto,aes(fill=part_ent_concernées_ZE_p)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %") +
  labs(title = "Entreprises concernées") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte1_annexe

carte2_annexe <-ggplot()+
  geom_sf(data=variable_traitements_carto,aes(fill=part_ent_malus_ZE_p)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %")+
  labs(title = "Entreprises en malus") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte2_annexe

carte3_annexe <-ggplot()+
  geom_sf(data=variable_traitements_carto,aes(fill=part_ent_tx_max_ZE_p)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %")+
  labs(title = "Entreprises au taux plafond") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 


carte3_annexe

carte4_annexe <-ggplot()+
  geom_sf(data=variable_traitements_carto,aes(fill=tx_moy_modulation_ZE_p)) +
  scale_fill_viridis() +
  theme_void() +
  labs(fill="En %")+
  labs(title = "Modulation moyenne") +
  theme(plot.title = element_text(hjust = 0.5,face = "bold")) 

carte4_annexe

cartes_all_annexe<-ggarrange(carte1_annexe,carte2_annexe,carte3_annexe,carte4_annexe,ncol = 2,nrow = 2)












