


# Ce script R vise à construire les variables de traitement ainsi que les instruments Bartik associés.
# Tous les calculs sont réalisés au niveau ZE;

# 1) Construction des variables de traitement à l'échelle des ZE:

# variable 1: proportion d'établissements concern?s par le bonus-malus (1?re modulation), en %;
# variable 2: proportion d'établissements subissant un malus (1?re modulation), en %;
# variable 3: proportion d'établissements subissant le taux plafond (5,05%), en %;
# variable 4: modulation moyenne du taux de contribution, en points de pourcentage (écart entre le taux modulé et le
# taux de référence pré-réforme);

variable_traitements <- base_etude_etab_BM_tb %>%
  mutate(modulation=tx_contribution-4.05) %>%
  group_by(zempt) %>%
  summarise(part_ent_concern?es_ZE=round(sum(eligibilité_BM_modulation1=="entreprise éligible")/n()*100,1),
            part_ent_concern?es_ZE_p=round(sum(eff_eqtp_et * (eligibilité_BM_modulation1=="entreprise éligible"),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
            part_ent_malus_ZE=round(sum(tx_contribution>4.05)/n()*100,1),
            part_ent_malus_ZE_p=round(sum(eff_eqtp_et * (tx_contribution>4.05),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
            part_ent_tx_max_ZE=round(sum(tx_contribution==5.05)/n()*100,1),
            part_ent_tx_max_ZE_p=round(sum(eff_eqtp_et * (tx_contribution==5.05),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
            tx_moy_modulation_ZE=mean(modulation,na.rm = T),
            tx_moy_modulation_ZE_p=weighted.mean(modulation,eff_eqtp_et/sum(eff_eqtp_et),na.rm = T),
            tx_moy_contrib_ZE=mean(tx_contribution,na.rm = T),
            tx_moy_contrib_ZE_p=weighted.mean(tx_contribution,eff_eqtp_et/sum(eff_eqtp_et),na.rm = T))

# Remarque: en termes de temporalité, ces variables de traitements calculés portent sur la première période de
# de modulation du bonus-malus (septembre 2022 - août 2023);
# seule la première variable de traitement peut également englober la première période d'observation du bonus-malus,
# à savoir: juillet 2021 - juin 2022.
# La dimension temporelle de la variable de traitement pourra dans une première spécification économétrique en 
# cross section être éludée (par ex, en prenant comme période d'estimation: 2022-2023 ou seulement l'année 2022...)

# 2) Construction des instruments ? la Bartik pour chaque variable de traitement ? l'échelle des ZE:

# a. Calcul de la composante "share" de l'instrument Bartik:
# Le poids de chaque secteur (a38) dans chaque ZE en 2021 (en termes effectifs en eqtp sur l'année 2020 ou 2019):

# connexion à la base Etablissements de BTS 2020:
etab_BTS_20<- open_dataset("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Etablissements_2021/etab_2020-0.parquet")
etab_BTS_20_arrow <- etab_BTS_20 %>% select(siret,eff_eqtp_et_20=eff_eqtp_et) %>% as_arrow_table()

# chargement de la base Etablissements de BTS 2019:
etab_BTS_19 <-haven::read_sas("//casd.fr/casdfs/Projets/POLEMOB/Data/DADS_DADS Etablissements_2020/etab.sas7bdat",
                              col_select = c("SIRET","EFF_EQTP_ET_1"))
etab_BTS_19_arrow <- etab_BTS_19 %>% rename(siret=SIRET,eff_eqtp_et_19=EFF_EQTP_ET_1) %>% as_arrow_table()

# Appariement avec la base d'étude:
base_etude_etab_BM_arrow <- base_etude_etab_BM_tb %>% as_arrow_table()

base_etude_etab_BM_arrow <- base_etude_etab_BM_arrow %>%
  left_join(etab_BTS_19_arrow,by="siret") %>%
  left_join(etab_BTS_20_arrow,by="siret") %>%
  left_join(etab_BTS_22_arrow,by="siret") %>% compute()

#extrait <- test %>% slice_head(n=100) %>% collect()
base_etude_etab_BM_tb <- base_etude_etab_BM_arrow %>% as_tibble()

eff_eqtp_et_ZE<-base_etude_etab_BM_tb %>%
  group_by(zempt)%>%
  summarise(eff_eqtp_et_ZE_21=sum(eff_eqtp_et,na.rm = T),
            eff_eqtp_et_ZE_20=sum(eff_eqtp_et_20,na.rm = T),
            eff_eqtp_et_ZE_19=sum(eff_eqtp_et_19,na.rm = T)
  )

eff_eqtp_et_A38_ZE<-base_etude_etab_BM_tb %>%
  group_by(zempt,a38)%>%
  summarise(eff_eqtp_et_a38_ZE_21=sum(eff_eqtp_et,na.rm = T),
            eff_eqtp_et_a38_ZE_20=sum(eff_eqtp_et_20,na.rm = T),
            eff_eqtp_et_a38_ZE_19=sum(eff_eqtp_et_19,na.rm = T)
  )

share_eff_eqtp_a38_ZE<-eff_eqtp_et_A38_ZE %>% left_join(eff_eqtp_et_ZE) %>%
  mutate(share_eff_a38_ZE_21=eff_eqtp_et_a38_ZE_21/eff_eqtp_et_ZE_21,
         share_eff_a38_ZE_20=eff_eqtp_et_a38_ZE_20/eff_eqtp_et_ZE_20,
         share_eff_a38_ZE_19=eff_eqtp_et_a38_ZE_19/eff_eqtp_et_ZE_19)

# b. Calcul de la composante "shock" de l'instrument Bartik:
# Calcul de la variable de traitement agr?g?e par secteur (au niveau national)

# version du calcul sans correction:
shock_a38 <- base_etude_etab_BM_tb %>%
  mutate(modulation=tx_contribution-4.05) %>%
  group_by(a38) %>%
  summarise(part_ent_concern?es_a38=round(sum(eligibilité_BM_modulation1=="entreprise éligible")/n()*100,1),
            part_ent_concern?es_a38_p=round(sum(eff_eqtp_et * (eligibilité_BM_modulation1=="entreprise éligible"),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
            part_ent_malus_a38=round(sum(tx_contribution>4.05)/n()*100,1),
            part_ent_malus_a38_p=round(sum(eff_eqtp_et * (tx_contribution>4.05),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
            part_ent_tx_max_a38=round(sum(tx_contribution==5.05)/n()*100,1),
            part_ent_tx_max_a38_p=round(sum(eff_eqtp_et * (tx_contribution==5.05),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
            tx_moy_modulation_a38=mean(modulation,na.rm = T),
            tx_moy_modulation_a38_p=weighted.mean(modulation,eff_eqtp_et/sum(eff_eqtp_et),na.rm = T))

# version du calcul avec correction:
# Pour chaque ZE l, on calcule la variable de traitement dans le secteur s (les 6 secteurs éligibles du BM)
# au niveau national mais après avoir exclu les établissements du secteur s implant?s dans la ZE l:
# Voir la fonction compute_tx_moy_contrib_sect
liste_ZE=as.vector(eff_eqtp_et_ZE$zempt)

compute_var_traitement_sect = function(secteur){
  var_traitement_sect_corr=tribble()
  for (l in liste_ZE){tx_moy_contrib_horsZE_l<-base_etude_etab_BM_tb %>%
    filter(!(ZE2020==l) & (a38==secteur)) %>%
    mutate(modulation=tx_contribution-4.05) %>%
    summarise(part_ent_concern?es_a38_2=round(sum(eligibilité_BM_modulation1=="entreprise éligible")/n()*100,1),
              part_ent_concern?es_a38_p_2=round(sum(eff_eqtp_et * (eligibilité_BM_modulation1=="entreprise éligible"),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
              part_ent_malus_a38_2=round(sum(tx_contribution>4.05)/n()*100,1),
              part_ent_malus_a38_p_2=round(sum(eff_eqtp_et * (tx_contribution>4.05),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
              part_ent_tx_max_a38_2=round(sum(tx_contribution==5.05)/n()*100,1),
              part_ent_tx_max_a38_p_2=round(sum(eff_eqtp_et * (tx_contribution==5.05),na.rm=T)/sum(eff_eqtp_et,na.rm = T)*100,1),
              tx_moy_modulation_a38_2=mean(modulation,na.rm = T),
              tx_moy_modulation_a38_p_2=weighted.mean(modulation,eff_eqtp_et/sum(eff_eqtp_et),na.rm = T)) %>%
    mutate(ZE2020=l) %>% relocate(ZE2020)
  
  var_traitement_sect_corr=rbind(var_traitement_sect_corr,tx_moy_contrib_horsZE_l)
  
  }
  return(var_traitement_sect_corr)
}

shock_a38_CA_corr <-compute_var_traitement_sect(secteur="CA") 
shock_a38_CC_corr <-compute_var_traitement_sect(secteur="CC") 
shock_a38_CG_corr <-compute_var_traitement_sect(secteur="CG") 
shock_a38_EZ_corr <-compute_var_traitement_sect(secteur="EZ") 
shock_a38_HZ_corr <-compute_var_traitement_sect(secteur="HZ") 
shock_a38_MC_corr <-compute_var_traitement_sect(secteur="MC") 

shock_a38_CA_corr <- shock_a38_CA_corr %>% mutate(a38="CA") %>% relocate(ZE2020,a38)
shock_a38_CC_corr <- shock_a38_CC_corr %>% mutate(a38="CC") %>% relocate(ZE2020,a38)
shock_a38_CG_corr <- shock_a38_CG_corr %>% mutate(a38="CG") %>% relocate(ZE2020,a38)
shock_a38_EZ_corr <- shock_a38_EZ_corr %>% mutate(a38="EZ") %>% relocate(ZE2020,a38)
shock_a38_HZ_corr <- shock_a38_HZ_corr %>% mutate(a38="HZ") %>% relocate(ZE2020,a38)
shock_a38_MC_corr <- shock_a38_MC_corr %>% mutate(a38="MC") %>% relocate(ZE2020,a38)

shock_a38_corr <- bind_rows(shock_a38_CA_corr,shock_a38_CC_corr,shock_a38_CG_corr,
                            shock_a38_EZ_corr,shock_a38_HZ_corr,shock_a38_MC_corr)

# c. Calcul de l'instrument Bartik pour chaque variable de traitement:
# IV_Bartik_ZE = Somme sur les secteurs a38 des produits share_eff_a38_ZE_19*shock_a38

# version  avec la composante "shock" non corrigée:
produit_pour_calcul_IV_Bartik <- share_eff_eqtp_a38_ZE %>%
  select(zempt,a38,share_eff_a38_ZE_19,share_eff_a38_ZE_20) %>%
  left_join(shock_a38,by="a38") %>%
  mutate(prod_part_ent_concernées=share_eff_a38_ZE_19*part_ent_concernées_a38,
         prod_part_ent_concernées_p=share_eff_a38_ZE_19*part_ent_concernées_a38_p,
         prod_part_ent_malus=share_eff_a38_ZE_19*part_ent_malus_a38,
         prod_part_ent_malus_p=share_eff_a38_ZE_19*part_ent_malus_a38_p,
         prod_part_ent_tx_max=share_eff_a38_ZE_19*part_ent_tx_max_a38,
         prod_part_ent_tx_max_p=share_eff_a38_ZE_19*part_ent_tx_max_a38_p,
         prod_tx_moy_modulation=share_eff_a38_ZE_19*tx_moy_modulation_a38,
         prod_tx_moy_modulation_p=share_eff_a38_ZE_19*tx_moy_modulation_a38_p
  ) %>%
  filter(!is.na(a38))

# version  avec la composante "shock" corrigée:
produit_pour_calcul_IV_Bartik_2 <- share_eff_eqtp_a38_ZE %>%
  select(zempt,a38,share_eff_a38_ZE_19,share_eff_a38_ZE_20) %>%
  left_join(shock_a38_corr,by=c("zempt"="ZE2020","a38")) %>%
  filter(!is.na(a38)) %>%
  replace(is.na(.),0) %>%
  mutate(prod_part_ent_concernées_2=share_eff_a38_ZE_19*part_ent_concernées_a38_2,
         prod_part_ent_concernées_p_2=share_eff_a38_ZE_19*part_ent_concernées_a38_p_2,
         prod_part_ent_malus_2=share_eff_a38_ZE_19*part_ent_malus_a38_2,
         prod_part_ent_malus_p_2=share_eff_a38_ZE_19*part_ent_malus_a38_p_2,
         prod_part_ent_tx_max_2=share_eff_a38_ZE_19*part_ent_tx_max_a38_2,
         prod_part_ent_tx_max_p_2=share_eff_a38_ZE_19*part_ent_tx_max_a38_p_2,
         prod_tx_moy_modulation_2=share_eff_a38_ZE_19*tx_moy_modulation_a38_2,
         prod_tx_moy_modulation_p_2=share_eff_a38_ZE_19*tx_moy_modulation_a38_p_2
  )

# IV Bartik calculé sans la correction:
IV_Bartik <- produit_pour_calcul_IV_Bartik %>%
  group_by(zempt) %>%
  summarise(Bartik_part_ent_concernées=sum(prod_part_ent_concernées),
            Bartik_part_ent_concernées_p=sum(prod_part_ent_concernées_p),
            Bartik_part_ent_malus=sum(prod_part_ent_malus),
            Bartik_part_ent_malus_p=sum(prod_part_ent_malus_p),
            Bartik_part_ent_tx_max=sum(prod_part_ent_tx_max),
            Bartik_part_ent_tx_max_p=sum(prod_part_ent_tx_max_p),
            Bartik_tx_moy_modulation=sum(prod_tx_moy_modulation),
            Bartik_tx_moy_modulation_p=sum(prod_tx_moy_modulation_p)
  )

IV_Bartik_2 <- produit_pour_calcul_IV_Bartik_2 %>%
  group_by(zempt) %>%
  summarise(Bartik_part_ent_concernées_2=sum(prod_part_ent_concernées_2),
            Bartik_part_ent_concernées_p_2=sum(prod_part_ent_concernées_p_2),
            Bartik_part_ent_malus_2=sum(prod_part_ent_malus_2),
            Bartik_part_ent_malus_p_2=sum(prod_part_ent_malus_p_2),
            Bartik_part_ent_tx_max_2=sum(prod_part_ent_tx_max_2),
            Bartik_part_ent_tx_max_p_2=sum(prod_part_ent_tx_max_p_2),
            Bartik_tx_moy_modulation_2=sum(prod_tx_moy_modulation_2),
            Bartik_tx_moy_modulation_p_2=sum(prod_tx_moy_modulation_p_2)
  )




