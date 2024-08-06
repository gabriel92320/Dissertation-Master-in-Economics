
# Ce script R réalise toutes les estimations économétriques dans le but d'évaluer un effet causal du bonus-malus
# sur les différents outcomes mesurés à l'échelle des ZE

# Estimation des effets causaux pour chaque outcome:

# Outcome 1: taux de séparation (nb_sép/EMA, en %)

# Outcome 2: taux d'embauche (nb_embauches/EMA, en %)

# Outcome 3: taux de CDD courts (nb_embauches_CDD_courts/embauches_CDD; en %)

# Outcome 4: taux d'embauches en CDI (flux d'embauches en CDI/flux total (CDD,CDI), en %)

# Outcome 5: niveau de l'emploi salarié (en log)

# Outcome 6: niveau des salaires bruts (en log)

# Note: on fait le choix d'utiliser ici les traitements non pondérés pour les estimations TSLS;
# On pourra mettre en Annexe les estimations obtenues lorsque l'on recourt aux traitements pondérés...


# Outcome 1: taux de séparation (en %), mesuré au niveau des établissements

# 1. TSLS (cross section 2022):

# All: outcome calculé sur le champ de tous les établissements implantés dans la ZE

# a. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome1_treatm1_all_2022 <- ivreg(tx_sep_all ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome1_treatm1_all_2022)
coeftest(TSLS_outcome1_treatm1_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome1_treatm2_all_2022 <- ivreg(tx_sep_all  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome1_treatm2_all_2022)
coeftest(TSLS_outcome1_treatm2_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome1_treatm3_all_2022 <- ivreg(tx_sep_all  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome1_treatm3_all_2022)
coeftest(TSLS_outcome1_treatm3_all_2022, vcoc = vcovHC, type="HC1")


# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome1_treatm1_t_2022 <- ivreg(tx_sep_et_t ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome1_treatm1_t_2022)
coeftest(TSLS_outcome1_treatm1_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome1_treatm2_t_2022 <- ivreg(tx_sep_et_t ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome1_treatm2_t_2022)
coeftest(TSLS_outcome1_treatm2_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome1_treatm3_t_2022 <- ivreg(tx_sep_et_t ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome1_treatm3_t_2022)
coeftest(TSLS_outcome1_treatm3_t_2022, vcoc = vcovHC, type="HC1")


# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée

# Traitement 1:
TSLS_outcome1_treatm1_nt_2022 <- ivreg(tx_sep_et_nt  ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome1_treatm1_nt_2022)
coeftest(TSLS_outcome1_treatm1_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome1_treatm2_nt_2022 <- ivreg(tx_sep_et_nt  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome1_treatm2_nt_2022)
coeftest(TSLS_outcome1_treatm2_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome1_treatm3_nt_2022 <- ivreg(tx_sep_et_nt  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome1_treatm3_nt_2022)
coeftest(TSLS_outcome1_treatm3_nt_2022, vcoc = vcovHC, type="HC1")


# 2. TSLS (panel 2019-2022):

# a. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome1_treatm1_all_2019_2022 <- plm(tx_sep_all ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome1_treatm1_all_2019_2022)
coeftest(TSLS_outcome1_treatm1_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome1_treatm2_all_2019_2022 <- plm(tx_sep_all ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome1_treatm2_all_2019_2022)
coeftest(TSLS_outcome1_treatm2_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome1_treatm3_all_2019_2022 <- plm(tx_sep_all ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome1_treatm3_all_2019_2022)
coeftest(TSLS_outcome1_treatm3_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome1_treatm1_t_2019_2022 <- plm(tx_sep_et_t ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome1_treatm1_t_2019_2022)
coeftest(TSLS_outcome1_treatm1_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome1_treatm2_t_2019_2022 <- plm(tx_sep_et_t ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome1_treatm2_t_2019_2022)
coeftest(TSLS_outcome1_treatm2_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome1_treatm3_t_2019_2022 <- plm(tx_sep_et_t ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome1_treatm3_t_2019_2022)
coeftest(TSLS_outcome1_treatm3_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)



# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée


# Traitement 1:
TSLS_outcome1_treatm1_nt_2019_2022 <- plm(tx_sep_et_nt ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome1_treatm1_nt_2019_2022)
coeftest(TSLS_outcome1_treatm1_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome1_treatm2_nt_2019_2022 <- plm(tx_sep_et_nt ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome1_treatm2_nt_2019_2022)
coeftest(TSLS_outcome1_treatm2_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome1_treatm3_nt_2019_2022 <- plm(tx_sep_et_nt ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome1_treatm3_nt_2019_2022)
coeftest(TSLS_outcome1_treatm3_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Outcome 2: Hiring rate

# Taux d'embauches (en %), mesuré au niveau des établissements: nb_embauches/eff_eqtp_et
# nombre d'embauches y compris interim (source: MMO du dispositif MIDAS).


# 1. TSLS (cross section 2022):

# a. All: outcome calculé sur le champ de tous les établissements implantés dans la ZE

# Traitement 1:
TSLS_outcome2_treatm1_all_2022 <- ivreg(tx_emb_all ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome2_treatm1_all_2022)
coeftest(TSLS_outcome2_treatm1_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome2_treatm2_all_2022 <- ivreg(tx_emb_all  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome2_treatm2_all_2022)
coeftest(TSLS_outcome2_treatm2_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome2_treatm3_all_2022 <- ivreg(tx_emb_all  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome2_treatm3_all_2022)
coeftest(TSLS_outcome2_treatm3_all_2022, vcoc = vcovHC, type="HC1")


# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome2_treatm1_t_2022 <- ivreg(tx_emb_et_t ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome2_treatm1_t_2022)
coeftest(TSLS_outcome2_treatm1_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome2_treatm2_t_2022 <- ivreg(tx_emb_et_t ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome2_treatm2_t_2022)
coeftest(TSLS_outcome2_treatm2_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome2_treatm3_t_2022 <- ivreg(tx_emb_et_t ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome2_treatm3_t_2022)
coeftest(TSLS_outcome2_treatm3_t_2022, vcoc = vcovHC, type="HC1")


# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée

# Traitement 1:
TSLS_outcome2_treatm1_nt_2022 <- ivreg(tx_emb_et_nt  ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome2_treatm1_nt_2022)
coeftest(TSLS_outcome2_treatm1_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome2_treatm2_nt_2022 <- ivreg(tx_emb_et_nt  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome2_treatm2_nt_2022)
coeftest(TSLS_outcome2_treatm2_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome2_treatm3_nt_2022 <- ivreg(tx_emb_et_nt  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome2_treatm3_nt_2022)
coeftest(TSLS_outcome2_treatm3_nt_2022, vcoc = vcovHC, type="HC1")


# 2. TSLS (panel 2019-2022):

# a. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome2_treatm1_all_2019_2022 <- plm(tx_emb_all ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome2_treatm1_all_2019_2022)
coeftest(TSLS_outcome2_treatm1_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome2_treatm2_all_2019_2022 <- plm(tx_emb_all ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome2_treatm2_all_2019_2022)
coeftest(TSLS_outcome2_treatm2_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome2_treatm3_all_2019_2022 <- plm(tx_emb_all ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome2_treatm3_all_2019_2022)
coeftest(TSLS_outcome2_treatm3_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome2_treatm1_t_2019_2022 <- plm(tx_emb_et_t ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome2_treatm1_t_2019_2022)
coeftest(TSLS_outcome2_treatm1_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome2_treatm2_t_2019_2022 <- plm(tx_emb_et_t ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome2_treatm2_t_2019_2022)
coeftest(TSLS_outcome2_treatm2_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome2_treatm3_t_2019_2022 <- plm(tx_emb_et_t ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome2_treatm3_t_2019_2022)
coeftest(TSLS_outcome2_treatm3_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)



# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée


# Traitement 1:
TSLS_outcome2_treatm1_nt_2019_2022 <- plm(tx_emb_et_nt ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome2_treatm1_nt_2019_2022)
coeftest(TSLS_outcome2_treatm1_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome2_treatm2_nt_2019_2022 <- plm(tx_emb_et_nt ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome2_treatm2_nt_2019_2022)
coeftest(TSLS_outcome2_treatm2_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome2_treatm3_nt_2019_2022 <- plm(tx_emb_et_nt ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome2_treatm3_nt_2019_2022)
coeftest(TSLS_outcome2_treatm3_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)



# Outcome 3: Short-term CDD rate -> flux d'embauches en CDD court/flux d'embauches en CDD, en %


# 1. TSLS (cross section 2022):

# a. All: outcome calculé sur le champ de tous les établissements implantés dans la ZE

# Traitement 1:
TSLS_outcome3_treatm1_all_2022 <- ivreg(tx_emb_CDD_court_et ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome3_treatm1_all_2022)
coeftest(TSLS_outcome3_treatm1_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome3_treatm2_all_2022 <- ivreg(tx_emb_CDD_court_et  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome3_treatm2_all_2022)
coeftest(TSLS_outcome3_treatm2_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome3_treatm3_all_2022 <- ivreg(tx_emb_CDD_court_et  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome3_treatm3_all_2022)
coeftest(TSLS_outcome3_treatm3_all_2022, vcoc = vcovHC, type="HC1")


# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome3_treatm1_t_2022 <- ivreg(tx_emb_CDD_court_et_t ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome3_treatm1_t_2022)
coeftest(TSLS_outcome3_treatm1_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome3_treatm2_t_2022 <- ivreg(tx_emb_CDD_court_et_t ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome3_treatm2_t_2022)
coeftest(TSLS_outcome3_treatm2_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome3_treatm3_t_2022 <- ivreg(tx_emb_CDD_court_et_t ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome3_treatm3_t_2022)
coeftest(TSLS_outcome3_treatm3_t_2022, vcoc = vcovHC, type="HC1")


# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée

# Traitement 1:
TSLS_outcome3_treatm1_nt_2022 <- ivreg(tx_emb_CDD_court_et_nt  ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome3_treatm1_nt_2022)
coeftest(TSLS_outcome3_treatm1_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome3_treatm2_nt_2022 <- ivreg(tx_emb_CDD_court_et_nt  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome3_treatm2_nt_2022)
coeftest(TSLS_outcome3_treatm2_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome3_treatm3_nt_2022 <- ivreg(tx_emb_CDD_court_et_nt  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome3_treatm3_nt_2022)
coeftest(TSLS_outcome3_treatm3_nt_2022, vcoc = vcovHC, type="HC1")


# 2. TSLS (panel 2019-2022):

# a. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome3_treatm1_all_2019_2022 <- plm(tx_emb_CDD_court_et ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome3_treatm1_all_2019_2022)
coeftest(TSLS_outcome3_treatm1_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome3_treatm2_all_2019_2022 <- plm(tx_emb_CDD_court_et ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome3_treatm2_all_2019_2022)
coeftest(TSLS_outcome3_treatm2_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome3_treatm3_all_2019_2022 <- plm(tx_emb_CDD_court_et ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome3_treatm3_all_2019_2022)
coeftest(TSLS_outcome3_treatm3_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome3_treatm1_t_2019_2022 <- plm(tx_emb_CDD_court_et_t ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome3_treatm1_t_2019_2022)
coeftest(TSLS_outcome3_treatm1_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome3_treatm2_t_2019_2022 <- plm(tx_emb_CDD_court_et_t ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome3_treatm2_t_2019_2022)
coeftest(TSLS_outcome3_treatm2_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome3_treatm3_t_2019_2022 <- plm(tx_emb_CDD_court_et_t ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome3_treatm3_t_2019_2022)
coeftest(TSLS_outcome3_treatm3_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)



# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée


# Traitement 1:
TSLS_outcome3_treatm1_nt_2019_2022 <- plm(tx_emb_CDD_court_et_nt ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome3_treatm1_nt_2019_2022)
coeftest(TSLS_outcome3_treatm1_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome3_treatm2_nt_2019_2022 <- plm(tx_emb_CDD_court_et_nt ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome3_treatm2_nt_2019_2022)
coeftest(TSLS_outcome3_treatm2_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome3_treatm3_nt_2019_2022 <- plm(tx_emb_CDD_court_et_nt ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome3_treatm3_nt_2019_2022)
coeftest(TSLS_outcome3_treatm3_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)




# Outcome 4: CDI hiring rate -> flux d'embauches en CDI/flux d'embauches en CDD ou en CDI, en %

# 1. TSLS (cross section 2022):

# a. All: outcome calculé sur le champ de tous les établissements implantés dans la ZE

# Traitement 1:
TSLS_outcome4_treatm1_all_2022 <- ivreg(tx_emb_CDI_et ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome4_treatm1_all_2022)
coeftest(TSLS_outcome4_treatm1_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome4_treatm2_all_2022 <- ivreg(tx_emb_CDI_et  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome4_treatm2_all_2022)
coeftest(TSLS_outcome4_treatm2_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome4_treatm3_all_2022 <- ivreg(tx_emb_CDI_et  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome4_treatm3_all_2022)
coeftest(TSLS_outcome4_treatm3_all_2022, vcoc = vcovHC, type="HC1")


# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome4_treatm1_t_2022 <- ivreg(tx_emb_CDI_et_t ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome4_treatm1_t_2022)
coeftest(TSLS_outcome4_treatm1_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome4_treatm2_t_2022 <- ivreg(tx_emb_CDI_et_t ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome4_treatm2_t_2022)
coeftest(TSLS_outcome4_treatm2_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome4_treatm3_t_2022 <- ivreg(tx_emb_CDI_et_t ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome4_treatm3_t_2022)
coeftest(TSLS_outcome4_treatm3_t_2022, vcoc = vcovHC, type="HC1")


# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée

# Traitement 1:
TSLS_outcome4_treatm1_nt_2022 <- ivreg(tx_emb_CDI_et_nt  ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome4_treatm1_nt_2022)
coeftest(TSLS_outcome4_treatm1_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome4_treatm2_nt_2022 <- ivreg(tx_emb_CDI_et_nt  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome4_treatm2_nt_2022)
coeftest(TSLS_outcome4_treatm2_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome4_treatm3_nt_2022 <- ivreg(tx_emb_CDI_et_nt  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome4_treatm3_nt_2022)
coeftest(TSLS_outcome4_treatm3_nt_2022, vcoc = vcovHC, type="HC1")


# 2. TSLS (panel 2019-2022):

# a. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome4_treatm1_all_2019_2022 <- plm(tx_emb_CDI_et ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome4_treatm1_all_2019_2022)
coeftest(TSLS_outcome4_treatm1_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome4_treatm2_all_2019_2022 <- plm(tx_emb_CDI_et ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome4_treatm2_all_2019_2022)
coeftest(TSLS_outcome4_treatm2_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome4_treatm3_all_2019_2022 <- plm(tx_emb_CDI_et ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome4_treatm3_all_2019_2022)
coeftest(TSLS_outcome4_treatm3_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome4_treatm1_t_2019_2022 <- plm(tx_emb_CDI_et_t ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome4_treatm1_t_2019_2022)
coeftest(TSLS_outcome4_treatm1_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome4_treatm2_t_2019_2022 <- plm(tx_emb_CDI_et_t ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome4_treatm2_t_2019_2022)
coeftest(TSLS_outcome4_treatm2_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome4_treatm3_t_2019_2022 <- plm(tx_emb_CDI_et_t ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome4_treatm3_t_2019_2022)
coeftest(TSLS_outcome4_treatm3_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)



# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée


# Traitement 1:
TSLS_outcome4_treatm1_nt_2019_2022 <- plm(tx_emb_CDI_et_nt ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome4_treatm1_nt_2019_2022)
coeftest(TSLS_outcome4_treatm1_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome4_treatm2_nt_2019_2022 <- plm(tx_emb_CDI_et_nt ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome4_treatm2_nt_2019_2022)
coeftest(TSLS_outcome4_treatm2_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome4_treatm3_nt_2019_2022 <- plm(tx_emb_CDI_et_nt ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome4_treatm3_nt_2019_2022)
coeftest(TSLS_outcome4_treatm3_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)



# Outcome 5: niveau de l'emploi salarié (en log)

# 1. TSLS (cross section 2022):

# All: outcome calculé sur le champ de tous les établissements implantés dans la ZE

# a. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome5_treatm1_all_2022 <- ivreg(log_eff_ZE_et_all  ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome5_treatm1_all_2022)
coeftest(TSLS_outcome5_treatm1_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome5_treatm2_all_2022 <- ivreg(log_eff_ZE_et_all  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome5_treatm2_all_2022)
coeftest(TSLS_outcome5_treatm2_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome5_treatm3_all_2022 <- ivreg(log_eff_ZE_et_all  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome5_treatm3_all_2022)
coeftest(TSLS_outcome5_treatm3_all_2022, vcoc = vcovHC, type="HC1")


# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome5_treatm1_t_2022 <- ivreg(log_eff_ZE_et_t  ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome5_treatm1_t_2022)
coeftest(TSLS_outcome5_treatm1_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome5_treatm2_t_2022 <- ivreg(log_eff_ZE_et_t  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome5_treatm2_t_2022)
coeftest(TSLS_outcome5_treatm2_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome5_treatm3_t_2022 <- ivreg(log_eff_ZE_et_t  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome5_treatm3_t_2022)
coeftest(TSLS_outcome5_treatm3_t_2022, vcoc = vcovHC, type="HC1")


# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée

# Traitement 1:
TSLS_outcome5_treatm1_nt_2022 <- ivreg(log_eff_ZE_et_nt  ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome5_treatm1_nt_2022)
coeftest(TSLS_outcome5_treatm1_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome5_treatm2_nt_2022 <- ivreg(log_eff_ZE_et_nt  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome5_treatm2_nt_2022)
coeftest(TSLS_outcome5_treatm2_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome5_treatm3_nt_2022 <- ivreg(log_eff_ZE_et_nt  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome5_treatm3_nt_2022)
coeftest(TSLS_outcome5_treatm3_nt_2022, vcoc = vcovHC, type="HC1")


# 2. TSLS (panel 2019-2022):

# a. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome5_treatm1_all_2019_2022 <- plm(log_eff_ZE_et_all ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome5_treatm1_all_2019_2022)
coeftest(TSLS_outcome5_treatm1_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)
# Weak instruments: first-stage regressions
relevance_Bartik_part_ent_concernées_panel<- plm(part_ent_concernées_ZE ~ Bartik_part_ent_concernées_2,data=data_econo,
                                                 effect = "twoways",
                                                 model="within",
                                                 index=c("zempt","annee"))
summary(relevance_Bartik_part_ent_concernées_panel)
coeftest(relevance_Bartik_part_ent_concernées_panel, vcoc = vcovCL, cluster= ~zempt)
# # Wu-Hausman test for panel models (H0: the preferred model is random effects (no correlation between the
# # unique errors and the regressors in the model -> no endogenity) vs H1: the model is fixed effects;
# wi<- plm(log_eff_ZE_et_all ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
#          data=data_econo,
#          effect = "twoways",
#          model="within",
#          index=c("zempt","annee"))
# re<-plm(log_eff_ZE_et_all ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
#         data=data_econo,
#         effect = "twoways",
#         model="random",
#         index=c("zempt","annee"))
# phtest(wi,re)

# Traitement 2:
TSLS_outcome5_treatm2_all_2019_2022 <- plm(log_eff_ZE_et_all ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome5_treatm2_all_2019_2022)
coeftest(TSLS_outcome5_treatm2_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)
# Weak instruments: first-stage regressions
relevance_Bartik_part_ent_malus_panel<- plm(part_ent_malus_ZE ~ Bartik_part_ent_malus_2,data=data_econo,
                                            effect = "twoways",
                                            model="within",
                                            index=c("zempt","annee"))
summary(relevance_Bartik_part_ent_malus_panel)
coeftest(relevance_Bartik_part_ent_malus_panel, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome5_treatm3_all_2019_2022 <- plm(log_eff_ZE_et_all ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome5_treatm3_all_2019_2022)
coeftest(TSLS_outcome5_treatm3_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)
# Weak instruments: first-stage regressions
relevance_Bartik_part_ent_tx_max_panel<- plm(part_ent_tx_max_ZE ~ Bartik_part_ent_tx_max_2,data=data_econo,
                                             effect = "twoways",
                                             model="within",
                                             index=c("zempt","annee"))
summary(relevance_Bartik_part_ent_tx_max_panel)
coeftest(relevance_Bartik_part_ent_tx_max_panel, vcoc = vcovCL, cluster= ~zempt)



# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome5_treatm1_t_2019_2022 <- plm(log_eff_ZE_et_t ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome5_treatm1_t_2019_2022)
coeftest(TSLS_outcome5_treatm1_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome5_treatm2_t_2019_2022 <- plm(log_eff_ZE_et_t ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome5_treatm2_t_2019_2022)
coeftest(TSLS_outcome5_treatm2_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome5_treatm3_t_2019_2022 <- plm(log_eff_ZE_et_t ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome5_treatm3_t_2019_2022)
coeftest(TSLS_outcome5_treatm3_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)



# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée


# Traitement 1:
TSLS_outcome5_treatm1_nt_2019_2022 <- plm(log_eff_ZE_et_nt ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome5_treatm1_nt_2019_2022)
coeftest(TSLS_outcome5_treatm1_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome5_treatm2_nt_2019_2022 <- plm(log_eff_ZE_et_nt ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome5_treatm2_nt_2019_2022)
coeftest(TSLS_outcome5_treatm2_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome5_treatm3_nt_2019_2022 <- plm(log_eff_ZE_et_nt ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome5_treatm3_nt_2019_2022)
coeftest(TSLS_outcome5_treatm3_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Outcome 6: niveau du salaire moyen par eqtp (en log)

# 1. TSLS (cross section 2022):

# All: outcome calculé sur le champ de tous les établissements implantés dans la ZE

# a. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome6_treatm1_all_2022 <- ivreg(log_sal_ZE_et_all  ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome6_treatm1_all_2022)
coeftest(TSLS_outcome6_treatm1_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome6_treatm2_all_2022 <- ivreg(log_sal_ZE_et_all  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome6_treatm2_all_2022)
coeftest(TSLS_outcome6_treatm2_all_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome6_treatm3_all_2022 <- ivreg(log_sal_ZE_et_all  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                        data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome6_treatm3_all_2022)
coeftest(TSLS_outcome6_treatm3_all_2022, vcoc = vcovHC, type="HC1")


# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome6_treatm1_t_2022 <- ivreg(log_sal_ZE_et_t  ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome6_treatm1_t_2022)
coeftest(TSLS_outcome6_treatm1_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome6_treatm2_t_2022 <- ivreg(log_sal_ZE_et_t  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome6_treatm2_t_2022)
coeftest(TSLS_outcome6_treatm2_t_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome6_treatm3_t_2022 <- ivreg(log_sal_ZE_et_t  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                      data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome6_treatm3_t_2022)
coeftest(TSLS_outcome6_treatm3_t_2022, vcoc = vcovHC, type="HC1")


# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée

# Traitement 1:
TSLS_outcome6_treatm1_nt_2022 <- ivreg(log_sal_ZE_et_nt  ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome6_treatm1_nt_2022)
coeftest(TSLS_outcome6_treatm1_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 2:
TSLS_outcome6_treatm2_nt_2022 <- ivreg(log_sal_ZE_et_nt  ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome6_treatm2_nt_2022)
coeftest(TSLS_outcome6_treatm2_nt_2022, vcoc = vcovHC, type="HC1")

# Traitement 3:
TSLS_outcome6_treatm3_nt_2022 <- ivreg(log_sal_ZE_et_nt  ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                       data=data_econo %>% filter(annee=="2022"))
summary(TSLS_outcome6_treatm3_nt_2022)
coeftest(TSLS_outcome6_treatm3_nt_2022, vcoc = vcovHC, type="HC1")


# 2. TSLS (panel 2019-2022):

# a. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome6_treatm1_all_2019_2022 <- plm(log_sal_ZE_et_all ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome6_treatm1_all_2019_2022)
coeftest(TSLS_outcome6_treatm1_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome6_treatm2_all_2019_2022 <- plm(log_sal_ZE_et_all ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome6_treatm2_all_2019_2022)
coeftest(TSLS_outcome6_treatm2_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome6_treatm3_all_2019_2022 <- plm(log_sal_ZE_et_all ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                           data=data_econo,
                                           effect = "twoways",
                                           model="within",
                                           index=c("zempt","annee"))
summary(TSLS_outcome6_treatm3_all_2019_2022)
coeftest(TSLS_outcome6_treatm3_all_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# b. Treated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise traitée

# Traitement 1:
TSLS_outcome6_treatm1_t_2019_2022 <- plm(log_sal_ZE_et_t ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome6_treatm1_t_2019_2022)
coeftest(TSLS_outcome6_treatm1_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome6_treatm2_t_2019_2022 <- plm(log_sal_ZE_et_t ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome6_treatm2_t_2019_2022)
coeftest(TSLS_outcome6_treatm2_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome6_treatm3_t_2019_2022 <- plm(log_sal_ZE_et_t ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                         data=data_econo,
                                         effect = "twoways",
                                         model="within",
                                         index=c("zempt","annee"))
summary(TSLS_outcome6_treatm3_t_2019_2022)
coeftest(TSLS_outcome6_treatm3_t_2019_2022, vcoc = vcovCL, cluster= ~zempt)



# c. Untreated: outcome calculé sur le champ des seuls établissements implantés dans la ZE qui sont rattachés
# é une entreprise non traitée


# Traitement 1:
TSLS_outcome6_treatm1_nt_2019_2022 <- plm(log_sal_ZE_et_nt ~ part_ent_concernées_ZE | Bartik_part_ent_concernées_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome6_treatm1_nt_2019_2022)
coeftest(TSLS_outcome6_treatm1_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)

# Traitement 2:
TSLS_outcome6_treatm2_nt_2019_2022 <- plm(log_sal_ZE_et_nt ~ part_ent_malus_ZE | Bartik_part_ent_malus_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome6_treatm2_nt_2019_2022)
coeftest(TSLS_outcome6_treatm2_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)


# Traitement 3:
TSLS_outcome6_treatm3_nt_2019_2022 <- plm(log_sal_ZE_et_nt ~ part_ent_tx_max_ZE | Bartik_part_ent_tx_max_2,
                                          data=data_econo,
                                          effect = "twoways",
                                          model="within",
                                          index=c("zempt","annee"))
summary(TSLS_outcome6_treatm3_nt_2019_2022)
coeftest(TSLS_outcome6_treatm3_nt_2019_2022, vcoc = vcovCL, cluster= ~zempt)

