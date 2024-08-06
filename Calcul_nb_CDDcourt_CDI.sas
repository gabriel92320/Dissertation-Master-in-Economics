
/* Librairie perso pour stocker les bases d'études à analyser en R: */
libname Mybases "C:\Users\POLEMOB_G_SKLENAR\Documents\Mémoire MiE\Bases_etudes";

/* Fichiers DPAE 2021 et 2022: */
libname dpae2019"\\casd.fr\casdfs\Projets\POLEMOB\Data\DPAE_DPAE_2019";
libname dpae2020"\\casd.fr\casdfs\Projets\POLEMOB\Data\DPAE_DPAE_2020";
libname dpae2021"\\casd.fr\casdfs\Projets\POLEMOB\Data\DPAE_DPAE_2021";
libname dpae2022"\\casd.fr\casdfs\Projets\POLEMOB\Data\DPAE_DPAE_2022";

/* 1. Calcul de la durée contractuelle pour chaque embauche en CDD */

data contrats_DPAE_2019 (keep=siret type_contrat duree_CDD_j date_embauche date_fin_cdd CDD_court CDD CDI);
retain siret type_contrat duree_CDD_j date_embauche date_fin_cdd CDD_court CDD CDI; 
set Dpae2019.Dpae_casd_2019;

/* Durée contractuelle d'un CDD: */
if type_contrat=1 then do;
duree_CDD_j=date_fin_cdd-date_embauche;
end;
else do;
duree_CDD_j=.;
end;

/* Compteur des CDD dit "courts", ie d'une durée<31 jours: */
if type_contrat=1 then do;
if duree_CDD_j<=31 then CDD_court=1;else CDD_court=0;
end;
else do;
CDD_court=.;
end;

/* Compteur des CDD: */
if type_contrat=1 then CDD=1;else CDD=0;

/* Compteur des CDI: */
if type_contrat=2 then CDI=1;else CDI=0;

where type_contrat in (1,2); /* exclusion des missions d'intérim, car pas de mention de l'entreprise utilisatrice dans les DPAE. */

run;


/* agrégation par établissement: */
proc means data=contrats_DPAE_2019 noprint missing;
class siret;
var duree_CDD_j CDD_court CDD CDI;
output out=etab_DPAE_2019 
mean(duree_CDD_j)=duree_moy_CDD_j
sum(CDD_court)=nb_CDD_court
sum(CDD)=nb_CDD
sum(CDI)=nb_CDI
; 
run;

data etab_DPAE_2019 (drop=_TYPE_);
set etab_DPAE_2019 (rename=(_FREQ_=nb_CDI_CDD));
if siret='' then delete;

duree_moy_CDD_j=round(duree_moy_CDD_j,.1);

/* outcome 3: ratio flux d'embauches en CDD courts/flux d'embauches en CDD */
if nb_CDD>0 then do;
tx_emb_CDD_court=round(nb_CDD_court/nb_CDD*100,.1);
end;
else do;
tx_emb_CDD_court=.;
end;

/* outcome 4: ratio flux d'embauches en CDI /flux d'embauches en CDD ou en CDI */
if nb_CDI_CDD>0 then do;
tx_emb_CDI=round(nb_CDI/nb_CDI_CDD*100,.1);
end;
else do;
tx_emb_CDI=.;
end;

run;



/* 2020 */

data contrats_DPAE_2020 (keep=siret type_contrat duree_CDD_j date_embauche date_fin_cdd CDD_court CDD CDI);
retain siret type_contrat duree_CDD_j date_embauche date_fin_cdd CDD_court CDD CDI; 
set Dpae2020.Dpae_casd_2020;

/* Durée contractuelle d'un CDD: */
if type_contrat=1 then do;
duree_CDD_j=date_fin_cdd-date_embauche;
end;
else do;
duree_CDD_j=.;
end;

/* Compteur des CDD dit "courts", ie d'une durée<31 jours: */
if type_contrat=1 then do;
if duree_CDD_j<=31 then CDD_court=1;else CDD_court=0;
end;
else do;
CDD_court=.;
end;

/* Compteur des CDD: */
if type_contrat=1 then CDD=1;else CDD=0;

/* Compteur des CDI: */
if type_contrat=2 then CDI=1;else CDI=0;

where type_contrat in (1,2); /* exclusion des missions d'intérim, car pas de mention de l'entreprise utilisatrice dans les DPAE. */

run;


/* agrégation par établissement: */
proc means data=contrats_DPAE_2020 noprint missing;
class siret;
var duree_CDD_j CDD_court CDD CDI;
output out=etab_DPAE_2020 
mean(duree_CDD_j)=duree_moy_CDD_j
sum(CDD_court)=nb_CDD_court
sum(CDD)=nb_CDD
sum(CDI)=nb_CDI
; 
run;

data etab_DPAE_2020 (drop=_TYPE_);
set etab_DPAE_2020 (rename=(_FREQ_=nb_CDI_CDD));
if siret='' then delete;

duree_moy_CDD_j=round(duree_moy_CDD_j,.1);

/* outcome 3: ratio flux d'embauches en CDD courts/flux d'embauches en CDD */
if nb_CDD>0 then do;
tx_emb_CDD_court=round(nb_CDD_court/nb_CDD*100,.1);
end;
else do;
tx_emb_CDD_court=.;
end;

/* outcome 4: ratio flux d'embauches en CDI /flux d'embauches en CDD ou en CDI */
if nb_CDI_CDD>0 then do;
tx_emb_CDI=round(nb_CDI/nb_CDI_CDD*100,.1);
end;
else do;
tx_emb_CDI=.;
end;

run;


/* 2021 */
data contrats_DPAE_2021 (keep=siret type_contrat duree_CDD_j date_embauche date_fin_cdd CDD_court CDD CDI);
retain siret type_contrat duree_CDD_j date_embauche date_fin_cdd CDD_court CDD CDI; 
set Dpae2021.Dpae_casd_2021;

/* Durée contractuelle d'un CDD: */
if type_contrat=1 then do;
duree_CDD_j=date_fin_cdd-date_embauche;
end;
else do;
duree_CDD_j=.;
end;

/* Compteur des CDD dit "courts", ie d'une durée<31 jours: */
if type_contrat=1 then do;
if duree_CDD_j<=31 then CDD_court=1;else CDD_court=0;
end;
else do;
CDD_court=.;
end;

/* Compteur des CDD: */
if type_contrat=1 then CDD=1;else CDD=0;

/* Compteur des CDI: */
if type_contrat=2 then CDI=1;else CDI=0;

where type_contrat in (1,2); /* exclusion des missions d'intérim, car pas de mention de l'entreprise utilisatrice dans les DPAE. */

run;


/* agrégation par établissement: */
proc means data=contrats_DPAE_2021 noprint missing;
class siret;
var duree_CDD_j CDD_court CDD CDI;
output out=etab_DPAE_2021 
mean(duree_CDD_j)=duree_moy_CDD_j
sum(CDD_court)=nb_CDD_court
sum(CDD)=nb_CDD
sum(CDI)=nb_CDI
; 
run;

data etab_DPAE_2021 (drop=_TYPE_);
set etab_DPAE_2021 (rename=(_FREQ_=nb_CDI_CDD));
if siret='' then delete;

duree_moy_CDD_j=round(duree_moy_CDD_j,.1);

/* outcome 3: ratio flux d'embauches en CDD courts/flux d'embauches en CDD */
if nb_CDD>0 then do;
tx_emb_CDD_court=round(nb_CDD_court/nb_CDD*100,.1);
end;
else do;
tx_emb_CDD_court=.;
end;

/* outcome 4: ratio flux d'embauches en CDI /flux d'embauches en CDD ou en CDI */
if nb_CDI_CDD>0 then do;
tx_emb_CDI=round(nb_CDI/nb_CDI_CDD*100,.1);
end;
else do;
tx_emb_CDI=.;
end;

run;

/* 2022 */
data contrats_DPAE_2022 (keep=siret type_contrat duree_CDD_j date_embauche date_fin_cdd CDD_court CDD CDI);
retain siret type_contrat duree_CDD_j date_embauche date_fin_cdd CDD_court CDD CDI; 
set Dpae2022.dpae_casd_2022_prov;

/* Durée contractuelle d'un CDD: */
if type_contrat=1 then do;
duree_CDD_j=date_fin_cdd-date_embauche;
end;
else do;
duree_CDD_j=.;
end;

/* Compteur des CDD dit "courts", ie d'une durée<31 jours: */
if type_contrat=1 then do;
if duree_CDD_j<=31 then CDD_court=1;else CDD_court=0;
end;
else do;
CDD_court=.;
end;

/* Compteur des CDD: */
if type_contrat=1 then CDD=1;else CDD=0;

/* Compteur des CDI: */
if type_contrat=2 then CDI=1;else CDI=0;

where type_contrat in (1,2); /* exclusion des missions d'intérim, car pas de mention de l'entreprise utilisatrice dans les DPAE. */

run;

data contrats_DPAE_2022;
set contrats_DPAE_2022;
where type_contrat in (1,2); 
run;

/* agrégation par établissement: */
proc means data=contrats_DPAE_2022 noprint missing;
class siret;
var duree_CDD_j CDD_court CDD CDI;
output out=etab_DPAE_2022 
mean(duree_CDD_j)=duree_moy_CDD_j
sum(CDD_court)=nb_CDD_court
sum(CDD)=nb_CDD
sum(CDI)=nb_CDI
; 
run;

data etab_DPAE_2022 (drop=_TYPE_);
set etab_DPAE_2022 (rename=(_FREQ_=nb_CDI_CDD));
if siret='' then delete;

duree_moy_CDD_j=round(duree_moy_CDD_j,.1);

/* outcome 3: ratio flux d'embauches en CDD courts/flux d'embauches en CDD */
if nb_CDD>0 then do;
tx_emb_CDD_court=round(nb_CDD_court/nb_CDD*100,.1);
end;
else do;
tx_emb_CDD_court=.;
end;

/* outcome 4: ratio flux d'embauches en CDI /flux d'embauches en CDD ou en CDI */
if nb_CDI_CDD>0 then do;
tx_emb_CDI=round(nb_CDI/nb_CDI_CDD*100,.1);
end;
else do;
tx_emb_CDI=.;
end;

run;

/* Sauvegarde des bases finales: */

data Mybases.etab_DPAE_2019;
set etab_DPAE_2019;
run;
data Mybases.etab_DPAE_2020;
set etab_DPAE_2020;
run;
data Mybases.etab_DPAE_2021;
set etab_DPAE_2021;
run;
data Mybases.etab_DPAE_2022;
set etab_DPAE_2022;
run;



