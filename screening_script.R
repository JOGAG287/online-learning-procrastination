###### Screening data.frame Procrastination & Perfectionisme project
#### January 6th 2021

library(tidyverse)

#### Import data
raw_data <- read_delim("raw-data.csv", ";", 
                       escape_double = FALSE, trim_ws = TRUE)

### Get ride of random eead1 var.
raw_data <- raw_data[ ,-c(108)]

# eead variables are characters need to change to numeric
str(raw_data[ ,c(108:121)])

table(raw_data[ ,c(108)])


### Recoder les variables eead

rec_data <- raw_data %>% 
    mutate(eead1_rec = dplyr::recode(
    eead1_1,
    "La plupart du temps" = 1,
    "Très souvent" = 2,
    "De temps en temps" = 3,
    "Jamais" = 4))

rec_data <- rec_data %>% 
    mutate(eead2_rec = dplyr::recode(
        eead2,
        "Tout à fait autant" = 1,
        "Pas tout à fait autan" = 2,
        "Un peu seulement" = 3,
        "Presque pas du tout" = 4))

rec_data <- rec_data %>% 
    mutate(eead3_rec = dplyr::recode(
        eead3,
        "Oui, très nettement et c’est plutôt g" = 1,
        "Oui, mais ce n’est pas trop grave" = 2,
        "Un peu, mais cela ne m’inquiète pas" = 3,
        "Pas du tout" = 4))

rec_data <- rec_data %>% 
    mutate(eead4_rec = dplyr::recode(
        eead4,
        "Autant que par le passé" = 1,
        "Pas tout à fait autant maintenan" = 2,
        "Vraiment moins qu’avant" = 3,
        "Plus du tout" = 4))

rec_data <- rec_data %>% 
    mutate(eead5_rec = dplyr::recode(
        eead5,
        "Très souvent" = 1,
        "Assez souvent" = 2,
        "De temps en temps mais pas trop souvent" = 3,
        "Seulement à l’occasion" = 4))

rec_data <- rec_data %>% 
    mutate(eead6_rec = dplyr::recode(
        eead6,
        "Jamais" = 1,
        "Pas souvent" = 2,
        "Parfois" = 3,
        "La plupart du temps" = 4))

rec_data <- rec_data %>% 
    mutate(eead7_rec = dplyr::recode(
        eead7,
        "Oui, tout à fai" = 1,
        "Habituellement" = 2,
        "Pas souvent" = 3,
        "Jamais" = 4))

rec_data <- rec_data %>% 
    mutate(eead8_rec = dplyr::recode(
        eead8,
        "Presque toujours" = 1,
        "Très souvent" = 2,
        "Parfois" = 3,
        "Pas du tout" = 4))

rec_data <- rec_data %>% 
    mutate(eead9_rec = dplyr::recode(
        eead9,
        "Jamais" = 1,
        "Parfois" = 2,
        "Assez souvent" = 3,
        "Très souvent" = 4))

rec_data <- rec_data %>% 
    mutate(eead10_rec = dplyr::recode(
        eead10,
        "Je ne m’y intéresse plus du tout" = 1,
        "Je n’y accorde pas autant d’attention que je le dev" = 2,
        "Il se peut que je n’y fasse pas autant attention" = 3,
        "J’y prête autant d’attention que par le passé" = 4))

rec_data <- rec_data %>% 
    mutate(eead11_rec = dplyr::recode(
        eead11,
        "Oui, beaucoup" = 1,
        "Assez" = 2,
        "Pas beaucoup" = 3,
        "Jamais" = 4))

rec_data <- rec_data %>% 
    mutate(eead12_rec = dplyr::recode(
        eead12,
        "Autant qu’avant" = 1,
        "Plutôt moins qu’av" = 2,
        "Bien moins qu’avant" = 3,
        "Presque jamais" = 4))

rec_data <- rec_data %>% 
    mutate(eead13_rec = dplyr::recode(
        eead13,
        "Vraiment très souven" = 1,
        "Assez souvent" = 2,
        "Pas très souvent" = 3,
        "Jamais" = 4))

rec_data <- rec_data %>% 
    mutate(eead14_rec = dplyr::recode(
        eead14,
        "Souvent" = 1,
        "Parfois" = 2,
        "Peu souvent" = 3,
        "Très raremen" = 4))

# Remove uncoded eead vars
recoded_data <- rec_data[ ,-c(108:121)]

save(recoded_data, file = "recod_data.Rda")

# All the code above is now useless ---------------------------------------
library(tidyverse)

summary(recoded_data)

## Missing data 
percentmiss <- function(x) {
    sum(is.na(x) / length(x)) * 100
}
rowmiss <- apply(recoded_data[ ,-c(1:22, 108, 115)], 1, percentmiss)
table(rowmiss)

nomiss_data <- subset(recoded_data ,rowmiss <= 5)

table(is.na(nomiss_data[ ,-c(1:22, 108, 115)])) # No NAs

save(nomiss_data, file = "nomiss_dat.Rda")


# Scores totaux -----------------------------------------------------------
library(tidyverse)

load("nomiss_dat.Rda")

# PPS
nomiss_data <- nomiss_data %>% 
    mutate(pps_tot = rowSums(.[23:33]))

# Perfectionisme - QP-R - Recherche Hauts Standards
nomiss_data <- nomiss_data %>% 
    mutate(qpr_rhs = rowSums(.[34:40]))

# Perfectionisme - QP-R - Préoccupations perfectionnistes
nomiss_data <- nomiss_data %>% 
    mutate(qpr_pp = rowSums(.[41:53]))

# Flex. Psy - mpfi
nomiss_data <- nomiss_data %>% 
    mutate(mpfi_tot = rowSums(.[54:65]))

# Self-efficacy - GSES
nomiss_data <- nomiss_data %>% 
    mutate(gses_tot = rowSums(.[66:75]))

# Questionnaire multimodal d'engagement scolaire 
# Engagement optimal

eng_opt_vars <- c("qmes1", "qmes10", "qmes19", "qmes4", "qmes13", 
                  "qmes22", "qmes7", "qmes16", "qmes25")

nomiss_data <- nomiss_data %>% 
    mutate(qmes_eng_opt = rowSums(select(., all_of(eng_opt_vars))))

# Questionnaire multimodal d'engagement scolaire 
# Sur Eng Scol

sur_eng_scol_vars <- c("qmes2", "qmes11", "qmes20", "qmes5", "qmes14", 
                  "qmes23", "qmes8", "qmes17", "qmes26")

nomiss_data <- nomiss_data %>% 
    mutate(qmes_sur_eng_scol = rowSums(select(., all_of(sur_eng_scol_vars))))

# Questionnaire multimodal d'engagement scolaire 
# Sous Eng Scol

sous_eng_scol_vars <- c("qmes3", "qmes12", "qmes21", "qmes6", "qmes15", 
                        "qmes24", "qmes9", "qmes18", "qmes27")

nomiss_data <- nomiss_data %>% 
    mutate(qmes_sous_eng_scol = rowSums(select(., all_of(sous_eng_scol_vars))))

# Satisfaction dans les études
nomiss_data <- nomiss_data %>% 
    mutate(ese_tot = rowSums(.[103:107]))

# save files

save(nomiss_data, file = "nomiss_totscores.Rda")

totscores <- nomiss_data[ ,c(1:22, 115, 130:138)]

save(totscores, file = "totscores.Rda")


# Analyses -----------------------------------------------------------------
library(psych)
load("totscores.Rda")

studyvars <- totscores[ ,-c(1:22)]

corr <- cor(studyvars, method = "pearson")
round((corr),2)

corr2 <- print(corr.test(studyvars, method="pearson", adjust = "holm", alpha =.05), short=FALSE)

pval <- corr2$raw.p
rval <- corr2$raw.r

xy <- cbind.data.frame(rval, pval)
xy

