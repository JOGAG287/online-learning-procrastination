############### This script analyses data from online learning - COVID ###############
### February 2021 ###
### Joel Gagnon ###


# Activate libraries ------------------------------------------------------
library(tidyverse)
library(ggplot2)
library(stringr)

# Load data ---------------------------------------------------------------
# load("totscores.Rda") - Data with sum scores on QMES
load("totscores_modif.Rda") # - Data with mean scores on QMES

colnames(totscores)

summary(totscores)

# Convert score on QMES from sum to mean ---------------------------------

#totscores_mod <- totscores %>% 
#    mutate(qmes_eng_opt = qmes_eng_opt / 9) %>% 
#    mutate(qmes_sur_eng_scol = qmes_sur_eng_scol / 9) %>% 
#    mutate(qmes_sous_eng_scol = qmes_sous_eng_scol / 9)

### Save new df ###
#save(totscores_mod, file = "totscores_modif.Rda")

# Analyses descriptives ---------------------------------------------------

## Distribution des genres
table(totscores_mod$genre)

## Distribution ethnie
table(totscores_mod$ethnie)

## Universite vs cegep
table(totscores_mod$school)

## Recode school variable to be university cepeg only

# Convert to lower letters
totscores_mod <- totscores_mod %>% 
    mutate(school_rec = str_to_lower(school))

table(totscores_mod$school_rec)

# Create patterns

vec_uni <- "uni|hec|poly"

vec_cegep <- "col|cége|lev|lim|enap|cec|ahun|bois"

# create new variables based on patterns

temp_data <- totscores_mod %>% 
    mutate(university = str_detect(school_rec, vec_uni)) %>% 
    mutate(cegep = str_detect(school_rec, vec_cegep))

table(temp_data$university)
table(temp_data$cegep)

# Same procedure for cegep_precisez var.
temp_data <- temp_data %>% 
    mutate(school_autre = str_to_lower(cegep_precisez))

# create new variables based on patterns
temp_data <- temp_data %>% 
    mutate(school_autre_uni = str_detect(school_autre, vec_uni)) %>% 
    mutate(school_autre_ceg = str_detect(school_autre, vec_cegep))

table(temp_data$school_autre_uni)
table(temp_data$school_autre_ceg)

# Factorize variables + Add NAs to factor variables
temp_data <- temp_data %>%
    mutate(uni_fac = factor(university,
                            levels = c(T, F),
                            labels = c(1, 2))) %>%  # where 1 == university
    mutate(uni_fac = recode_factor(uni_fac, 
                                   "2" = NA_character_)) %>%
    mutate(cegep_fac = factor(cegep,
                              levels = c(T, F),
                              labels = c(2, 1))) %>%  # Where 2 == cegep
    mutate(cegep_fac = recode_factor(cegep_fac, 
                                     "1" = NA_character_)) %>% 
    mutate(uni_fac_2 = factor(school_autre_uni,
                              levels = c(T, F),
                              labels = c(1, 2))) %>% 
    mutate(cegep_fac_2 = factor(school_autre_ceg,
                                levels = c(T, F),
                                labels = c(2, 1))) %>%
    mutate(uni_fac_2 = recode_factor(uni_fac_2,
                                     "2" = NA_character_)) %>% 
    mutate(cegep_fac_2 = recode_factor(cegep_fac_2,
                                       "1" = NA_character_))
    
str(temp_data)

see <- temp_data %>% 
    select(uni_fac, uni_fac_2, cegep_fac, cegep_fac_2)

# Combine variables
temp_data <- temp_data %>% 
    unite("school_regex", uni_fac, uni_fac_2, cegep_fac, cegep_fac_2, na.rm =T)

temp_data$school_regex[temp_data$school_regex == "1"] <- 1
temp_data$school_regex[temp_data$school_regex == "1_2"] <- 1
temp_data$school_regex[temp_data$school_regex == "2_2"] <- 2

table(temp_data$school_regex)
str(temp_data$school_regex)

# Recode united factor variable
temp_data <- temp_data %>% 
    mutate(school_regex_fac = factor(school_regex,
                                     labels = c("university", "cegep")))

table(temp_data$school_regex_fac)
str(temp_data$school_regex_fac)

# Save complete working df
save(temp_data, file = "regex_df.Rda")

# Remove duplicate variables

vec <- names(temp_data[ ,c(1:32, 39)])

donnees <- temp_data %>% 
    select(vec)

donnees


# Exploring sociodemo data ------------------------------------------------

## Possibly need to explore whether full time vs part time affects proc
table(donnees$regime_et)

table(donnees$prog_et)

## Possibly need to explore this variable also
table(donnees$format_cours)

## Also this one
table(donnees$traval_hrs)

## Compare LD vs non-LD
table(donnees$dx_tr_app)

## Wonder what this variable may be
table(donnees$diff_strat_et)

# Compare groups based on procrastination score ---------------------------

## keep only relevant variables

vec <- names(donnees[ ,c(1,3, 5, 10, 13, 18, 23, 26:31, 33)])


df <- donnees %>% 
    select(vec)

colnames(df)

## Pivot to long format

df <- df %>% 
    mutate(partno = 1:nrow(df))

vec_socio <- names(df[ ,c(1:6)])

df_long <- df %>% 
    pivot_longer(cols = 7:13,
                 names_to = c("variable"),
                 values_to = c("values"))
View(df_long)

save(df_long, file = "longdata.Rda")


### Remove in person participants as there are only 3 of them
df_long$format_cours[df_long$format_cours == "Complètement en présentiel"] <- NA

## Remove other gender
df_long$genre[df_long$genre == "Autre (spécifier ci-dessous"] <- NA

# Gender on procrastination -----------------------------------------------
df_proc <- df_long %>% 
    filter(variable == "pps_tot")

df_proc <- na.omit(df_proc)


ggplot(df_proc, aes(variable, values, fill = genre)) +
    geom_boxplot() +
    labs( x = "procrastination", y = "Score de procrastination",
          title = "Comparaison des scores de procrastination en fonction du genre") +
    theme_minimal()

ggplot(df_proc, aes(values, fill = genre)) +
    geom_histogram(bins = 25) +
    facet_wrap(~genre, ncol = 1) +
    labs( x = "procrastination", y = "Score de procrastination",
          title = "Comparaison des scores de procrastination en fonction du genre") +
    theme_minimal()

# Gender on school engagement ---------------------------------------------

### Engagement optimal

df_eng_opt <- df_long %>%
    filter(variable == "qmes_eng_opt")

df_eng_opt <- na.omit(df_eng_opt)

ggplot(df_eng_opt, aes(variable, values, fill = genre)) +
    geom_boxplot() + 
    labs( x = "Engagement optimal", y = "Score d'engagment",
           title = "Comparaison des scores d'engagement optimal en fonction du genre") +
    theme_minimal()

### Surengagement
df_sureng <- df_long %>%
    filter(variable == "qmes_sur_eng_scol")

df_sureng <- na.omit(df_sureng)

ggplot(df_sureng, aes(variable, values, fill = genre)) +
    geom_boxplot() + 
    labs( x = "Sur-Engagement", y = "Score de surengagement",
          title = "Comparaison des scores de surengagement en fonction du genre") +
    theme_minimal()

### Sousrengagement
df_souseng <- df_long %>%
    filter(variable == "qmes_sous_eng_scol")

df_souseng <- na.omit(df_souseng)

ggplot(df_souseng, aes(variable, values, fill = genre)) +
    geom_boxplot() + 
    labs( x = "Sous-Engagement", y = "Score de sous-engagement",
          title = "Comparaison des scores de sous-engagement en fonction du genre") +
    theme_minimal()

# Study time on procrastination -------------------------------------------
df_proc <- df_long %>%
    filter(variable == "pps_tot")

df_proc <- na.omit(df_proc)

ggplot(df_souseng, aes(variable, values, fill = regime_et)) +
    geom_boxplot() + 
    labs( x = "Procrastination", y = "Score de procrastination",
          title = "Comparaison des scores de procrastination en fonction du régime d'études") +
    theme_minimal()

# Study time on engagement ------------------------------------------------
df_eng_opt <- df_long %>%
    filter(variable == "qmes_eng_opt")

df_eng_opt <- na.omit(df_eng_opt)

ggplot(df_eng_opt, aes(variable, values, fill = regime_et)) +
    geom_boxplot() + 
    labs( x = "Engagement optimal", y = "Score d'engagment",
          title = "Comparaison des scores d'engagement optimal en fonction du régime d'études") +
    theme_minimal()

### Surengagement
df_sureng <- df_long %>%
    filter(variable == "qmes_sur_eng_scol")

df_sureng <- na.omit(df_sureng)

ggplot(df_sureng, aes(variable, values, fill = regime_et)) +
    geom_boxplot() + 
    labs( x = "Sur-Engagement", y = "Score de surengagement",
          title = "Comparaison des scores de surengagement en fonction du régime d'études") +
    theme_minimal()

### Sousrengagement
df_souseng <- df_long %>%
    filter(variable == "qmes_sous_eng_scol")

df_souseng <- na.omit(df_souseng)

ggplot(df_souseng, aes(variable, values, fill = regime_et)) +
    geom_boxplot() + 
    labs( x = "Sous-Engagement", y = "Score de sous-engagement",
          title = "Comparaison des scores de sous-engagement en fonction du régime d'études") +
    theme_minimal()

# Online vs partially online on procrastination -----------------------------------
df_proc <- df_long %>%
    filter(variable == "pps_tot")

df_proc <- na.omit(df_proc)

ggplot(df_proc, aes(variable, values, fill = format_cours)) +
    geom_boxplot() + 
    labs( x = "Procrastination", y = "Score de procrastination",
          title = "Comparaison des scores de procrastination en fonction de cours en ligne ou en présentiel") +
    theme_minimal()

###                          ###
### Worth investigating here ###
###                          ###

### Run t-test
library(rstatix)
library(coin)

## Factorize variable

df_proc <- df_proc %>% 
    mutate(format_cours = factor(format_cours,
                                 labels = c("distance", "partiel_distance")))

## Levene
df_proc %>% 
    levene_test(values~format_cours)

## Run t-test
df_proc %>% 
    t_test(values ~ format_cours,
           paired = F, 
           alternative = "two.sided")

df_proc %>% 
    group_by(format_cours) %>% 
    summarize(mean = mean(values), 
              sd = sd(values))

###                                      ###
### conclusion : Potentially interesting ###
###                                      ###


# Comparing scores online vs in-person procrastination --------------------

## Compute total PPS score

pps_vec <- names(cleanmerge[ ,c(2:12)])

pps <- cleanmerge %>% 
    mutate(pps_tot = rowSums(select(., all_of(pps_vec))))

summary(pps)

## Keep only totscore and gender
pps_df <- pps %>% 
    select(sexe, pps_tot)

## Save file
save(pps_df, file = "pps_big_data.Rda")

## T test
load("pps_big_data.Rda")

online_proc <- df %>% 
    filter(format_cours == "Complètement à distance (en ligne") %>% 
    select(pps_tot)

online_proc %>% 
    summarize(mean = mean(pps_tot), sd = sd(pps_tot))

t.test(online_proc$pps_tot,
       pps_df$pps_tot,
       paired = F,
       alternative = "greater")

p_online_proc <- df %>% 
    filter(format_cours == "Partiellement à distance (en ligne") %>% 
    select(pps_tot)

p_online_proc %>% 
    summarize(mean = mean(pps_tot), sd = sd(pps_tot))

t.test(p_online_proc$pps_tot,
       pps_df$pps_tot,
       paired = F,
       alternative = "greater")


mean(pps_df$pps_tot, na.rm = T)

# Online vs partially online on engagement -----------------------------------

df_eng_opt <- df_long %>%
    filter(variable == "qmes_eng_opt")

df_eng_opt <- na.omit(df_eng_opt)

ggplot(df_eng_opt, aes(variable, values, fill = format_cours)) +
    geom_boxplot() + 
    labs( x = "Engagement optimal", y = "Score d'engagment",
          title = "Comparaison des scores d'engagement optimal en fonction de cours en ligne ou en présentiel") +
    theme_minimal()

### Surengagement
df_sureng <- df_long %>%
    filter(variable == "qmes_sur_eng_scol")

df_sureng <- na.omit(df_sureng)

ggplot(df_sureng, aes(variable, values, fill = format_cours)) +
    geom_boxplot() + 
    labs( x = "Sur-Engagement", y = "Score de surengagement",
          title = "Comparaison des scores de surengagement en fonction de cours en ligne ou en présentiel") +
    theme_minimal()

###                          ###
### Worth investigating here ###
###                          ###

ggplot(df_sureng, aes(values, fill = format_cours)) +
    geom_histogram(alpha = 1.75, position = position_dodge(.6),
                   bins = 45, binwidth = .25) + 
    labs( x = "Sur-Engagement", y = "Score de surengagement",
          title = "Comparaison des scores de surengagement en fonction de cours en ligne ou en présentiel") +
    theme_minimal() +
    scale_fill_brewer(palette = "Dark2")

### Run t-test
library(rstatix)
library(coin)

## Factorize variable

df_sureng <- df_sureng %>% 
    mutate(format_cours = factor(format_cours,
                                 labels = c("distance", "partiel_distance")))

## Levene
df_sureng %>% 
    levene_test(values~format_cours)

## Run t-test
df_sureng %>% 
    t_test(values ~ format_cours,
           paired = F, 
           alternative = "two.sided")

df_sureng %>% 
    group_by(format_cours) %>% 
    summarize(mean = mean(values), 
              sd = sd(values))

###                                  ###
### conclusion : nothing to see here ###
###                                  ###

### Sousrengagement
df_souseng <- df_long %>%
    filter(variable == "qmes_sous_eng_scol")

df_souseng <- na.omit(df_souseng)

ggplot(df_souseng, aes(variable, values, fill = format_cours)) +
    geom_boxplot() + 
    labs( x = "Sous-Engagement", y = "Score de sous-engagement",
          title = "Comparaison des scores de sous-engagement en fonction de cours en ligne ou en présentiel") +
    theme_minimal()

# School satisfaction between online and partially online -----------------
df_satis <- df_long %>%
    filter(variable == "ese_tot")

df_satis <- na.omit(df_satis)

ggplot(df_satis, aes(variable, values, fill = format_cours)) +
    geom_boxplot() + 
    labs( x = "Sous-Engagement", y = "Score de satisfaction envers les études",
          title = "Comparaison des scores de satisfaction envers les étudest en fonction de cours en ligne ou en présentiel") +
    theme_minimal()

# Niveau de proc : college vs university -------------
load("recod_data.Rda")

library(tidyverse)

proc_dat <- recoded_data %>%
    mutate(proc_ouvert = factor(
        proc_ouvert,
        labels = c("À diminué", "Le même",
                   "Empiré beaucoup", "Empiré quelque peu")
    ))

proc_dat <- proc_dat %>% 
    group_by(cycle_et) %>%
    select(proc_ouvert, cycle_et)
   
    
proc_dat %>% 
    drop_na(cycle_et) %>% 
    ggplot(aes(proc_ouvert, fill = cycle_et)) +
    geom_bar(position = "dodge") +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_discrete(name = "Niveau de procrastination")

# Niveau de proc : tr.apprentissage ------------------
table(recoded_data$dx_tr_app)
str(recoded_data$dx_tr_app)

ld_dat <- recoded_data %>%
    mutate(dx_tr_app = factor(dx_tr_app,
                              labels = c("Non", "Oui"))) %>%
    mutate(proc_ouvert = factor(
        proc_ouvert,
        labels = c("À diminué", "Le même",
                   "Empiré beaucoup", "Empiré quelque peu")
    )) %>% 
    select(dx_tr_app, proc_ouvert) %>% 
    group_by(dx_tr_app)

ld_dat %>% 
    drop_na(dx_tr_app) %>% 
    ggplot(aes(proc_ouvert, fill = dx_tr_app)) +
    geom_bar(position = "dodge") +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_discrete(name = "Niveau de procrastination")

# Niveau de proc online vs presentiel ------------------

format <- recoded_data %>%
    mutate(proc_ouvert = factor(
        proc_ouvert,
        labels = c("À diminué", "Le même",
                   "Empiré beaucoup", "Empiré quelque peu")
    ))

format <- format %>% 
    group_by(format_cours) %>%
    select(proc_ouvert, format_cours)

format %>% 
   drop_na(format_cours) %>% 
    ggplot(aes(proc_ouvert, fill = format_cours)) +
    geom_bar(position = "dodge") +
    theme(axis.text.x = element_text(angle = 90)) +
    scale_x_discrete(name = "Niveau de procrastination")



