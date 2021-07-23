#### This script analyses cluster profils ####
#### July 23th 2021
#### Joel Gagnon


## Import library
library(tidyverse)
library(ggplot2)

## Load data
load(file = "totscores_w_cluster.Rda")


## Factorize proc_ouvert and cluster variables

totscores_mod <- totscores_mod %>% 
    mutate(cluster_res.hk = factor(cluster_res.hk,
                            labels = c("1", "2"))) %>% 
    mutate(cluster_res.diana = factor(cluster_res.diana,
                                      labels = c("1", "2"))) %>% 
    mutate(proc_ouv = factor(proc_ouvert,
                             labels = c("À diminué",
                                        "Demeuré le même",
                                        "Empiré beaucoup",
                                        "Empiré quelque peu")))
str(totscores_mod)

### Look for proc_ouvert differences

totscores_mod %>% 
    ggplot(aes(proc_ouv, fill = cluster_res.hk)) +
    geom_histogram(stat = "count", binwidth = .5, alpha = .5) +
    facet_wrap(~cluster_res.hk, ncol = 1) +
    theme(axis.text.x = element_text(angle = 90))

totscores_mod %>% 
    ggplot(aes(proc_ouv, fill = cluster_res.diana)) +
    geom_histogram(stat = "count", binwidth = .5, alpha = .5) +
    facet_wrap(~cluster_res.diana, ncol = 1) +
    theme(axis.text.x = element_text(angle = 90))


### Look for proc differences

totscores_mod %>% 
    ggplot(aes(pps_tot, fill = cluster_res.hk)) +
    geom_histogram(binwidth = .5, alpha = .5) +
    facet_wrap(~cluster_res.hk, ncol = 1) +
    theme(axis.text.x = element_text(angle = 90))

totscores_mod %>% 
    ggplot(aes(pps_tot, fill = cluster_res.diana)) +
    geom_histogram(binwidth = .5, alpha = .5) +
    facet_wrap(~cluster_res.diana, ncol = 1) +
    theme(axis.text.x = element_text(angle = 90))



library(rstatix)

totscores_mod %>% 
    group_by(cluster_res.hk) %>% 
    get_summary_stats(pps_tot)

totscores_mod %>%
    t_test(pps_tot ~ cluster_res.hk)


sum_stats <- totscores_mod %>% 
    group_by(cluster_res.hk) %>% 
    get_summary_stats(type = "mean_sd")

options(max.print=999)

sum_stats %>% 
    select(-n) %>% 
    print(n = 24)

### Look for differences in perfectionnisme
totscores_mod %>% 
    t_test(qpr_pp ~ cluster_res.hk)

totscores_mod %>% 
    t_test(qpr_rhs ~ cluster_res.hk)

totscores_mod %>% 
    group_by(cluster) %>% 
    get_summary_stats(qpr_rhs)




## t-test differences beween all other variables
totscores_mod %>%
    t_test(mpfi_tot ~ cluster)

totscores_mod %>%
    t_test(gses_tot ~ cluster)

totscores_mod %>%
    t_test(qmes_eng_opt ~ cluster)

totscores_mod %>%
    t_test(qmes_sur_eng_scol ~ cluster)

totscores_mod %>%
    t_test(qmes_sous_eng_scol ~ cluster)

totscores_mod %>%
    t_test(ese_tot ~ cluster)


# Test with clusters based on proc variable only --------------------------

totscores_mod <- totscores_mod %>% 
    mutate(cluster_proc = factor(cluster_proc, 
                                 labels = c("1", "2", "3", "4")))

totscores_mod %>% 
    ggplot(aes(proc_ouv, fill = cluster_proc)) +
    geom_histogram(stat = "count", binwidth = .5, alpha = .5) +
    facet_wrap(~cluster_proc, ncol = 1) +
    theme(axis.text.x = element_text(angle = 90))

### Look for proc differences

totscores_mod %>% 
    ggplot(aes(pps_tot, fill = cluster_proc)) +
    geom_histogram(binwidth = .5, alpha = .5) +
    facet_wrap(~cluster_proc, ncol = 1) +
    theme(axis.text.x = element_text(angle = 90))


totscores_mod %>% 
    group_by(cluster_proc) %>% 
    get_summary_stats(pps_tot)

totscores_mod %>% 
    anova_test(pps_tot ~ cluster_proc)


## anova beween all other variables
totscores_mod %>%
    anova_test(mpfi_tot ~ cluster_proc)

totscores_mod %>%
    anova_test(gses_tot ~ cluster_proc)

totscores_mod %>%
    anova_test(qmes_eng_opt ~ cluster_proc)

totscores_mod %>%
    anova_test(qmes_sur_eng_scol ~ cluster_proc)

totscores_mod %>%
    anova_test(qmes_sous_eng_scol ~ cluster_proc)

totscores_mod %>%
    anova_test(ese_tot ~ cluster_proc)

### Look for differences in perfectionnisme
totscores_mod %>% 
    anova_test(qpr_pp ~ cluster_proc)

totscores_mod %>% 
    anova_test(qpr_rhs ~ cluster_proc)
