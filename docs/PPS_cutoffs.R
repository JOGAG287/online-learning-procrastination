# Cluster based on PPS cutoffs --------------------------------------------

load(file = "totscores_w_cluster.Rda")


totscores_mod <- noout_dat

## Factorize proc_ouvert and cluster variables

totscores_mod <- totscores_mod %>% 
    mutate(cluster_res.hk = factor(cluster_res.hk,
                                   labels = c("1", "2"))) %>% 
    mutate(proc_ouv = factor(proc_ouvert,
                             labels = c("À diminué",
                                        "Demeuré le même",
                                        "Empiré beaucoup",
                                        "Empiré quelque peu")))

### Create group variable based on cutoffs


totscores_mod <- totscores_mod %>% 
    mutate(pps_cutoffs = dplyr::case_when(
        pps_tot <= 27 ~ "low", 
        (pps_tot > 27 & pps_tot <= 39) ~ "medium",
        (pps_tot > 39) ~ "high")) 

totscores_mod <- totscores_mod %>% 
    mutate(pps_cutoffs = factor(pps_cutoffs,
                                levels = c("low", "medium", "high")))



totscores_mod %>%
    anova_test(mpfi_tot ~ pps_cutoffs)

totscores_mod %>%
    anova_test(gses_tot ~ pps_cutoffs)

totscores_mod %>%
    anova_test(qmes_eng_opt ~ pps_cutoffs)

totscores_mod %>%
    anova_test(qmes_sur_eng_scol ~ pps_cutoffs)

totscores_mod %>%
    anova_test(qmes_sous_eng_scol ~ pps_cutoffs)

totscores_mod %>%
    anova_test(ese_tot ~ pps_cutoffs)


totscores_mod %>% 
    ggplot(aes(proc_ouv, fill = pps_cutoffs)) +
    geom_histogram(stat = "count", binwidth = .5, alpha = .5) +
    facet_wrap(~pps_cutoffs, ncol = 1) +
    theme(axis.text.x = element_text(angle = 90))


### Look for differences in perfectionnisme
totscores_mod %>% 
    anova_test(qpr_pp ~ pps_cutoffs)

totscores_mod %>% 
    anova_test(qpr_rhs ~ pps_cutoffs)

totscores_mod %>% 
    group_by(pps_cutoffs) %>% 
    get_summary_stats(qpr_rhs)


cont_table <- table(totscores_mod$proc_ouvert, totscores_mod$pps_cutoffs)
chisq.test(totscores_mod$proc_ouvert, totscores_mod$pps_cutoffs)


expect_fun <- function(x){
    expt_table = x[1] * sum(x[, 1])
    return(table(expt_table))
}
   
expt_table = xx[1] * sum()

expect_fun(xy)
test
