# "Trust in the Count"
# Script 4: Conjoint

rm(list = ls())
gc()

# Load packages
library(labelled)
library(cregg)
library(kableExtra)
#library(devtools)
#install_github("naoki-egami/factorEx", dependencies=TRUE)
library(factorEx)
library(tidyverse)
# deal with package loading conflicts
conflicted::conflict_prefer('select', 'dplyr', quiet = TRUE)
conflicted::conflict_prefer('filter', 'dplyr', quiet = TRUE)

# Set working directory
# setwd("~/Desktop/trust-in-the-count")

# Read data
trust_in_the_count <-
  readRDS("data/trust_in_the_count_analysis.rds")


# Clean data for conjoint analysis ----------------------------------------
## Code additional necessary variables
### Audit Knowledge
trust_in_the_count <-
  trust_in_the_count |>
  mutate(
    audit_knowledge =
      (QCIA_2 == 'More than 50 percent of states, but not all') +
      (QCIA_3 == 'A small percentage of ballots cast') +
      (QCIA_4 == 'Audits are required by law') +
      (QCIA_5 == 'Not often at all') +
      (QCIA_8 == 'Less than 1 percent'),
    audit_knowledge_level = ifelse(audit_knowledge %in% c(0,1),"Low","High") |> factor()
  )

## Build dataframe for conjoint analysis with cregg
### We want a dataframe with the following variables.
###   Chosen: a binary variable representing whether or not the county was chosen
###   Percent: the percent of ballots examined
###   Diffs: The number of ballots that were different
###   Performer: Who conducted the audit
###   Available: How the audit was made available
###   Party: The winner's party
###   Office: The office of the winner

# Attention check passers only
base_df <- trust_in_the_count |> filter(AC_PASS_ALL == 1)

# 1st repetition: County A vs. County B
conjtop <-
  base_df |> select(
    caseid,
    weight,
    QCIA_12_table1,
    QCIA_13_table1,
    county_a_ballots,
    county_a_results,
    county_a_who,
    county_a_availability,
    county_a_winner,
    county_a_office,
    pid7_collapsed
  )
conjtop$QCIA_12_table1[unlabelled(conjtop$QCIA_12_table1) == 'County A'] <- 1
conjtop$QCIA_12_table1[unlabelled(conjtop$QCIA_12_table1) == 'County B'] <- 0

conjbot <- base_df |> select(
  caseid,
  weight,
  QCIA_12_table1,
  QCIA_14_table1,
  county_b_ballots,
  county_b_results,
  county_b_who,
  county_b_availability,
  county_b_winner,
  county_b_office,
  pid7_collapsed
)

conjbot$QCIA_12_table1[unlabelled(conjbot$QCIA_12_table1) == 'County A' |
                         unlabelled(conjbot$QCIA_12_table1) == 1] <- 0
conjbot$QCIA_12_table1[unlabelled(conjbot$QCIA_12_table1) == 'County B' |
                         unlabelled(conjbot$QCIA_12_table1) == 2] <- 1
names(conjtop) <- gsub('county_a_','',names(conjtop))
names(conjbot) <- gsub('county_b_','',names(conjbot))
names(conjtop)[grepl('QCIA_13',names(conjtop))] <- 'conf'
names(conjbot)[grepl('QCIA_14',names(conjbot))] <- 'conf'
conj <- rbind(conjtop, conjbot)
conj$comparision = 1
names(conj)[names(conj) == 'QCIA_12_table1'] <- 'chosen'

# 2nd repetition: County C vs. County D
conjtop <-
  base_df |> select(
    caseid,
    weight,
    QCIA_12_table2,
    QCIA_13_table2,
    county_c_ballots,
    county_c_results,
    county_c_who,
    county_c_availability,
    county_c_winner,
    county_c_office,
    pid7_collapsed
  )
conjtop$QCIA_12_table2[unlabelled(conjtop$QCIA_12_table2) == 'County C'] <- 1
conjtop$QCIA_12_table2[unlabelled(conjtop$QCIA_12_table2) == 'County D'] <- 0

conjbot <- base_df |> select(
  caseid,
  weight,
  QCIA_12_table2,
  QCIA_14_table2,
  county_d_ballots,
  county_d_results,
  county_d_who,
  county_d_availability,
  county_d_winner,
  county_d_office,
  pid7_collapsed
)
conjbot$QCIA_12_table2[unlabelled(conjbot$QCIA_12_table2) == 'County C' |
                         unlabelled(conjbot$QCIA_12_table2) == 1] <- 0
conjbot$QCIA_12_table2[unlabelled(conjbot$QCIA_12_table2) == 'County D' |
                         unlabelled(conjbot$QCIA_12_table2) == 2] <- 1
names(conjtop) <- gsub('county_c_','',names(conjtop))
names(conjbot) <- gsub('county_d_','',names(conjbot))
names(conjtop)[grepl('QCIA_13',names(conjtop))] <- 'conf'
names(conjbot)[grepl('QCIA_14',names(conjbot))] <- 'conf'
nextconj <- rbind(conjtop, conjbot)
nextconj$comparision = 2
names(nextconj)[names(nextconj) == 'QCIA_12_table2'] <- 'chosen'
conj <- rbind(conj, nextconj)

# 3rd repetition: County E vs. County F
conjtop <-
  base_df |> select(
    caseid,
    weight,
    QCIA_12_table3,
    QCIA_13_table3,
    county_e_ballots,
    county_e_results,
    county_e_who,
    county_e_availability,
    county_e_winner,
    county_e_office,
    pid7_collapsed
  )
conjtop$QCIA_12_table3[unlabelled(conjtop$QCIA_12_table3) == 'County E'] <- 1
conjtop$QCIA_12_table3[unlabelled(conjtop$QCIA_12_table3) == 'County F'] <- 0

conjbot <- base_df |> select(
  caseid,
  weight,
  QCIA_12_table3,
  QCIA_14_table3,
  county_f_ballots,
  county_f_results,
  county_f_who,
  county_f_availability,
  county_f_winner,
  county_f_office,
  pid7_collapsed
)

conjbot$QCIA_12_table3[unlabelled(conjbot$QCIA_12_table3) == 'County E' |
                         unlabelled(conjbot$QCIA_12_table3) == 1] <- 0
conjbot$QCIA_12_table3[unlabelled(conjbot$QCIA_12_table3) == 'County F' |
                         unlabelled(conjbot$QCIA_12_table3) == 2] <- 1
names(conjtop) <- gsub('county_e_','',names(conjtop))
names(conjbot) <- gsub('county_f_','',names(conjbot))
names(conjtop)[grepl('QCIA_13',names(conjtop))] <- 'conf'
names(conjbot)[grepl('QCIA_14',names(conjbot))] <- 'conf'
nextconj <- rbind(conjtop, conjbot)
nextconj$comparision = 3
names(nextconj)[names(nextconj) == 'QCIA_12_table3'] <- 'chosen'
conj <- rbind(conj, nextconj)

conj <- conj |>
  mutate(
    # Recode conidence score
    conf = ifelse(conf == 9, NA, conf),
    # Relabeling for better plot element labeling
    ballots = fct_recode(as.factor(ballots),
                         "1% of all ballots" = "1% of total ballots cast",
                         "5% of all ballots" = "5% of total ballots cast",
                         "10% of all ballots" = "10% of ballots cast"
    ),
    results = fct_recode(as.factor(results),
                         "0 ballots change" = "0 ballots",
                         "10 ballots change" = "10 ballots",
                         "100 ballots change" = "100 ballots"
    ),
    who = fct_recode(as.factor(who),
                     "State election administrators" = "State administrators"
    ),
    availability = fct_recode(as.factor(availability),
                              "No public results" = "No publicly available results of audit",
                              "Results given to media" = "Publicly communicating results of the audit to the media",
                              "Results released to the public" = "Publicly posting final results to the public"
    ),
    winner = as.factor(winner),
    office = as.factor(office),
    caseid = as.factor(caseid),
    pid7_collapsed = as.factor(pid7_collapsed),
    weight = weight,
    # Add variable for whether respondent PID match winner 
    cop_win = ifelse(
      pid7_collapsed == 'Democrat' & winner == 'Democrat' | pid7_collapsed == 'Republican' & winner == 'Republican',
      "Winner",
      "Loser") |> factor() |> relevel("Winner"),
    pair_id = str_c(caseid, comparision,sep = "-")
  ) |> 
  # Add additional variables for SM plots
  left_join(
    base_df |> 
      select(caseid, audit_knowledge_level, audit_info_add_group) |>
      mutate(caseid = as.factor(caseid))
    )


# Prep for Plots ----------------------------------------------------------
## Function to set plot arrangement
ArrangeForPlot <- function(df){
  #Put rows into a readable order for plotting
  df <- df[c(2,3,1, #1%, 5%, 10% ballots
             6,5,4, #0, 10, then 100 ballots
             8,9,7, #Local, state, outside
             12,11,10, #None, media, public
             13:nrow(df)
  ),
  ]
  #Rename feature for labels
  levels(df$feature)[levels(df$feature) == 'ballots'] <- "Audit size"
  levels(df$feature)[levels(df$feature) == 'results'] <- "Number of errors"
  levels(df$feature)[levels(df$feature) == 'who'] <- "Who audits"
  levels(df$feature)[levels(df$feature) == 'availability'] <- "Availability"
  levels(df$feature)[levels(df$feature) == 'winner'] <- "Winner's party"
  levels(df$feature)[levels(df$feature) == 'office'] <- "Office"
  return(df)
}

feature_colors <- c('#D7191C','#FDAE61','#A6611A','#ABDDA4','#2B83BA','#7B3294','#FFFFFF')
party_colors <- c("#AAAAAA","navy","red")
party_shape <- c(15,16,17)
party_labels <- c("Independent", "Democrat", "Republican")

# Main Analysis -----------------------------------------------------------
#### Figure 3 ####
amce_vals <-
  amce(
    data = conj,
    formula = chosen ~ ballots + results + who + availability + winner + office,
    weights = ~ weight,
    id = ~ caseid
  )
amce_to_plot <- ArrangeForPlot(amce_vals)
jpeg('plots/figure_3.jpeg', width=8, height=7, units='in', res=600)
plot(amce_to_plot,
     vline = 0,
     size = 2.5,
     theme = theme_bw(base_size=12)
) +  scale_color_manual(values = feature_colors) +
  geom_hline(yintercept=3, linetype='dashed', color='black') +
  geom_hline(yintercept=6, linetype='dashed', color='black') +
  geom_hline(yintercept=10, linetype='dashed', color='black') +
  geom_hline(yintercept=14, linetype='dashed', color='black') +
  geom_hline(yintercept=18, linetype='dashed', color='black')
dev.off()

#### Figure 4 ####
amce_vals_by_pty <- cj(data = conj,
                       formula = chosen ~ ballots + results + who + 
                         availability + winner + office,
                       weights = ~ weight,
                       id = ~ caseid,
                       estimate = 'amce',
                       by = ~pid7_collapsed
)
amcepty_to_plot <- ArrangeForPlot(amce_vals_by_pty)
names(amcepty_to_plot)[names(amcepty_to_plot) == 'pid7_collapsed'] <- 'Party'

jpeg('plots/figure_4.jpeg', width=8, height=7, units='in', res=600)
plot(amcepty_to_plot,
     vline = 0,
     size = 2.5,
     group = 'Party',
     theme = theme_bw(base_size=12)
) +
  aes(shape = Party) +
  scale_color_manual(values = party_colors, breaks = party_labels) +
  scale_shape_manual(values = party_shape, breaks = party_labels) +
  geom_hline(yintercept=3, linetype='dashed', color='black') +
  geom_hline(yintercept=6, linetype='dashed', color='black') +
  geom_hline(yintercept=10, linetype='dashed', color='black') +
  geom_hline(yintercept=14, linetype='dashed', color='black') +
  geom_hline(yintercept=18, linetype='dashed', color='black')
dev.off()

#### Retrieving Raw AMCE values ####
# AMCE values for main features
as_tibble(amce_to_plot) |>
  kable(format = "latex",booktabs = T,caption = "AMCE Summary") |>
  save_kable(file = "tables/amce_estimates.tex")

as_tibble(amcepty_to_plot) |>
  kable(format = "latex",booktabs = T,caption = "AMCE Summary (by Respondent PID)") |>
  save_kable(file = "tables/amce_estimates_by_pid.tex")

# Supplementary Materials --------------------------------------------------
#### SM, Figure E-2 ####
mm_vals_by_infoadd <-
  cj(
    data = conj,
    formula = chosen ~ ballots + results + who +  availability + winner + office,
    weights = ~ weight,
    id = ~ caseid,
    estimate = 'mm',
    by = ~ audit_info_add_group
  )
mmia_to_plot <- ArrangeForPlot(mm_vals_by_infoadd)

jpeg('plots/sm_figure_e-2.jpeg', width=8, height=7, units='in', res=600)
plot(mmia_to_plot,
     vline = 0,
     size = 2.5,
     group = "audit_info_add_group",
     theme = theme_bw(base_size=12)
) +
  aes(shape = audit_info_add_group, color = audit_info_add_group) +
  scale_color_manual(name = 'Treatment Condition',values = c('#1b9e77','#d95f02','#7570b3','#e7298a'), breaks = c("Control", "Audit-0", "Audit-1", "Audit-2")) +
  scale_shape_manual(name = 'Treatment Condition',values = c(15,16,17,18), breaks = c("Control", "Audit-0", "Audit-1", "Audit-2")) +
  guides(color = guide_legend(), shape = guide_legend()) +
  geom_hline(yintercept=3, linetype='dashed', color='black') +
  geom_hline(yintercept=6, linetype='dashed', color='black') +
  geom_hline(yintercept=10, linetype='dashed', color='black') +
  geom_hline(yintercept=14, linetype='dashed', color='black') +
  geom_hline(yintercept=18, linetype='dashed', color='black') +
  geom_vline(xintercept = 0.5, color = 'darkgrey')
dev.off()

#### SM, Figure E-3 ####
mm_vals_by_winner <-
  cj(
    data = conj,
    formula = chosen ~ ballots + results + who +  availability + winner + office,
    weights = ~ weight,
    id = ~ caseid,
    estimate = 'mm',
    by = ~ cop_win
  )
mm_cop_win_to_plot <- ArrangeForPlot(mm_vals_by_winner)

jpeg('plots/sm_figure_e-3.jpeg', width=8, height=7, units='in', res=600)
plot(mm_cop_win_to_plot,
     vline = 0,
     size = 2.5,
     group = "cop_win",
     theme = theme_bw(base_size=12)
) +
  aes(shape = cop_win, color = cop_win) +
  scale_color_manual(name = 'Co-partisan Candidate',values = c('darkgreen','goldenrod'), breaks = c("Winner","Loser")) +
  scale_shape_manual(name = 'Co-partisan Candidate',values = c(15,16), breaks = c("Winner","Loser")) +
  guides(color = guide_legend(), shape = guide_legend()) +
  geom_hline(yintercept=3, linetype='dashed', color='black') +
  geom_hline(yintercept=6, linetype='dashed', color='black') +
  geom_hline(yintercept=10, linetype='dashed', color='black') +
  geom_hline(yintercept=14, linetype='dashed', color='black') +
  geom_hline(yintercept=18, linetype='dashed', color='black') +
  geom_vline(xintercept = 0.5, color = 'darkgrey')
dev.off()

#### SM, Figure E-4 ####
amce_vals_by_winner <-
  cj(
    data = conj,
    formula = chosen ~ ballots + results + who +  availability + winner + office,
    weights = ~ weight,
    id = ~ caseid,
    estimate = 'amce',
    by = ~ cop_win
  )
amce_cop_win_to_plot <- ArrangeForPlot(amce_vals_by_winner)

jpeg('plots/sm_figure_e-4.jpeg', width=8, height=7, units='in', res=600)
plot(amce_cop_win_to_plot,
     vline = 0,
     size = 2.5,
     group = "cop_win",
     theme = theme_bw(base_size=12)
) +
  aes(shape = cop_win, color = cop_win) +
  scale_color_manual(name = 'Co-partisan Candidate',values = c('darkgreen','goldenrod'), breaks = c("Winner","Loser")) +
  scale_shape_manual(name = 'Co-partisan Candidate',values = c(15,16), breaks = c("Winner","Loser")) +
  guides(color = guide_legend(), shape = guide_legend()) +
  geom_hline(yintercept=3, linetype='dashed', color='black') +
  geom_hline(yintercept=6, linetype='dashed', color='black') +
  geom_hline(yintercept=10, linetype='dashed', color='black') +
  geom_hline(yintercept=14, linetype='dashed', color='black') +
  geom_hline(yintercept=18, linetype='dashed', color='black') +
  geom_vline(xintercept = 0.5, color = 'darkgrey')
dev.off()

#### SM, Figure E-5 ####
mm_vals_by_knowledge <-
  cj(
    data = conj,
    formula = chosen ~ ballots + results + who +  availability + winner + office,
    weights = ~ weight,
    id = ~ caseid,
    estimate = 'mm',
    by = ~ audit_knowledge_level
  )
mm_knowledge_to_plot <- ArrangeForPlot(mm_vals_by_knowledge)

jpeg('plots/sm_figure_e-5.jpeg', width=8, height=7, units='in', res=600)
plot(mm_knowledge_to_plot,
     vline = 0,
     size = 2.5,
     group = "audit_knowledge_level",
     theme = theme_bw(base_size=12)
) +
  aes(shape = audit_knowledge_level, color = audit_knowledge_level) +
  scale_color_manual(name = 'Knowledge Level',values = c('#fc8d62','#8da0cb'), breaks = c("High","Low")) +
  scale_shape_manual(name = 'Knowledge Level',values = c(15,16), breaks = c("High","Low")) +
  guides(color = guide_legend(), shape = guide_legend()) +
  geom_hline(yintercept=3, linetype='dashed', color='black') +
  geom_hline(yintercept=6, linetype='dashed', color='black') +
  geom_hline(yintercept=10, linetype='dashed', color='black') +
  geom_hline(yintercept=14, linetype='dashed', color='black') +
  geom_hline(yintercept=18, linetype='dashed', color='black') +
  geom_vline(xintercept = 0.5, color = 'darkgrey')
dev.off()

#### SM, Figure E-6 ####
amce_vals_by_knowledge <-
  cj(
    data = conj,
    formula = chosen ~ ballots + results + who +  availability + winner + office,
    weights = ~ weight,
    id = ~ caseid,
    estimate = 'amce',
    by = ~ audit_knowledge_level
  )
amce_knowledge_to_plot <- ArrangeForPlot(amce_vals_by_knowledge)

jpeg('plots/sm_figure_e-6.jpeg', width=8, height=7, units='in', res=600)
plot(amce_knowledge_to_plot,
     vline = 0,
     size = 2.5,
     group = "audit_knowledge_level",
     theme = theme_bw(base_size=12)
) +
  aes(shape = audit_knowledge_level, color = audit_knowledge_level) +
  scale_color_manual(name = 'Knowledge Level',values = c('#fc8d62','#8da0cb'), breaks = c("High","Low")) +
  scale_shape_manual(name = 'Knowledge Level',values = c(15,16), breaks = c("High","Low")) +
  guides(color = guide_legend(), shape = guide_legend()) +
  geom_hline(yintercept=3, linetype='dashed', color='black') +
  geom_hline(yintercept=6, linetype='dashed', color='black') +
  geom_hline(yintercept=10, linetype='dashed', color='black') +
  geom_hline(yintercept=14, linetype='dashed', color='black') +
  geom_hline(yintercept=18, linetype='dashed', color='black') +
  geom_vline(xintercept = 0.5, color = 'darkgrey')
dev.off()


#### SM, Figure E-7 ####
mm_vals <-
  mm(
    data = conj,
    formula = chosen ~ ballots + results + who + availability + winner + office,
    weights = ~ weight,
    id = ~ caseid
  )
mm_to_plot <- ArrangeForPlot(mm_vals)

jpeg('plots/sm_figure_e-7.jpeg', width=8, height=7, units='in', res=600)
plot(mm_to_plot,
     vline = 0.5,
     size = 2.5,
     theme = theme_bw(base_size=12)
) +  scale_color_manual(values = feature_colors) +
  geom_hline(yintercept=3, linetype='dashed', color='black') +
  geom_hline(yintercept=6, linetype='dashed', color='black') +
  geom_hline(yintercept=10, linetype='dashed', color='black') +
  geom_hline(yintercept=14, linetype='dashed', color='black') +
  geom_hline(yintercept=18, linetype='dashed', color='black')
dev.off()

#### SM, Figure E-8 ####
mm_vals_by_pty <- cj(data = conj,
                     formula = chosen ~ ballots + results + who + 
                       availability + winner + office,
                     weights = ~ weight,
                     id = ~ caseid,
                     estimate = 'mm',
                     by = ~pid7_collapsed
)
mmpty_to_plot <- ArrangeForPlot(mm_vals_by_pty)
names(mmpty_to_plot)[names(mmpty_to_plot) == 'pid7_collapsed'] <- 'Party'

jpeg('plots/sm_figure_e-8.jpeg', width=8, height=7, units='in', res=600)
plot(mmpty_to_plot,
     vline = 0,
     size = 2.5,
     group = 'Party',
     theme = theme_bw(base_size=12)
) +
  aes(shape = Party) +
  scale_color_manual(values = party_colors, breaks = party_labels) +
  scale_shape_manual(values = party_shape, breaks = party_labels) +
  geom_hline(yintercept=3, linetype='dashed', color='black') +
  geom_hline(yintercept=6, linetype='dashed', color='black') +
  geom_hline(yintercept=10, linetype='dashed', color='black') +
  geom_hline(yintercept=14, linetype='dashed', color='black') +
  geom_hline(yintercept=18, linetype='dashed', color='black') +
  geom_vline(xintercept = 0.5, color = 'darkgrey')
dev.off()


#### SM, Table E-9 ####
# Create population target distribution -- based on Jaffe et al. (2022)
empirical_dist = list()
empirical_dist$ballots = setNames(c(1/30, 12/30, 17/30), unique(conj$ballots))
## leaving this as uniform for now
empirical_dist$results = setNames(c(1/3, 1/3, 1/3), unique(conj$results))
empirical_dist$who = setNames(c(23/35, 12/35, 2/35), unique(conj$who))
empirical_dist$availability = setNames(c(22/35, 5/35, 8/35), unique(conj$availability))
empirical_dist$winner = setNames(c(0.5, 0.5), unique(conj$winner))
empirical_dist$office = setNames(c(0.5, 0.5), unique(conj$office))

# filter to complete cases 
conj_complete <- conj |> select(-conf) |> na.omit(conj)
conj_complete_dem <- conj_complete |> filter(pid7_collapsed == 'Democrat')
conj_complete_rep <- conj_complete |> filter(pid7_collapsed == 'Republican')

pAMCE <-
  design_pAMCE(
    chosen ~ ballots + results + who + availability + winner + office,
    data = conj_complete,
    pair_id = conj_complete$pair_id,
    target_dist = empirical_dist,
    target_type = 'marginal'
  )

pAMCE_output <- summary(pAMCE)[, -c(1, 7)] |>
  mutate(
    factor = factor(factor) |>
      fct_recode(
        "Audit size" = 'ballots',
        "Number of errors" = 'results',
        "Who audits" = "who",
        "Availability" = "availability",
        "Winner's party" = "winner",
        "Office" = "office"
      ),
    level = factor(level) |>
     fct_recode("10% of all ballots" = "10% of all ",
                "5% of all ballots" = "5% of all ")
  ) |>
  kable(format = 'latex', digits = 3, booktabs = TRUE, caption = 'pAMCEs',
        col.names = c("Attribute","Level","Estimate","SE","p-value")) |>
  kable_styling(latex_options = c("hold_position")) |>
  save_kable("tables/sm_table_e-9.tex")

#### SM, Table E-10 ####

pAMCE_rep <- design_pAMCE(
  chosen ~ ballots + results + who + availability + winner + office,
  data = conj_complete_rep,
  pair_id = conj_complete_rep$pair_id,
  target_dist = empirical_dist,
  target_type = 'marginal'
)

pAMCE_rep_output <- summary(pAMCE_rep)[, -c(1, 7)] |>
  mutate(
    factor = factor(factor) |>
      fct_recode(
        "Audit size" = 'ballots',
        "Number of errors" = 'results',
        "Who audits" = "who",
        "Availability" = "availability",
        "Winner's party" = "winner",
        "Office" = "office"
      ),
    level = factor(level) |>
      fct_recode("10% of all ballots" = "10% of all ",
                 "5% of all ballots" = "5% of all ")
  ) |>
  kable(format = 'latex', digits = 3, booktabs = TRUE, caption = 'pAMCEs, Republican Respondents',
        col.names = c("Attribute","Level","Estimate","SE","p-value")) |>
  kable_styling(latex_options = c("hold_position")) |>
  save_kable("tables/sm_table_e-10.tex")

pAMCE_dem <- design_pAMCE(
  chosen ~ ballots + results + who + availability + winner + office,
  data = conj_complete_dem,
  pair_id = conj_complete_dem$pair_id,
  target_dist = empirical_dist,
  target_type = 'marginal'
)

#### SM, Table E-10 ####
pAMCE_dem_output <- summary(pAMCE_dem)[, -c(1, 7)] |>
  mutate(
    factor = factor(factor) |>
      fct_recode(
        "Audit size" = 'ballots',
        "Number of errors" = 'results',
        "Who audits" = "who",
        "Availability" = "availability",
        "Winner's party" = "winner",
        "Office" = "office"
      ),
    level = factor(level) |>
      fct_recode("10% of all ballots" = "10% of all ",
                 "5% of all ballots" = "5% of all ")
  ) |>
  kable(format = 'latex', digits = 3, booktabs = TRUE, caption = 'pAMCEs, Democrat Respondents',
        col.names = c("Attribute","Level","Estimate","SE","p-value")) |>
  kable_styling(latex_options = c("hold_position")) |>
  save_kable("tables/sm_table_e-11.tex")
