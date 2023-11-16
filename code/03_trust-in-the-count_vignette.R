# "Trust in the Count"
# Script 3: Vignette

rm(list = ls())
gc()

# Load packages
library(tidyverse)
library(survey)
library(modelsummary)
library(marginaleffects)
library(broom)
library(kableExtra)
library(xtable)

# Set working directory
# setwd("~/Desktop/trust-in-the-count")

# Read data
trust_in_the_count <-
  readRDS("data/trust_in_the_count_analysis.rds")

#### personalized plot theme function ####
theme_trust_in_the_count <- function () {
  theme_minimal(base_size = 10) %+replace%
    theme(
      panel.background  = element_blank(),
      panel.border = element_blank(),
      plot.background = element_blank(),
      plot.margin = unit(c(1, 2, 1, 1), "cm"),
      legend.box.background = element_rect(fill = "white", color = NA),
      legend.key = element_rect(
        fill = NA,
        color = NA,
        size = 4
      ),
      legend.text = element_text(color = "black", size = 12),
      legend.title = element_text(color = "black", size = 14),
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.grid.major = element_line(color = "#C4C4C4"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "black"),
      axis.title.y = element_text(
        color = "black",
        size = 16,
        angle = 90,
        margin = margin(
          t = 0,
          r = 10,
          b = 0,
          l = 0
        )
      ),
      axis.title.x = element_text(
        color = "black",
        size = 16,
        angle = 0,
        margin = margin(
          t = 10,
          r = 0,
          b = 0,
          l = 0
        )
      ),
      plot.title = element_text(
        size = 20,
        color = "black",
        hjust = 0.5,
        vjust = 2,
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = 12,
        color = "black",
        hjust = 0.5,
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        size = 8,
        margin = margin(t = 10),
        color =
          "black",
        hjust = 0
      )
    )
}

#### Prep relabeling for modelsummary ####
gm <- list(
  list(
    "raw" = "nobs",
    "clean" = "N",
    "fmt" = function(x)
      format(round(x, 3))
  ),
  list(
    "raw" = "r.squared",
    "clean" = "R2",
    "fmt" = function(x)
      format(round(x, 3))
  )
)
se_note <- "Models use HC2 robust standard errors."
coef_labels <- c(
  "audit_info_add_groupAudit-0" = "Audit-0",
  "audit_info_add_groupAudit-1" = "Audit-1",
  "audit_info_add_groupAudit-2" = "Audit-2",
  "pid7_collapsedDemocrat" = "Democrat",
  "pid7_collapsedRepublican" = "Republican",
  "audit_info_add_groupAudit-0:pid7_collapsedDemocrat" = "Audit-0 x Democrat",
  "audit_info_add_groupAudit-1:pid7_collapsedDemocrat" = "Audit-1 x Democrat",
  "audit_info_add_groupAudit-2:pid7_collapsedDemocrat" = "Audit-2 x Democrat",
  "audit_info_add_groupAudit-0:pid7_collapsedRepublican" = "Audit-0 x Republican",
  "audit_info_add_groupAudit-1:pid7_collapsedRepublican" = "Audit-1 x Republican",
  "audit_info_add_groupAudit-2:pid7_collapsedRepublican" = "Audit-2 x Republican",
  "fraud_battery" = "Perceptions of Fraud",
  "audit_info_add_groupAudit-0:fraud_battery" = "Audit-0 x Perceptions of Fraud",
  "audit_info_add_groupAudit-1:fraud_battery" = "Audit-1 x Perceptions of Fraud",
  "audit_info_add_groupAudit-2:fraud_battery" = "Audit-2 x Perceptions of Fraud",
  "conspiracy_battery" = "Belief in Political Conspiracies",
  "audit_info_add_groupAudit-0:conspiracy_battery" = "Audit-0 x Belief in Political Conspiracies",
  "audit_info_add_groupAudit-1:conspiracy_battery" = "Audit-1 x Belief in Political Conspiracies",
  "audit_info_add_groupAudit-2:conspiracy_battery" = "Audit-2 x Belief in Political Conspiracies",
  "election_denial" = "Election Denialism",
  "audit_info_add_groupAudit-0:election_denial" = "Audit-0 x Election Denialism",
  "audit_info_add_groupAudit-1:election_denial" = "Audit-1 x Election Denialism",
  "audit_info_add_groupAudit-2:election_denial" = "Audit-2 x Election Denialism",
  "(Intercept)" = "(Intercept)"
)

# Information Addition ----------------------------------------------------
##### Main Analysis #####
model_all_winner <-
  lm(infoadd_winner ~ audit_info_add_group,
     data = trust_in_the_count,
     weights = weight)
model_all_conduct <-
  lm(infoadd_conduct ~ audit_info_add_group,
     data = trust_in_the_count,
     weights = weight)
model_all_accurate <-
  lm(infoadd_accurate ~ audit_info_add_group,
     data = trust_in_the_count,
     weights = weight)

model_ac_winner <-
  lm(
    infoadd_winner ~ audit_info_add_group,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )
model_ac_conduct <-
  lm(
    infoadd_conduct ~ audit_info_add_group,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )
model_ac_accurate <-
  lm(
    infoadd_accurate ~ audit_info_add_group,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )

# Figure 2
model_ac_winner_coefplot <-
  modelplot(
    model_ac_winner,
    coef_map = c(
      "audit_info_add_groupAudit-0" = "Audit-0",
      "audit_info_add_groupAudit-1" = "Audit-1",
      "audit_info_add_groupAudit-2" = "Audit-2"
    ),
    draw = F
  ) |>
  ggplot(aes(
    x = term,
    y = estimate,
    ymin = conf.low,
    ymax = conf.high,
    group = term
  )) +
  geom_pointrange(size = 1, linewidth = 1) +
  coord_flip() +
  theme_trust_in_the_count() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = 'red',
    linewidth = 1
  ) +
  geom_label(aes(label = sprintf("%.3f (p = %.3f)", estimate, p.value)),
             vjust = 1.5,
             fill = "#D2D2D2") +
  labs(x = "Treatment Group", y = "Estimated Treatment Effect") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 15, margin = margin(t = 20)),
    axis.title.y = element_text(size = 15, margin = margin(r = 20)),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "mm")
  )

jpeg(
  "plots/figure_2.jpeg",
  width = 11,
  height = 4.5,
  units = 'in',
  res = 600
)
model_ac_winner_coefplot
dev.off()

# SM, Table D-3 -- regression table associated with Figure 2
info_add <- modelsummary(
  models = list(
    "Winner" = model_ac_winner,
    "Conduct" = model_ac_conduct,
    "Accurate" = model_ac_accurate,
    "Winner" = model_all_winner,
    "Conduct" = model_all_conduct,
    "Accurate" = model_all_accurate
  ),
  statistic = "p = {p.value}",
  vcov = "HC2",
  coef_map = coef_labels,
  output = "latex",
  gof_map = gm,
  escape = T,
  notes = se_note,
  title = "Effects of Information on Confidence in Audit Outcomes",
  threeparttable = T
)  |>
  add_header_above(c(
    " " = 1,
    "Attentive Respondents" = 3,
    "All Respondents" = 3
  )) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  save_kable("tables/sm_table_d-3.tex")

##### Supplementary Models #####
# SM, Table D-4 -- PID as a moderator
model_all_winner_moderator_pid <-
  lm(
    infoadd_winner ~ audit_info_add_group * pid7_collapsed,
    data = trust_in_the_count,
    weights = weight
  )
model_all_conduct_moderator_pid <-
  lm(
    infoadd_conduct ~ audit_info_add_group * pid7_collapsed,
    data = trust_in_the_count,
    weights = weight
  )
model_all_accurate_moderator_pid <-
  lm(
    infoadd_accurate ~ audit_info_add_group * pid7_collapsed,
    data = trust_in_the_count,
    weights = weight
  )

model_ac_winner_moderator_pid <-
  lm(
    infoadd_winner ~ audit_info_add_group * pid7_collapsed,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )
model_ac_conduct_moderator_pid <-
  lm(
    infoadd_conduct ~ audit_info_add_group * pid7_collapsed,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )
model_ac_accurate_moderator_pid <-
  lm(
    infoadd_accurate ~ audit_info_add_group * pid7_collapsed,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )

info_add_moderator_pid <- modelsummary(
  models = list(
    "Winner" = model_ac_winner_moderator_pid,
    "Conduct" = model_ac_conduct_moderator_pid,
    "Accurate" = model_ac_accurate_moderator_pid,
    "Winner" = model_all_winner_moderator_pid,
    "Conduct" = model_all_conduct_moderator_pid,
    "Accurate" = model_all_accurate_moderator_pid
  ),
  statistic = "p = {p.value}",
  vcov = "HC2",
  coef_map = coef_labels,
  output = "latex",
  gof_map = gm,
  escape = T,
  notes = se_note,
  title = "Effects of Information Moderated by Party ID on Confidence in Audit Outcomes",
  threeparttable = T
)  |>
  add_header_above(c(
    " " = 1,
    "Attentive Respondents" = 3,
    "All Respondents" = 3
  )) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  save_kable("tables/sm_table_d-4.tex")

# SM, Table D-5 -- subgroup analysis by PID
model_all_winner_all <-
  lm(infoadd_winner ~ audit_info_add_group,
     data = trust_in_the_count,
     weights = weight)
model_all_winner_dem <-
  lm(
    infoadd_winner ~ audit_info_add_group,
    data = trust_in_the_count |> filter(pid7_collapsed == 'Democrat'),
    weights = weight
  )
model_all_winner_rep <-
  lm(
    infoadd_winner ~ audit_info_add_group,
    data = trust_in_the_count |> filter(pid7_collapsed == 'Republican'),
    weights = weight
  )
model_all_winner_ind <-
  lm(
    infoadd_winner ~ audit_info_add_group,
    data = trust_in_the_count |> filter(pid7_collapsed == 'Independent'),
    weights = weight
  )

model_ac_winner_all <-
  lm(
    infoadd_winner ~ audit_info_add_group,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )
model_ac_winner_dem <-
  lm(
    infoadd_winner ~ audit_info_add_group,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1 &
                                          pid7_collapsed == 'Democrat'),
    weights = weight
  )
model_ac_winner_rep <-
  lm(
    infoadd_winner ~ audit_info_add_group,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1 &
                                          pid7_collapsed == 'Republican'),
    weights = weight
  )
model_ac_winner_ind <-
  lm(
    infoadd_winner ~ audit_info_add_group,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1 &
                                          pid7_collapsed == 'Independent'),
    weights = weight
  )

info_add_separate_party <- modelsummary(
  models = list(
    "All"  = model_ac_winner_all
    ,
    "Dem." = model_ac_winner_dem
    ,
    "Rep." = model_ac_winner_rep
    ,
    "Ind." = model_ac_winner_ind
    ,
    "All" = model_all_winner_all
    ,
    "Dem." = model_all_winner_dem
    ,
    "Rep." = model_all_winner_rep
    ,
    "Ind." = model_all_winner_ind
  ),
  statistic = "p = {p.value}",
  vcov = "HC2",
  coef_map = coef_labels,
  output = "latex",
  gof_map = gm,
  escape = T,
  notes = se_note,
  title = "Effects of Information by Party ID Subgroup on Confidence in Election Outcome",
  threeparttable = T
)  |>
  add_header_above(c(
    " " = 1,
    "Attentive Respondents" = 4,
    "All Respondents" = 4
  )) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  landscape() |>
  save_kable("tables/sm_table_d-5.tex")

# SM, Table D-6 -- perceptions of fraud as a moderator
model_all_winner_moderator_fraud_battery <-
  lm(
    infoadd_winner ~ audit_info_add_group * fraud_battery,
    data = trust_in_the_count,
    weights = weight
  )
model_all_conduct_moderator_fraud_battery <-
  lm(
    infoadd_conduct ~ audit_info_add_group * fraud_battery,
    data = trust_in_the_count,
    weights = weight
  )
model_all_accurate_moderator_fraud_battery <-
  lm(
    infoadd_accurate ~ audit_info_add_group * fraud_battery,
    data = trust_in_the_count,
    weights = weight
  )

model_ac_winner_moderator_fraud_battery <-
  lm(
    infoadd_winner ~ audit_info_add_group * fraud_battery,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )
model_ac_conduct_moderator_fraud_battery <-
  lm(
    infoadd_conduct ~ audit_info_add_group * fraud_battery,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )
model_ac_accurate_moderator_fraud_battery <-
  lm(
    infoadd_accurate ~ audit_info_add_group * fraud_battery,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )

info_add_moderator_fraud_battery <- modelsummary(
  models = list(
    "Winner" = model_ac_winner_moderator_fraud_battery,
    "Conduct" = model_ac_conduct_moderator_fraud_battery,
    "Accurate" = model_ac_accurate_moderator_fraud_battery,
    "Winner" = model_all_winner_moderator_fraud_battery,
    "Conduct" = model_all_conduct_moderator_fraud_battery,
    "Accurate" = model_all_accurate_moderator_fraud_battery
  ),
  statistic = "p = {p.value}",
  vcov = "HC2",
  coef_map = coef_labels,
  output = "latex",
  gof_map = gm,
  escape = T,
  notes = se_note,
  title = "Effects of Information by Perceptions of Systemic Voter Fraud on Confidence in Audit Outcomes",
  threeparttable = T
)  |>
  add_header_above(c(
    " " = 1,
    "Attentive Respondents" = 3,
    "All Respondents" = 3
  )) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  save_kable("tables/sm_table_d-6.tex")

# SM, Table D-7 -- belief in political conspiracies as a moderator
model_all_winner_moderator_conspiracy_battery <-
  lm(
    infoadd_winner ~ audit_info_add_group * conspiracy_battery,
    data = trust_in_the_count,
    weights = weight
  )
model_all_conduct_moderator_conspiracy_battery <-
  lm(
    infoadd_conduct ~ audit_info_add_group * conspiracy_battery,
    data = trust_in_the_count,
    weights = weight
  )
model_all_accurate_moderator_conspiracy_battery <-
  lm(
    infoadd_accurate ~ audit_info_add_group * conspiracy_battery,
    data = trust_in_the_count,
    weights = weight
  )

model_ac_winner_moderator_conspiracy_battery <-
  lm(
    infoadd_winner ~ audit_info_add_group * conspiracy_battery,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )
model_ac_conduct_moderator_conspiracy_battery <-
  lm(
    infoadd_conduct ~ audit_info_add_group * conspiracy_battery,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )
model_ac_accurate_moderator_conspiracy_battery <-
  lm(
    infoadd_accurate ~ audit_info_add_group * conspiracy_battery,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )

info_add__moderator_conspiracy_battery <- modelsummary(
  models = list(
    "Winner" = model_ac_winner_moderator_conspiracy_battery,
    "Conduct" = model_ac_conduct_moderator_conspiracy_battery,
    "Accurate" = model_ac_accurate_moderator_conspiracy_battery,
    "Winner" = model_all_winner_moderator_conspiracy_battery,
    "Conduct" = model_all_conduct_moderator_conspiracy_battery,
    "Accurate" = model_all_accurate_moderator_conspiracy_battery
  ),
  statistic = "p = {p.value}",
  vcov = "HC2",
  coef_map = coef_labels,
  output = "latex",
  gof_map = gm,
  escape = T,
  notes = se_note,
  title = "Effects of Information by Belief in Political Conspiracies on Confidence in Audit Outcomes",
  threeparttable = T
)  |>
  add_header_above(c(
    " " = 1,
    "Attentive Respondents" = 3,
    "All Respondents" = 3
  )) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  save_kable("tables/sm_table_d-7.tex")

# SM, Table D-8 -- election denialism as a moderator
model_all_winner_moderator_election_denial <-
  lm(
    infoadd_winner ~ audit_info_add_group * election_denial,
    data = trust_in_the_count,
    weights = weight
  )
model_all_conduct_moderator_election_denial <-
  lm(
    infoadd_conduct ~ audit_info_add_group * election_denial,
    data = trust_in_the_count,
    weights = weight
  )
model_all_accurate_moderator_election_denial <-
  lm(
    infoadd_accurate ~ audit_info_add_group * election_denial,
    data = trust_in_the_count,
    weights = weight
  )

model_ac_winner_moderator_election_denial <-
  lm(
    infoadd_winner ~ audit_info_add_group * election_denial,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )
model_ac_conduct_moderator_election_denial <-
  lm(
    infoadd_conduct ~ audit_info_add_group * election_denial,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )
model_ac_accurate_moderator_election_denial <-
  lm(
    infoadd_accurate ~ audit_info_add_group * election_denial,
    data = trust_in_the_count |> filter(AC_PASS_ALL == 1),
    weights = weight
  )

info_add_moderator_election_denial <- modelsummary(
  models = list(
    "Winner" = model_ac_winner_moderator_election_denial,
    "Conduct" = model_ac_conduct_moderator_election_denial,
    "Accurate" = model_ac_accurate_moderator_election_denial,
    "Winner" = model_all_winner_moderator_election_denial,
    "Conduct" = model_all_conduct_moderator_election_denial,
    "Accurate" = model_all_accurate_moderator_election_denial
  ),
  statistic = "p = {p.value}",
  vcov = "HC2",
  coef_map = coef_labels,
  output = "latex",
  gof_map = gm,
  escape = T,
  notes = se_note,
  title = "Effects of Information by Election Denialism on Confidence in Audit Outcomes",
  threeparttable = T
)  |>
  add_header_above(c(
    " " = 1,
    "Attentive Respondents" = 3,
    "All Respondents" = 3
  )) |>
  kable_styling(latex_options = c("hold_position", "scale_down")) |>
  save_kable("tables/sm_table_d-8.tex")


# Ancillary Analyses ------------------------------------------------------
##### Order of Magnitude #####
order_mag <- trust_in_the_count |>
  select(caseid, weight, audit_ordermag_group, ordermag)
order_mag$audit_ordermag_group <- fct_recode(
  order_mag$audit_ordermag_group,
  "10 votes" = "Low",
  "100 votes" = "Medium",
  "500 votes" = "High"
)
order_mag_design <-
  svydesign( ~ 1,
             weights = ~ weight,
             data = order_mag,
             degf = "adjust")

order_mag_results <-
  svyby(
    ~ ordermag,
    by = ~ audit_ordermag_group,
    design = order_mag_design,
    FUN = svymean,
    na.rm = T
  ) |>
  as.data.frame()

# SM, Table F-12
order_mag_pairwise <-
  pairwise.t.test(order_mag$ordermag, order_mag$audit_ordermag_group,) |>
  tidy()
order_mag_pairwise$diff <-
  c(
    order_mag_results$ordermag[2] - order_mag_results$ordermag[1],
    order_mag_results$ordermag[3] - order_mag_results$ordermag[1],
    order_mag_results$ordermag[2] - order_mag_results$ordermag[3]
  )
order_mag_pairwise <- order_mag_pairwise |>
  select(
    `Group 1` = group1,
    `Group 2` = group2,
    `Difference` = diff,
    p = p.value
  )
order_mag <-
  kable(
    order_mag_pairwise,
    digits = 3,
    caption = "Pairwise Comparison of Mean Confidence in Election Results by 'Order of Magnitude' Treatment",
    label = "tab:ordermag",
    booktabs = T,
    format = "latex"
  ) |>
  save_kable("tables/sm_table_f-12.tex")

# SM, Figure F-9
order_mag_plot <-
  ggplot(order_mag_results, aes(x = audit_ordermag_group, y = ordermag)) +
  geom_pointrange(
    aes(ymin = ordermag - 1.96 * se, ymax = ordermag + 1.96 * se),
    size = 1,
    linewidth = 1
  ) +
  theme_trust_in_the_count() +
  labs(x = "Vote Difference Displayed", y = "Mean of Rescaled Confidence Level") +
  coord_flip() +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    axis.title.x = element_text(size = 15, margin = margin(t = 20)),
    axis.title.y = element_text(size = 15, margin = margin(r = 20))
  )

jpeg(
  "plots/sm_figure_f-9.jpeg",
  width = 11,
  height = 5.5,
  units = 'in',
  res = 600
)
order_mag_plot
dev.off()
