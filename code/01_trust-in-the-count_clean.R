# Trust in the Count"
# Script 1: Data Clean

rm(list = ls())
gc()

# Load packages
library(tidyverse)
library(labelled)

# Set working directory
# setwd("~/Desktop/trust-in-the-count")

# Read data
trust_in_the_count <- readRDS("data/trust_in_the_count_raw.rds")

#### min-max rescale function ####
rescale <- function(value, min, max){return((value-min)/(max-min))}

# Filtering Out Attention Checks ------------------------------------------
## AC_SCORE = number of attention checks passed
## AC_PASS_ALL = dummy of whether respondent passed all ACs
attention_check <- trust_in_the_count |>
  mutate(AC_1 = ifelse(QAC_1_new %in% c("15","15.","15,","fifteen","Fifteen","Fithteen","Fifteen 15"),1,0),
         AC_2 = ifelse(QAC_2_new==5,1,0),
         AC_3 = ifelse(attn_1==7,1,0)) |>
  group_by(caseid) |>
  summarise(AC_SCORE = sum(AC_1 + AC_2 + AC_3,na.rm = T)) |>
  mutate(AC_PASS_ALL = ifelse(AC_SCORE == 3, 1, 0))

trust_in_the_count <- trust_in_the_count |> left_join(attention_check)

# Recoding Group,  Demo Vars ----------------------------------------------
# No updating raw values -- simply adding extra variables for use later
trust_in_the_count <- trust_in_the_count |>
  mutate(
    # Party ID: 7pt scale, factor
    pid7_factor = as.factor(case_when(
      pid7 == 1 ~ "Strong Democrat",
      pid7 == 2 ~ "Not very strong Democrat",
      pid7 == 3 ~ "Lean Democrat",
      pid7 == 4 ~ "Independent",
      pid7 == 5 ~ "Lean Republican",
      pid7 == 6 ~ "Not very strong Republican",
      pid7 == 7 ~ "Strong Republican")),
    # Party ID: 7pt scale collapsed to 3 item factor
    pid7_collapsed = as.factor(case_when(
      pid7 <= 3 ~ "Democrat",
      pid7 >= 5 ~ "Republican",
      pid7 == 4 ~ "Independent"
    )) |> fct_relevel("Independent"),
    # Perception of fraud: rescale each question
    ## Values 0-1, with larger values associated with perceiving more fraud
    across(QDEM_12:QDEM_17, ~ ifelse(. %in% c(1:4), ., NA)),
    across(QDEM_12:QDEM_17, ~ 5 - .),
    across(QDEM_12:QDEM_17, ~rescale(.,1,4)),
    # Election denialism: rescale
    ## Values 0-1, with larger values associated with more agreement
    QDEM_20 = ifelse(QDEM_20 %in% c(1:4),QDEM_20,NA),
    election_denial = rescale(QDEM_20,1,4),
    # Conspiracy battery: rescale, higher value more belief in them
    ## Values 0-1, with larger values associated with more agreement
    across(QDEM_22:QDEM_28, ~ ifelse(. %in% c(1:5), ., NA)),
    across(QDEM_22:QDEM_28, ~rescale(.,1,5)),
    # Treatment Group: information addition; factor
    audit_info_add_group = as.factor(case_when(
      mod2_block2_treat == 1 ~ "Control",
      mod2_block2_treat == 2 ~ "Audit-0",
      mod2_block2_treat == 3 ~ "Audit-1",
      mod2_block2_treat == 4 ~ "Audit-2"
    )) |> fct_relevel("Control","Audit-0","Audit-1","Audit-2"),
    # Treatment Group: order of magnitude
    audit_ordermag_group = as.factor(case_when(
      mod2_block4_treat == 1 ~ "Low",
      mod2_block4_treat == 2 ~ "Medium",
      mod2_block4_treat == 3 ~ "High",
    )) |> fct_relevel("Low","Medium","High")
  )

# Create battery scales
trust_in_the_count$fraud_battery <- rowMeans(trust_in_the_count[c("QDEM_12", "QDEM_13","QDEM_14", "QDEM_15","QDEM_16", "QDEM_17")], na.rm = TRUE)
trust_in_the_count$conspiracy_battery <- rowMeans(trust_in_the_count[c("QDEM_22", "QDEM_23","QDEM_24", "QDEM_25","QDEM_26", "QDEM_27", "QDEM_28")], na.rm = TRUE)

# Recode Study Dependent/Independent Variables ----------------------------
# Make answers to descriptive/background questions into factors
trust_in_the_count$QCIA_1 <- to_factor(trust_in_the_count$QCIA_1)
trust_in_the_count$QCIA_2 <- to_factor(trust_in_the_count$QCIA_2)
trust_in_the_count$QCIA_3 <- to_factor(trust_in_the_count$QCIA_3)
trust_in_the_count$QCIA_4 <- to_factor(trust_in_the_count$QCIA_4)
trust_in_the_count$QCIA_5 <- to_factor(trust_in_the_count$QCIA_5)
trust_in_the_count$QCIA_6 <- to_factor(trust_in_the_count$QCIA_6)
trust_in_the_count$QCIA_7 <- to_factor(trust_in_the_count$QCIA_7)
trust_in_the_count$QCIA_8 <- to_factor(trust_in_the_count$QCIA_8)

trust_in_the_count <- trust_in_the_count |>
  mutate(
    ## Information Addition: IDKs as NAs, rescale
    # Values for DV on 0-1 scale, larger value = more agreement
    infoadd_winner = ifelse(QCIA_9 %in% c(1:4), QCIA_9, NA) |> rescale(1,4),
    infoadd_conduct = ifelse(QCIA_10 %in% c(1:4), QCIA_10, NA) |> rescale(1,4),
    infoadd_accurate = ifelse(QCIA_11 %in% c(1:4), QCIA_11, NA) |> rescale(1,4),
    ## Order of magnitude
    ordermag = ifelse(QCIA_15 %in% c(1:4),QCIA_15,NA) |> rescale(1,4)
  )

saveRDS(trust_in_the_count, file = "data/trust_in_the_count_analysis.rds")