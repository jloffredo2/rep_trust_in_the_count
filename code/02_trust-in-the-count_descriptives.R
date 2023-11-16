# "Trust in the Count"
# Script 2: Descriptives

rm(list = ls())
gc()

# Load packages
library(tidyverse)
library(survey)
library(patchwork)
library(labelled)
library(patchwork)
library(gtsummary)
library(kableExtra)
library(scales)

# Set working directory
# setwd("~/Desktop/trust-in-the-count")

# Read data
trust_in_the_count <- readRDS("data/trust_in_the_count_analysis.rds")

#### personalized plot theme function ####
theme_trust_in_the_count <- function () { 
  theme_minimal(base_size=10) %+replace% 
    theme(
      panel.background  = element_blank(),
      panel.border = element_blank(),
      plot.background = element_blank(), 
      plot.margin = unit(c(1,2,1,1), "cm"),
      legend.box.background = element_rect(fill="white", color=NA),
      legend.key = element_rect(fill=NA, color=NA, size = 4),
      legend.text = element_text(color = "black", size = 12),
      legend.title = element_text(color = "black", size = 14),
      legend.position = "bottom",
      legend.direction = "horizontal",
      panel.grid.major = element_line(color = "#C4C4C4"),
      panel.grid.minor = element_blank(),
      axis.text = element_text(color = "black"),
      axis.title.y = element_text(color = "black", size = 16, angle = 90,
                                  margin = margin(t = 0, r = 10, b = 0, l = 0)),
      axis.title.x = element_text(color = "black", size = 16, angle = 0,
                                  margin = margin(t = 10, r = 0, b = 0, l = 0)),
      plot.title = element_text(size = 20, color = "black", hjust = 0.5, 
                                vjust=2, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 12, color = "black", hjust = 0.5, 
                                   margin = margin(b = 12)),
      plot.caption = element_text(size = 8, margin = margin(t = 10), color = 
                                    "black", hjust = 0)
    )
}


# Figure 1 ----------------------------------------------------------------
# Creates barplot for descriptive questions
# Graph Descriptive Questions ---------------------------------------------
descriptives <- trust_in_the_count |> select(caseid, weight, QCIA_1:QCIA_8,pid7_collapsed)

descriptives[, 3:10][descriptives[, 3:10] %in% c(8, 9)] <- NA

descriptives_design <- svydesign(id=~1, weights=~ weight, data=descriptives)

##### In a given election, how many states perform post-election audits? #####
state_prop_df <- svymean(~QCIA_2, descriptives_design, na.rm = T) |> as.data.frame() 
state_prop_df <- rownames_to_column(state_prop_df, "response")
state_prop_df$pid7_collapsed = "All"
state_prop_df <- state_prop_df |> select(-SE)

state_prop_party <- svyby(~QCIA_2,by = ~pid7_collapsed,design = descriptives_design,FUN = svymean,na.rm=T) |>
  gather(`QCIA_2All states`:`se.QCIA_2not asked`,key = "response", value = "mean") |>
  filter(!str_detect(response,"se\\.")) |>
  select(colnames(state_prop_df)) 

state_prop_df <- rbind(state_prop_df,state_prop_party) |>
  mutate(response = str_remove(response,"QCIA_2"),
         pid7_collapsed = fct_relevel(factor(pid7_collapsed), "All", "Democrat","Republican"))|>
  filter(!(response %in% c("skipped","not asked")))

state_prop_df <- state_prop_df |> mutate(
  response = fct_recode(
    as.factor(response),
    "All states"="All states",
    "No states"="No states",
    "< 10%"="Less than 10 percent of states",
    "10-50%"="Between 10 and 50 percent of states",
    "> 50%, but not all"="More than 50 percent of states, but not all",
    "I don't know"= "I don't know") |>
    fct_relevel("No states","< 10%","10-50%","> 50%, but not all","All states","I don't know")
)|> filter(pid7_collapsed != "Independent")

state_prop <- state_prop_df |>
  ggplot(aes(x = response, y = mean,fill=pid7_collapsed)) +
  geom_bar(stat='identity',position = "dodge") +
  theme_trust_in_the_count() +
  geom_text(aes(label=paste(round(mean*100),"%", sep="")),
            position = position_dodge(width = 0.9),
            vjust = 2,
            size=2.75,
            color="white", fontface='bold',)+
  scale_fill_manual(values = c("#AAAAAA","navy","red")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(labels = label_wrap(20)) +
  labs(fill = "Party ID", y = ""
       ,title = str_wrap("(1) In a given election, how many states perform post-election audits?",200)
  ) +
  theme(legend.position = 'bottom',axis.text.x = element_text(size = 10), axis.title.x = element_blank(),axis.text.y = element_text(size = 10),plot.title = element_text(size=14, face = 'bold'),legend.text = element_text(size = 12),legend.title = element_text(size=12))

##### How often do audits change the winner of elections nationwide? #####
change_winner_df <- svymean(~QCIA_5, descriptives_design, na.rm = T) |> as.data.frame() 
change_winner_df <- rownames_to_column(change_winner_df, "response")
change_winner_df$pid7_collapsed = "All"
change_winner_df <- change_winner_df |> select(-SE)

change_winner_party <- svyby(~QCIA_5,by = ~pid7_collapsed,design = descriptives_design,FUN = svymean,na.rm=T) |>
  gather(`QCIA_5Not often at all`:`se.QCIA_5not asked`,key = "response", value = "mean") |>
  filter(!str_detect(response,"se\\.")) |>
  select(colnames(change_winner_df)) 

change_winner_df <- rbind(change_winner_df,change_winner_party) |>
  mutate(response = str_remove(response,"QCIA_5"),
         pid7_collapsed = fct_relevel(factor(pid7_collapsed), "All", "Democrat","Republican"))|>
  filter(!(response %in% c("skipped","not asked")))

change_winner_df <- change_winner_df |> mutate(
  response = fct_relevel(as.factor(response),"Not often at all","Not very often","Fairly often","Very often","I don't know")
) |>
  filter(mean!=0)|> filter(pid7_collapsed != "Independent")

change_winner <- change_winner_df |>
  ggplot(aes(x = response, y = mean,fill=pid7_collapsed)) +
  geom_bar(stat='identity',position = "dodge") +
  theme_trust_in_the_count() +
  geom_text(aes(label=paste(round(mean*100),"%", sep="")),
            position = position_dodge(width = 0.9),
            vjust = 2,
            size=2.75,
            color="white", fontface='bold')+
  scale_fill_manual(values = c("#AAAAAA","navy","red")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(labels = label_wrap(20)) +
  labs(fill = "Party ID", y = ""
       ,title = str_wrap("(2) How often do audits change the winner of elections nationwide?",200)
  ) +
  theme(legend.position = 'bottom',axis.text.x = element_text(size = 10), axis.title.x = element_blank(),axis.text.y = element_text(size = 10),plot.title = element_text(size=14, face = 'bold'),legend.text = element_text(size = 12),legend.title = element_text(size=12))

##### Do you agree or disagree with this statement: "Election audits are effective in detecting errors in how ballots were counted"? #####
effective_df <- svymean(~QCIA_7, descriptives_design, na.rm = T) |> as.data.frame() 
effective_df <- rownames_to_column(effective_df, "response")
effective_df$pid7_collapsed = "All"
effective_df <- effective_df |> select(-SE)

effective_party <- svyby(~QCIA_7,by = ~pid7_collapsed,design = descriptives_design,FUN = svymean,na.rm=T) |>
  gather(`QCIA_7Disagree`:`se.QCIA_7not asked`,key = "response", value = "mean") |>
  filter(!str_detect(response,"se\\.")) |>
  select(colnames(effective_df)) 

effective_df <- rbind(effective_df,effective_party) |>
  mutate(response = str_remove(response,"QCIA_7"),
         pid7_collapsed = fct_relevel(factor(pid7_collapsed), "All", "Democrat","Republican"))|>
  filter(!(response %in% c("skipped","not asked")))
effective_df <- effective_df |> mutate(
  response = fct_relevel(as.factor(response),"Disagree","Somewhat disagree","Neither agree nor disagree","Somewhat agree","Agree")
) |>
  filter(mean!=0)|> filter(pid7_collapsed != "Independent")

effective <- effective_df |>
  ggplot(aes(x = response, y = mean,fill=pid7_collapsed)) +
  geom_bar(stat='identity',position = "dodge") +
  theme_trust_in_the_count() +
  geom_text(aes(label=paste(round(mean*100),"%", sep="")),
            position = position_dodge(width = 0.9),
            vjust = 2,
            size=2.75,
            color="white", fontface='bold')+
  scale_fill_manual(values = c("#AAAAAA","navy","red")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(labels = label_wrap(20)) +
  labs(fill = "Party ID", y = "") +
  ggtitle("(3) Do you agree or disagree with this statement:\n“Election audits are effective in detecting errors in how ballots were counted”?") +
  theme(legend.position = 'bottom',axis.text.x = element_text(size = 10), axis.title.x = element_blank(),axis.text.y = element_text(size = 10),plot.title = element_text(size=14, face = 'bold'),legend.text = element_text(size = 12),legend.title = element_text(size=12))

##### What percent of ballots do you think are incorrectly counted on average? #####
incorrect_df <- svymean(~QCIA_8, descriptives_design, na.rm = T) |> as.data.frame() 
incorrect_df <- rownames_to_column(incorrect_df, "response")
incorrect_df$pid7_collapsed = "All"
incorrect_df <- incorrect_df |> select(-SE)

incorrect_party <- svyby(~QCIA_8,by = ~pid7_collapsed,design = descriptives_design,FUN = svymean,na.rm=T) |>
  gather(`QCIA_8Less than 1 percent`:`se.QCIA_8not asked`,key = "response", value = "mean") |>
  filter(!str_detect(response,"se\\.")) |>
  select(colnames(incorrect_df)) 

incorrect_df <- rbind(incorrect_df,incorrect_party) |>
  mutate(response = str_remove(response,"QCIA_8"),
         pid7_collapsed = fct_relevel(factor(pid7_collapsed), "All", "Democrat","Republican"))|>
  filter(!(response %in% c("skipped","not asked")))

incorrect_df <- incorrect_df |> mutate(
  response = fct_recode(
    as.factor(response),
    "Less than 1 %"="Less than 1 percent",
    "Between 1-5%"="Between 1 and 5 percent",
    "Between 5-10%"="Between 5 and 10 percent",
    "More than 10%"="More than 10 percent") |>
    fct_relevel("Less than 1 %","Between 1-5%","Between 5-10%","More than 10%")
) |>
  filter(mean!=0)|> filter(pid7_collapsed != "Independent")

incorrect <- incorrect_df |>
  ggplot(aes(x = response, y = mean,fill=pid7_collapsed)) +
  geom_bar(stat='identity',position = "dodge") +
  theme_trust_in_the_count() +
  geom_text(aes(label=paste(round(mean*100),"%", sep="")),
            position = position_dodge(width = 0.9),
            vjust = 2,
            size=2.75,
            color="white", fontface='bold')+
  scale_fill_manual(values = c("#AAAAAA","navy","red")) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_discrete(labels = label_wrap(20)) +
  labs(fill = "Party ID", y = ""
       ,title = str_wrap("(4) What percent of ballots do you think are incorrectly counted on average?",200)
  ) +
  theme(legend.position = 'bottom',axis.text.x = element_text(size = 10), axis.title.x = element_blank(),axis.text.y = element_text(size = 10),plot.title = element_text(size=14, face = 'bold'),legend.text = element_text(size = 12),legend.title = element_text(size=12))

jpeg("plots/figure_1.jpeg",width = 8.5,height = 11,units = 'in', res = 600)
wrap_plots(state_prop,change_winner,effective,incorrect,guides = "collect",ncol = 1) &
  theme(legend.position = 'bottom',plot.margin = unit(c(2.5, 0.5, 2.5, 0.5), "mm"))
dev.off()

# SM, Table C-2 -----------------------------------------------------------
## Creates summary statistic table for respondents by Information Addition
## treatment group

###### recode demographics for display ######
full_survey <- trust_in_the_count |>
  select(audit_info_add_group
         ,pid7_collapsed
         ,educ
         ,votereg
         ,birthyr
  ) |>
  mutate(
    educ = fct_recode(
      factor(educ)
      ,"No HS" = "1"
      ,"High school graduate" = '2'
      ,"Some college" = "3"
      ,"2-year" = "4"
      ,"4-year" = "5"
      ,"Post-grad" = "6"),
    votereg = fct_recode(
      factor(votereg)
      ,"Yes" = "1"
      ,"No" = "2"
      ,"Don't Know" = "3"
    ),
    age = 2023 - as.numeric(birthyr),
    age_cat = case_when(age <= 29 ~ 1, 
                        age <= 44 ~ 2, 
                        age <= 64 ~ 3, 
                        age >= 65 ~ 4),
    age_cat = fct_recode(
      factor(age_cat)
      ,"18-29" = "1"
      ,"30-44" = '2'
      ,'45-64' = "3"
      ,"65+" = "4"
    )
  )

###### build summary table ######
sample_summary <- tbl_summary(
  full_survey
  ,by=audit_info_add_group
  ,label = list(
    pid7_collapsed = "Party ID"
    ,educ = "Education"
    ,votereg = "Voter Registration Status"
    ,age_cat = "Age")
  ,include = -c(age,birthyr)
) |>
  add_overall() |>
  as_kable_extra(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    caption = 'Survey Sample Summary Statistics (by Information Addition Treatment Condition)'
  ) |> 
  kable_styling(latex_options=c("scale_down","HOLD_position")) |>
  save_kable("tables/sm_table_c-2.tex")
