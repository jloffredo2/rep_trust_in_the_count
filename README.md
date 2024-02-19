# Replication File for Jaffe et al., "Trust in the Count: Improving Voter Confidence with Post-Election Audits" (Public Opinion Quarterly)

## Structure
* The scripts below---found in the `code` folder---do the following for replicating the tables and figures in the paper:
  * `01_trust-in-the-count_clean.R`: codes main variables used in analysis; creates analysis table `trust_in_the_count_analysis.rds`
  * `02_trust-in-the-count_descriptives.R`: conducts descriptive analyses; produces Figure 1 and Supplementary Material, Table C-2
  * `03_trust-in-the-count_vignette.R`: conducts analysis for *Information Addition* and *Order of Magnitude* experiments; produces Figure 2, Supplementary Material Figure F-9, Tables D-3 through D-8, and F-12
  * `04_trust-in-the-count_conjoint.R`: conducts analysis for *Audit Attributes* experiment; produces Figure 3, Figure 4, Supplementary Material Figures E-2 through E-8, and Supplementary Material Tables E-9 through E-11
* Raw data collected by YouGov can be found in `data/trust_in_the_count_raw.rds`; data prepped for analysis can be found in `data/trust_in_the_count_analysis.rds`
* All figures presented in the main paper and the supplementary material are `.jpeg` files and found in the `plots` folder
* All tables presented in the main paper and the supplementary material are `.tex` files and found in the `tables` folder
* `codebook.docx` contains information on all variables in `data/trust_in_the_count_raw.rds`
* `rep_trust_in_the_count.Rproj` is an R project that can be used to replicate the tables and figures in the paper