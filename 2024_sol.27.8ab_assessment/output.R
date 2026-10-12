## Extract results of interest, write TAF output tables

## Before: sam_outputs.RDS (model): sam_res, sam_stock as FLR object
## and sam_indices as a list
## After: sam_diagnostics.txt (output);

library("icesTAF")
library("icesAdvice")
library("FLCore")
library("FLEDA")
# library("FLsam")
library("FLAssess")
library("FLash")
library("tidyverse")
library("stockassessment")

### option for ggplot2
theme_set(theme_classic())

### Source function to perform VPA diagnostics
source("utilities_output.R")

###-----------------------------------------------------------------------------
fbar_min <- 3
fbar_max <- 7

mkdir("output")

###-----------------------------------------------------------------------------
### load stock and outputs
fit_sam <- readRDS("model/sam_model_fit.rds")
fit_sam_table <- modeltable(fit_sam)

sam_stock <- FLfse::SAM2FLStock(fit_sam,
                                catch_estimate = TRUE,
                                stock.wt_est = FALSE,
                                catch.wt_est = FALSE)

### Units
### totals (product of the next two), numbers, weights-at-age
Units <- c("tonnes", "thousands", "kg")
sam_ass_stock <- totalStk(sam_stock, Units)
saveRDS(sam_ass_stock, "output/sam_ass_stock.rds")

###-----------------------------------------------------------------------------
### f at age from sam outputs
fatage <- xtab2taf((sam_stock@harvest@.Data)[, , 1, 1, 1, 1]) %>%
  round(., digits = 2)
row.names(fatage) <- sam_stock@range["min"]:sam_stock@range["max"]
# fatage <- t(fatage)
write.taf(fatage, dir = "output")

###-----------------------------------------------------------------------------
### weight at age from sam outputs
watage_stock <- xtab2taf((sam_stock@stock.wt@.Data)[, , 1, 1, 1, 1]) %>%
  round(., digits = 2)
row.names(watage_stock) <- sam_stock@range["min"]:sam_stock@range["max"]
# fatage <- t(fatage)
write.taf(watage_stock, dir = "output")

###-----------------------------------------------------------------------------
###  weight at age from sam outputs
watage_catch <- xtab2taf((sam_stock@catch.wt@.Data)[, , 1, 1, 1, 1]) %>%
  round(., digits = 2)
row.names(watage_catch) <- sam_stock@range["min"]:sam_stock@range["max"]
# fatage <- t(fatage)
write.taf(watage_catch, dir = "output")

###-----------------------------------------------------------------------------
### n at age from sam outputs
natage <- xtab2taf((sam_stock@stock.n@.Data)[, , 1, 1, 1, 1]) %>%
  round(., digits = 0)
row.names(natage) <- sam_stock@range["min"]:sam_stock@range["max"]
# natage <- t(natage)
write.taf(natage, dir = "output")


###-----------------------------------------------------------------------------
### Retro-analysis
### run the retro on 5 years
n_retro_years <- 5

if (file.exists("output/df_retro_analysis.rds")) {
  df_retro_analysis <- read_rds("output/df_retro_analysis.rds")
} else {

  ### Run retro
  df_retro_analysis <- retro(fit_sam,
                             year = n_retro_years)

  saveRDS(df_retro_analysis,
          "output/df_retro_analysis.rds")

  ### Write table to output directory
  # tab_retro_analysis <- xtab2taf(df_retro_analysis)
  # write.taf(tab_retro_analysis, dir = "output")

  ### Mohn's Rho calculation
  df_mohn_retro <- as.data.frame(stockassessment::mohn(df_retro_analysis)) %>%
    rename(value = `stockassessment::mohn(df_retro_analysis)`) %>%
    mutate(Variable = row.names(as.data.frame(stockassessment::mohn(df_retro_analysis))),
           Value = icesRound(value)) %>%
    select(Variable, Value)

  row.names(df_mohn_retro) <- NULL
  saveRDS(df_mohn_retro,
          "output/df_mohn_retro.rds")
  write.taf(df_mohn_retro, dir = "output")
}

if (file.exists("output/lo_fit_sam.rds")) {
  lo_fit_sam <- read_rds("output/lo_fit_sam.rds")
} else {
  # leave-one-out
  lo_fit_sam <- leaveout(fit_sam)

  saveRDS(lo_fit_sam,
          "output/lo_fit_sam.rds")
}

###-----------------------------------------------------------------------------
### compute residuals
###-----------------------------------------------------------------------------
### Observation residuals
residuals_obs <- residuals(fit_sam)
saveRDS(residuals_obs, "output/residuals_obs.rds")

###-----------------------------------------------------------------------------
### joint sample residuals or process residuals
residuals_process <- procres(fit_sam)
saveRDS(residuals_process, "output/residuals_process.rds")

###-----------------------------------------------------------------------------
df_rec <- stockassessment::rectable(fit_sam) %>%
  as.data.frame() %>%
  mutate(Year = rownames(faytable(fit_sam)),
         Variable = "Rec")
saveRDS(df_rec, "output/df_rec.rds")

df_ssb <- stockassessment::ssbtable(fit_sam) %>%
  as.data.frame() %>%
  mutate(Year = rownames(faytable(fit_sam)),
         Variable = "SSB")
saveRDS(df_ssb, "output/df_ssb.rds")

df_tsb <- stockassessment::ssbtable(fit_sam) %>%
  as.data.frame() %>%
  mutate(Year = rownames(faytable(fit_sam)),
         Variable = "TSB")
saveRDS(df_ssb, "output/df_tsb.rds")

df_catch <- stockassessment::catchtable(fit_sam) %>%
  as.data.frame() %>%
  mutate(Year = rownames(faytable(fit_sam)),
         Variable = "Catch")
saveRDS(df_catch, "output/df_catch.rds")

df_fbar <- stockassessment::fbartable(fit_sam) %>%
  as.data.frame() %>%
  mutate(Year = rownames(faytable(fit_sam)),
         Variable = "Fbar")
saveRDS(df_fbar, "output/df_fbar.rds")

df_fit_summary <- bind_rows(df_catch, df_rec,
                            df_fbar, df_ssb) %>%
  mutate(Year = factor(Year))

row.names(df_fit_summary) <- NULL

saveRDS(df_fit_summary,
        "output/df_fit_summary.rds")

###-----------------------------------------------------------------------------
### F at age
df_f_at_age <- as.data.frame(faytable(fit_sam)) %>%
  mutate(Year = rownames(faytable(fit_sam))) %>%
  pivot_longer(., -Year,
               names_to = "Age",
               values_to = "FatAge")

###-----------------------------------------------------------------------------
### Observation error
df_observation_error <- sam_observation_error(fit_sam)

###-----------------------------------------------------------------------------
### Biomass at age (Total biomass not SSB)
df_n_at_age_stock <- flr2taf(sam_stock@stock.n) %>%
  mutate(Category = "Stock") %>%
  pivot_longer(cols = -c("Year", "Category"),
               names_to = "Age",
               values_to = "Numbers")

df_w_at_age_stock <- flr2taf(sam_stock@stock.wt) %>%
  mutate(Category = "Stock") %>%
  pivot_longer(cols = -c("Year", "Category"),
               names_to = "Age",
               values_to = "Mean_Weight")

df_B_at_age_stock <- left_join(df_n_at_age_stock, df_w_at_age_stock,
                               by = join_by(Year, Category, Age)) %>%
  mutate(Biomass =  Numbers * Mean_Weight) %>%
  group_by(Year, Category) %>%
  mutate(Total_Biomass = sum(Biomass, na.rm = TRUE),
         Total_number = sum(Numbers, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Prop_total_biomass = Biomass/Total_Biomass)

saveRDS(df_B_at_age_stock, "output/df_B_at_age_stock.rds")

###-----------------------------------------------------------------------------
### Run STF
source("short_term_forecast.R")
