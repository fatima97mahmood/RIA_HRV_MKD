## Preparing Data

x_1<- x

# creating country pairs and pair_id
x_1 <- x_1 %>%
  mutate(pair = paste(pmin(exporter_iso3, importer_iso3), pmax(exporter_iso3, importer_iso3), sep = "_")) %>%
  group_by(pair) %>%
  mutate(pair_id = cur_group_id())

## Create fixed effects
x_1 = x_1 %>%
  unite("fe_exp_year",c(exporter_iso3,year),sep="_",remove=FALSE) %>%
  unite("fe_imp_year",c(importer_iso3,year),sep="_",remove=FALSE)

## Specify country-specific intra-national trade dummies
x_1 = x_1 %>%
  mutate(D_trade_ii = ifelse(exporter_iso3==importer_iso3,exporter_iso3,"international"))

# creating dummies for RTAs

x_1 <- x_1 %>%
  mutate(efta_MKD = ifelse((exporter_iso3 == "MKD" | importer_iso3 == "MKD" ) & year >= 2002, 1, 0))

x_1 <- x_1 %>%
  mutate(HRV_efta = ifelse((exporter_iso3 == "HRV" | importer_iso3 == "HRV" ) & year >= 2002 & year <= 2013, 1, 0))

x_1 <- x_1 %>%
  mutate(rta_bilateral_1997 = ifelse(((exporter_iso3 == "MKD" & importer_iso3 == "HRV")| (importer_iso3 == "MKD" & exporter_iso3 == "HRV")) & year >= 1997 & year <= 2007, 1, 0))

x_1 <- x_1 %>%
  mutate(rta_bilateral_2003 = ifelse(((exporter_iso3 == "MKD" & importer_iso3 == "HRV")| (importer_iso3 == "MKD" & exporter_iso3 == "HRV")) & year >= 2003 & year <= 2013, 1, 0))

x_1 <- x_1 %>%
  mutate(ec_MKD = ifelse((exporter_iso3 == "MKD" | importer_iso3 == "MKD" ) & year >= 2004, 1, 0))

x_1 <- x_1 %>%
  mutate(HRV_ec = ifelse((exporter_iso3 == "HRV" | importer_iso3 == "HRV" ) & year >= 2005 & year <= 2013, 1, 0))

x_1 <- x_1 %>%
  mutate(cefta_MKD = ifelse((exporter_iso3 == "MKD" | importer_iso3 == "MKD" ) & year >= 2007, 1, 0))

x_1 <- x_1 %>%
  mutate(HRV_cefta = ifelse((exporter_iso3 == "HRV" | importer_iso3 == "HRV" ) & year >= 2007 & year <= 2013, 1, 0))

# creating EU dummy 
# List of EU countries for each enlargement
EU_1981 <- c("BEL", "DNK", "FRA", "DEU", "GRC", "IRL", "ITA", "LUX", "NLD", "GBR")
EU_1986 <- c(EU_1981, "PRT", "ESP")
EU_1995 <- c(EU_1986, "AUT", "FIN", "SWE")
EU_2004 <- c(EU_1995, "CYP", "CZE", "EST", "HUN", "LVA", "LTU", "MLT", "POL", "SVK", "SVN")
EU_2007 <- c(EU_2004, "BGR", "ROU")
EU_2013 <- c(EU_2007, "HRV")


# Create dummy variable
x_1$EU_membership <- ifelse((x_1$exporter_iso3 %in% EU_1981 | x_1$importer_iso3 %in% EU_1981) & x_1$year >= 1981 & x_1$year < 1986 |
                         (x_1$exporter_iso3 %in% EU_1986 | x_1$importer_iso3 %in% EU_1986) & x_1$year >= 1986 & x_1$year < 1995 |
                         (x_1$exporter_iso3 %in% EU_1995 | x_1$importer_iso3 %in% EU_1995) & x_1$year >= 1995 & x_1$year < 2004 |
                         (x_1$exporter_iso3 %in% EU_2004 | x_1$importer_iso3 %in% EU_2004) & x_1$year >= 2004 & x_1$year < 2007 |
                         (x_1$exporter_iso3 %in% EU_2007 | x_1$importer_iso3 %in% EU_2007) & x_1$year >= 2007 & x_1$year < 2013 |
                         (x_1$exporter_iso3 %in% EU_2013 | x_1$importer_iso3 %in% EU_2013) & x_1$year >= 2013, 1, 0)

x_1$EU_membership_bi <- ifelse((x_1$exporter_iso3 %in% EU_1981 & x_1$importer_iso3 %in% EU_1981) & x_1$year >= 1981 & x_1$year < 1986 |
                              (x_1$exporter_iso3 %in% EU_1986 & x_1$importer_iso3 %in% EU_1986) & x_1$year >= 1986 & x_1$year < 1995 |
                              (x_1$exporter_iso3 %in% EU_1995 & x_1$importer_iso3 %in% EU_1995) & x_1$year >= 1995 & x_1$year < 2004 |
                              (x_1$exporter_iso3 %in% EU_2004 & x_1$importer_iso3 %in% EU_2004) & x_1$year >= 2004 & x_1$year < 2007 |
                              (x_1$exporter_iso3 %in% EU_2007 & x_1$importer_iso3 %in% EU_2007) & x_1$year >= 2007 & x_1$year < 2013 |
                              (x_1$exporter_iso3 %in% EU_2013 & x_1$importer_iso3 %in% EU_2013) & x_1$year >= 2013, 1, 0)

# Creating Cefta dummy 
CEFTA_1992 <- c("POL", "HUN", "CZE", "SVK")
CEFTA_1996 <- c(CEFTA_1992, "SVN")
CEFTA_1997 <- c(CEFTA_1996, "ROU")
CEFTA_1999 <- c(CEFTA_1997, "BGR")
CEFTA_2003 <- c(CEFTA_1999, "HRV")
CEFTA_2004 <- c("ROU", "BGR", "HRV")
CEFTA_2006 <- c("HRV", "MKD")
CEFTA_2007 <- c(CEFTA_2006, "ALB", "BIH", "MDA", "SRB", "MNE", "XKX")
CEFTA_2013 <- setdiff(CEFTA_2007, "HRV")

x_1$CEFTA_membership <- ifelse((x_1$exporter_iso3 %in% CEFTA_1992 | x_1$importer_iso3 %in% CEFTA_1992) & x_1$year >= 1992 |
                            (x_1$exporter_iso3 %in% CEFTA_1996 | x_1$importer_iso3 %in% CEFTA_1996) & x_1$year >= 1996 |
                            (x_1$exporter_iso3 %in% CEFTA_1997 | x_1$importer_iso3 %in% CEFTA_1997) & x_1$year >= 1997 |
                            (x_1$exporter_iso3 %in% CEFTA_1999 | x_1$importer_iso3 %in% CEFTA_1999) & x_1$year >= 1999 |
                            (x_1$exporter_iso3 %in% CEFTA_2003 | x_1$importer_iso3 %in% CEFTA_2003) & x_1$year >= 2003 |
                            (x_1$exporter_iso3 %in% CEFTA_2004 | x_1$importer_iso3 %in% CEFTA_2004) & x_1$year >= 2004 |
                            (x_1$exporter_iso3 %in% CEFTA_2006 | x_1$importer_iso3 %in% CEFTA_2006) & x_1$year >= 2006 |
                            (x_1$exporter_iso3 %in% CEFTA_2007 | x_1$importer_iso3 %in% CEFTA_2007) & x_1$year >= 2007 |
                            (x_1$exporter_iso3 %in% CEFTA_2013 | x_1$importer_iso3 %in% CEFTA_2013) & x_1$year >= 2013, 1, 0)

x_1$CEFTA_membership_bi <- ifelse((x_1$exporter_iso3 %in% CEFTA_1992 & x_1$importer_iso3 %in% CEFTA_1992) & x_1$year >= 1992 |
                                 (x_1$exporter_iso3 %in% CEFTA_1996 & x_1$importer_iso3 %in% CEFTA_1996) & x_1$year >= 1996 |
                                 (x_1$exporter_iso3 %in% CEFTA_1997 & x_1$importer_iso3 %in% CEFTA_1997) & x_1$year >= 1997 |
                                 (x_1$exporter_iso3 %in% CEFTA_1999 & x_1$importer_iso3 %in% CEFTA_1999) & x_1$year >= 1999 |
                                 (x_1$exporter_iso3 %in% CEFTA_2003 & x_1$importer_iso3 %in% CEFTA_2003) & x_1$year >= 2003 |
                                 (x_1$exporter_iso3 %in% CEFTA_2004 & x_1$importer_iso3 %in% CEFTA_2004) & x_1$year >= 2004 |
                                 (x_1$exporter_iso3 %in% CEFTA_2006 & x_1$importer_iso3 %in% CEFTA_2006) & x_1$year >= 2006 |
                                 (x_1$exporter_iso3 %in% CEFTA_2007 & x_1$importer_iso3 %in% CEFTA_2007) & x_1$year >= 2007 |
                                 (x_1$exporter_iso3 %in% CEFTA_2013 & x_1$importer_iso3 %in% CEFTA_2013) & x_1$year >= 2013, 1, 0)

# Dummies for directionalilty 
x_1$HRV_MKD <- ifelse(x_1$exporter_iso3 == "HRV" & x_1$importer_iso3 == "MKD", 1, 0)
x_1$MKD_HRV <- ifelse(x_1$exporter_iso3 == "MKD" & x_1$importer_iso3 == "HRV", 1, 0)
x_1$HRV_EU <- ifelse(x_1$exporter_iso3 == "HRV" & x_1$EU_membership == 1, 1, 0)
x_1$MKD_EU <- ifelse(x_1$exporter_iso3 == "MKD" & x_1$EU_membership == 1, 1, 0)
x_1$EU_HRV <- ifelse(x_1$importer_iso3 == "HRV" & x_1$EU_membership == 1, 1, 0)
x_1$EU_MKD <- ifelse(x_1$importer_iso3 == "MKD" & x_1$EU_membership == 1, 1, 0)
x_1$HRV_CEFTA <- ifelse(x_1$exporter_iso3 == "HRV" & x_1$CEFTA_membership == 1, 1, 0)
x_1$MKD_CEFTA <- ifelse(x_1$exporter_iso3 == "MKD" & x_1$CEFTA_membership == 1, 1, 0)
x_1$CEFTA_HRV <- ifelse(x_1$importer_iso3 == "HRV" & x_1$CEFTA_membership == 1, 1, 0)
x_1$CEFTA_MKD <- ifelse(x_1$importer_iso3 == "MKD" & x_1$CEFTA_membership == 1, 1, 0)

# changing the dummies to only include overlapping years to take care of multicolleniatry from FE

x_1 <- x_1 %>%
  mutate(efta = ifelse(((exporter_iso3 == "MKD" & importer_iso3 == "HRV")| (importer_iso3 == "MKD" & exporter_iso3 == "HRV")) & year >= 2002 & year <= 2013, 1, 0))


x_1 <- x_1 %>%
  mutate(ec = ifelse(((exporter_iso3 == "MKD" & importer_iso3 == "HRV")| (importer_iso3 == "MKD" & exporter_iso3 == "HRV")) & year >= 2005 & year <= 2013, 1, 0))


x_1 <- x_1 %>%
  mutate(cefta = ifelse(((exporter_iso3 == "MKD" & importer_iso3 == "HRV")| (importer_iso3 == "MKD" & exporter_iso3 == "HRV")) & year >= 2007 & year <= 2013, 1, 0))

## for Annual Trade Regressions

# summing data for each year and country pair to sum over broad sectors
x_2 <- x_1 %>%
  group_by(year, exporter_iso3, importer_iso3) %>%
  summarize(total_trade = sum(trade))
x_2 <- merge(x_2, x_1, by = c("year", "exporter_iso3", "importer_iso3"), all.x = TRUE)
x_2 <- distinct(x_2, year, exporter_iso3, importer_iso3, .keep_all = TRUE)
x_2 <- x_2 %>% select(-broad_sector)

# creating variables for exporter and importer total trades
x_2 = x_2 %>%
  group_by(exporter_iso3,year) %>%
  mutate(Y_it = sum(trade)) %>%
  group_by(importer_iso3,year) %>%
  mutate(E_jt = sum(trade))

# taking logs
x_2 = x_2 %>%
  mutate(across(c(total_trade,Y_it,E_jt,distance),~log(.x),.names="ln_{.col}"))

# OLS
fit_ols_simple = feols(ln_total_trade ~ efta_MKD + HRV_efta + rta_bilateral_1997 + rta_bilateral_2003 + ec_MKD + HRV_ec + cefta_MKD + HRV_cefta +ln_distance + contiguity + common_language + common_colonizer + ln_Y_it + ln_E_jt,
                data = x_2 %>%
                  filter(trade > 0 & exporter_iso3 != importer_iso3),
                vcov = cluster ~ pair_id)
summary(fit_ols_simple)

fit_ols_bilateral = feols(ln_total_trade ~ efta_MKD + HRV_efta + rta_bilateral_1997 + rta_bilateral_2003 + ec_MKD + HRV_ec + cefta_MKD + HRV_cefta +ln_distance + contiguity + common_language + common_colonizer + ln_Y_it + ln_E_jt,
                          data = x_2 %>%
                            filter(trade > 0 & exporter_iso3 != importer_iso3 & (exporter_iso3 == "HRV" | importer_iso3 == "HRV"| importer_iso3 == "MKD" | exporter_iso3 == "MKD")),
                          vcov = cluster ~ pair_id)
summary(fit_ols_bilateral)

# for output table
fit_ols = feols(ln_total_trade ~ rta_bilateral_1997 + rta_bilateral_2003 + efta + ec + cefta +ln_distance + contiguity + common_language + common_colonizer + ln_Y_it + ln_E_jt,
                data = x_2 %>%
                  filter(trade > 0 & exporter_iso3 != importer_iso3),
                vcov = cluster ~ pair_id)
summary(fit_ols)

# FEOLS
fit_feols = feols(ln_total_trade ~ rta_bilateral_1997 + rta_bilateral_2003 +ln_distance + contiguity + common_language + common_colonizer|
                    fe_exp_year + fe_imp_year,
                  data = x_2 %>%
                    filter(trade > 0 & exporter_iso3 != importer_iso3),
                  vcov = cluster ~ pair_id)
summary(fit_feols)

## for output table

fit_feols_all = feols(ln_total_trade ~ rta_bilateral_1997 + rta_bilateral_2003 + efta + ec + cefta +ln_distance + contiguity + common_language + common_colonizer|
                        fe_exp_year + fe_imp_year,
                      data = x_2 %>%
                        filter(trade > 0 & exporter_iso3 != importer_iso3),
                      vcov = cluster ~ pair_id)
summary(fit_feols_all)

# PPML
rta_poisson = fepois(total_trade ~ rta_bilateral_1997 + rta_bilateral_2003 + efta + ec + cefta + ln_distance + contiguity + common_language + common_colonizer |
                       fe_exp_year + fe_imp_year,
                     data = x_2 %>%
                       filter(exporter_iso3 != importer_iso3),
                     vcov = cluster ~ pair_id)
summary(rta_poisson)

rta_poisson_intra = fepois(total_trade ~ rta_bilateral_1997 + rta_bilateral_2003 + efta + ec + cefta + ln_distance + contiguity + common_language + common_colonizer |
                             fe_exp_year + fe_imp_year + D_trade_ii,
                           data = x_2,
                           vcov = cluster ~ pair_id)
summary(rta_poisson_intra)

# PPML best specification  lets go step by step!

rta_endo_1 = fepois(total_trade ~ rta_bilateral_1997 + rta_bilateral_2003 |
                    fe_exp_year + fe_imp_year + pair,
                  data = x_2,
                  vcov = cluster ~ pair_id)
summary(rta_endo_1)

rta_endo_2 = fepois(total_trade ~ rta_bilateral_1997 + rta_bilateral_2003 + efta + ec + cefta |
                    fe_exp_year + fe_imp_year + pair,
                  data = x_2,
                  vcov = cluster ~ pair_id)
summary(rta_endo_2)

rta_endo_3 = fepois(total_trade ~ rta_bilateral_1997 + rta_bilateral_2003 + efta + ec + cefta + EU_membership + CEFTA_membership |
                      fe_exp_year + fe_imp_year + pair,
                    data = x_2,
                    vcov = cluster ~ pair_id)
summary(rta_endo_3)

rta_endo_4 = fepois(total_trade ~ rta_bilateral_1997 + rta_bilateral_2003 + efta + ec + cefta + EU_membership + CEFTA_membership + EU_membership_bi + CEFTA_membership_bi|
                      fe_exp_year + fe_imp_year + pair,
                    data = x_2,
                    vcov = cluster ~ pair_id)
summary(rta_endo_4)

rta_endo_5 = fepois(total_trade ~ rta_bilateral_1997 + rta_bilateral_2003 + efta + ec + cefta + HRV_MKD + MKD_HRV + EU_membership + CEFTA_membership + EU_membership_bi + CEFTA_membership_bi|
                      fe_exp_year + fe_imp_year + pair,
                    data = x_2,
                    vcov = cluster ~ pair_id)
summary(rta_endo_5)

rta_endo_6 = fepois(total_trade ~  EU_membership + CEFTA_membership + EU_membership_bi + CEFTA_membership_bi + HRV_EU + HRV_CEFTA + EU_HRV + CEFTA_HRV + MKD_EU + MKD_CEFTA + EU_MKD + CEFTA_MKD|
                      fe_exp_year + fe_imp_year + pair,
                    data = x_2,
                    vcov = cluster ~ pair_id)
summary(rta_endo_6)

rta_endo_7 = fepois(total_trade ~ rta_bilateral_1997 + rta_bilateral_2003 + efta + ec + cefta + EU_membership + CEFTA_membership + EU_membership_bi + CEFTA_membership_bi + HRV_MKD + MKD_HRV + HRV_EU + HRV_CEFTA + EU_HRV + CEFTA_HRV + MKD_EU + MKD_CEFTA + EU_MKD + CEFTA_MKD|
                      fe_exp_year + fe_imp_year + pair,
                    data = x_2,
                    vcov = cluster ~ pair_id)
summary(rta_endo_7)

#exporting results
tab_results <- huxreg(
  "(1) OLS" = fit_ols,
  "(2) FEOLS" = fit_feols_all,
  "(3) PPML Bilateral" = rta_endo_1,
  "(4) PPML All RTAs" = rta_endo_2,
  "(5) PPML Memberships" = rta_endo_3,
  "(6) PPML Bilateral Memberships" = rta_endo_4,
  "(7) PPML Directionality" = rta_endo_5,
  "(8) PPML Directionality & Memberships" = rta_endo_6,
  "(9) PPML Directionality, Memberships & RTAs" = rta_endo_7,
  coefs = c(
    "Exporter Output" = "ln_Y_it",
    "Importer Expenditure" = "ln_E_jt",
    "Log distance" = "ln_distance",
    "Contiguity" = "contiguity",
    "Colony" = "common_colonizer",
    "Common language" = "common_language",
    "RTA 1997" = "rta_bilateral_1997",
    "RTA 2003" = "rta_bilateral_2003",
    "EFTA" = "efta",
    "EC" = "ec",
    "CEFTA" = "cefta",
    "EU Membership" = "EU_membership",
    "CEFTA Membership" = "CEFTA_membership",
    "CEFTA Membership Bilateral" = "CEFTA_membership_bi",
    "HRV MKD" = "HRV_MKD",
    "MKD HRV" = "MKD_HRV",
    "HRV EU" = "HRV_EU",
    "MKD EU" = "MKD_EU",
    "EU HRV" = "EU_HRV",
    "EU MKD" = "EU_MKD",
    "HRV CEFTA" = "HRV_CEFTA",
    "MKD CEFTA" = "MKD_CEFTA",
    "CEFTA HRV" = "CEFTA_HRV",
    "CEFTA MKD" = "CEFTA_MKD"
  )
) %>%
  set_tb_padding(0) %>%
  set_col_width(c(0.3, rep(0.7 / 7, 7))) %>%
  set_align(everywhere, -1, "center") %>%
  set_label("tab_results")

# Print the table
print(tab_results)

## Export table to latex
cat(to_latex(tab_results),file=here("output","tables","tab_results.tex"))

## Export table to word
tab_results_docx = as_flextable(tab_results)
save_as_docx(tab_results_docx, path = here("output","tables","tab_results.docx"))


## for Sector wise Regressions

# creating variables for exporter and importer total trades
x_3 = x_1 %>%
  group_by(exporter_iso3,year) %>%
  mutate(Y_it = sum(trade)) %>%
  group_by(importer_iso3,year) %>%
  mutate(E_jt = sum(trade))

# taking logs
x_3 = x_3 %>%
  mutate(across(c(trade,Y_it,E_jt,distance),~log(.x),.names="ln_{.col}"))

x_agri <- subset(x_3, broad_sector == "Agriculture")
x_manu <- subset(x_3, broad_sector == "Manufacturing")
x_serv <- subset(x_3, broad_sector == "Services")
x_min <- subset(x_3, broad_sector == "Mining and Energy")



# Define the function to fit the Poisson regression model
fit_poisson_model <- function(formula, data, vcov_formula) {
  model <- fepois(formula, data = data, vcov = vcov_formula)
  result_summary <- summary(model)
  return(result_summary)
}

# List of dataset names
dataset_names <- c("x_serv", "x_agri", "x_manu", "x_min")

# Create empty lists to store the results
rta_endo_1_results <- list()
rta_endo_2_results <- list()

# Iterate over the dataset names
for (dataset_name in dataset_names) {
  tryCatch({
    # Debugging: Print dataset name before loading
    cat("Loading dataset:", dataset_name, "\n")
    
    # Load the dataset
    data <- get(dataset_name)
    
    # Fit the endo model with directionality
    rta_endo_2_results[[dataset_name]] <- fit_poisson_model(
      trade ~ rta_bilateral_1997 + rta_bilateral_2003 + efta + ec + cefta + EU_membership + CEFTA_membership + EU_membership_bi + CEFTA_membership_bi + HRV_MKD + MKD_HRV + HRV_EU + HRV_CEFTA + EU_HRV + CEFTA_HRV + MKD_EU + MKD_CEFTA + EU_MKD + CEFTA_MKD |
        fe_exp_year + fe_imp_year + pair,
      data = data,
      vcov_formula = cluster ~ pair_id
    )
    
    # Fit the endo model
    rta_endo_1_results[[dataset_name]] <- tryCatch({
      fit_poisson_model(
        trade ~ rta_bilateral_1997 + rta_bilateral_2003 + efta + ec + cefta |
          fe_exp_year + fe_imp_year + pair,
        data = data,
        vcov_formula = cluster ~ pair_id
      )
    }, error = function(e) {
      # Print error message
      cat("Error occurred for endo_1 model in dataset:", dataset_name, "\n")
      cat("Error message:", conditionMessage(e), "\n\n")
      return(NULL)  # Return NULL if the model fails
    })
    
    # Check if endo_1 model was fitted successfully
    if (!is.null(rta_endo_1_results[[dataset_name]])) {
      # Print the summaries
      cat("Dataset:", dataset_name, "\n")
      cat("Summary for rta_endo_1:\n")
      print(rta_endo_1_results[[dataset_name]])
      cat("\n")
    }
    
    # Print the summary for endo_2 model
    cat("Summary for rta_endo_2:\n")
    print(rta_endo_2_results[[dataset_name]])
    cat("\n")
    
  }, error = function(e) {
    # Print error message
    cat("Error occurred for dataset:", dataset_name, "\n")
    cat("Error message:", conditionMessage(e), "\n\n")
  })
}

#exporting results
tab_results <- huxreg(
  "(1) RTAs Agriculture" = rta_endo_1_results$x_agri,
  "(2) RTAs Manufacturing" = rta_endo_1_results$x_manu,
  "(3) RTAs Mining & Energy" = rta_endo_1_results$x_min,
  "(4) RTAs & Directionality Agriculture" = rta_endo_2_results$x_agri,
  "(5) RTAs & Directionality Manufacturing" = rta_endo_2_results$x_manu,
  "(6) RTAs & Directionality Mining & Energy" = rta_endo_2_results$x_min,
  "(7) RTAs & Directionality Services" = rta_endo_2_results$x_serv,
  coefs = c(
    "RTA 1997" = "rta_bilateral_1997",
    "RTA 2003" = "rta_bilateral_2003",
    "EFTA" = "efta",
    "EC" = "ec",
    "CEFTA" = "cefta",
    "EU Membership" = "EU_membership",
    "CEFTA Membership" = "CEFTA_membership",
    "EU Membership Bilateral" = "EU_membership_bi",
    "CEFTA Membership Bilateral" = "CEFTA_membership_bi",
    "HRV MKD" = "HRV_MKD",
    "MKD HRV" = "MKD_HRV",
    "HRV EU" = "HRV_EU",
    "MKD EU" = "MKD_EU",
    "EU HRV" = "EU_HRV",
    "EU MKD" = "EU_MKD",
    "HRV CEFTA" = "HRV_CEFTA",
    "MKD CEFTA" = "MKD_CEFTA",
    "CEFTA HRV" = "CEFTA_HRV",
    "CEFTA MKD" = "CEFTA_MKD"
  )
) %>%
  set_tb_padding(0) %>%
  set_col_width(c(0.3, rep(0.7 / 7, 7))) %>%
  set_align(everywhere, -1, "center") %>%
  set_label("tab_results")

# Print the table
print(tab_results)


## Considering individual RTAs to account for lead, lag and globalization effects
x_2 = x_2 %>%
  mutate(D_inter = ifelse(importer_iso3 != exporter_iso3, 1, 0),
         intl_border_year = paste0("intl_border_",year)) %>%
  pivot_wider(names_from="intl_border_year",
              values_from="D_inter",
              values_fill = 0)

analyze_rta_effects <- function(data, rta_var_name, border_years) {
  rta_var <- sym(rta_var_name)
  
  data <- data %>%
    group_by(exporter_iso3, importer_iso3) %>%
    arrange(year) %>%
    mutate(
      rta_lead = lead(!!sym(rta_var), n=4, order_by = year, default = NA),
      rta_lag4 = lag(!!sym(rta_var), n= 4, order_by = year, default = NA),
      rta_lag8 = lag(!!sym(rta_var), n = 8, order_by = year, default = NA),
      rta_lag12 = lag(!!sym(rta_var), n = 12, order_by = year, default = NA)
    )
  
  rta_lead_model <- fepois(
    fml = as.formula(sprintf("total_trade ~ %s + rta_lead | fe_exp_year + fe_imp_year", rta_var_name)),
    data = data,
    vcov = ~ pair_id
  )
  
  rta_lag_model <- fepois(
    fml = as.formula(sprintf("total_trade ~ %s + rta_lag4 + rta_lag8 + rta_lag12 | fe_exp_year + fe_imp_year + pair", rta_var_name)),
    data = data,
    vcov = ~ pair_id
  )
  
  # Visualize the evolution of coefficients over time
  coeff_rta_lag <- coefficients(rta_lag_model)
  coeff_rta_lag <- cbind.data.frame(coeff_rta_lag, confint(rta_lag_model))
  colnames(coeff_rta_lag) <- c("coeff", "lower_ci", "upper_ci")
  coeff_rta_lag <- coeff_rta_lag %>%
    rownames_to_column(var = "variable") %>%
    mutate(variable = factor(variable, levels = unique(variable), ordered = TRUE))
  
  fig_rta_evo <- ggplot(coeff_rta_lag) + 
    geom_point(aes(x = variable, y = coeff)) + 
    geom_line(aes(x = variable, y = coeff, group = 1), linetype = linetypes[2]) +
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, x = variable), width = 0.2) + 
    theme_minimal() +
    theme(panel.grid = element_blank(), axis.title.y = element_text(angle = 0, vjust = 0.5)) +
    ylab("Coefficients") +
    xlab("Variable")
  
  rta_glob_model <- fepois(
    fml = as.formula(sprintf("total_trade ~ %s + rta_lag4 + rta_lag8 + rta_lag12 + %s | fe_exp_year + fe_imp_year + pair", rta_var_name, paste(border_years, collapse = " + "))),
    data = data,
    vcov = ~ pair_id
  )
  
  # Return summaries of the models
  return(list(
    rta_data = data,
    rta_lead_summary = summary(rta_lead_model),
    rta_lag_summary = summary(rta_lag_model),
    rta_glob_summary = summary(rta_glob_model),
    fig_rta_evo = fig_rta_evo
  ))
}

# For Bilateral 1997
results_bi1997 <- analyze_rta_effects(x_2, "rta_bilateral_1997", c("intl_border_1993", "intl_border_1997", "intl_border_2001", "intl_border_2005", "intl_border_2009", "intl_border_2013"))

# Access the results
results_bi1997$rta_lead_summary
results_bi1997$rta_lag_summary
results_bi1997$rta_glob_summary
fig_rta_bi1997 = results_bi1997$fig_rta_evo
ggsave(file=here("output","figures","fig_rta_bi1997_evo.png"), plot=fig_rta_bi1997,
       height=20/1.7,units="cm")

# For Bilateral 2003
results_bi2003 <- analyze_rta_effects(x_2, "rta_bilateral_2003", c("intl_border_1999","intl_border_2003", "intl_border_2007", "intl_border_2011", "intl_border_2015", "intl_border_2019"))

# Access the results
results_bi2003$rta_lead_summary
results_bi2003$rta_lag_summary
results_bi2003$rta_glob_summary
fig_rta_bi2003 = results_bi2003$fig_rta_evo
ggsave(file=here("output","figures","fig_rta_bi2003_evo.png"), plot=fig_rta_bi2003,
       height=20/1.7,units="cm")

# For EFTA
results_efta <- analyze_rta_effects(x_2, "efta", c("intl_border_1998","intl_border_2002", "intl_border_2006", "intl_border_2010", "intl_border_2014","intl_border_2018"))

# Access the results
results_efta$rta_lead_summary
results_efta$rta_lag_summary
results_efta$rta_glob_summary
fig_rta_efta = results_efta$fig_rta_evo
ggsave(file=here("output","figures","fig_rta_efta_evo.png"), plot=fig_rta_efta,
       height=20/1.7,units="cm")

# For CEFTA
results_cefta <- analyze_rta_effects(x_2, "cefta", c("intl_border_2003","intl_border_2007", "intl_border_2011", "intl_border_2015", "intl_border_2019"))

# Access the results
results_cefta$rta_lead_summary
results_cefta$rta_lag_summary
results_cefta$rta_glob_summary
fig_rta_cefta = results_cefta$fig_rta_evo
ggsave(file=here("output","figures","fig_rta_cefta_evo.png"), plot=fig_rta_cefta,
       height=20/1.7,units="cm")

# For EC
results_ec <- analyze_rta_effects(x_2, "ec", c("intl_border_2000","intl_border_2004", "intl_border_2008", "intl_border_2012", "intl_border_2016"))

# Access the results
results_ec$rta_lead_summary
results_ec$rta_lag_summary
results_ec$rta_glob_summary
fig_rta_ec = results_ec$fig_rta_evo
ggsave(file=here("output","figures","fig_rta_ec_evo.png"), plot=fig_rta_ec,
       height=20/1.7,units="cm")

#exporting results
tab_results <- huxreg(
  "(1) Bilateral 1997" = results_bi1997$rta_lead_summary,
  "(2) Bilateral 2003" = results_bi2003$rta_lead_summary,
  "(3) EFTA" = results_efta$rta_lead_summary,
  "(4) EC" = results_ec$rta_lead_summary,
  "(5) CEFTA" = results_cefta$rta_lead_summary,
  coefs = c(
    "RTA 1997" = "rta_bilateral_1997",
    "RTA 2003" = "rta_bilateral_2003",
    "EFTA" = "efta",
    "EC" = "ec",
    "CEFTA" = "cefta",
    "RTA Lead (4)" = "rta_lead"
  )
) %>%
  set_tb_padding(0) %>%
  set_col_width(c(0.3, rep(0.7 / 7, 7))) %>%
  set_align(everywhere, -1, "center") %>%
  set_label("tab_results")
print(tab_results)

# Exporting results
tab_results <- huxreg(
  "(1) Bilateral 1997" = results_bi1997$rta_lag_summary,
  "(2) Bilateral 2003" = results_bi2003$rta_lag_summary,
  "(3) EFTA" = results_efta$rta_lag_summary,
  "(4) EC" = results_ec$rta_lag_summary,
  "(5) CEFTA" = results_cefta$rta_lag_summary,
  coefs = c(
    "RTA 1997" = "rta_bilateral_1997",
    "RTA 2003" = "rta_bilateral_2003",
    "EFTA" = "efta",
    "EC" = "ec",
    "CEFTA" = "cefta",
    "RTA Lag (4)" = "rta_lag4",
    "RTA Lag (8)" = "rta_lag8",
    "RTA Lag (12)" = "rta_lag12"  # Removed the trailing comma
  )
) %>%
  set_tb_padding(0) %>%
  set_col_width(c(0.3, rep(0.7 / 7, 7))) %>%
  set_align(everywhere, -1, "center") %>%
  set_label("tab_results")

# Print the table
print(tab_results)

#exporting results
tab_results <- huxreg(
  "(1) Bilateral 1997" = results_bi1997$rta_glob_summary,
  "(2) Bilateral 2003" = results_bi2003$rta_glob_summary,
  "(3) EFTA" = results_efta$rta_glob_summary,
  "(4) EC" = results_ec$rta_glob_summary,
  "(5) CEFTA" = results_cefta$rta_glob_summary,
  coefs = c(
    "RTA 1997" = "rta_bilateral_1997",
    "RTA 2003" = "rta_bilateral_2003",
    "EFTA" = "efta",
    "EC" = "ec",
    "CEFTA" = "cefta",
    "RTA Lag (4)" = "rta_lag4",
    "RTA Lag (8)" = "rta_lag8",
    "RTA Lag (12)" = "rta_lag12",
    "Intl Border 2000" = "intl_border_2000",
    "Intl Border 2001" = "intl_border_2001",
    "Intl Border 2002" = "intl_border_2002",
    "Intl Border 2003" = "intl_border_2003",
    "Intl Border 2004" = "intl_border_2004",
    "Intl Border 2005" = "intl_border_2005",
    "Intl Border 2006" = "intl_border_2006",
    "Intl Border 2007" = "intl_border_2007",
    "Intl Border 2008" = "intl_border_2008",
    "Intl Border 2009" = "intl_border_2009",
    "Intl Border 2010" = "intl_border_2010",
    "Intl Border 2011" = "intl_border_2011",
    "Intl Border 2012" = "intl_border_2012",
    "Intl Border 2013" = "intl_border_2013",
    "Intl Border 2014" = "intl_border_2014",
    "Intl Border 2015" = "intl_border_2015",
    "Intl Border 2016" = "intl_border_2016",
    "Intl Border 2018" = "intl_border_2018",
    "Intl Border 2019" = "intl_border_2019"
  )
) %>%
  set_tb_padding(0) %>%
  set_col_width(c(0.3, rep(0.7 / 7, 7))) %>%
  set_align(everywhere, -1, "center") %>%
  set_label("tab_results")

# Print the table
print(tab_results)