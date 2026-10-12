################################################################################
### R functions developped to process xsa outputs
################################################################################

# NAME: totalStk
# DOES: Adds up totals for stock objects - computes landings, discards, catch, stock totals and sets units
totalStk <- function(stk, Units){
  landings(stk) <- computeLandings(stk)
  discards(stk) <- computeDiscards(stk)
  catch.n(stk)  <- landings.n(stk) + discards.n(stk)
  catch.wt(stk) <- (landings.n(stk) * landings.wt(stk) + discards.n(stk) * discards.wt(stk))/catch.n(stk)
  catch(stk)    <- computeCatch(stk)
  stock(stk)    <- computeStock(stk)
  units(stk)[1:17] <- as.list(c(rep(Units,4), "NA", "NA", "f", "NA", "NA"))
  return(stk)
}

## plot observational variance - observation error
sam_observation_error <- function(fit, print_plot = FALSE){
  cf <- fit$conf$keyVarObs
  fn <- attr(fit$data, "fleetNames")
  ages <- fit$conf$minAge:fit$conf$maxAge
  pt <- partable(fit)
  sd <- unname(exp(pt[grep("logSdLogObs",rownames(pt)),1]))
  v <- cf
  v[] <- c(NA,sd)[cf + 2]
  res <- data.frame(fleet = fn[as.vector(row(v))],
                    name = paste0(fn[as.vector(row(v))]," age ",ages[as.vector(col(v))]),
                    sd = as.vector(v))
  res <- res[complete.cases(res),]
  o <- order(res$sd)
  res <- res[o,]
  if(print_plot == TRUE) {
    par(mar = c(13,6,2,1))
    barplot(res$sd,
            names.arg = res$name,
            las = 2,
            col = colors()[as.integer(as.factor(res$fleet))*10],
            ylab = "SD");  box()
  }
  return(res)
}

###-----------------------------------------------------------------------------
### extract stf data and store in a data frame for advice table
extract_sam_stf_for_advice_table <- function(sam_stf_output,
                                             TAC_ref,
                                             Advice_ref,
                                             SSB_ref,
                                             col_names = NULL,
                                             Blim = 7470){




  if (!is.null(sam_stf_output)) {

    df_sam_stf_output <- attributes(sam_stf_output)$shorttab %>%
      as.data.frame() %>%
      rownames_to_column(var = "Variable") %>%
      mutate(Variable = str_split(Variable, ":") %>%
               sapply(., function(x) x[2])) %>%
      pivot_longer(-Variable, names_to = "Year")

    df_stf_output <- data.frame(Basis = attributes(sam_stf_output)$label,
                                Total_catch = df_sam_stf_output %>%
                                  filter(Variable == "catch", Year == (assessment_year + 1)) %>%
                                  pull(value),
                                Proj_landings = df_sam_stf_output %>%
                                  filter(Variable == "Land", Year == (assessment_year + 1)) %>%
                                  pull(value),
                                Proj_discards = df_sam_stf_output %>%
                                  filter(Variable == "Discard", Year == (assessment_year + 1)) %>%
                                  pull(value),
                                F_proj_landings = df_sam_stf_output %>%
                                  filter(Variable == "fbar", Year == (assessment_year + 1)) %>%
                                  pull(value),
                                SSB = df_sam_stf_output %>%
                                  filter(Variable == "ssb", Year == (assessment_year + 2)) %>%
                                  pull(value),
                                SSB_change = -(1 - df_sam_stf_output %>%
                                                 filter(Variable == "ssb", Year == (assessment_year + 2)) %>%
                                                 pull(value)/SSB_ref) * 100,
                                TAC_change = -(1 - df_sam_stf_output %>%
                                                 filter(Variable == "catch", Year == (assessment_year + 1)) %>%
                                                 pull(value)/TAC_ref) * 100,
                                Advice_change = -(1 - df_sam_stf_output %>%
                                                    filter(Variable == "catch", Year == (assessment_year + 1)) %>%
                                                    pull(value)/Advice_ref) * 100,
                                Proba_Blim = sum(sam_stf_output[[4]]$ssb < Blim, na.rm = TRUE) /
                                  length(sam_stf_output[[4]]$ssb))
  } else{
    df_stf_output <- data.frame(Basis = scenario_name,
                                Total_catch = NA,
                                Proj_landings = NA,
                                Proj_discards = NA,
                                F_proj_landings = NA,
                                SSB = NA,
                                SSB_change = NA,
                                TAC_change = NA,
                                Advice_change = NA,
                                Proba_Blim = NA)
  }

  if (!is.null(col_names)) {
    colnames(df_stf_output) <- col_names
  }

  return(df_stf_output)
}

###-----------------------------------------------------------------------------
### extract stf data and store in a data frame for advice table
extract_sam_stf_for_advice_comparison <- function(sam_stf_output,
                                                  assessment_year) {

  if (!is.null(sam_stf_output)) {

    df_sam_stf_output <- attributes(sam_stf_output)$shorttab %>%
      as.data.frame() %>%
      tibble::rownames_to_column(var = "Variable") %>%
      dplyr::mutate(Variable = str_split(Variable, ":") %>%
               sapply(., function(x) x[2])) %>%
      tidyr::pivot_longer(-Variable, names_to = "Year", values_to = "Value") %>%
      dplyr::mutate(Source = glue("WGBIE-{assessment_year}"),
             Year = as.numeric(Year),
             Type = dplyr::case_when(Year == assessment_year ~ "Intermediate year",
                              Year == (assessment_year -1) ~ "Data year",
                              Year == (assessment_year + 1) ~ "Advice year",
                              Year == (assessment_year + 2) ~ "Terminal year"),
             Variable = dplyr::case_when(Variable == "rec" ~ "Recruitment",
                                  Variable == "ssb" ~ "SSB",
                                  Variable == "catch" ~ "Total catch",
                                  Variable == "fbar" ~ "Fbar")) %>%
      dplyr::filter(Variable %in% c("Recruitment", "SSB", "Total catch", "Fbar"))

    return(df_sam_stf_output)
  }
}

