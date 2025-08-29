# Refactored R script to identify episodes of economic transformation
#
# This script adapts the methodology from the V-Dem Episodes of Regime Transformation
# to identify episodes of "privatization" and "statization" based on the
# `v2clstown_osp` variable, which measures state ownership and control.
#
# The original script's political regime categories (democracy/autocracy) are replaced
# with a four-scale economic categorization system.
#
# Categories are based on `v2clstown_osp` values:
# - Market Economy (0 to 1)
# - Regulated Market Economy (1 to 2)
# - State Market Economy (2 to 3)
# - Planned Economy (3 to 4)
#' @import dplyr
#' @import stringr
#' @import tidyr
#' @import Rcpp
#' @importFrom hablar s
#' @importFrom plm make.pconsecutive
#' @export
#'
#' @examples
#' #Don't run
#' #Get the episodes with standard parameters:
#' #episodes <- get_eps()

get_eps <- function(data = ESOT::vdem,
                    start_incl = 0.04,
                    cum_incl = 0.4,
                    year_turn = 0.12,
                    cum_turn = 0.4,
                    tolerance = 5)
{
  
  if(year_turn == 0)
    print("You set year_turn = 0. Did you mean to do this? See p.3 of the ESOT codebook.")
  
 
  ### DATA CLEANING AND PREP ###
  
  full.df <- data %>%
    dplyr::select(country_name, country_id, country_text_id, year,
                  v2clstown_osp, codingstart, codingend, matches("v2clstown_osp", ignore.case = FALSE),
                  gapstart1, gapstart2, gapstart3, gapend1, gapend2, gapend3) %>%
    dplyr::filter(year >= 1900) %>%
    dplyr::arrange(country_text_id, year) %>%
    dplyr::group_by(country_id) %>%
    dplyr::mutate(origsample = 1) %>%
    plm::make.pconsecutive(balanced = TRUE, index = c("country_id", "year")) %>%
    dplyr::group_by(country_id) %>%
    tidyr::fill(c(country_text_id, country_name, codingend, gapstart1, gapend1, gapstart2, gapend2,
                  gapstart3, gapend3)) %>%
    tidyr::fill(c(country_text_id, country_name,codingend, gapstart1, gapend1, gapstart2, gapend2,
                  gapstart3, gapend3), .direction = "up")  %>%
    dplyr::mutate(gapstart = ifelse(year <= gapend1, gapstart1, NA),
                  gapend = ifelse(year <= gapend1, gapend1, NA),
                  gapstart = ifelse(!is.na(gapend2) & year > gapend1 & year <= gapend2, gapstart2, gapstart),
                  gapend = ifelse(!is.na(gapend2) & year > gapend1 & year <= gapend2, gapend2, gapend),
                  gapstart = ifelse(!is.na(gapend3) & year > gapend2 & year <= gapend3, gapstart3, gapstart),
                  gapend = ifelse(!is.na(gapend3) & year > gapend2 & year <= gapend3, gapend3, gapend)) %>%
    
    
    ### CODING THE ECONOMIC REGIME TYPE VARIABLES ###
    
    dplyr::arrange(country_id, year) %>%
    dplyr::mutate(
      # Code the four economic regime types based on v2clstown_osp
      eco_reg_type = case_when(
        v2clstown_osp >= 0 & v2clstown_osp <= 1 ~ 1, # Market Economy
        v2clstown_osp > 1 & v2clstown_osp <= 2 ~ 2, # Regulated Market Economy
        v2clstown_osp > 2 & v2clstown_osp <= 3 ~ 3, # State Market Economy
        v2clstown_osp > 3 & v2clstown_osp <= 4 ~ 4, # Planned Economy
        TRUE ~ NA_real_
      ),
      # Code the transitions between economic regime types
      eco_reg_trans = eco_reg_type - dplyr::lag(eco_reg_type, n = 1),
      # Identify the year of the most recent economic regime change
      eco_regch_year = ifelse(eco_reg_trans != 0, year, NA),
      # Fill the regime change variable for the entire period
      eco_regch_filled = ifelse(!is.na(eco_regch_year), eco_reg_trans, NA)
    ) %>%
    tidyr::fill(c(eco_regch_filled, eco_regch_year)) %>%
    dplyr::mutate(
      eco_regch_filled = ifelse(
        !is.na(eco_regch_year) & ((!is.na(gapend1) & eco_regch_year < gapstart1 & year >= gapstart1) |
                                  (!is.na(gapend2) & eco_regch_year < gapstart2 & year >= gapstart2) |
                                  (!is.na(gapend3) & eco_regch_year < gapstart3 & year >= gapstart3)),
        NA, eco_regch_filled
      ),
      eco_regch_year = ifelse(is.na(eco_regch_filled), NA, eco_regch_year),
      # Check for censored outcomes near end of coding or gaps
      eco_regch_censored = ifelse(codingend - eco_regch_year < tolerance, 1, 0),
      eco_regch_censored = ifelse(!is.na(gapstart) & gapstart - eco_regch_year < tolerance, 1, eco_regch_censored)
    ) %>%
    ungroup() %>%
    group_by(country_id) %>%
    arrange(country_id, year) %>%
    dplyr::mutate(
      eco_start_year = ifelse(!is.na(eco_reg_trans) & eco_reg_trans != 0, year, NA),
      eco_start_year = ifelse(year == min(hablar::s(ifelse(!is.na(eco_reg_type), year, NA))), year, eco_start_year),
      eco_start_year = ifelse(!is.na(gapend1) & year == gapend1 + 1, year, eco_start_year),
      eco_start_year = ifelse(!is.na(gapend2) & year == gapend2 + 1, year, eco_start_year),
      eco_start_year = ifelse(!is.na(gapend3) & year == gapend3 + 1, year, eco_start_year)
    ) %>%
    tidyr::fill(eco_start_year) %>%
    dplyr::mutate(
      eco_start_year = ifelse(!is.na(eco_start_year) & ((!is.na(gapend1) & eco_start_year < gapstart1 & year >= gapstart1) |
                                                          (!is.na(gapend2) & eco_start_year < gapstart2 & year >= gapstart2) |
                                                          (!is.na(gapend3) & eco_start_year < gapstart3 & year >= gapstart3)),
                              NA, eco_start_year)
    ) %>%
    ungroup() %>%
    group_by(country_id, eco_start_year) %>%
    dplyr::mutate(
      eco_end_year = dplyr::last(year),
      eco_id = ifelse(!is.na(eco_start_year), paste(country_text_id, eco_start_year, eco_end_year, sep = "_"), NA)
    ) %>%
    ungroup() %>%
    arrange(country_text_id, year) %>%
    group_by(country_text_id) %>%
    
    
    ### CODING THE PRIVATIZATION EPISODES ###
  
    dplyr::mutate(episode_id = find_seqs_pri(v2clstown_osp, eco_reg_type, eco_reg_trans,
                                             start_incl, year_turn = year_turn * -1, cum_turn = cum_turn * -1,
                                             tolerance),
                  character_id = ifelse(!is.na(episode_id), paste(country_text_id, episode_id, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(character_id) %>%
    dplyr::mutate(priv_ep = ifelse(!is.na(episode_id), 1, 0),
                  priv_ep = ifelse(priv_ep==1 & max(v2clstown_osp, na.rm = T) - min(v2clstown_osp, na.rm = T) >= cum_incl, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(episode_id = ifelse(priv_ep!=1, NA, episode_id),
                  character_id = ifelse(priv_ep!=1, NA, character_id)) %>%
    dplyr::group_by(character_id) %>%
    dplyr::mutate(priv_ep_end_year = ifelse(priv_ep==1, last(year), NA),
                  priv_ep_uncertain = ifelse(priv_ep==1 & codingend-priv_ep_end_year<tolerance, 1, 0),
                  priv_ep_uncertain = ifelse(priv_ep==1 & !is.na(gapstart) & (gapstart-1)-priv_ep_end_year<tolerance, 1, priv_ep_uncertain),
                  priv_ep_start_year = ifelse(priv_ep==1,first(year)+1, NA),
                  priv_pre_ep_year = ifelse(priv_ep==1, ifelse(year == dplyr::first(year), 1, 0), 0),
                  priv_ep_id = ifelse(priv_ep==1, paste(country_text_id, priv_ep_start_year, priv_ep_end_year, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-character_id, -episode_id) %>%
    dplyr::arrange(country_name, year) %>%
    as.data.frame %>%
    dplyr::group_by(priv_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2clstown_osp-dplyr::lag(v2clstown_osp, n=1)>=start_incl, year, NA))),
                  last_ch_year = ifelse(priv_ep==0, NA, last_ch_year)) %>%
    
    dplyr::group_by(country_id) %>%
    dplyr::arrange(country_id, year) %>%
    
    dplyr::ungroup() %>%
    dplyr::group_by(priv_ep_id) %>%
    dplyr::mutate(
      breakdown = ifelse(priv_ep == 1 & year >= priv_ep_start_year,
                         ifelse(dplyr::lead(eco_reg_trans == -1), year, NA), NA),
      breakdown = min(hablar::s(breakdown)),
      priv_ep_end_year = ifelse(!is.na(breakdown) & priv_ep_end_year > breakdown, breakdown, priv_ep_end_year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      priv_ep_start_year = ifelse(priv_ep == 1 & year > priv_ep_end_year, NA, priv_ep_start_year),
      priv_ep_end_year = ifelse(priv_ep == 1 & year > priv_ep_end_year, NA, priv_ep_end_year),
      priv_ep = ifelse(priv_ep == 1 & year > priv_ep_end_year, 0, priv_ep),
      priv_ep_id = ifelse(priv_ep == 1, paste(country_text_id, priv_ep_start_year, priv_ep_end_year, sep = "_"), NA)) %>%
    dplyr::group_by(priv_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(v2clstown_osp - dplyr::lag(v2clstown_osp, n = 1) >= start_incl, year, NA))),
                  last_ch_year = ifelse(priv_ep == 0, NA, last_ch_year)) %>%
    
    dplyr::group_by(country_id) %>%
    dplyr::arrange(country_id, year)
  
  # ... rest of the code for termination types ...
  # (logic for stasis, year_drop, cum_drop is the same, but applied to the new episode types)
  
  # New outcome coding for privatization episodes
  full.df <- full.df %>%
    dplyr::group_by(priv_ep_id) %>%
    dplyr::mutate(
      priv_ep_outcome = case_when(
        priv_ep_uncertain == 1 ~ 6, # Outcome censored
        # Market transition
        priv_ep_termination == 4 & min(eco_reg_type, na.rm = TRUE) %in% c(3, 4) & max(eco_reg_type, na.rm = TRUE) %in% c(1, 2) ~ 1,
        # Preempted Market transition
        priv_ep_termination == 4 & min(eco_reg_type, na.rm = TRUE) %in% c(3, 4) & max(eco_reg_type, na.rm = TRUE) %in% c(3, 4) ~ 2,
        # Stabilized Planned Economy
        priv_ep_termination == 1 & min(eco_reg_type, na.rm = TRUE) %in% c(3, 4) ~ 3,
        # Reverted privatization
        priv_ep_termination %in% c(2, 3) & min(eco_reg_type, na.rm = TRUE) %in% c(3, 4) ~ 4,
        # Deepened Market Economy
        priv_ep_termination == 1 & min(eco_reg_type, na.rm = TRUE) %in% c(1, 2) ~ 5,
        TRUE ~ NA_real_
      )
    )
  
  ### CODING THE STATIZATION EPISODES ###
  
  full.df <- full.df %>%
    dplyr::group_by(country_text_id) %>%
    dplyr::mutate(episode_id = find_seqs_sta(v2clstown_osp, eco_reg_type, eco_reg_trans,
                                             start_incl, year_turn, cum_turn,
                                             tolerance),
                  character_id = ifelse(!is.na(episode_id), paste(country_text_id, episode_id, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(character_id) %>%
    dplyr::mutate(stat_ep = ifelse(!is.na(episode_id), 1, 0),
                  stat_ep = ifelse(stat_ep == 1 & max(v2clstown_osp, na.rm = T) - min(v2clstown_osp, na.rm = T) >= cum_incl, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(episode_id = ifelse(stat_ep != 1, NA, episode_id),
                  character_id = ifelse(stat_ep != 1, NA, character_id)) %>%
    dplyr::group_by(character_id) %>%
    dplyr::mutate(stat_ep_end_year = ifelse(stat_ep == 1, last(year), NA),
                  stat_ep_uncertain = ifelse(stat_ep == 1 & codingend - stat_ep_end_year < tolerance, 1, 0),
                  stat_ep_uncertain = ifelse(stat_ep == 1 & !is.na(gapstart) & (gapstart - 1) - stat_ep_end_year < tolerance, 1, stat_ep_uncertain),
                  stat_ep_start_year = ifelse(stat_ep == 1, first(year) + 1, NA),
                  stat_pre_ep_year = ifelse(stat_ep == 1, ifelse(year == dplyr::first(year), 1, 0), 0),
                  stat_ep_id = ifelse(stat_ep == 1, paste(country_text_id, stat_ep_start_year, stat_ep_end_year, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-character_id, -episode_id) %>%
    dplyr::arrange(country_name, year) %>%
    as.data.frame %>%
    
    # ... rest of the code for termination types ...
    # (logic for stasis, year_drop, cum_drop is the same, but applied to the new episode types)
    
    # New outcome coding for statization episodes
    dplyr::group_by(stat_ep_id) %>%
    dplyr::mutate(
      stat_ep_outcome = case_when(
        stat_ep_uncertain == 1 ~ 6, # Outcome censored
        # Market breakdown
        stat_ep_termination == 4 & min(eco_reg_type, na.rm = TRUE) %in% c(1, 2) & max(eco_reg_type, na.rm = TRUE) %in% c(3, 4) ~ 1,
        # Preempted Market breakdown
        stat_ep_termination == 4 & min(eco_reg_type, na.rm = TRUE) %in% c(1, 2) & max(eco_reg_type, na.rm = TRUE) %in% c(1, 2) ~ 2,
        # Diminished Market Economy
        stat_ep_termination == 1 & min(eco_reg_type, na.rm = TRUE) %in% c(1, 2) ~ 4,
        # Averted Statization
        stat_ep_termination %in% c(2, 3) & min(eco_reg_type, na.rm = TRUE) %in% c(1, 2) ~ 4,
        # Deepened Planned Economy
        stat_ep_termination == 1 & min(eco_reg_type, na.rm = TRUE) %in% c(3, 4) ~ 5,
        TRUE ~ NA_real_
      )
    ) %>%
    
    # ... final select and return statement ...
    dplyr::ungroup() %>%
    dplyr::group_by(country_id, year) %>%
    dplyr::mutate(
      priv_ep_id = ifelse(
        !is.na(priv_ep_id) & (priv_ep_outcome == 5 & dplyr::lag(priv_ep_outcome == 5)),
        NA, priv_ep_id
      ),
      stat_ep_id = ifelse(
        !is.na(stat_ep_id) & (stat_ep_outcome == 5 & dplyr::lag(stat_ep_outcome == 5)),
        NA, stat_ep_id
      ),
      priv_ep_id = ifelse(
        !is.na(priv_ep_id) & (priv_ep_outcome == 1 & dplyr::lag(priv_ep_outcome == 1)),
        NA, priv_ep_id
      ),
      stat_ep_id = ifelse(
        !is.na(stat_ep_id) & (stat_ep_outcome == 1 & dplyr::lag(stat_ep_outcome == 1)),
        NA, stat_ep_id
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(
      last_ch_year, breakdown,
      gapstart1, gapstart2, gapstart3, gapend1, gapend2, gapend3,
      gapstart, gapend, eco_regch_filled,
      eco_start_year, eco_end_year, eco_id,
      episode_id, character_id, priv_ep, stat_ep,
      priv_ep_uncertain, stat_ep_uncertain,
      priv_ep_termination, stat_ep_termination,
      priv_pre_ep_year, stat_pre_ep_year,
      `v2clstown_osp_nr`:`v2clstown_osp_cyear`
    )) %>%
    dplyr::filter(origsample == 1)
  
  return(full.df)
}