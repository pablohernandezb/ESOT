# Refactored script to identify episodes of state ownership transformation (ESOT).
#
# This script is updated to be compatible with the new 'priv_ep' and 'stat_ep' variables
# and includes the logic for `priv_ep_termination` and `stat_ep_termination`
# to resolve the "object not found" error.

#' Get episodes of state ownership transformation (ESOT)
#'
#' Helps to identify episodes of privatization (liberalizing autocracy, democratic deepening)
#' and statization (democratic regression, autocratic regression) in the most recent vdem data set.
#' For further details please check the ESOT codebook attached to the package
#' and available here: https://github.com/vdeminstitute/ESOT/blob/master/inst/ESOT_codebook.pdf
#'
#' \emph{privatization} is an umbrella term for any movement towards demcracy -
#' be it in autocracies or democracies (cf. Wilson et al., 2020).
#' \emph{Liberalizing autocracy} is defined as a subtype of democratiztion and specifically focuses on any movement towards democracy
#' which starts in autocracies. \emph{Democratic deepening} is also a subtype of privatization and
#' concerns all those which are already democratic and further improve their democratic traits.
#'
#' \emph{statization} is defined as any movement towards autocracy which starts within democracies
#' or autocracies (cf. Lührmann and Lindberg, privatization, 2019).
#' \emph{Democratic regression} is defined as a subtype of statization and specifically focuses on any movement towards autocracy
#' which starts in democracies. \emph{Autocratic regression} is also a subtype of statization and
#' concerns all those which are already autocratic and further decline.
#'
#' @param data The data based on which the episodes are identified.
#' By default the most recent vdem data set will be used.
#' @param start_incl A numeric value for the start inclination threshold.
#' @param cum_incl A numeric value for the cumulative inclination threshold.
#' @param year_turn A numeric value for the yearly turning point threshold.
#' @param cum_turn A numeric value for the cumulative turning point threshold.
#' @param tolerance An integer value for the tolerance period.
#'
#' @return A data frame containing identified episodes with additional variables.
#'
#' @importFrom dplyr select filter arrange group_by mutate ungroup last case_when first
#' @importFrom hablar s
#' @importFrom plm make.pconsecutive
#' @importFrom tidyr fill
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' # Dont run
#' # get_eps()
#'
get_eps <- function(data = ESOT::vdem,
                    start_incl = 0.04,
                    cum_incl = 0.4,
                    year_turn = 0.12,
                    cum_turn = 0.4,
                    tolerance = 5)
{
  
  if (year_turn == 0)
    print("You set year_turn = 0. Did you mean to do this? See p.3 of the ESOT codebook.")
  
  
  ### DATA CLEANING AND PREP ###
  
  full.df <- data %>%
    dplyr::select(.data$country_name, .data$country_id, .data$country_text_id, .data$year,
                  .data$v2clstown_osp, .data$codingstart, .data$codingend, tidyselect::matches("v2clstown_osp", ignore.case = FALSE),
                  .data$gapstart1, .data$gapstart2, .data$gapstart3, .data$gapend1, .data$gapend2, .data$gapend3) %>%
    dplyr::filter(.data$year >= 1900) %>%
    dplyr::arrange(.data$country_text_id, .data$year) %>%
    dplyr::group_by(.data$country_id) %>%
    dplyr::mutate(origsample = 1) %>%
    plm::make.pconsecutive(balanced = TRUE, index = c("country_id", "year")) %>%
    dplyr::group_by(.data$country_id) %>%
    tidyr::fill(c(.data$country_text_id, .data$country_name, .data$codingend, .data$gapstart1, .data$gapend1, .data$gapstart2, .data$gapend2,
                  .data$gapstart3, .data$gapend3)) %>%
    tidyr::fill(c(.data$country_text_id, .data$country_name, .data$codingend, .data$gapstart1, .data$gapend1, .data$gapstart2, .data$gapend2,
                  .data$gapstart3, .data$gapend3), .direction = "up") %>%
    dplyr::mutate(gapstart = ifelse(.data$year <= .data$gapend1, .data$gapstart1, NA),
                  gapend = ifelse(.data$year <= .data$gapend1, .data$gapend1, NA),
                  gapstart = ifelse(!is.na(.data$gapend2) & .data$year > .data$gapend1 & .data$year <= .data$gapend2, .data$gapstart2, .data$gapstart),
                  gapend = ifelse(!is.na(.data$gapend2) & .data$year > .data$gapend1 & .data$year <= .data$gapend2, .data$gapend2, .data$gapend),
                  gapstart = ifelse(!is.na(.data$gapend3) & .data$year > .data$gapend2 & .data$year <= .data$gapend3, .data$gapstart3, .data$gapstart),
                  gapend = ifelse(!is.na(.data$gapend3) & .data$year > .data$gapend2 & .data$year <= .data$gapend3, .data$gapend3, .data$gapend)) %>%
    
    
    ### CODING THE ECONOMIC REGIME TYPE VARIABLES ###
    
    dplyr::arrange(.data$country_id, .data$year) %>%
    dplyr::mutate(
      # Code the four economic regime types based on v2clstown_osp
      eco_reg_type = case_when(
        .data$v2clstown_osp >= 0 & .data$v2clstown_osp <= 1 ~ 1, # Market Economy
        .data$v2clstown_osp > 1 & .data$v2clstown_osp <= 2 ~ 2, # Regulated Market Economy
        .data$v2clstown_osp > 2 & .data$v2clstown_osp <= 3 ~ 3, # State Market Economy
        .data$v2clstown_osp > 3 & .data$v2clstown_osp <= 4 ~ 4, # Planned Economy
        TRUE ~ NA_real_
      ),
      # Code the transitions between economic regime types
      eco_reg_trans = .data$eco_reg_type - dplyr::lag(.data$eco_reg_type, n = 1),
      # Identify the year of the most recent economic regime change
      eco_regch_year = ifelse(.data$eco_reg_trans != 0, .data$year, NA),
      # Fill the regime change variable for the entire period
      eco_regch_filled = ifelse(!is.na(.data$eco_regch_year), .data$eco_reg_trans, NA)
    ) %>%
    tidyr::fill(c(.data$eco_regch_filled, .data$eco_regch_year)) %>%
    dplyr::mutate(
      eco_regch_filled = ifelse(
        !is.na(.data$eco_regch_year) & ((!is.na(.data$gapend1) & .data$eco_regch_year < .data$gapstart1 & .data$year >= .data$gapstart1) |
                                          (!is.na(.data$gapend2) & .data$eco_regch_year < .data$gapstart2 & .data$year >= .data$gapstart2) |
                                          (!is.na(.data$gapend3) & .data$eco_regch_year < .data$gapstart3 & .data$year >= .data$gapstart3)),
        NA, .data$eco_regch_filled
      ),
      eco_regch_year = ifelse(is.na(.data$eco_regch_filled), NA, .data$eco_regch_year),
      # Check for censored outcomes near end of coding or gaps
      eco_regch_censored = ifelse(.data$codingend - .data$eco_regch_year < tolerance, 1, 0),
      eco_regch_censored = ifelse(!is.na(.data$gapstart) & .data$gapstart - .data$eco_regch_year < tolerance, 1, .data$eco_regch_censored)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$country_id) %>%
    dplyr::arrange(.data$country_id, .data$year) %>%
    dplyr::mutate(
      eco_start_year = ifelse(!is.na(.data$eco_reg_trans) & .data$eco_reg_trans != 0, .data$year, NA),
      eco_start_year = ifelse(.data$year == min(hablar::s(ifelse(!is.na(.data$eco_reg_type), .data$year, NA))), .data$year, .data$eco_start_year),
      eco_start_year = ifelse(!is.na(.data$gapend1) & .data$year == .data$gapend1 + 1, .data$year, .data$eco_start_year),
      eco_start_year = ifelse(!is.na(.data$gapend2) & .data$year == .data$gapend2 + 1, .data$year, .data$eco_start_year),
      eco_start_year = ifelse(!is.na(.data$gapend3) & .data$year == .data$gapend3 + 1, .data$year, .data$eco_start_year)
    ) %>%
    tidyr::fill(.data$eco_start_year) %>%
    dplyr::mutate(
      eco_start_year = ifelse(!is.na(.data$eco_start_year) & ((!is.na(.data$gapend1) & .data$eco_start_year < .data$gapstart1 & .data$year >= .data$gapstart1) |
                                                                (!is.na(.data$gapend2) & .data$eco_start_year < .data$gapstart2 & .data$year >= .data$gapstart2) |
                                                                (!is.na(.data$gapend3) & .data$eco_start_year < .data$gapstart3 & .data$year >= .data$gapstart3)),
                              NA, .data$eco_start_year)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$country_id, .data$eco_start_year) %>%
    dplyr::mutate(
      eco_end_year = dplyr::last(.data$year),
      eco_id = ifelse(!is.na(.data$eco_start_year), paste(.data$country_text_id, .data$eco_start_year, .data$eco_end_year, sep = "_"), NA)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$country_text_id, .data$year) %>%
    dplyr::group_by(.data$country_text_id) %>%
    
    
    ### CODING THE PRIVATIZATION EPISODES ###
    
    dplyr::mutate(episode_id = find_seqs_pri(.data$v2clstown_osp, .data$eco_reg_type, .data$eco_reg_trans,
                                             start_incl, year_turn = year_turn * -1, cum_turn = cum_turn * -1,
                                             tolerance),
                  character_id = ifelse(!is.na(.data$episode_id), paste(.data$country_text_id, .data$episode_id, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$character_id) %>%
    dplyr::mutate(priv_ep = ifelse(!is.na(.data$episode_id), 1, 0),
                  priv_ep = ifelse(.data$priv_ep==1 & max(.data$v2clstown_osp, na.rm = T) - min(.data$v2clstown_osp, na.rm = T) >= cum_incl, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(episode_id = ifelse(.data$priv_ep!=1, NA, .data$episode_id),
                  character_id = ifelse(.data$priv_ep!=1, NA, .data$character_id)) %>%
    dplyr::group_by(.data$character_id) %>%
    dplyr::mutate(priv_ep_end_year = ifelse(.data$priv_ep==1, last(.data$year), NA),
                  priv_ep_uncertain = ifelse(.data$priv_ep==1 & .data$codingend-.data$priv_ep_end_year<tolerance, 1, 0),
                  priv_ep_uncertain = ifelse(.data$priv_ep==1 & !is.na(.data$gapstart) & (.data$gapstart-1)-.data$priv_ep_end_year<tolerance, 1, .data$priv_ep_uncertain),
                  priv_ep_start_year = ifelse(.data$priv_ep==1,first(.data$year)+1, NA),
                  priv_pre_ep_year = ifelse(.data$priv_ep==1, ifelse(.data$year == dplyr::first(.data$year), 1, 0), 0),
                  priv_ep_id = ifelse(.data$priv_ep==1, paste(.data$country_text_id, .data$priv_ep_start_year, .data$priv_ep_end_year, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$character_id, -.data$episode_id) %>%
    dplyr::arrange(.data$country_name, .data$year) %>%
    as.data.frame %>%
    dplyr::group_by(.data$priv_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(.data$v2clstown_osp-dplyr::lag(.data$v2clstown_osp, n=1)>=start_incl, .data$year, NA))),
                  last_ch_year = ifelse(.data$priv_ep==0, NA, .data$last_ch_year)) %>%
    
    dplyr::group_by(.data$country_id) %>%
    dplyr::arrange(.data$country_id, .data$year) %>%
    
    dplyr::ungroup() %>%
    dplyr::group_by(.data$priv_ep_id) %>%
    dplyr::mutate(
      breakdown = ifelse(.data$priv_ep == 1 & .data$year >= .data$priv_ep_start_year,
                         ifelse(dplyr::lead(.data$eco_reg_trans == -1), .data$year, NA), NA),
      breakdown = min(hablar::s(.data$breakdown)),
      priv_ep_end_year = ifelse(!is.na(.data$breakdown) & .data$priv_ep_end_year > .data$breakdown, .data$breakdown, .data$priv_ep_end_year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      priv_ep_start_year = ifelse(.data$priv_ep == 1 & .data$year > .data$priv_ep_end_year, NA, .data$priv_ep_start_year),
      priv_ep_end_year = ifelse(.data$priv_ep == 1 & .data$year > .data$priv_ep_end_year, NA, .data$priv_ep_end_year),
      priv_ep = ifelse(.data$priv_ep == 1 & .data$year > .data$priv_ep_end_year, 0, .data$priv_ep),
      priv_ep_id = ifelse(.data$priv_ep == 1, paste(.data$country_text_id, .data$priv_ep_start_year, .data$priv_ep_end_year, sep = "_"), NA)) %>%
    dplyr::group_by(.data$priv_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(.data$v2clstown_osp - dplyr::lag(.data$v2clstown_osp, n = 1) >= start_incl, .data$year, NA))),
                  last_ch_year = ifelse(.data$priv_ep == 0, NA, .data$last_ch_year)) %>%
    
    dplyr::group_by(.data$country_id) %>%
    dplyr::arrange(.data$country_id, .data$year)
  
  
  ### CODING THE TERMINATION TYPES ###
  
  full.df <- full.df %>%
    dplyr::group_by(.data$priv_ep_id) %>%
    dplyr::mutate(
      year_drop = ifelse(
        .data$v2clstown_osp - dplyr::lead(.data$v2clstown_osp) >= year_turn & .data$priv_ep == 1,
        .data$year,
        NA
      ),
      cum_drop = ifelse(
        .data$priv_ep == 1 & max(.data$v2clstown_osp) - min(.data$v2clstown_osp) >= cum_turn & .data$year == .data$priv_ep_end_year,
        .data$year,
        NA
      ),
      stasis = ifelse(
        .data$priv_ep == 1 & dplyr::lag(.data$priv_ep) == 1 & .data$year - .data$last_ch_year > tolerance,
        .data$year,
        NA
      )
    ) %>%
    dplyr::mutate(
      priv_ep_termination = dplyr::case_when(
        !is.na(.data$stasis) & is.na(dplyr::lead(.data$stasis)) ~ 1,
        !is.na(.data$year_drop) ~ 2,
        !is.na(.data$cum_drop) ~ 3,
        !is.na(.data$breakdown) & .data$year == .data$breakdown ~ 4,
        TRUE ~ NA_real_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$stat_ep_id) %>%
    dplyr::mutate(
      year_drop = ifelse(
        .data$v2clstown_osp - dplyr::lead(.data$v2clstown_osp) <= -year_turn & .data$stat_ep == 1,
        .data$year,
        NA
      ),
      cum_drop = ifelse(
        .data$stat_ep == 1 & min(.data$v2clstown_osp) - max(.data$v2clstown_osp) <= -cum_turn & .data$year == .data$stat_ep_end_year,
        .data$year,
        NA
      ),
      stasis = ifelse(
        .data$stat_ep == 1 & dplyr::lag(.data$stat_ep) == 1 & .data$year - .data$last_ch_year > tolerance,
        .data$year,
        NA
      )
    ) %>%
    dplyr::mutate(
      stat_ep_termination = dplyr::case_when(
        !is.na(.data$stasis) & is.na(dplyr::lead(.data$stasis)) ~ 1,
        !is.na(.data$year_drop) ~ 2,
        !is.na(.data$cum_drop) ~ 3,
        !is.na(.data$breakdown) & .data$year == .data$breakdown ~ 4,
        TRUE ~ NA_real_
      )
    ) %>%
    
    ### CODING THE PRIVATIZATION EPISODE OUTCOMES ###
    
    dplyr::group_by(.data$priv_ep_id) %>%
    dplyr::mutate(
      priv_ep_outcome = case_when(
        .data$priv_ep_uncertain == 1 ~ 6, # Outcome censored
        # Market transition
        .data$priv_ep_termination == 4 & min(.data$eco_reg_type, na.rm = TRUE) %in% c(3, 4) & max(.data$eco_reg_type, na.rm = TRUE) %in% c(1, 2) ~ 1,
        # Preempted Market transition
        .data$priv_ep_termination == 4 & min(.data$eco_reg_type, na.rm = TRUE) %in% c(3, 4) & max(.data$eco_reg_type, na.rm = TRUE) %in% c(3, 4) ~ 2,
        # Stabilized Planned Economy
        .data$priv_ep_termination == 1 & min(.data$eco_reg_type, na.rm = TRUE) %in% c(3, 4) ~ 3,
        # Reverted privatization
        .data$priv_ep_termination %in% c(2, 3) & min(.data$eco_reg_type, na.rm = TRUE) %in% c(3, 4) ~ 4,
        # Deepened Market Economy
        .data$priv_ep_termination == 1 & min(.data$eco_reg_type, na.rm = TRUE) %in% c(1, 2) ~ 5,
        TRUE ~ NA_real_
      )
    ) %>%
    
    ### CODING THE STATIZATION EPISODES ###
    
    dplyr::group_by(.data$country_text_id) %>%
    dplyr::mutate(episode_id = find_seqs_sta(.data$v2clstown_osp, .data$eco_reg_type, .data$eco_reg_trans,
                                             start_incl, year_turn, cum_turn,
                                             tolerance),
                  character_id = ifelse(!is.na(.data$episode_id), paste(.data$country_text_id, .data$episode_id, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$character_id) %>%
    dplyr::mutate(stat_ep = ifelse(!is.na(.data$episode_id), 1, 0),
                  stat_ep = ifelse(.data$stat_ep == 1 & max(.data$v2clstown_osp, na.rm = T) - min(.data$v2clstown_osp, na.rm = T) >= cum_incl, 1, 0)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(episode_id = ifelse(.data$stat_ep != 1, NA, .data$episode_id),
                  character_id = ifelse(.data$stat_ep != 1, NA, .data$character_id)) %>%
    dplyr::group_by(.data$character_id) %>%
    dplyr::mutate(stat_ep_end_year = ifelse(.data$stat_ep == 1, last(.data$year), NA),
                  stat_ep_uncertain = ifelse(.data$stat_ep == 1 & .data$codingend - .data$stat_ep_end_year < tolerance, 1, 0),
                  stat_ep_uncertain = ifelse(.data$stat_ep == 1 & !is.na(.data$gapstart) & (.data$gapstart - 1) - .data$stat_ep_end_year < tolerance, 1, .data$stat_ep_uncertain),
                  stat_ep_start_year = ifelse(.data$stat_ep == 1, first(.data$year) + 1, NA),
                  stat_pre_ep_year = ifelse(.data$stat_ep == 1, ifelse(.data$year == dplyr::first(.data$year), 1, 0), 0),
                  stat_ep_id = ifelse(.data$stat_ep == 1, paste(.data$country_text_id, .data$stat_ep_start_year, .data$stat_ep_end_year, sep = "_"), NA)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$character_id, -.data$episode_id) %>%
    dplyr::arrange(.data$country_name, .data$year) %>%
    as.data.frame %>%
    
    dplyr::group_by(.data$stat_ep_id) %>%
    dplyr::mutate(
      breakdown = ifelse(.data$stat_ep == 1 & .data$year >= .data$stat_ep_start_year,
                         ifelse(dplyr::lead(.data$eco_reg_trans == 1), .data$year, NA), NA),
      breakdown = min(hablar::s(.data$breakdown)),
      stat_ep_end_year = ifelse(!is.na(.data$breakdown) & .data$stat_ep_end_year > .data$breakdown, .data$breakdown, .data$stat_ep_end_year)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      stat_ep_start_year = ifelse(.data$stat_ep == 1 & .data$year > .data$stat_ep_end_year, NA, .data$stat_ep_start_year),
      stat_ep_end_year = ifelse(.data$stat_ep == 1 & .data$year > .data$stat_ep_end_year, NA, .data$stat_ep_end_year),
      stat_ep = ifelse(.data$stat_ep == 1 & .data$year > .data$stat_ep_end_year, 0, .data$stat_ep),
      stat_ep_id = ifelse(.data$stat_ep == 1, paste(.data$country_text_id, .data$stat_ep_start_year, .data$stat_ep_end_year, sep = "_"), NA)) %>%
    dplyr::group_by(.data$stat_ep_id) %>%
    dplyr::mutate(last_ch_year = max(hablar::s(ifelse(.data$v2clstown_osp - dplyr::lag(.data$v2clstown_osp, n = 1) <= -start_incl, .data$year, NA))),
                  last_ch_year = ifelse(.data$stat_ep == 0, NA, .data$last_ch_year)) %>%
    
    dplyr::group_by(.data$country_id) %>%
    dplyr::arrange(.data$country_id, .data$year) %>%
    
    ### CODING THE STATIZATION EPISODE OUTCOMES ###
    
    dplyr::group_by(.data$stat_ep_id) %>%
    dplyr::mutate(
      stat_ep_outcome = case_when(
        .data$stat_ep_uncertain == 1 ~ 6, # Outcome censored
        # Market breakdown
        .data$stat_ep_termination == 4 & min(.data$eco_reg_type, na.rm = TRUE) %in% c(1, 2) & max(.data$eco_reg_type, na.rm = TRUE) %in% c(3, 4) ~ 1,
        # Preempted Market breakdown
        .data$stat_ep_termination == 4 & min(.data$eco_reg_type, na.rm = TRUE) %in% c(1, 2) & max(.data$eco_reg_type, na.rm = TRUE) %in% c(1, 2) ~ 2,
        # Diminished Market Economy
        .data$stat_ep_termination == 1 & min(.data$eco_reg_type, na.rm = TRUE) %in% c(1, 2) ~ 4,
        # Averted Statization
        .data$stat_ep_termination %in% c(2, 3) & min(.data$eco_reg_type, na.rm = TRUE) %in% c(1, 2) ~ 4,
        # Deepened Planned Economy
        .data$stat_ep_termination == 1 & min(.data$eco_reg_type, na.rm = TRUE) %in% c(3, 4) ~ 5,
        TRUE ~ NA_real_
      )
    ) %>%
    
    ### FINAL CLEANING ###
    
    dplyr::ungroup() %>%
    dplyr::group_by(.data$country_id, .data$year) %>%
    dplyr::mutate(
      priv_ep_id = ifelse(
        !is.na(.data$priv_ep_id) & (.data$priv_ep_outcome == 5 & dplyr::lag(.data$priv_ep_outcome == 5)),
        NA, .data$priv_ep_id
      ),
      stat_ep_id = ifelse(
        !is.na(.data$stat_ep_id) & (.data$stat_ep_outcome == 5 & dplyr::lag(.data$stat_ep_outcome == 5)),
        NA, .data$stat_ep_id
      ),
      priv_ep_id = ifelse(
        !is.na(.data$priv_ep_id) & (.data$priv_ep_outcome == 1 & dplyr::lag(.data$priv_ep_outcome == 1)),
        NA, .data$priv_ep_id
      ),
      stat_ep_id = ifelse(
        !is.na(.data$stat_ep_id) & (.data$stat_ep_outcome == 1 & dplyr::lag(.data$stat_ep_outcome == 1)),
        NA, .data$stat_ep_id
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-c(
      .data$last_ch_year, .data$breakdown,
      .data$gapstart1, .data$gapstart2, .data$gapstart3, .data$gapend1, .data$gapend2, .data$gapend3,
      .data$gapstart, .data$gapend, .data$eco_regch_filled,
      .data$eco_start_year, .data$eco_end_year, .data$eco_id,
      .data$episode_id, .data$character_id, .data$priv_ep, .data$stat_ep,
      .data$priv_ep_uncertain, .data$stat_ep_uncertain,
      .data$priv_pre_ep_year, .data$stat_pre_ep_year,
      tidyselect::matches("v2clstown_osp_nr")
    )) %>%
    dplyr::filter(.data$origsample == 1)
  
  return(full.df)
}