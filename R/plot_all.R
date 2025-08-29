#' Plot Episodes of State Ownership Transformation (ESOT) over time.
#'
#' `plot_all` plots the global number/share of countries undergoing
#'  privatization and statization Episodes of State Ownership Transformation (ESOT)
#'  in a selected time frame.
#'
#' This function is a wrapper for [ggplot2:ggplot()] and produces a plot that shows
#' privatization and statization episodes over time.
#' The function calls the [ESOT:get_eps()] function to identify episodes.
#'
#'#' @param abs Logical value: if TRUE, the absolute number of countries in an episode for each year is plotted.
#' If FALSE, the share of countries (in \%) undergoing privatization or statization is plotted.
#'
#' @param years Vector with two numeric values indicating the minimum and maximum year to be plotted.
#'
#' @param start_incl What is the minimum annual change on V-Dem's Electoral Democracy Index (EDI) necessary to trigger an episode? 
#' This is the absolute value of the first difference in the EDI required for the onset 
#' of either a privatization (+) or statization episode (–).
#'
#' @param cum_incl What is the minimum amount of total change on the EDI necessary to constitute a manifest episode?
#' A potential episode might be a period involving any amount of changes over a period following an annual change equal 
#' to the start inclusion (e.g. 0.01). To identify substantial changes, we set a cumulative inclusion threshold. 
#' This is the absolute value of the total amount of change needed on the EDI to be considered manifest.
#'
#' @param year_turn What is the amount of annual change in the opposite direction to trigger the termination of an episode? 
#' An episode may end when the case suddenly moves in the opposite direction. 
#' For example, during an episode of privatization, a country may experience a sudden drop on the EDI. 
#' This could signal the onset of an statization episode. To avoid overlap between episodes, 
#' we set the absolute value of a change in the opposite direction on the EDI 
#' as a trigger for the termination of an episode. \emph{Note: Advanced users who wish to remove this criteria altogether 
#' should set the value of year\_turn equal to cum\_turn. 
#' Setting this to zero would allow for an episode to terminate when any year of no change is encountered.}
#' 
#' @param cum_turn What is the amount of gradual change in the opposite direction to trigger the termination of an episode?
#' An episode may end when the case begins moving in the opposite direction gradually. For example, 
#' during an episode of privatization, a country may experience a gradual drop on the EDI over a number of years 
#' that signals privatization has ended. This could also signal the onset of an statization episode. 
#' To avoid overlap between episodes, we set the absolute value of a gradual change in the opposite direction 
#' on the EDI over the tolerance period (e.g. 5 years) as a trigger for the termination of an episode.
#'
#' @param tolerance What is the number of years considered as tolerance for stasis or a gradual movement in the opposite direction?
#' The tolerance defines the number of years an episode is allowed to remain in stasis 
#' (i.e. no more movements equal to the start inclusion) and/or move in the opposite direction before it is terminated. 
#' This parameter also defines the number of years necessary for a case to be considered a democratic breakdown or 
#' stabilized electoral autocracy. \emph{Therefore, care is necessary when manipulating the default value. 
#' This could lead to large changes in the composition of episodes. 
#' We set the default to 5 years because this is the typical amount for an electoral cycle for most countries.}
#'
#' @param lang Language for labels. Default is "en" (English).
#' @return The output of this function is a [ggplot2:ggplot()] object with the number/share of statization episodes per year.

#' @import ggplot2
#' @import dplyr
#' 
#' @export

localizations <- list(
  en = list(
    year = "Year",
    statization = "Statization",
    privatization = "Privatization",
    number_countries = "Number of Countries",
    countries_percent = "Countries (%)",
    no_data = "Error: Data not available for time range",
    no_episodes = "No episodes during selected period."
  ),
  es = list(
    year = "Año",
    statization = "Estatización",
    privatization = "Privatización",
    number_countries = "Número de países",
    countries_percent = "Países (%)",
    no_data = "Error: Datos no disponibles para el rango de tiempo",
    no_episodes = "No hay episodios durante el período seleccionado."
  )
)

get_label <- function(key, lang = "en") {
  if (!lang %in% names(localizations)) lang <- "en"
  label <- localizations[[lang]][[key]]
  if (is.null(label)) return(key)
  label
}

plot_all <- function(abs = TRUE,
                     years = c(1900, 2023),
                     start_incl  = 0.04,
                     cum_incl  = 0.4,
                     year_turn = 0.12,
                     cum_turn = 0.4,
                     tolerance = 5,
                     lang = "en") {
  
  # Fetch episodes data
  eps <- get_eps(data = ESOT::vdem,
                  start_incl = start_incl,
                  cum_incl = cum_incl,
                  year_turn = year_turn,
                  cum_turn = cum_turn,
                  tolerance = tolerance)
  
  
  # --- Input Validation (same as original) ---
  stopifnot(is.logical(abs), length(abs) == 1)
  stopifnot(is.numeric(years), length(years) == 2, years[2] > years[1])
  stopifnot(is.numeric(start_incl), length(start_incl) == 1)
  stopifnot(is.numeric(cum_incl), length(cum_incl) == 1)
  stopifnot(is.numeric(year_turn), length(year_turn) == 1)
  stopifnot(is.numeric(cum_turn), length(cum_turn) == 1)
  stopifnot(is.numeric(tolerance), length(tolerance) == 1)
  
  # Check for data range
  if(min(years) < min(ESOT::vdem$year) | max(years) > max(ESOT::vdem$year)) {
    get_label("no_data", lang)
  }
  
  # --- Refactored Plotting Logic ---
  
  # Use new variable names 'priv_ep' and 'stat_ep'
  if (isTRUE(abs)) {
    eps_year <- eps %>%
      dplyr::filter(dplyr::between(.data$year, min(years), max(years))) %>%
      {if(nrow(.) == 0) stop(get_label("no_episodes", lang)) else .} %>%
      dplyr::group_by(.data$year) %>%
      dplyr::summarise(priv_eps = sum(.data$priv_ep),
                       stat_eps = sum(.data$stat_ep)) %>%
      tidyr::pivot_longer(cols = c(.data$priv_eps, .data$stat_eps), names_to = "ep_type", values_to = "countries")
    
  } else {
    eps_year <- eps %>%
      dplyr::filter(dplyr::between(.data$year, min(years), max(years))) %>%
      {if(nrow(.) == 0) stop(get_label("no_episodes", lang)) else .} %>%
      dplyr::group_by(.data$year) %>%
      dplyr::summarise(priv_eps = sum(.data$priv_ep) / length(unique(eps$country_id)),
                       stat_eps = sum(.data$stat_ep) / length(unique(eps$country_id))) %>%
      tidyr::pivot_longer(cols = c(.data$priv_eps, .data$stat_eps), names_to = "ep_type", values_to = "countries")
  }
  
  # --- Plotting the Data ---
  
  p <- ggplot2::ggplot(data = eps_year, ggplot2::aes(x = .data$year, y = .data$countries, group = .data$ep_type, linetype = .data$ep_type)) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = seq(round(min(years) / 10) * 10, round(max(years) / 10) * 10, 10)) +
    # Update labels to reflect new episode types
    ggplot2::scale_linetype(name = "", breaks = c("stat_eps", "priv_eps"), labels = c(get_label("statization", lang), get_label("privatization", lang))) +
    ggplot2::xlab(get_label("year", lang)) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "bottom")
  
  if (isTRUE(abs)) {
    p + ggplot2::ylab(get_label("number_countries", lang))
  } else {
    p + ggplot2::ylab(get_label("countries_percent", lang))
  }
}
