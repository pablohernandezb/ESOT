#' Plot Episodes of Regime Transformation (ERT) over time.
#'
#' `plot_all` plots the global number/share of countries undergoing
#'  democratization and autocratization Episodes of Regime Transformation (ERT)
#'  in a selected time frame.
#'
#' This function is a wrapper for [ggplot2:ggplot()] and produces a plot that shows
#' democratization and autocratization episodes over time.
#' The function calls the [ERT:get_eps()] function to identify episodes.
#'
#'#' @param abs Logical value: if TRUE, the absolute number of countries in an episode for each year is plotted.
#' If FALSE, the share of countries (in \%) undergoing democratization or autocratization is plotted.
#'
#' @param years Vector with two numeric values indicating the minimum and maximum year to be plotted.
#'
#' @param start_incl What is the minimum annual change on V-Dem's Electoral Democracy Index (EDI) necessary to trigger an episode? 
#' This is the absolute value of the first difference in the EDI required for the onset 
#' of either a democratization (+) or autocratization episode (–).
#'
#' @param cum_incl What is the minimum amount of total change on the EDI necessary to constitute a manifest episode?
#' A potential episode might be a period involving any amount of changes over a period following an annual change equal 
#' to the start inclusion (e.g. 0.01). To identify substantial changes, we set a cumulative inclusion threshold. 
#' This is the absolute value of the total amount of change needed on the EDI to be considered manifest.
#'
#' @param year_turn What is the amount of annual change in the opposite direction to trigger the termination of an episode? 
#' An episode may end when the case suddenly moves in the opposite direction. 
#' For example, during an episode of democratization, a country may experience a sudden drop on the EDI. 
#' This could signal the onset of an autocratization episode. To avoid overlap between episodes, 
#' we set the absolute value of a change in the opposite direction on the EDI 
#' as a trigger for the termination of an episode. \emph{Note: Advanced users who wish to remove this criteria altogether 
#' should set the value of year\_turn equal to cum\_turn. 
#' Setting this to zero would allow for an episode to terminate when any year of no change is encountered.}
#' 
#' @param cum_turn What is the amount of gradual change in the opposite direction to trigger the termination of an episode?
#' An episode may end when the case begins moving in the opposite direction gradually. For example, 
#' during an episode of democratization, a country may experience a gradual drop on the EDI over a number of years 
#' that signals democratization has ended. This could also signal the onset of an autocratization episode. 
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
#' @return The output of this function is a [ggplot2:ggplot()] object with the number/share of autocratization episodes per year.

#' @import ggplot2
#' @import dplyr
#' 
#' @export

localizations <- list(
  en = list(
    year = "Year",
    autocratization = "Autocratization",
    democratization = "Democratization",
    number_countries = "Number of Countries",
    countries_percent = "Countries (%)",
    no_data = "Error: Data not available for time range",
    no_episodes = "No episodes during selected period."
  ),
  es = list(
    year = "Año",
    autocratization = "Autocratización",
    democratization = "Democratización",
    number_countries = "Número de países",
    countries_percent = "Países (%)",
    no_data = "Error: Datos no disponibles para el rango de tiempo",
    no_episodes = "No hay episodios durante el período seleccionado."
  )
)

get_label <- function(key, lang = "es") {
  if (!lang %in% names(localizations)) lang <- "en"
  label <- localizations[[lang]][[key]]
  if (is.null(label)) return(key)
  label
}

plot_all <- function(abs = T,
                     years = c(1900, 2023),
                     start_incl  = 0.01,
                     cum_incl  = 0.1,
                     year_turn = 0.03,
                     cum_turn = 0.1,
                     tolerance = 5,
                     lang = "en") {
  
  eps <- ERT::get_eps(data = ERT::vdem,
                      start_incl = start_incl,
                      cum_incl = cum_incl,
                      year_turn = year_turn,
                      cum_turn = cum_turn,
                      tolerance = tolerance)
  
  
  stopifnot(is.logical(abs), length(abs) == 1)
  
  stopifnot(is.numeric(years), length(years) == 2, years[2] > years[1])
  
  stopifnot(is.numeric(start_incl), length(start_incl) == 1)
  
  stopifnot(is.numeric(cum_incl), length(cum_incl) == 1)
  
  stopifnot(is.numeric(year_turn), length(year_turn) == 1)
  
  stopifnot(is.numeric(cum_turn), length(cum_turn) == 1)
  
  stopifnot(is.numeric(tolerance), length(tolerance) == 1)
  
  #perhaps this is redundant 
  if(min(years)<min(ERT::vdem$year) | max(years)>max(ERT::vdem$year))
    get_label("no_data", lang)
  
  if (isTRUE(abs)) {
    eps_year <- eps %>%
      dplyr::filter(between(year, min(years), max(years))) %>%
      {if(nrow(.) == 0) stop(get_label("no_episodes", lang)) else .} %>% 
      dplyr::group_by(year) %>%
      dplyr::summarise(dem_eps = sum(dem_ep),
                       aut_eps = sum(aut_ep)) %>%
      tidyr::pivot_longer(cols = c(dem_eps, aut_eps), names_to = "ep_type", values_to = "countries")
    
  } else {
    eps_year <- eps %>%
      dplyr::filter(between(year, min(years), max(years))) %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(dem_eps = sum(dem_ep) / length(unique(country_id)),
                       aut_eps = sum(aut_ep) / length(unique(country_id))) %>%
      tidyr::pivot_longer(cols = c(dem_eps, aut_eps), names_to = "ep_type", values_to = "countries")
  }
  
  p <-  ggplot2::ggplot(data = eps_year, aes(x = year, y = countries, group = ep_type, linetype = ep_type)) +
    geom_line() +
    scale_x_continuous(breaks = seq(round(min(years) / 10) * 10, round(max(years) / 10) * 10, 10)) +
    scale_linetype(name = "", breaks = c("aut_eps", "dem_eps"), labels = c(get_label("autocratization", lang), get_label("democratization", lang))) +
    xlab(get_label("year", lang)) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  if (isTRUE(abs)) {
    p +  ylab(get_label("number_countries", lang))
  }  else {
    p +  ylab(get_label("countries_percent", lang))
  }
}
