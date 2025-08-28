#' Plot Episodes of State Ownership Transformation (ESOT) over time.
#'
#' `plot_episodes` plots Episodes of State Ownership Transformation (ESOT) over time for a selected country and a selected time frame.
#'
#' This function is a wrapper for [ggplot2:ggplot()] and produces a plot that shows
#' democratization and autocratization episodes for a selected country over time.
#' The legend includes information on the start and end data of each episode,
#' as well as the episode outcome. The function calls the [ESOT:get_eps()] function
#' to identify episodes.
#'
#' @param years Vector with two numeric values indicating the minimum and maximum year to be plotted.
#'
#' @param country Character vector containing the country for which episodes should be shown. Only entries from the
#'  country_name column in the V-Dem data set are accepted.
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
#' @param data The data based on which the episodes are identified.
#' By default the most recent vdem data set.
#'
#' @param lang Language for labels. Default is "en" (English).
#'
#' @return The output of this function is a [ggplot2:ggplot()] object with episodes for a selected country.

#' @import ggplot2
#' @import dplyr
#'
#' @examples
#' \dontrun{
#'
#' # Plot episodes for Belgium between 1910 and 2010.
#'
#'  plot_episodes(country = c("Belgium"),
#'                years = c(1910, 2010))
#' }
#' @export

localizations <- list(
  en = list(
    episode = "Episode",
    episode_type = "Episode type",
    autocratization = "Autocratization",
    democratization = "Democratization",
    overlap = "Overlap",
    year = "Year",
    edi = "Electoral Democracy Index",
    no_episodes = "No episodes during selected period.",
    warning_overlap = "Warning: Some episodes overlap!",
    stop_more_than_one_country = "Error: More than one country selected",
    stop_no_country = "Error: No country selected",
    stop_country_not_found = "Error: Country not found",
    stop_data_not_available = "Error: Data not available for time range",
    stop_years_format = "Error: 'years' must be a numeric vector of length 2 with years[2] > years[1]",
    stop_country_format = "Error: 'country' must be a character vector",
    stop_start_incl_format = "Error: 'start_incl' must be a numeric value",
    stop_cum_incl_format = "Error: 'cum_incl' must be a numeric value",
    stop_year_turn_format = "Error: 'year_turn' must be a numeric value",
    stop_cum_turn_format = "Error: 'cum_turn' must be a numeric value",
    stop_tolerance_format = "Error: 'tolerance' must be a numeric value",
    outcome_censored = "Outcome censored",
    outcome_dem_transition = "Democratic transition",
    outcome_dem_preempted = "Preempted democratic transition",
    outcome_dem_stabilized = "Stabilized electoral autocracy",
    outcome_dem_reverted = "Reverted liberalization",
    outcome_dem_deepened = "Deepened democracy",
    outcome_aut_censored = "Outcome censored",
    outcome_aut_breakdown = "Democratic breakdown",
    outcome_aut_preempted = "Preempted democratic breakdown",
    outcome_aut_diminished = "Diminished democracy",
    outcome_aut_averted = "Averted regression",
    outcome_aut_regressed = "Regressed autocracy"
  ),
  es = list(
    episode = "Episodio",
    episode_type = "Tipo de episodio",
    autocratization = "Autocratización",
    democratization = "Democratización",
    overlap = "Superposición",
    year = "Año",
    edi = "Índice de Democracia Electoral",
    no_episodes = "No hay episodios durante el período seleccionado.",
    warning_overlap = "¡Advertencia: Algunos episodios se superponen!",
    stop_more_than_one_country = "Error: Se seleccionó más de un país",
    stop_no_country = "Error: No se seleccionó ningún país",
    stop_country_not_found = "Error: País no encontrado",
    stop_data_not_available = "Error: Datos no disponibles para el rango de años",
    stop_years_format = "Error: 'years' debe ser un vector numérico de longitud 2 con years[2] > years[1]",
    stop_country_format = "Error: 'country' debe ser un vector de caracteres",
    stop_start_incl_format = "Error: 'start_incl' debe ser un valor numérico",
    stop_cum_incl_format = "Error: 'cum_incl' debe ser un valor numérico",
    stop_year_turn_format = "Error: 'year_turn' debe ser un valor numérico",
    stop_cum_turn_format = "Error: 'cum_turn' debe ser un valor numérico",
    stop_tolerance_format = "Error: 'tolerance' debe ser un valor numérico",
    outcome_censored = "Resultado truncado",
    outcome_dem_transition = "Transición democrática",
    outcome_dem_preempted = "Transición democrática prevenida",
    outcome_dem_stabilized = "Autocracia electoral estabilizada",
    outcome_dem_reverted = "Liberalización revertida",
    outcome_dem_deepened = "Democracia profundizada",
    outcome_aut_breakdown = "Ruptura democrática",
    outcome_aut_preempted = "Ruptura democrática prevenida",
    outcome_aut_diminished = "Democracia disminuida",
    outcome_aut_averted = "Regresión evitada",
    outcome_aut_regressed = "Autocracia regresiva"
  )
)

country_names <- list(
  en = list(
    Mexico = "Mexico",
    Suriname = "Suriname",
    Sweden = "Sweden",
    Switzerland = "Switzerland",
    Ghana = "Ghana",
    South_Africa = "South Africa",
    Japan = "Japan",
    Burma_Myanmar = "Burma/Myanmar",
    Russia = "Russia",
    Albania = "Albania",
    Egypt = "Egypt",
    Yemen = "Yemen",
    Colombia = "Colombia",
    Poland = "Poland",
    Brazil = "Brazil",
    United_States_of_America = "United States of America",
    Portugal = "Portugal",
    El_Salvador = "El Salvador",
    South_Yemen = "South Yemen",
    Bangladesh = "Bangladesh",
    Bolivia = "Bolivia",
    Haiti = "Haiti",
    Honduras = "Honduras",
    Mali = "Mali",
    Pakistan = "Pakistan",
    Peru = "Peru",
    Senegal = "Senegal",
    South_Sudan = "South Sudan",
    Sudan = "Sudan",
    Vietnam = "Vietnam",
    Republic_of_Vietnam = "Republic of Vietnam",
    Afghanistan = "Afghanistan",
    Argentina = "Argentina",
    Ethiopia = "Ethiopia",
    India = "India",
    Kenya = "Kenya",
    North_Korea = "North Korea",
    South_Korea = "South Korea",
    Kosovo = "Kosovo",
    Lebanon = "Lebanon",
    Nigeria = "Nigeria",
    Philippines = "Philippines",
    Tanzania = "Tanzania",
    Taiwan = "Taiwan",
    Thailand = "Thailand",
    Uganda = "Uganda",
    Venezuela = "Venezuela",
    Benin = "Benin",
    Bhutan = "Bhutan",
    Burkina_Faso = "Burkina Faso",
    Cambodia = "Cambodia",
    Indonesia = "Indonesia",
    Mozambique = "Mozambique",
    Nepal = "Nepal",
    Nicaragua = "Nicaragua",
    Niger = "Niger",
    Zambia = "Zambia",
    Zimbabwe = "Zimbabwe",
    Guinea = "Guinea",
    Ivory_Coast = "Ivory Coast",
    Mauritania = "Mauritania",
    Canada = "Canada",
    Australia = "Australia",
    Botswana = "Botswana",
    Burundi = "Burundi",
    Cape_Verde = "Cape Verde",
    Central_African_Republic = "Central African Republic",
    Chile = "Chile",
    Costa_Rica = "Costa Rica",
    Timor_Leste = "Timor-Leste",
    Ecuador = "Ecuador",
    France = "France",
    Germany = "Germany",
    Guatemala = "Guatemala",
    Iran = "Iran",
    Iraq = "Iraq",
    Ireland = "Ireland",
    Italy = "Italy",
    Jordan = "Jordan",
    Latvia = "Latvia",
    Lesotho = "Lesotho",
    Liberia = "Liberia",
    Malawi = "Malawi",
    Maldives = "Maldives",
    Mongolia = "Mongolia",
    Morocco = "Morocco",
    Netherlands = "Netherlands",
    Panama = "Panama",
    Papua_New_Guinea = "Papua New Guinea",
    Qatar = "Qatar",
    Sierra_Leone = "Sierra Leone",
    Spain = "Spain",
    Syria = "Syria",
    Tunisia = "Tunisia",
    Türkiye = "Türkiye",
    Ukraine = "Ukraine",
    United_Kingdom = "United Kingdom",
    Uruguay = "Uruguay",
    Algeria = "Algeria",
    Angola = "Angola",
    Armenia = "Armenia",
    Azerbaijan = "Azerbaijan",
    Belarus = "Belarus",
    Cameroon = "Cameroon",
    Chad = "Chad",
    China = "China",
    Democratic_Republic_of_the_Congo = "Democratic Republic of the Congo",
    Republic_of_the_Congo = "Republic of the Congo",
    Djibouti = "Djibouti",
    Dominican_Republic = "Dominican Republic",
    Eritrea = "Eritrea",
    Gabon = "Gabon",
    The_Gambia = "The Gambia",
    Georgia = "Georgia",
    Guinea_Bissau = "Guinea-Bissau",
    Jamaica = "Jamaica",
    Kazakhstan = "Kazakhstan",
    Kyrgyzstan = "Kyrgyzstan",
    Laos = "Laos",
    Libya = "Libya",
    Madagascar = "Madagascar",
    Moldova = "Moldova",
    Namibia = "Namibia",
    Palestine_West_Bank = "Palestine/West Bank",
    Rwanda = "Rwanda",
    Somalia = "Somalia",
    Sri_Lanka = "Sri Lanka",
    Eswatini = "Eswatini",
    Tajikistan = "Tajikistan",
    Togo = "Togo",
    Trinidad_and_Tobago = "Trinidad and Tobago",
    Turkmenistan = "Turkmenistan",
    German_Democratic_Republic = "German Democratic Republic",
    Palestine_Gaza = "Palestine/Gaza",
    Somaliland = "Somaliland",
    Uzbekistan = "Uzbekistan",
    Austria = "Austria",
    Bahrain = "Bahrain",
    Barbados = "Barbados",
    Belgium = "Belgium",
    Bosnia_and_Herzegovina = "Bosnia and Herzegovina",
    Bulgaria = "Bulgaria",
    Comoros = "Comoros",
    Croatia = "Croatia",
    Cuba = "Cuba",
    Cyprus = "Cyprus",
    Czechia = "Czechia",
    Denmark = "Denmark",
    Equatorial_Guinea = "Equatorial Guinea",
    Estonia = "Estonia",
    Fiji = "Fiji",
    Finland = "Finland",
    Greece = "Greece",
    Guyana = "Guyana",
    Hong_Kong = "Hong Kong",
    Iceland = "Iceland",
    Israel = "Israel",
    Kuwait = "Kuwait",
    Lithuania = "Lithuania",
    Luxembourg = "Luxembourg",
    North_Macedonia = "North Macedonia",
    Malaysia = "Malaysia",
    Malta = "Malta",
    Mauritius = "Mauritius",
    Montenegro = "Montenegro",
    New_Zealand = "New Zealand",
    Norway = "Norway",
    Oman = "Oman",
    Paraguay = "Paraguay",
    Romania = "Romania",
    Sao_Tome_and_Principe = "Sao Tome and Principe",
    Saudi_Arabia = "Saudi Arabia",
    Serbia = "Serbia",
    Seychelles = "Seychelles",
    Singapore = "Singapore",
    Slovakia = "Slovakia",
    Slovenia = "Slovenia",
    Solomon_Islands = "Solomon Islands",
    Vanuatu = "Vanuatu",
    United_Arab_Emirates = "United Arab Emirates",
    Palestine_British_Mandate = "Palestine/British Mandate",
    Hungary = "Hungary",
    Zanzibar = "Zanzibar"
  ),
  es = list(
    Mexico = "México",
    Suriname = "Surinam",
    Sweden = "Suecia",
    Switzerland = "Suiza",
    Ghana = "Ghana",
    South_Africa = "Sudáfrica",
    Japan = "Japón",
    Burma_Myanmar = "Birmania/Myanmar",
    Russia = "Rusia",
    Albania = "Albania",
    Egypt = "Egipto",
    Yemen = "Yemen",
    Colombia = "Colombia",
    Poland = "Polonia",
    Brazil = "Brasil",
    United_States_of_America = "Estados Unidos de América",
    Portugal = "Portugal",
    El_Salvador = "El Salvador",
    South_Yemen = "Yemen del Sur",
    Bangladesh = "Bangladés",
    Bolivia = "Bolivia",
    Haiti = "Haití",
    Honduras = "Honduras",
    Mali = "Malí",
    Pakistan = "Pakistán",
    Peru = "Perú",
    Senegal = "Senegal",
    South_Sudan = "Sudán del Sur",
    Sudan = "Sudán",
    Vietnam = "Vietnam",
    Republic_of_Vietnam = "República de Vietnam",
    Afghanistan = "Afganistán",
    Argentina = "Argentina",
    Ethiopia = "Etiopía",
    India = "India",
    Kenya = "Kenia",
    North_Korea = "Corea del Norte",
    South_Korea = "Corea del Sur",
    Kosovo = "Kosovo",
    Lebanon = "Líbano",
    Nigeria = "Nigeria",
    Philippines = "Filipinas",
    Tanzania = "Tanzania",
    Taiwan = "Taiwán",
    Thailand = "Tailandia",
    Uganda = "Uganda",
    Venezuela = "Venezuela",
    Benin = "Benín",
    Bhutan = "Bután",
    Burkina_Faso = "Burkina Faso",
    Cambodia = "Camboya",
    Indonesia = "Indonesia",
    Mozambique = "Mozambique",
    Nepal = "Nepal",
    Nicaragua = "Nicaragua",
    Niger = "Níger",
    Zambia = "Zambia",
    Zimbabwe = "Zimbabue",
    Guinea = "Guinea",
    Ivory_Coast = "Costa de Marfil",
    Mauritania = "Mauritania",
    Canada = "Canadá",
    Australia = "Australia",
    Botswana = "Botsuana",
    Burundi = "Burundi",
    Cape_Verde = "Cabo Verde",
    Central_African_Republic = "República Centroafricana",
    Chile = "Chile",
    Costa_Rica = "Costa Rica",
    Timor_Leste = "Timor Oriental",
    Ecuador = "Ecuador",
    France = "Francia",
    Germany = "Alemania",
    Guatemala = "Guatemala",
    Iran = "Irán",
    Iraq = "Irak",
    Ireland = "Irlanda",
    Italy = "Italia",
    Jordan = "Jordania",
    Latvia = "Letonia",
    Lesotho = "Lesoto",
    Liberia = "Liberia",
    Malawi = "Malaui",
    Maldives = "Maldivas",
    Mongolia = "Mongolia",
    Morocco = "Marruecos",
    Netherlands = "Países Bajos",
    Panama = "Panamá",
    Papua_New_Guinea = "Papúa Nueva Guinea",
    Qatar = "Catar",
    Sierra_Leone = "Sierra Leona",
    Spain = "España",
    Syria = "Siria",
    Tunisia = "Túnez",
    Türkiye = "Turquía",
    Ukraine = "Ucrania",
    United_Kingdom = "Reino Unido",
    Uruguay = "Uruguay",
    Algeria = "Argelia",
    Angola = "Angola",
    Armenia = "Armenia",
    Azerbaijan = "Azerbaiyán",
    Belarus = "Bielorrusia",
    Cameroon = "Camerún",
    Chad = "Chad",
    China = "China",
    Democratic_Republic_of_the_Congo = "República Democrática del Congo",
    Republic_of_the_Congo = "República del Congo",
    Djibouti = "Yibuti",
    Dominican_Republic = "República Dominicana",
    Eritrea = "Eritrea",
    Gabon = "Gabón",
    The_Gambia = "Gambia",
    Georgia = "Georgia",
    Guinea_Bissau = "Guinea-Bisáu",
    Jamaica = "Jamaica",
    Kazakhstan = "Kazajistán",
    Kyrgyzstan = "Kirguistán",
    Laos = "Laos",
    Libya = "Libia",
    Madagascar = "Madagascar",
    Moldova = "Moldavia",
    Namibia = "Namibia",
    Palestine_West_Bank = "Palestina/Cisjordania",
    Rwanda = "Ruanda",
    Somalia = "Somalia",
    Sri_Lanka = "Sri Lanka",
    Eswatini = "Esuatini",
    Tajikistan = "Tayikistán",
    Togo = "Togo",
    Trinidad_and_Tobago = "Trinidad y Tobago",
    Turkmenistan = "Turkmenistán",
    German_Democratic_Republic = "República Democrática Alemana",
    Palestine_Gaza = "Palestina/Gaza",
    Somaliland = "Somalilandia",
    Uzbekistan = "Uzbekistán",
    Austria = "Austria",
    Bahrain = "Baréin",
    Barbados = "Barbados",
    Belgium = "Bélgica",
    Bosnia_and_Herzegovina = "Bosnia y Herzegovina",
    Bulgaria = "Bulgaria",
    Comoros = "Comoras",
    Croatia = "Croacia",
    Cuba = "Cuba",
    Cyprus = "Chipre",
    Czechia = "Chequia",
    Denmark = "Dinamarca",
    Equatorial_Guinea = "Guinea Ecuatorial",
    Estonia = "Estonia",
    Fiji = "Fiyi",
    Finland = "Finlandia",
    Greece = "Grecia",
    Guyana = "Guyana",
    Hong_Kong = "Hong Kong",
    Iceland = "Islandia",
    Israel = "Israel",
    Kuwait = "Kuwait",
    Lithuania = "Lituania",
    Luxembourg = "Luxemburgo",
    North_Macedonia = "Macedonia del Norte",
    Malaysia = "Malasia",
    Malta = "Malta",
    Mauritius = "Mauricio",
    Montenegro = "Montenegro",
    New_Zealand = "Nueva Zelanda",
    Norway = "Noruega",
    Oman = "Omán",
    Paraguay = "Paraguay",
    Romania = "Rumanía",
    Sao_Tome_and_Principe = "Santo Tomé y Príncipe",
    Saudi_Arabia = "Arabia Saudita",
    Serbia = "Serbia",
    Seychelles = "Seychelles",
    Singapore = "Singapur",
    Slovakia = "Eslovaquia",
    Slovenia = "Eslovenia",
    Solomon_Islands = "Islas Salomón",
    Vanuatu = "Vanuatu",
    United_Arab_Emirates = "Emiratos Árabes Unidos",
    Palestine_British_Mandate = "Palestina/Mandato Británico",
    Hungary = "Hungría",
    Zanzibar = "Zanzíbar"
  )
)

get_label <- function(key, lang = "en") {
  if (!lang %in% names(localizations)) lang <- "en"
  label <- localizations[[lang]][[key]]
  if (is.null(label)) return(key)
  label
}

get_country_name <- function(name, lang = "en") {
  key <- gsub("[ -]", "_", name)
  if (!lang %in% names(country_names)) lang <- "en"
  if (!is.null(country_names[[lang]][[key]])) {
    return(country_names[[lang]][[key]])
  }
  # Try direct match if key not found
  if (!is.null(country_names[[lang]][[name]])) {
    return(country_names[[lang]][[name]])
  }
  return(name)
}

plot_episodes <- function(years = c(1900, 2023),
                          country = c("Sweden"),
                          start_incl  = 0.01,
                          cum_incl  = 0.1,
                          year_turn = 0.03,
                          cum_turn = 0.1,
                          tolerance = 5,
                          data = ESOT::vdem,
                          lang = "en") {
  
  eps <- ESOT::get_eps(data = data,
                      start_incl = start_incl,
                      cum_incl = cum_incl,
                      year_turn = year_turn,
                      cum_turn = cum_turn,
                      tolerance = tolerance)
  
  
  if(!(is.numeric(years) && length(years) == 2 && years[2] > years[1]))
    stop(get_label("stop_years_format", lang))

  if(!is.character(country))
    stop(get_label("stop_country_format", lang))

  if(length(country) > 1)
    stop(get_label("stop_more_than_one_country", lang))

  if(length(country) == 0)
    stop(get_label("stop_no_country", lang))

  if(!country %in% data$country_name)
    stop(get_label("stop_country_not_found", lang))

  if(max(years) < min(eps %>% dplyr::filter(country_name==country) %>% dplyr::pull(year)) | max(years)>max(eps %>% dplyr::filter(country_name==country) %>% dplyr::pull(year)))
    stop(get_label("stop_data_not_available", lang))

  if(!(is.numeric(start_incl) && length(start_incl) == 1))
    stop(get_label("stop_start_incl_format", lang))

  if(!(is.numeric(cum_incl) && length(cum_incl) == 1))
    stop(get_label("stop_cum_incl_format", lang))

  if(!(is.numeric(year_turn) && length(year_turn) == 1))
    stop(get_label("stop_year_turn_format", lang))

  if(!(is.numeric(cum_turn) && length(cum_turn) == 1))
    stop(get_label("stop_cum_turn_format", lang))

  if(!(is.numeric(tolerance) && length(tolerance) == 1))
    stop(get_label("stop_tolerance_format", lang))
  
  
  year <- country_name <- dem_ep <- aut_ep <- overlap_eps <- country_text_id <- v2clstown <-
    ep_type <- episode <- vdem <- aut_ep_start_year <- aut_ep_end_year <-
    dem_ep_start_year <- dem_ep_end_year <- aut_pre_ep_year <-
    dem_pre_ep_year <- episode_id <- countries <- NULL
  
  eps_year <- eps %>%
    dplyr::filter(country_name == country, dplyr::between(year, min(years), max(years))) %>%
    dplyr::filter(dem_ep == 1 | aut_ep == 1) 
  
  if(nrow(eps_year)>1){
    eps_year <- eps_year %>% 
      dplyr::mutate(overlap_eps = ifelse(!is.na(aut_ep_id) & !is.na(dem_ep_id), "overlaps", NA)) %>% 
      tidyr::pivot_longer(cols = c(aut_ep_id, dem_ep_id, overlap_eps), names_to = "ep_type", values_to = "episode") %>%
      dplyr::select(country_name, country_text_id, year, v2clstown, ep_type, episode,
                    aut_ep_start_year, aut_ep_end_year, aut_ep_outcome,
                    dem_ep_start_year, dem_ep_end_year,
                    aut_pre_ep_year, dem_pre_ep_year, dem_ep_outcome,
                   aut_ep_censored, dem_ep_censored) %>%
      dplyr::filter((ep_type == "dem_ep_id" & dem_pre_ep_year == 0) |
                      (ep_type == "aut_ep_id" & aut_pre_ep_year == 0) |
                      ep_type == "overlaps" & aut_pre_ep_year == 0 & dem_pre_ep_year == 0) %>%
      drop_na(episode) %>%
      group_by(year) %>%
      mutate(overlap_eps = n(),
             outcome_dem_ep = case_when(dem_ep_outcome == 6 ~ get_label("outcome_censored", lang), 
                                        dem_ep_censored == 1 ~ get_label("outcome_censored", lang),
                                        dem_ep_outcome == 1 ~ get_label("outcome_dem_transition", lang),
                                        dem_ep_outcome == 2 ~ get_label("outcome_dem_preempted", lang),
                                        dem_ep_outcome == 3 ~ get_label("outcome_dem_stabilized", lang),
                                        dem_ep_outcome == 4 ~ get_label("outcome_dem_reverted", lang),
                                        dem_ep_outcome == 5 ~ get_label("outcome_dem_deepened", lang),
                                        T ~ NA_character_),
             outcome_aut_ep = case_when(aut_ep_outcome == 6 ~ get_label("outcome_censored", lang),
                                        aut_ep_censored == 1 ~ get_label("outcome_censored", lang),
                                        aut_ep_outcome == 1 ~ get_label("outcome_aut_breakdown", lang),
                                        aut_ep_outcome == 2 ~ get_label("outcome_aut_preempted", lang),
                                        aut_ep_outcome == 3 ~ get_label("outcome_aut_diminished", lang),
                                        aut_ep_outcome == 4 ~ get_label("outcome_aut_averted", lang),
                                        aut_ep_outcome == 5 ~ get_label("outcome_aut_regressed", lang),
                                        T ~ NA_character_),
             episode_id = ifelse(ep_type == "aut_ep_id", paste0("AUT: ", aut_ep_start_year, "-", aut_ep_end_year, " ", outcome_aut_ep), episode),
             episode_id = ifelse(ep_type == "dem_ep_id", paste0("DEM: ", dem_ep_start_year, "-", dem_ep_end_year, " ", outcome_dem_ep), episode_id)) %>%
      ungroup()
    
    polyarchy <- eps %>%
      filter(country_name == country, between(year, min(years), max(years))) %>%
      ungroup() %>%
      select(year, v2clstown)
    
    if(max(eps_year$overlap_eps) > 1) {
      print(get_label("warning_overlap", lang))
    }
    
    p <-   ggplot2::ggplot() +
      geom_line(data = eps_year, aes(group = episode_id, color = episode_id, linetype = ep_type,x = year, y = v2clstown)) +
      geom_line(data = polyarchy, aes(x = year, y = v2clstown), alpha = 0.35) +
      scale_colour_grey(breaks = levels(factor(eps_year$episode_id[eps_year$episode_id!="overlaps"])),
                        name = get_label("episode", lang), start = 0.01, end = 0.01) +
      scale_linetype_manual(name = get_label("episode_type", lang), breaks = c("aut_ep_id", "dem_ep_id", "overlaps"),
                            labels = c(get_label("autocratization", lang), get_label("democratization", lang), get_label("overlap", lang)),
                            values = c("dashed", "dotted", "solid")) +
      scale_x_continuous(breaks = seq(round(min(years) / 10) * 10, round(max(years) / 10) * 10, 10)) +
      xlab(get_label("year", lang)) +  ylab(get_label("edi", lang)) + ylim(0,1) +
      theme_bw() +
      guides(color = guide_legend(override.aes = list(size = 0))) +
      ggtitle(get_country_name(country, lang))
    
    if (isTRUE(length(which(eps_year$ep_type == "dem_ep_id")) > 0)){
      
      if (any(eps_year$year%in%c(eps_year$dem_ep_start_year))) {
        p <- p +  geom_point(data = eps_year, aes(x = year, y = ifelse(year == dem_ep_start_year, v2clstown, NA)), shape = 2, alpha = 0.75) 
        
      } else {
        p
      }
      
      if (any(eps_year$year%in%c(eps_year$dem_ep_end_year))) {
        p <- p +geom_point(data = eps_year, aes(x = year, y = ifelse(year == dem_ep_end_year, v2clstown, NA)), shape = 17, alpha = 0.75)
      } else {
        p
      }
    }
    
    if (isTRUE(length(which(eps_year$ep_type == "aut_ep_id")) > 0)) {
      
      if (any(eps_year$year%in%c(eps_year$aut_ep_start_year))){
        p <- p +  geom_point(data = eps_year, aes(x = year, y = ifelse(year == aut_ep_start_year, v2clstown, NA)), shape = 1, alpha = 0.75) 
      } else {
        p
      }
      if (any(eps_year$year%in%c(eps_year$aut_ep_end_year))){
        p<- p+ geom_point(data = eps_year, aes(x = year, y = ifelse(year == aut_ep_end_year, v2clstown, NA)), shape = 16, alpha = 0.75)
      } else {
        p
      }
    }
    p
    
    
  } else {
    print(get_label("no_episodes", lang))
    
    polyarchy <- eps %>%
      filter(country_name == country, between(year, min(years), max(years))) %>%
      ungroup() %>%
      select(year, v2clstown)
    
    p <-ggplot2::ggplot() +
      geom_line(data = polyarchy, aes(x = as.numeric(year), y = v2clstown), alpha = 0.35) +
      scale_x_continuous(breaks = seq(round(min(years) / 10) * 10, round(max(years) / 10) * 10, 10)) +
      xlab(get_label("year", lang)) +  ylab(get_label("edi", lang)) + ylim(0,1) +
      theme_bw() +
      ggtitle(get_country_name(country, lang))
    
    p
    
  }
}




