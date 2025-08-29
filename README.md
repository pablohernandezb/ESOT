

An R package to load, explore, and work with Episodes of State Ownership Transformation (ESOT) using the V-Dem dataset—based on the original Episodes of Regime Transformation (ERT) project by the [V-Dem Institute](https://www.v-dem.net/). This forked version includes support for localization in Spanish (`"es"`) and uses the variable `v2clstown_osp` (State Ownership of Economy) to identify episodes of privatization and statization.

## Episodes of State Ownership Transformation (ESOT) ##

#### Load, explore, and work with the ESOT dataset (for details see also the [ERT Codebook](https://github.com/vdeminstitute/ERT/blob/master/inst/ERT_codebook.pdf)): ####

* NOTE: For non-R users, we provide [the V-Dem dataset as csv or .xlsx file](https://github.com/vdeminstitute/ERT/blob/master/inst). However, we recommend loading the dataset via the package to flexibly set parameters for generating episodes.
* RELEASES: This package is based on the V-Dem dataset v15. For earlier releases, see the "Releases" section in the original repository.

#### Functions ####
* `get_eps`: Identify episodes of state ownership transformation (privatization and statization) in the most recent V-Dem data set, using the `v2clstown_osp` variable. Privatization is defined as any movement towards less state ownership, while statization is any movement towards more state ownership.
* `find_overlap`: Find potential overlaps between episodes of privatization and statization, depending on how the thresholds are set.
* `plot_episodes`: Plot episodes of state ownership transformation over time for a selected country.
* `plot_all`: Plot the share or absolute number of all countries in episodes of state ownership transformation over time.

#### ShinyApp for validity tests ####

* For additional transparency, we provide a [ShinyApp for validation](https://episodes.shinyapps.io/validation/) which allows users to flexibly adjust the ESOT parameters and test how changes to the default thresholds affect the episodes and their validity.

## Installation ##

```r
# Install the development version of the ESOT package 
# (this package is an ongoing project, 
# keep checking for updates)

# First, you need to have the devtools package installed
install.packages("devtools")
# now, install the package directly from GitHub
devtools::install_github("pablohernandezb/ESOT")

# NOTE: Make sure you have an updated R version and
# - since the package is a development version - 
# an updated version of xcode (Mac), rtools (Windows), r-base-dev (Linux)
# installed. If you have troubles with the installation 
# write to contact@v-dem.net at the V-Dem Institute.
```

## Localization ##

This fork supports localization for plot labels and country names in English (`"en"`) and Spanish (`"es"`). You can set the language for labels and country names using the `lang` argument in plotting functions. The default is English (`"en"`).

#### Internal Functions ####

* `get_label`: Retrieve localized labels for plot elements and messages, supporting multiple languages.
* `get_country_name`: Retrieve localized country names for use in plots and summaries.

**Example:**
```r
# Plot all episodes of privatization and statization in Spanish
plot_all(lang = "es")

# Plot the privatization and statization episodes for Venezuela in Spanish
plot_episodes(country = c("Venezuela"), years = c(1900, 2023), lang = "es")
```

## Citation ##

If you use this package, please cite the creators of the ERT dataset:

- Lührmann, Anna, and Staffan I. Lindberg. "A third wave of autocratization is here: what is new about it?" Democratization 26.7 (2019): 1095-1113. [Link](https://www.tandfonline.com/doi/full/10.1080/13510347.2019.1582029)
- Maerz, Seraphine F., et al. "Episodes of Regime Transformation (ERT): A new dataset for studying democratization and autocratization." Journal of Peace Research 60.4 (2023): 681-695. [Link](https://journals.sagepub.com/doi/10.1177/00223433231168192)
- Wilson, Matthew Charles, et al. "Episodes of liberalization in autocracies: A new approach to quantitatively studying democratization." Political Science Research and Methods 10.2 (2022): 279-296. [Link](https://www.cambridge.org/core/journals/political-science-research-and-methods/article/episodes-of-liberalization-in-autocracies-a-new-approach-to-quantitatively-studying-democratization/CD86064BF11FEEC8BD9354921E3C9BE3)

For more details, see the documentation. Feel free to reach out <hi@pablohernandezb.dev> if you have any recommendations, comments, or questions!