#' age_bander
#'
#' assigns each element in a vector of ages to a specific age band
#' based on the 19 bands commonly in use in Public Health
#'
#' @param df dataframe or tibble
#' @param x column to use as a base for age_band
#' @param factored convert age_band to ordered factor?
#'
#' @return
#' @export
#'
#' @examples
age_bander <- function(df,
                       x,
                       factored = TRUE,
                       printout = FALSE){

  df <- df %>%
    dplyr::mutate(age_band =
                    dplyr::case_when(
                      {{x}} %in% c(0:4) ~ "00-04",
                      {{x}} %in% c(5:9) ~ "05-09",
                      {{x}} %in% c(10:14) ~ "10-14",
                      {{x}} %in% c(15:19) ~ "15-19",
                      {{x}} %in% c(20:24) ~ "20-24",
                      {{x}} %in% c(25:29) ~ "25-29",
                      {{x}} %in% c(30:34) ~ "30-34",
                      {{x}} %in% c(35:39) ~ "35-39",
                      {{x}} %in% c(40:44) ~ "40-44",
                      {{x}} %in% c(45:49) ~ "45-49",
                      {{x}} %in% c(50:54) ~ "50-54",
                      {{x}} %in% c(55:59) ~ "55-59",
                      {{x}} %in% c(60:64) ~ "60-64",
                      {{x}} %in% c(65:69) ~ "65-69",
                      {{x}} %in% c(70:74) ~ "70-74",
                      {{x}} %in% c(75:79) ~ "75-79",
                      {{x}} %in% c(80:84) ~ "80-84",
                      {{x}} %in% c(85:89) ~ "85-89",
                      {{x}} >= 90 ~ "90+",
                      TRUE ~ NA_character_)
    )

if (factored) {
  df <- df %>%
    dplyr::mutate(age_band = factor(age_band,
                             levels = c("00-04",
                                        "05-09",
                                        "10-14",
                                        "15-19",
                                        "20-24",
                                        "25-29",
                                        "30-34",
                                        "35-39",
                                        "40-44",
                                        "45-49",
                                        "50-54",
                                        "55-59",
                                        "60-64",
                                        "65-69",
                                        "70-74",
                                        "75-79",
                                        "80-84",
                                        "85-89",
                                        "90+"),
                             ordered = TRUE)
    )
}


df

}
