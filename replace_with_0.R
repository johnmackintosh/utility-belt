# replace NA with 0
# base R for simplicity

replace_with_0 <- function(x){
    ifelse(is.na(x), 0, x)
}


# dplyr version for speed
replace0 <- function(x){
  dplyr::if_else(condition = is.na(x),
          true = 0,
          false = x)
}

