#'  Quickly adds totals to a dataframe, preserving the class of existing columns
#'
#' @param .df  a dataframe or tibble
#' @param total_text the text you want to appear in the total row
#' @param col_position where you want the total text to appear, assuming there is an existing column of categorical variables to sum.
#' In this version, it will appear in the first (left hand )column
#'
#' @return modified dataframe with column totals
#' @export
#'
#' @examples
#'df <- data.frame(
#'trt = c("A","B","C", "D"),
#'resp = c(1, 5, 3, 4),
#'group = c(1,2,1,2),
#'upper = c(1.1, 5.3, 3.3, 4.2),
#'lower = c(0.8, 4.6, 2.4, 3.6),
#'scotland = 3.2,
#'council = 4)
#' table_totals(df)
#' df |> table_totals()

table_totals <- function(.df,
                         total_text = "Total",
                         col_position = "first") {
  # needs checking
  stopifnot("requires data.frame input" = inherits(.df, "data.frame"))
  stopifnot("total_text must be a character string" = inherits(total_text, "character"))
  stopifnot(inherits(col_position, "character"))
  
  # is it a tibble?
  
  tibble_flag <- inherits(.df, "tbl_df") | inherits(.df, "tbl")
  
  # if so, convert to data.frame
  if (tibble_flag) {
    .df <- as.data.frame(.df)
  }
  
  int_str  <- dim(.df) # dimensions of data
  
  stopifnot("input dataframe must contain at least one row of data" = int_str[1] >= 1L)
  
  new_row <- int_str [1] + 1 #row number for new row
  
  out <- .df
  
  if (col_position == "first") {
    # if existing categorical variables are in the first column , then sum columns 2: number of columns in dataframe
    
    cols_to_sum <- c(2:int_str[2])
    
    out[new_row, cols_to_sum]  = colSums(out[, cols_to_sum])
    
    out[new_row, 1] <- total_text
    
  }
  
  if (!tibble_flag) {
    return(out)
  }
  
  # if it was originally a tibble, switch it back
  
  if (tibble_flag) {
    out <- tibble::as_tibble(out)
    return(out)
  }
  
}
