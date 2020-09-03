is_row_valid <- function(data_row) {
  if (is.na(data_row[['display_name']])) {return(FALSE)}
  if (!stringr::str_to_lower(data_row[['language']]) %in% c("english", "spanish")) {return(FALSE)}
  
  if (is.na(data_row[['today_date']])) {return(FALSE)}
  if (!is.na(data_row[['previous_date']]) &&
      data_row[['previous_date']] > data_row[['today_date']]) {return(FALSE)}
  
  
  if (!data_row[['today_act_score']] %in% 5:25) {return(FALSE)}
  if (!data_row[['previous_act_score']] %in% 5:25) {return(FALSE)}
  
  # make sure display name contains letters
  # unicode characters
  # https://stackoverflow.com/questions/2385701/regular-expression-for-first-and-last-name
  if (!stringr::str_detect(data_row[['display_name']], "^[\\p{L}'][ \\p{L}'-]*[\\p{L}]$")) {return(FALSE)}
  
  return(TRUE)
}
