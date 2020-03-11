is_row_valid <- function(data_row) {
  if (is.na(data_row[['display_name']])) {return(FALSE)}
  if (!stringr::str_to_lower(data_row[['language']]) %in% c("english", "spanish")) {return(FALSE)}
  
  if (is.na(data_row[['today_date']])) {return(FALSE)}
  if (!is.na(data_row[['previous_date']]) &&
      data_row[['previous_date']] > data_row[['today_date']]) {return(FALSE)}
  
  
  if (!data_row[['today_act_score']] %in% 5:25) {return(FALSE)}
  if (!data_row[['previous_act_score']] %in% 5:25) {return(FALSE)}
  
  return(TRUE)
}
