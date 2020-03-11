gen_asthma_date_blob <- function(date, language) {
  if (language == "spanish") {
    translated_mo <- translate_eng_mo_to_sp(strftime(date, "%B"))
    format_eng <- strftime(date,
                           glue::glue("Fecha:  %d de {translated_mo}, %Y"))
  } else {
    return(strftime(date, "Date:  %B %d, %Y"))
  }
}

gen_asthma_interpretive_statement_blob <- function(today_act_score, language) {
  if (today_act_score %in% 5:15) {
    if (language == "spanish") {
      return("Su asma está muy mal controlada")
    } else {
      return("Your asthma is very poorly controlled")
    }
  } else if (today_act_score %in% 16:19) {
    if (language == "spanish") {
      return("Su asma está mal controlada")
    } else {
      return("Your asthma is not well controlled")
    }
  } else if (today_act_score %in% 20:25) {
    if (language == "spanish") {
      return("Su asma está bien controlada")
    } else {
      return("Your asthma is well controlled")
    }
  } else {
    stop(sprintf("Invalid act score given. Expected 5-25, got %s", today_act_score))
  }
}

gen_asthma_progress_statment <- function(today_act_score, previous_act_score, language) {
  if (previous_act_score - today_act_score >= 3) {
    if (language == "spanish") {
      return("Ha empeorado desde su última visita.")
    } else {
      return("It has gotten worse since your last visit.")
    }
  } else if (today_act_score <= 19 && abs(previous_act_score - today_act_score) <= 2) {
    if (language == "spanish") {
      return("Sigue igual que desde su última visita.")
    } else {
      return("It is about the same as at your last visit.")
    }
  } else if (today_act_score >= 20 &&
             (today_act_score - previous_act_score >= 0 || previous_act_score - today_act_score >= 2)) {
    return("")
  } else if (today_act_score <= 19 && today_act_score - previous_act_score >= 3) {
    if (language == "spanish") {
      return("Ha habido mejora!")
    } else {
      return("You made good progress!")
    }
  } else if (today_act_score >= 20 && previous_act_score <= 19) {
    if (language == "spanish") {
      return("Muy bien!")
    } else {
      return("Great job!")
    }
  } else {
    stop("Unknown progress statement condition")
  }
}
