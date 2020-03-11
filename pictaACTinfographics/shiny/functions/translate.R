translate_eng_mo_to_sp <- function(eng_mo) {
  eng_mo <- stringr::str_to_lower(eng_mo)
  switch(eng_mo,
         'january' = 'enero',
         'february' = 'febrero',
         'march' = 'marzo',
         'april' = 'abril',
         'may' = 'mayo',
         'june' = 'junio',
         'july' = 'julio',
         'august' = 'agosto',
         'september' = 'septiembre',
         'october' = 'octubre',
         'november' = 'noviembre',
         'december' = 'diciembre',
         stop(glue::glue("Unknown english month to translate into spanish: {eng_mo}"))
  )
}
