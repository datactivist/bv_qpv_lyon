#' Trouve le code Insee d'une commune
#'
#' @param nom_commune le nom de la commune recherchée
#' @param code_departement le code du département dans lequel se trouve la commune (optionnel)
#' @return le code insee de la commune recherchée (ou `NA_character` si l'API ne trouve pas)
#' @export
#'
match_commune <- function(nom_commune, code_departement = NA) {
  tryCatch({
    req <- httr::GET(paste0("https://geo.api.gouv.fr/communes?nom=", URLencode(nom_commune), ifelse(is.na(code_departement), "", paste0("&codeDepartement=", URLencode(code_departement))), "&boost=population"))
    httr::warn_for_status(req)
    httr::stop_for_status(req)
    httr::content(req, as = "parsed")[[1]]$code
  }, error = function(e) NA_character_) # l'API renvoie une réponse vide quand elle n'a pas de réponse. On ne peut pas la parser donc on attrape les erreurs et on renvoie une valeur manquante
}
