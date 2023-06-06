#' Créer des BV à partir de la liste électorale
#'
#' @param liste la liste électorale (telle qu'issue de la lecture d'un fichier extrait du REU)
#' @param code_insee_commune le code insee de la commune concernée
#' @param seuil le seuil de conservation des adresses (entre 0 et 1). Plus il est proche de 1 plus on risque d'avoir des bizarreries dans le contour des BV
#' @param code_bv la variable contenant le code du bureau de vote 
#' @param code_insee la variable contenant le code insee de la commune
#' @param numero_voie la variable contenant le numéro dans la voie
#' @param libelle_voie la variable contenant le libellé de la voie
#'
#' @return un objet sf
#' @export
#'
create_bv <- function(liste, code_insee_commune, seuil = 0.9, code_bv = `numéro du bureau de vote`, code_insee = `Code INSEE`, numero_voie = NumeroVoie, libelle_voie = LibelleVoie) {
  
  # ne garder que les lignes uniques
  liste <- liste %>% distinct()
  
  # ne garder que la commune considérée
  liste <- liste %>% 
    filter({{code_insee}} %in% code_insee_commune)
  
  # obtenir le contour de la commune
  contour <- httr::GET(paste0("https://geo.api.gouv.fr/communes/", code_insee_commune, "?geometry=contour&format=geojson"))
  enveloppe <- sf::st_read(content(contour, as = "text"), quiet = TRUE)
  enveloppe <- enveloppe %>% 
    select(-codesPostaux)

  # s'il n'y a qu'un BV dans la commune, renvoyer la commune
  
  if (liste %>% distinct({{code_bv}}) %>% nrow() %in% 1) {
    return(
      st_sf(bureau_vote_id = paste0(code_insee_commune, "0001"), voronois = enveloppe$geometry, sf_column_name = "voronois")
    )
  }
  
  
  # géocoder les adresses
  message("géocodage des adresses en cours")
  data_geocode <- liste %>% 
    mutate(bv = as.character({{code_bv}})) %>% 
    mutate(adresse = paste0({{numero_voie}}, " ", {{libelle_voie}})) %>% 
    banR::geocode_tbl(adresse = adresse, code_insee = {{code_insee}})
  
  data_geocode <- data_geocode %>% 
    filter(!is.na(longitude), !is.na(latitude)) %>% 
    st_as_sf(coords = c("longitude", "latitude")) %>% 
    st_set_crs(4326)
  
  data_geocode <- data_geocode %>% 
    filter(result_score >= seuil) %>% 
    distinct(result_id, .keep_all = TRUE) %>% 
    st_intersection(enveloppe) # on s'assure de ne pas avoir de point en dehors de la commune
  
  
  voronois <- data_geocode %>% 
    st_union() %>% 
    st_voronoi() %>% 
    st_collection_extract()
  
  voronois <- voronois[unlist(st_intersects(data_geocode, voronois))]
  
  voronois <- st_intersection(voronois, enveloppe)
  
  data_geocode$voronois <- voronois
  
  adresses <- data_geocode %>% 
    st_set_geometry("voronois")
  
  # contrôle visuel
  # adresses %>% 
  #   group_by(bv) %>% 
  #   summarise %>% 
  #   mapview::mapview()
  
  adresses %>% 
    mutate(bureau_vote_id = str_trim(bv),
           bureau_vote_id = str_pad(bureau_vote_id, 4, "left", "0"),
           bureau_vote_id = paste0(code_insee_commune, bureau_vote_id)) %>% 
    group_by(bureau_vote_id) %>% 
    summarise
}
