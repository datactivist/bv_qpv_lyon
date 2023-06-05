#' Importe les résultats de l'élection présidentielle de 2017
#'
#' @param url le lien vers le fichier brut à importer
#' @param columns les colonnes à garder et leur typage (voir la valeur par défaut)
#' @param labels vecteur indiquant le numéro des colonnes contenant les étiquettes à partir desquelles les résultats sont agrégés. Généralement, leur espacement est régulier, donc peut s'écrire sous la forme de, e.g., seq(10, 100, 10).
#' @param gap décalage entre les colonnes avec les étiquettes et les colonnes avec le nombre de voix. Par défaut, 3.
#' @return un `tibble` avec les résultats en format long
#' @export
#'
import_pres <- function(file, 
                        columns = list(`Code du département` = col_character(),
                                    `Libellé du département` = col_character(), 
                                    `Code de la circonscription` = col_character(), 
                                    `Code de la commune` = col_character(), 
                                    `Libellé de la commune` = col_character(), 
                                    `Code du b.vote` = col_character(), 
                                    Inscrits = col_integer(), 
                                    Abstentions = col_integer(), 
                                    `Exprimés` = col_integer(), 
                                    Blancs = col_integer(), 
                                    Nuls = col_integer(), 
                                    Nom = col_character(), 
                                    Voix = col_integer()),
                        labels = seq(24, 94, 7),
                        gap = 2) {
  encoding <- readr::guess_encoding(readr::read_lines_raw(file, n_max = 1000))[[1, "encoding"]]
  pres <- readr::read_csv2(file, locale = locale(encoding = encoding), col_types = columns)
  
  pres_cleaned <- LireMinInterieur::lire(pres, keep = names(columns)[-which(names(columns) %in% c("Nom", "Voix"))], col = labels, gap = gap)
  
  pres_cleaned <- pres_cleaned %>% 
    # put geographical codes in the right format
    mutate(CodeDepartement = str_pad(string = `Code du département`, width = 2, side = "left", pad = "0")) %>% # has to be in a format like "02"
    # pas mal de refactoring à faire encore ici
    mutate(NumeroCirco = str_pad(`Code de la circonscription`, 2, "left", "0")) %>%
    mutate(CodeCirco = paste0(CodeDepartement, NumeroCirco)) %>% 
    mutate(CodeCommune = str_pad(string = `Code de la commune`, width = 3, side = "left", pad = "0")) %>% 
    mutate(CodeInsee = paste0(CodeDepartement, CodeCommune)) %>%  # unique commune ID
    # computing missing values
    mutate(NumeroBV = str_pad(`Code du b.vote`, width = "4", side = "left", pad = "0")) %>% 
    mutate(CodeBV = paste0(CodeInsee, NumeroBV)) %>% 
    mutate(Votants = Inscrits - Abstentions) %>% 
    mutate(Votants_ins = Votants / Inscrits * 100) %>% 
    mutate(Abstentions_ins = Abstentions / Inscrits * 100) %>% 
    mutate(Blancs_ins = Blancs/ Inscrits * 100) %>% 
    mutate(Blancs_vot = Blancs / Votants * 100) %>% 
    mutate(Nuls_ins = Nuls / Inscrits * 100) %>% 
    mutate(Nuls_vot = Nuls / Votants * 100) %>% 
    mutate(Exprimés_ins = Exprimés / Inscrits * 100) %>% 
    mutate (Exprimés_vot = Exprimés / Votants * 100) %>% 
    # specify integers %>% 
    mutate_at(vars(Inscrits, Abstentions, Votants, Blancs, Nuls, Exprimés, `DUPONT-AIGNAN`:FILLON), as.integer) %>% 
    # reorder
    select(CodeBV, CodeInsee, CodeDepartement, Département = `Libellé du département`, CodeCirco, Commune = `Libellé de la commune`, NumeroBV, Inscrits, Abstentions, Abstentions_ins, Votants, Votants_ins, Blancs, Blancs_ins, Blancs_vot, Nuls, Nuls_ins, Nuls_vot, Exprimés, Exprimés_ins, Exprimés_vot, `DUPONT-AIGNAN`:FILLON, `DUPONT-AIGNAN.ins`:FILLON.exp) %>% 
    # nicer, more modern dataframe class
    as_tibble()
}