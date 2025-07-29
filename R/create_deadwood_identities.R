#' Deadwood identities - Create a column that combines species and type of deadwood
#'
#' @param TreMs A TreMs dataframe
#'
#' @returns A TreMs dataframe with a DeadwoodIdentities column
#'
#' @examples
#' \dontrun{
#' TreMs <- create_deadwood_identities(TreMs)
#' }
create_deadwood_identities <- function(TreMs) {
  #Creating deadwood identities 
  TreMs |> 
    dplyr::mutate(DeadwoodIdentities_species = 
      dplyr::case_match(
        Treedata.Treespecies,
         c("Fagus sylvatica") ~ "F. sylvatica",
         c("Picea abies") ~ "P. abies",
         c("Abies alba") ~ "A. alba",
         c("Dead broadleaf","Tilia cordata") ~ "Broadleaf",
         c("Dead conifer","Pinus sylvestris","Larix decidua") ~ "Conifer",
         c("Dead no identification") ~ "No ID",
         .default = "No ID"
      ),
      DeadwoodIdentities_type = 
        dplyr::case_match(
          Treedata.Type_of_deadwood,
          c("Stump (<1.3m) (natural)", "Stump (<1.3m) (artificial)") ~ "Stump",
          c("Entire lying tree (natural)", "Entire lying tree (artificial)") ~ "Entire Tree",
          c("Log/piece of wood (natural)", "Log/piece of wood (artificial)") ~ "Log",
          .default = "Log"
        ),
        DeadwoodIdentities = paste(DeadwoodIdentities_species, DeadwoodIdentities_type)
      ) |> 
    dplyr::select(-c(DeadwoodIdentities_species, DeadwoodIdentities_type)) |> 
    dplyr::relocate(DeadwoodIdentities, .before = 8)
 
}



