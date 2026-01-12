#' Create a summary table for TreMs data
#'
#' @param x TreMs dataframe
#' @param grouping_variable variable to be used for grouping
#'
#' @returns A summary table
#'
#' @export
#' @examples
#' \dontrun{
#' TreeIdentitiesSummary <- summarize_identities(
#' TreMs, 
#' grouping_variable = TreeIdentities2
#' )
#' }
summarize_identities <- function(x, grouping_variable) {
  Freq <- Treedata.DBH_cm <- Abundance <- Richness <- NULL
  var <- rlang::enquo(grouping_variable)
  x |> 
    dplyr::group_by(!!var) |> 
    dplyr::summarise(
      Freq = dplyr::n(),
      Percentage = (Freq/nrow(x))*100,
      Treedata.DBH_cm.mean = mean(Treedata.DBH_cm),
      Treedata.DBH_cm.sd = stats::sd(Treedata.DBH_cm),
      Treedata.DBH_cm.min = min(Treedata.DBH_cm),
      Treedata.DBH_cm.max = max(Treedata.DBH_cm),
      Abundance.mean = mean(Abundance),
      Abundance.sd = stats::sd(Abundance),
      Abundance.max = max(Abundance),
      Richness.mean = mean(Richness, na.rm = TRUE),
      Richness.sd = stats::sd(Richness, na.rm = TRUE),
      Richness.max = max(Richness)
    )
}