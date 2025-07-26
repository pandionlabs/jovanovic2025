#' Make a count table by decay stage
#'
#' @param TreMs A TreMs dataframe
#' @param save_name Path to save the dataframe
#'
#' @returns A count table dataframe
#'
#' @export
#' @examples
#' \dontrun{
#' make_count_table_by_decay(TreMs, save_name = "Count_Table_by_Decay_Stage.csv")
#' }
make_count_table_by_decay <- function(TreMs, save_name = "data/derivatives/Count_Table_by_Decay_Stage.csv") {
  count_table <- as.data.frame(table(TreMs$TreeIdentities2, TreMs$Treedata.Tree_Decay))
  # Rename columns for clarity
  colnames(count_table) <- c("TreeIdentities2", "Decay_Stage", "Count")

  # Save to CSV
  write.table(count_table, file = save_name, sep = ",", quote = FALSE, row.names = FALSE)
  return(count_table)
}