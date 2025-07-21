#' Title
#'
#' @param count_column
#' @param save_name
#'
#' @returns
#'
#' @export
#' @examples
make_count_table <- function(count_column, save_name) {
  fs::dir_create(fs::path_dir(save_name))
  table(count_column) |> 
  as.data.frame() |> 
    write.table(
      file = save_name, 
      sep = ",", 
      quote = FALSE, 
      row.names = FALSE
    )
}