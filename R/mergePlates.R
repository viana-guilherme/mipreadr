#' calculates all of the biological replicate means
#' @param inputlist A vector of list names to be collapsed
#' @export
#'
mergePlates <- function(inputlist) {

  # collecting all of the technical replicates data into a single - larger - data frame
  all_bio_reps <- NULL

  for (exp in 1:length(inputlist)) {
    all_bio_reps <- dplyr::bind_rows(all_bio_reps, inputlist[[exp]]$collapsedTechnicalReps)
  }

  # calculate mean and standard deviation for numeric variables on a sample basis
  suppressMessages(
    bio_reps <- purrr::map_dfr(.x = unique(all_bio_reps$Repeat), ~ {
      all_bio_reps |>
      dplyr::group_by(Sample, Condition) |>
      dplyr::filter(Repeat == .x) |>
      dplyr::summarise(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric) & !c(Replicate, Repeat),
                                     list(Mean = ~mean(.x), SD = ~sd(.x)),
                                     .names = "{.col}_{.fn}")) |>
      dplyr::mutate(Repeat = .x) |>
      dplyr::ungroup() }) |>
      dplyr::relocate(Repeat, .after = Condition)
    )

  return(bio_reps)
}
