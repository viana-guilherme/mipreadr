#' calculates all of the biological replicate means
#' @param inputlist A vector of list names to be collapsed
#' @export
#'
biologicalReps <- function(inputlist) {

  all_bio_reps <- NULL

  # collecting all of the technical replicates data
  # into a single larger data frame

  for(exp in 1:length(inputlist)) {
    all_bio_reps <- dplyr::bind_rows(all_bio_reps,
                                     inputlist[[exp]]$collapsedTechnicalReps)
  }

    # suppressMessages(all_bio_reps
  collapsed_bio_reps <- purrr::map_dfr(.x = unique(all_bio_reps$Repeat), ~ {

    all_bio_reps |>
    dplyr::group_by(Sample, Condition) |>
    dplyr::filter(Repeat == .x) |>
    dplyr::summarise(BiologicalRepMean = mean(Mean),
                       BiologicalRepSD = sd(Mean)) |>
    dplyr::mutate(Repeat = .x)
    }) |>
    dplyr::ungroup()

  return(collapsed_bio_reps)

}
