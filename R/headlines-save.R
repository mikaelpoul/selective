#' Save headlines as file
#'
#' Write headlines to file.
#'
#' @param folder Which folder to write the files. Defaults to working directory.
#' @param csv_for_data_collector Whether to write .csv-files (which we sendt to our data collector).
#' @param rdata_for_package Whether to write the .RData-file we included in this compendium (\code{data("headlines")}).
#' @keywords headlines prepublication
#'
#' @importFrom utils write.csv
#'
#' @examples
#'\dontrun{
#'  headlines_save()
#' }
#' @export
headlines_save <- function(folder = getwd(),
                           csv_for_data_collector = TRUE,
                           rdata_for_package = TRUE) {

  headlines_hard <- headlines("hard")
  headlines_soft <- headlines("soft")

  ## Experiment 1 ("Breaking dissonance")
  exp_1_hard <- headlines_hard[headlines_hard$party != "none", c("id", "headline")]
  exp_1_soft <- headlines_soft[, c("id", "headline")]

  ## Experiment 2 ("Making dissonance")
  exp_2_hard_1 <- headlines_hard[(headlines_hard$party != "none" | headlines_hard$valance != "neutral"), c("id", "headline")]
  exp_2_hard_2 <- headlines_hard[(headlines_hard$party != "none" | headlines_hard$valance == "neutral"), c("id", "headline")]
  exp_2_hard_3 <- headlines_hard[(headlines_hard$party == "none" | headlines_hard$valance != "neutral"), c("id", "headline")]
  exp_2_hard_4 <- headlines_hard[(headlines_hard$party == "none" | headlines_hard$valance == "neutral"), c("id", "headline")]

  if (csv_for_data_collector) {
    write.csv(exp_1_hard, file = paste0(folder, "/overskrifter_r7pad14_politikk.csv"), row.names = FALSE)
    write.csv(exp_1_soft, file = paste0(folder, "/overskrifter_r7pad14_underholdning.csv"), row.names = FALSE)
    write.csv(exp_2_hard_1, file = paste0(folder, "/overskrifter_selexp2_politikk_1.csv"), row.names = FALSE)
    write.csv(exp_2_hard_2, file = paste0(folder, "/overskrifter_selexp2_politikk_2.csv"), row.names = FALSE)
    write.csv(exp_2_hard_3, file = paste0(folder, "/overskrifter_selexp2_politikk_3.csv"), row.names = FALSE)
    write.csv(exp_2_hard_4, file = paste0(folder, "/overskrifter_selexp2_politikk_4.csv"), row.names = FALSE)
  }

  if (rdata_for_package) {
    save(headlines_hard, headlines_soft,
         exp_1_hard, exp_1_soft,
         exp_2_hard_1, exp_2_hard_2, exp_2_hard_3, exp_2_hard_4,
         file = paste0(folder, "/headlines.RData"))
  }

  invisible(NULL)
}
