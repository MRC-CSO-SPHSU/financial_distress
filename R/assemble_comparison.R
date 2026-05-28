assemble_comparison <- function(ltmle_results, mi_results, iptw_results) {
  ltmle_results |>
    dplyr::left_join(mi_results,   by = "intervention") |>
    dplyr::left_join(iptw_results, by = "intervention") |>
    dplyr::relocate("intervention")
}
