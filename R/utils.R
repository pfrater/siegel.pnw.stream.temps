pkg_name <- "siegel.pnw.stream.temps"

get_pkg_huc_dir <- function(huc) {
  huc <- substr(huc, 1, 6)
  system.file(huc, package = pkg_name)
}

#' Convert COMID to HUC and vice versa
#'
#' These are simple helper functions that take either a comid or huc as an
#' argument and return the other. COMIDs are nested with HUCs, so
#' \code{comid_to_huc} is 1-to-1, but \code{huc_to_comid} is 1-to-many.
#'
#' @param comids Any valid comid. See huc_comid_xref$comid for options
#' @param huc.level The huc.level to return. Defaults to 6 (i.e. HUC6)
#'
#' @returns A vector of resulting HUCs (for \code{comid_to_huc}) or COMIDs
#' (for \code{huc_to_comid})
#' @export
#' @name comid_to_huc
#'
#' @examples
#' comid_to_huc(23940175)
comid_to_huc <- function(comids, huc.level = 6) {
  out <-
    siegel.pnw.stream.temps::huc_comid_xref |>
    dplyr::filter(comid %in% comids) |>
    dplyr::mutate(huc = substr(huc, 1, huc.level)) |>
    dplyr::pull(huc) |>
    unique()
  return(out)
}

#' @rdname comid_to_huc
#' @export
huc_to_comids <- function(hucs) {
  if (nchar(hucs) > 12) {
    warning (
      "Apologies. I can only go to HUC10 levels and will now trim ",
      "your HUC to 12 digits"
    )
    hucs <- substr(hucs, 1, 12)
  }
  out <-
    siegel.pnw.stream.temps::huc_comid_xref |>
    dplyr::mutate(huc = substr(huc, 1, nchar(hucs))) |>
    dplyr::filter(huc %in% hucs) |>
    dplyr::pull(comid) |>
    unique()
  return(out)
}
