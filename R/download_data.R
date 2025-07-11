
#' Download stream temperature data for a specific HUC-6 watershed
#'
#' This function will download data from
#' [Siegel et al. 2023. Daily stream temperature predictions for free-flowing streams in the Pacific Northwest](https://zenodo.org/records/8174951),
#' extract it, and embed it within the package directory structure so that it
#' is accessible by \code{\link{read_stream_temps}}
#'
#' @param hucs A HUC watershed. Can be any level--will get truncated to HUC6
#' @param comids Any COMID that can be found in \code{huc_comid_xref}
#' @param timeout If timeout errors are experienced on download, this argument
#' can be increased
#'
#' @returns Downloads a file from (Siegel et al. 2023. Daily stream temperature
#' predictions for free-flowing streams in the Pacific
#' Northwest)[https://zenodo.org/records/8174951]
#' @export
#' @name download_data
#'
#' @examples
#' \dontrun{
#'   # these download files of .75 to 4GB, so can take a bit
#'   download_huc_data(hucs = 170103)
#'   download_huc_data(comid = 23940175)
#' }
download_huc_data <- function(hucs = NULL, comids = NULL, timeout = 300) {
  if (is.null(hucs)) {
    if (is.null(comids)) {
      stop("You must supply either a 6-digit HUC or a COMID")
    } else {
      hucs <- comid_to_huc(comids)
    }
  } else {
    hucs <- substr(hucs, 1, 6)
  }
  if (!any((hucs %in% pnw_hucs))) {
    stop(
      "You must supply a HUC6 that is found in pnw_hucs ",
      "or a COMID that is nested within such"
    )
  }
  if (!is.null(timeout)) {
    default_timeout_option <- getOption("timeout")
    on.exit(options(timeout = default_timeout_option))
    options(timeout = timeout)
  }
  out <- lapply(hucs, \(x) {
    pnw_huc_file <- paste0("st_pred_", x, ".7z")
    pnw_huc_url <- paste0(siegel_pnw_url, pnw_huc_file, "?download=1")
    huc_dir <- get_pkg_huc_dir(x)
    huc_dir_file <- paste0(huc_dir, "/", pnw_huc_file)
    if (!dir.exists(huc_dir)) {
      make_huc_dir <- function(hucdir) {
        dir.create(paste0(system.file(package = pkg_name), "/", hucdir))
      }
      make_huc_dir(x)
      # stupid hack workaround to deal with permissions error when downloading
      # file to freshly created directory
      huc_dir <- get_pkg_huc_dir(x)
      current_wd <- getwd()
      setwd(huc_dir)
      setwd(current_wd)
    }
    download.file(
      url = pnw_huc_url,
      destfile = huc_dir_file
    )
    cat("Extracting files; will take a bit...\n")
    test <- tryCatch({
      archive::archive_extract(
        archive = huc_dir_file,
        huc_dir
      )
    }, error = function(e) return(NULL))
  })
}

#' @rdname download_data
#' @export
download_all_hucs <- function(timeout = 1000) {
  download_huc_data(hucs = pnw_hucs, timeout = timeout)
}
