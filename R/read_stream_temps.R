#' Read stream temperature data for a specific COMID or HUC and date range
#'
#' @param comid The NDHPlus COMID for a stream reach. Anything found in
#' huc_comid_xref$comid is a valid option
#' @param start.date Start date entered as "YYYY-MM-DD". If left NULL (default)
#' it will either return everything prior to the end.date or the full range of
#' dates (if end.date = NULL)
#' @param end.date End date entered as "YYYY-MM-DD". If left NULL (default)
#' it will either return everything after the start.date or the full range of
#' dates (if start.date = NULL)
#' @param huc Optionally. Can enter any HUC from HUC6 to HUC10. Anything found
#' in huc_comid_xref$huc is a valid option. Entering a huc without a comid
#' will return all COMIDs for that HUC level (be careful...you might get a lot
#' of data back)
#'
#' @returns A data.frame of predicted stream temperatures by date for each
#' comid
#' @export
#'
#' @examples
#' \dontrun{
#'  temps <- read_stream_temps(comid = 22878865, start.date = "2015-01-01", end.date = "2015-12-31")
#'  temps2 <- read_stream_temps(comid = 22878865, start.date = "2019-01-01")
#'  temps3 <- read_stream_temps(comid = 22878865, end.date = "1990-03-31")
#' }
read_stream_temps <- function(
    comid = NULL,
    start.date = NULL,
    end.date = NULL,
    huc = NULL
) {
  if (is.null(huc)) {
    if (is.null(comid)) {
      stop("You must supply either a HUC or a COMID")
    } else {
      huc <- comid_to_huc(comid)
    }
  } else {
    if (is.null(comid)) {
      comid <- huc_to_comids(huc)
    }
  }
  huc_sixes <- comid_to_huc(comid)
  huc_tens <- comid_to_huc(comid, huc.level = 10)
  out <- lapply(huc_sixes, \(x) {
    lapply(huc_tens, \(y) {
      filename <- paste0(
        get_pkg_huc_dir(x),
        "/st_pred_", y, ".csv"
      )
      browser()
      callback_fun <-
        if (!is.null(comid) & !is.null(start.date) & !is.null(end.date)) {
          \(x, pos) {dplyr::filter(x, COMID %in% comid, tim.date %in% date_seq)}
        } else if (!is.null(comid) & !is.null(start.date) & is.null(end.date)) {
          \(x, pos) {dplyr::filter(x, COMID %in% comid, tim.date > start.date)}
        } else if (!is.null(comid) & is.null(start.date) & !is.null(end.date)) {
          \(x, pos) {dplyr::filter(x, COMID %in% comid, tim.date < end.date)}
        } else if (!is.null(comid) & is.null(start.date) & is.null(end.date)) {
          \(x, pos) {dplyr::filter(x, COMID %in% comid)}
        } else if (is.null(comid) & !is.null(start.date) & !is.null(end.date)) {
          \(x, pos) {dplyr::filter(x, tim.date %in% date_seq)}
        } else if (is.null(comid) & !is.null(start.date) & is.null(end.date)) {
          \(x, pos) {dplyr::filter(x, tim.date > start.date)}
        } else if (is.null(comid) & is.null(start.date) & !is.null(end.date)) {
          \(x, pos) {dplyr::filter(x, tim.date < end.date)}
        }
      if (is.null(comid) & is.null(start.date) & is.null(end.date)) {
        x_y_data <- readr::read_csv(filename, show_col_types = FALSE)
      } else {
        if (!is.null(start.date) & !is.null(end.date)) {
          date_seq <- seq(
            lubridate::as_date(start.date),
            lubridate::as_date(end.date),
            by = 1
          )
        }
        x_y_data <-
          readr::read_csv_chunked(
            filename,
            callback = readr::DataFrameCallback$new(callback_fun),
            show_col_types = FALSE
          )
      }
      return(x_y_data)
    }) |> do.call(what = "rbind")
  }) |> do.call(what = "rbind")
  out <- dplyr::rename(out, date = "tim.date")
  return(out)
}
