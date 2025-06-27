
huc <- nhdplusTools::get_huc(id = 17, type = "huc02")
huc10 <- nhdplusTools::get_huc(AOI = huc, type = "huc10")
nhd_data <- nhdplusTools::get_nhdplus(AOI = huc, realization = "flowline")
save(
  nhd_data,
  huc10,
  file = here::here("data-raw/huc_comid_xref/nhd_data.RData")
)




# original way of doing it, but downloading all stream temp hucs took
# too much memory on my machine
# huc_dirs <- dir(system.file(package = pkg_name))
#
# huc_comid_xref <- lapply(huc_dirs[1], \(x) {
#   huc_comid_files <- dir(get_pkg_huc_dir(x), pattern = "\\.csv")
#   cat("\n")
#   huc_comid_data <- lapply(huc_comid_files, \(y) {
#     cat(".")
#     huc10 <- gsub("st_pred_|\\.csv", "", y)
#     huc_comid_data <-
#       readr::read_csv(
#         paste0(get_pkg_huc_dir(x), "/", y),
#         col_select = "COMID",
#         show_col_types = FALSE,
#         progress = FALSE
#       ) |>
#       dplyr::distinct() |>
#       dplyr::rename(comid = COMID) |>
#       dplyr::mutate(huc = huc10) |>
#       dplyr::select(huc, comid)
#     return(huc_comid_data)
#   }) |> do.call(what = "rbind")
# }) |> do.call(what = "rbind")

usethis::use_data(
  huc_comid_xref,
  overwrite = TRUE
)
