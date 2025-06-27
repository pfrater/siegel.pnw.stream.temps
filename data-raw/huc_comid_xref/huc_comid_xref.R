
devtools::load_all()

load(here::here("data-raw/huc_comid_xref/nhd_data.RData"))

sf::sf_use_s2(FALSE)

# takes about 2-3 minutes
huc_comid_xref <-
  nhd_data |>
  sf::st_make_valid() |>
  dplyr::filter(comid == 22878865) |>
  sf::st_transform(crs = 3071) |>
  sf::st_join(sf::st_make_valid(huc10) |> sf::st_transform(crs = 3071)) |>
  sf::st_drop_geometry() |>
  dplyr::mutate(huc6 = substr(huc10, 1, 6)) |>
  dplyr::filter(huc6 %in% pnw_hucs) |>
  dplyr::select(huc10, comid) |>
  dplyr::rename(huc = huc10)

usethis::use_data(
  huc_comid_xref,
  overwrite = TRUE
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
