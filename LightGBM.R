##############
## LightGBM ##
##############
PKG_URL <- "https://github.com/microsoft/LightGBM/releases/download/v3.0.0rc1/lightgbm-3.0.0-1-r40-windows.zip"
local_file <- paste0("lightgbm.", tools::file_ext(PKG_URL))
download.file(
  url = PKG_URL
  , destfile = local_file
)

install.packages(
  pkgs = local_file
  , type = "binary"
  , repos = NULL
)
