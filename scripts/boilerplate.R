# 
# # File path(s) ------------------------------------------------------------
# 
# create_file_path <- function(dir, file_name, ext) {
#   here::here(glue::glue("{dir}/{file_name}.{ext}"))
# }
# 
# file_paths <- purrr::map_chr(
#   # File names
#   list(
#     <df_name> = "<file_name>",
#   ),
#   # Iteratively create file paths for each file
#   ~ create_file_path("<dir>", .x, "<ext>")
# )
# 
# # Import ------------------------------------------------------------------
# 
# # Insert importation helper function
# <import_helper_function>
# 
# # Iteratively import dfs
# <data>_dfs <- purrr::map(
#   file_paths,
#   ~ <import_function>
# )
# 
# # Tidy --------------------------------------------------------------------
# 
# # Insert tidying function
# <tidying_helper_function>
# 
# tidied_<data> <- <tidying_function>(<raw_df>)
# 
# # Export ------------------------------------------------------------------
# 
# # Insert export helper function
# 
# get_todays_date <- function() {
#   base::format(base::Sys.Date(), "%Y%m%d")
# }
# 
# <helper_function>(
#   <object>,
#   dir = "<dir_path>",
#   file_name = paste0(get_todays_date(), "_<file_name>")
# )
