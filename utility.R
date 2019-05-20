str_db_filename <- 'big_long.sqlite'

db_updated <- function() {
  if (file.exists(str_db_filename))
    file.info(str_db_filename)$mtime[1]
  else
    ""
}