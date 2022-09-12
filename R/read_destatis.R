read_destatis <- function(file, select = NULL) {
  file <- file.path("data/context", file)

  .data <- readLines(file(file, encoding = "windows-1252"))
  ncols <- stringr::str_count(.data[1], ";")
  .data <- paste(.data, collapse = "\n")
  
  # Tag special symbols as NA
  .data <- str_replace_all(.data, regex(";[\\.x\\/-]"), ";")

  .data <- readr::read_delim(
    .data,
    delim = ";",
    locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
    show_col_types = FALSE,
    trim_ws = TRUE
  )

  # Hamburg and Berlin are not kreise, but they do have a KRS key
  .data[.data$ags %in% c("02", "11"), "ags"] <- c("02000", "11000")
  
  # Only select records with amtliche Kreisschlüssel + Hamburg and Berlin
  .data <- .data[nchar(.data$ags) == 5, ]
  
  # Delete all records that have NAs for all rows as these are kreise that
  # were resolved in the past
  .data <- .data[rowSums(is.na(.data)) < ncols - 3, ]

  .data[c("ags", "admin", select)]
}


read_bka <- function() {
  crime <- read_csv2("data/context/kriminalität.csv")
  crime <- crime[crime$ags %in% "------", ]
  crime$ags <- NULL

  names(crime)[names(crime) %in% "Gemeindeschluessel"] <- "ags"
  crime$ags <- vapply(crime$ags, \(x) {
    if (nchar(x) == 4) {
      paste0(0, x)
    } else as.character(x)
  }, character(1))
  
  crime <- crime[c("ags", "Stadt-/Landkreis", "Haeufigkeitszahl")]
  names(crime) <- c("ags", "name", "crimes_per_100k")
  crime
}