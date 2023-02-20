source("R/packages.R")

# Downloads the INKAR database
download_inkar <- function() {
  file <- "data/context/inkar_2021.zip"
  download.file(
    "https://www.bbr-server.de/imagemap/inkar/download/inkar_2021.zip",
    "data/context/inkar_2021.zip"
  )
  unzip(file, exdir = "data/context/inkar_2021")
}


# Reads in INKAR database and filters by indicator, time and scale to return
# exactly 401 elements
read_inkar <- function(indicator,
                       time = "latest",
                       scale = "Kreise",
                       database = "data/context/inkar_2021",
                       obj = NULL) {
  if (is.null(obj)) {
    db <- read_inkar_db(database)
  } else {
    db <- obj
  }
  
  if (time %in% "latest") {
    time <- inkar_available(
      "time", indicator = indicator, scale = scale, obj = db
    )

    if (length(time) > 1) {
      time <- time %>%
        as.numeric() %>%
        max(na.rm = TRUE)
    }
    
    if (!length(time)) {
      cli::cli_abort("{.val {indicator}} not available for time {.val {time}} and scale {.val {scale}}.")
    }
  }

  db %>%
    filter(Raumbezug == scale,
           Zeitbezug == time,
           Indikator == indicator) %>%
    filter(Bereich   == unique(Bereich)[1]) %>%
    left_join(inkar_ref(), by = c("Name" = "gen")) %>%
    rename_with(.fn = \(x) indicator, .cols = "Wert")
}


# Requests indicator from IÖR WFS and reads it into an sf dataframe
read_ioer <- function(indicator, time = "latest", scale = "krs") {
  req <- request("https://monitor.ioer.de/monitor_api/user") %>%
    req_url_query(
      id = ioer_ids[[indicator]],
      service = "wfs",
      key = Sys.getenv("IOER_KEY"), # api key is stored as env variable
      version = "2.0.0",
      request = "GetCapabilities"
    )

  feature_types <- req_url_query(req, request = "GetCapabilities") %>%
    req_perform() %>%
    resp_body_xml() %>%
    xml2::as_list() %>%
    magrittr::extract2("WFS_Capabilities") %>%
    magrittr::extract2("FeatureTypeList")
  typeName <- vapply(
    feature_types,
    \(x) x$Name[[1]],
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  ) %>%
    magrittr::extract(str_detect(., scale)) %>%
    when(time != "latest" ~ magrittr::extract(., str_detect(., time)),
         ~ magrittr::extract(., length(.)))
  
  if (!length(typeName)) {
    cli::cli_abort("No feature types found for scale {.val {scale}} and time {.val {time}}.")
  }
  
  url <- req %>%
    req_url_query(
      id = ioer_ids[[indicator]],
      service = "wfs",
      key = Sys.getenv("IOER_KEY"), # api key is stored as env variable
      version = "2.0.0",
      request = "GetFeature",
      typeName = typeName
    ) %>%
    magrittr::extract2("url")
  
  read_sf(url) %>%
    mutate(value = as.numeric(value))
}


# Reads INKAR spatial reference
inkar_ref <- function(database = "data/context/inkar_2021") {
  if (is.null(database)) {
    database <- download_inkar()
  }
  
  ref <- readxl::read_xlsx(
    file.path(database, "Referenz Gemeinden, Kreise, NUTS.xlsx"),
    sheet = "Kreise",
    col_types = "text",
    skip = 1
  ) %>%
    dplyr::select(ags = "Kreise Kennziffer", gen = "Kreise Name") %>%
    mutate(ags = str_remove(ags, regex("000$"))) %>%
    mutate(ags = case_when(nchar(ags) == 4 ~ paste0(0, ags), TRUE ~ ags))
  
  ref
}


# Reads entire INKAR database
read_inkar_db <- function(database = "data/context/inkar_2021") {
  if (is.null(database)) {
    database <- download_inkar()
  }
  
  readr::read_csv2(
    file.path(database, "inkar_2021.csv"),
    col_select = c("Bereich", "Indikator", "Raumbezug", "Name", "Zeitbezug", "Wert"),
    col_types = c("c", "c", "c", "c", "i", "d")
  )
}


# Reads INKAR database and returns all values of `what` for a given indicator,
# time and/or scale
inkar_available <- function(what = "indicators",
                            indicator = NULL,
                            time = NULL,
                            scale = "Kreise",
                            database = "data/context/inkar_2021",
                            obj = NULL) {
  if (is.null(obj)) {
    db <- read_inkar_db(database)
  } else {
    db <- obj
  }
  
  what <- switch(what,
    indicators = "Indikator",
    time = "Zeitbezug",
    scale = "Raumbezug"
  )
  
  if (!is.null(indicator)) {
    db <- db %>% filter(Indikator == indicator)
  }
  
  if (!is.null(time)) {
    db <- db %>% filter(Zeitbezug == time)
  }
  
  if (!is.null(scale)) {
    db <- db %>% filter(Raumbezug == scale)
  }
  
  db %>%
    pull(all_of(what)) %>%
    unique()
}


repair_ags <- function(ags) {
  vapply(ags, function(x) {
    if (nchar(x) == 4) {
      paste0(0, x)
    } else x
  }, FUN.VALUE = character(1))
}


repair_variable_names <- function(names) {
  is_ioer <- map_lgl(names, ~any(str_detect(.x, ioer_sel)))
  names[is_ioer] <- names(ioer_sel)[str_which(names[is_ioer], ioer_sel)]
  names[str_detect(names, "Photovoltaik-Freiflächenanlagen")] <- "solarflächenanteil"
  names[str_detect(names, "naturbetonter Flächen")] <- "naturbetont"

  is_inkar <- map_lgl(names, ~any(str_detect(inkar_sel, .x) | .x %in% inkar_sel))
  names[is_inkar] <- names(inkar_sel)[str_detect(names[is_inkar], inkar_sel) |
                                        inkar_sel %in% names[is_inkar]]
    
  names
}


cleanup_inkar <- function(ind, include_gen = TRUE) {
  name <- unique(ind$Indikator)

  ind %>%
    rename(gen = Name) %>%
    dplyr::select(ags, gen, contains(name)) %>%
    when(!include_gen ~ dplyr::select(., !gen), ~.) %>%
    as_tibble(.name_repair = repair_variable_names)
}


cleanup_ioer <- function(ind, include_gen = TRUE) {
  name <- unique(ind$Indikatorname)
  
  ind %>%
    dplyr::select(ags, gen, value) %>%
    when(!include_gen ~ dplyr::select(., !gen), ~.) %>%
    st_drop_geometry() %>%
    rename_with(\(x) name, .cols = "value") %>%
    as_tibble(.name_repair = repair_variable_names)
}


nested_list_to_sql <- function(x) {
  paste(names(x), "IN", x) %>%
    str_replace_all(fixed("c("), "(") %>%
    str_replace_all(fixed("books"), "('books')") %>%
    paste(collapse = " OR ")
}


# Links indicator names to indicator IDs
ioer_ids <- list(
  "Abbau-/Haldenfläche" = "F13RG",
  "Ackerfläche" = "F03RG",
  "Anteil Defizitfläche" = "P05RG",
  "Anteil Schienenverkehrsfläche" = "V06RG",
  "Anteil Windparkfläche" = "E02RG",
  "Anzahl Windkraftanlagen pro 10000 Einwohner" = "E01DE",
  "Ausnutzungsdichte" = "D04KG",
  "Bebaute Siedlungs und Verkehrsdichte" = "S12RG",
  "Bebaute Siedlungsfläche ohne Industrie und Gewerbe" = "S10RG",
  "Bebaute Siedlungsfläche" = "S15RG",
  "Bebaute SuV pro Einwohner" = "B21MT",
  "Bodenversiegelung" = "S40RG",
  "Dispersion" = "D02KG",
  "Effektive Maschenweite Freiräume" = "U02KG",
  "Effektive Maschenweite Wälder" = "U06KG",
  "Erreichbarkeit größerer städtischer Grünflächen" = "O04RG",
  "Erreichbarkeit naher städtischer Grünflächen" = "O03RG",
  "Erreichbarkeit städtischer Grünflächen" = "O01RG",
  "Flächenanspruch" = "D05KG",
  "Flächenneuinanspruchnahme baulich geprägter SuV im Fünfjahresmittel" = "N02EG",
  "Flächenproduktivität" = "S50KT",
  "Freiraumfläche pro Einwohner" = "B10MT",
  "Freiraumfläche" = "F01RG",
  "Freifraumverlust pro Einwohner" = "B10ET",
  "Gebäudedichte in Gebietsfläche" = "G01DG",
  "Gehölzdominierte Ökotondichte" = "U30DG",
  "Gesamtkraftverkehrsnetzdichte" = "V30DG",
  "Gewichtete Zersiedelung" = "D01KG",
  "Gewässerfläche" = "F11RG",
  "Grünanteil" = "P01RG",
  "Grünfläche pro Einwohner" = "P01MT",
  "Grünflächenausstattung pro Einwohner" = "O02MT",
  "Grünlandfläche" = "F04RG",
  "Hochspannungseinwirkung" = "B31RT",
  "Industrie und Gewerbefläche (Siedlung)" = "S05RT",
  "Industrie und Gewerbefläche" = "S05RG",
  "Industrie- und Gewerbefläche pro Einwohner" = "B06MT",
  "Landschaftsschutz" = "L09RG",
  "Landwirtschaftsfläche pro Einwohner" = "B11MT",
  "Landwirtschaftsfläche" = "F02RG",
  "Moorflächenanteil" = "F18RG",
  "Natur- und Artenschutz" = "L08RG",
  "Naturbetonte Flächen" = "U18RG",
  "Hemerobie" = "U20KG",
  "Relative fünfjährliche Flächenneuinanspruchnahme baulich geprägter SuV" = "N02ET",
  "Relative fünfjährliche Flächenneuinanspruchnahme SuV" = "N01ET",
  "Schienennetzdichte" = "V04DG",
  "Schutzgebiete" = "L01RG",
  "Siedlungflächenanteil im Überschwemmungsgebiet" = "R05RT",
  "Siedlungs- und Verkehrsfläche pro Einwohner" = "B20MT",
  "Siedlungs- und Verkehrsfläche" = "S11RG",
  "Siedlungsdichte" = "B02DT",
  "Siedlungsfläche" = "S02RG",
  "Siedlungsfreifläche (Siedlung)" = "S08RT",
  "Siedlungsfreifläche" = "S08RG",
  "Siedlungslast im Überschwemmungsgebiet" = "R04RT",
  "Solarflächenanteil" = "E03RG",
  "Straßennetzdichte in Siedlung" = "V03DT",
  "Straßennetzdichte" = "V03DG",
  "Straßenverkehrsfläche an SuV" = "V02RT",
  "Straßenverkehrsfläche" = "V02RG",
  "SuV bezogene Siedlungsfläche" = "S14RT",
  "SuV-Grünanteil" = "P02RT",
  "Unzerschnittene Freiräume > 100 km²" = "U03RG",
  "Unzerschnittene Freiräume > 50 km²" = "U04RG",
  "Unzerschnittene Wälder > 50 km²" = "U07RG",
  "Urbane Durchdringung" = "D03KG",
  "Verkehrsfläche an SuV" = "V01RT",
  "Verkehrsfläche pro Einwohner" = "B14MT",
  "Verkehrsfläche" = "V01RG",
  "Verkehrsflächennutzungsdichte" = "V05DT",
  "Verlust von Freiraumfläche pro Einwohner" = "N03ET",
  "Wald- und Gehölzfläche pro Einwohner" = "B12MT",
  "Wald- und Gehölzfläche" = "F07RG",
  "Waldfläche" = "O06",
  "Windkraftanlagendichte" = "E01DG",
  "Wohngebäude-Materiallager pro Einwohner" = "M01MT",
  "Wohngebäude-Materiallager pro Wohn- und Mischnutzfläche" = "M01RT",
  "Wohngebäudedichte in baulich geprägter Siedlungsfläche" = "G03DC",
  "Wohngebäudedichte in Gebietsfläche" = "G03DG"
)
