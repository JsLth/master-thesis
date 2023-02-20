# Control randomness
rseed <- 1111

# Keywords for tweet mining
keywords <- c(
  "umwelt", "klima", "klimawandel", "klimakrise", "klimakatastrophe",
  "klimaschutz", "klimahysterie", "treibhausgas", "co2",
  "erderwaermung", "erderhitzung", "energiewende", "energiekrise",
  "landwirtschaft", "biodiversitaet", "regenwald",
  "plastik", "mikroplastik", "wasser", "trinkwasser",
  "wasserknappheit", "grundwasser", "öko", "feinstaub",
  "#grüne", "#grünen", "#diegrünen", "#gruene",
  "#gruenen", "#gruenermist", "#klimakriseistjetzt", "#letztegeneration",
  "#fridaysforfuture"
)

# Dataframe ready for LaTeX formatting
keywords_df <- data.frame(
  Domains = c("Climate change", "Biosphere integrity", "Land-system change",
    "Freshwater use", "Biochemical flows", "Ocean acidification",
    "Atmospheric aerosol loading", "Stratospheric ozone depletion",
    "Novel entities", "Environment politics", "General environment"),
  "Query parameter" = c(
    "klima, klimawandel, klimakrise, klimakatastrope, klimaschutz, klimahysterie, treibhausgas, co2, erderwärmung, erderhitzung, energiewende, energiekrise, #klimakriseistjetzt",
    "biodiversität, regenwald",
    "regenwald, landwirtschaft",
    "wasser, trinkwasser, grundwasser",
    "grundwasser, landwirtschaft",
    "",
    "feinstaub",
    "",
    "plastik, mikroplastik, verschwendung",
    "energiewende, energiekrise, öko, #grüne, #grünen, #diegruenen, #gruene, #gruenen, #gruenermist, #letztegeneration, #fridaysforfuture",
    "umwelt, #letztegeneration, #fridaysforfuture"
  )
)

# seedwords
seed <- data.frame(
  supporting = c(
    "klimakriseistjetzt", "klimakrise", "klimakatastrophe", "klimagerechtigkeit",
    "climatejustice", "klimanotstand", "climatecrisis", "climateemergency",
    "endfossilfuels", "artenkollaps", "artensterben",
    "artenschutz", "biodiversität", "biodiversitätskrise", "regenwald",
    "artenvielfalt", "naturschutz", "umweltschutz", "umweltzerstörung",
    "klimastreik", "fridaysforfuture", "hitzewelle", "wasserknappheit",
    "wasserversorgung", "klimakollaps", "klimaapokalypse", "climateaction",
    "climateactionnow", "climateendgame", "umweltkatastrophe",
    "umweltverschmutzung", "klimazerstörung", "waldsterben", "borkenkäfer",
    "ressourcenverschwendung", "insektensterben", "klimafolgen"
  ),
  opposing = c(
    "klimahysterie", "ökofaschisten", "öko", "öko-", "ökoterroristen",
    "öko-terroristen", "klimaterroristen", "klima-terroristen", "klima-chaoten",
    "gruenermist", "grünermist", "ideologie", "ideologien", "gruenesekte",
    "gruenekhmer", "grueneninkompetenz", "ideologisch", "freiheit",
    "ideologische", "meinungsfreiheit", "linksgrüne", "linksgrünen",
    "klimakleber", "klima-kleber", "klima-klebern", "letztedegeneration",
    "woke", "woken", "staatsfunk", "klima-extremisten", "öko-sozialismus",
    "öko-faschisten", "eigenverantwortung", "indoktrination", "links-grüne",
    "klima-sekte", "wohlstand"
  )
)

# Allowed OSM keys and geographical levels for geocoding
allowed_keys <- c("boundary", "natural", "place")
allowed_levels <- c("street", "locality", "district", "city")

# Indicators to be selected from INKAR database
inkar_sel <- c(
  academic = "Beschäftigte mit akademischem Berufsabschluss",
  industry = "Industriequote",
  primary = "Erwerbstätige Primärer Sektor",
  creative = "Beschäftigte in Kreativbranchen",
  life_expectancy = "Lebenserwartung",
  rightwing_votes = "Stimmenanteile AfD",
  recreation = "Erholungsfläche je Einwohner",
  capex = "Ausgaben für Sachinvestitionen",
  urban_funding = "Städtebauförderung (kurzfristig)",
  car_density = "Pkw-Dichte",
  under_6 = "Einwohner unter 6 Jahre",
  under_18 = "Einwohner von 6 bis unter 18 Jahren",
  under_25 = "Einwohner von 18 bis unter 25 Jahren",
  under_30 = "Einwohner von 25 bis unter 30 Jahren"
)

# Indicators to be pulled from IÖR WFS
ioer_sel <- c(
  wind_turbines = "Anzahl Windkraftanlagen pro 10000 Einwohner",
  land_take = "Relative fünfjährliche Flächenneuinanspruchnahme baulich geprägter SuV",
  env_protection = "Natur- und Artenschutz",
  flood_exposure = "Siedlungslast im Überschwemmungsgebiet"
)

# OSM features to be selected for the creation of a scenes indicator
osm_features <- list(
  amenity = c(
    "cafe", "library", "school", "social_facility", "social_centre",
    "community_centre", "public_bookcase", "animal_shelter"
  ),
  leisure = c("fitness_centre", "fitness_station", "hackerspace"),
  office = c(
    "ngo", "association", "charity", "educational_institution", "research",
    "engineer", "it", "geodesist", "surveyor"
  ),
  shop = c("books")
)

# Final variable selection
var_sel <- c(
  industry = "Industriequote",
  creative = "Beschäftigte in Kreativbranchen",
  academic = "Beschäftigte mit akademischem Berufsabschluss",
  primary = "Anteil Bruttowertschöpfung Primärer Sektor",
  under_30 = "Einwohner unter 30 Jahren",
  life_expectancy = "Lebenserwartung",
  car_density = "Pkw-Dichte",
  scenes = "Cultural Scenes",
  rightwing_votes = "Stimmenanteile AfD",
  land_take = "Relative fünfjährliche Flächenneuinanspruchnahme baulich geprägter SuV",
  urban_funding = "Städtebauförderung (kurzfristig)",
  capex = "Ausgaben für Sachinvestitionen",
  env_protection = "Natur- und Artenschutz",
  wind_turbines = "Anzahl Windkraftanlagen pro 10000 Einwohner",
  flood_exposure = "Siedlungslast im Überschwemmungsgebiet",
  recreation = "Erholungsfläche je Einwohner"
)

# Formatted and translated variants of variable selection
sel_eng <- c(
  industry = "Industry employment",
  creative = "Creative employment",
  academic = "Graduate employment",
  primary = "Primary employment",
  under_30 = "Younger than 30",
  life_expectancy = "Life expectancy",
  car_density = "Car density",
  scenes = "Cultural amenities",
  rightwing_votes = "Right-wing votership",
  land_take = "Land consumption",
  urban_funding = "Urban funding",
  capex = "Capital expenditure",
  env_protection = "Nature conservation",
  wind_turbines = "Wind turbines",
  flood_exposure = "Flood exposition",
  recreation = "Recreational areas"
)

# LaTeX-ready variable selection
# change to tabularx
var_sel_latex <- data.frame(
  "Name" = sel_eng[names(var_sel)],
  "Description" = c(
    "Percentage of employees in extractive, manufacturing or building industries, energy/water supply or garbage disposal",
    "Percentage of employees in creative industries such as film, music, radio, print media, architecture and design",
    "Gross domestic product measured as 1000 € per capita",
    "Percentage of employees in agriculture, forestry and fishing",
    "Percentage of residents younger than 30 years relative to all residents",
    "Average life expectancy of a newborn in years",
    "Number of cars per 1,000 residents",
    "Number of cultural amenities indicating egalitarian or rational scenescapes per capita according to \\textcite[100f]{Silver2016}, particularly cafés, libraries, schools, social facilities, social centers, community centers, public bookcases, animal shelters, fitness centers, fitness stations, hackerspaces, NGOs, associations, charities, educational institutions, research facilities and engineering/IT/geodesy/land survey offices based on their respective OSM feature tags",
    "Valid secondary votes for the `Alternative für Deutschland' party relative to all secondary votes",
    "Percentage rate of change in built-up settlement and transport areas within five years \\footnote{A full account of the methodology is given in \\textcite{Schorcht2016}}",
    "Proposed long-term federal financial assistance for urban development funding measured in € per capita, includes means for redevelopment and development, urban monument protection, socially integrative cities (`Soziale Stadt'), urban redevelopment, active city and district centers, smaller cities and communities and future urban greening",
    "Expenditure on property, plant and equipment measured in € per capita, i.e., construction measures, acquisition of movable property and acquisition of immovable property",
    "Share of national parks, nature reserves, fauna-flora habitat areas and European bird sanctuaries divided by the total area",
    "Number of onshore wind turbines per 10,000 residents",
    "Share of built-up settlement and traffic areas in the officially designated floodplain divided by the area of the designated floodplain",
    "Recreational areas per 10,000 residents, i.e., undeveloped areas to be used for sports, recreation or exhibition of animals and plants"
  ),
  "Source" = c(
    "INKAR",
    "INKAR",
    "INKAR",
    "INKAR",
    "INKAR",
    "INKAR",
    "INKAR",
    "OSM",
    "INKAR",
    "IÖR",
    "INKAR",
    "INKAR",
    "IÖR",
    "IÖR",
    "IÖR",
    "INKAR"
  ),
  "Year" = as.character(c(
    2019,
    2019,
    2019,
    2019,
    2019,
    2017,
    2019,
    2022,
    2017,
    2021,
    2019,
    2019,
    2019,
    2020,
    2021,
    2019
  ))
)


ft <- flextable::flextable(var_sel_latex) %>%
  flextable::theme_booktabs() %>%
  flextable::width(width = 1, j = "Name") %>%
  flextable::width(width = 4, j = "Description") %>%
  flextable::width(width = 1, j = "Source") %>%
  flextable::width(width = 0.5, j = "Year") %>%
  flextable::fontsize(size = 9, part = "all")

doc <- officer::read_docx() %>%
  flextable::body_add_flextable(value = ft) %>%
  print("test.docx")