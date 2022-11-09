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


seed <- list(
  supporting = c(
    "klimakatastroph", "klimakris", "umweltschutz",
    "wissenschaft", "klimakriseistjetzt",
    "artenkollaps", "govegan", "klimaziel", "klimaschutz"
  ),
  opposing = c(
    "klimahysteri", "gruenermist",
    "grueneninkompetenz", "panik",
     "volk", "eigenverantwort", "propaganda",
    "staatsfunk"
  )
)


seed <- list(
  supporting = c(
    "klimakriseistjetzt", "wasserknappheit", "klimakatastrophe", "artenkollaps",
    "waldbrände", "dürre",  "flächenverbrauch", "ressourcen"
  ),
  opposing = c(
    "klimahysterie", "grünermist", "meinungsfreiheit", "staatsfunk", #"narrativ",
    "ökofaschisten", "gruenesekte", "ideologen", "linksgrünen"#, "indoktrination"
  )
)


allowed_keys <- c("boundary", "natural", "place")
allowed_levels <- c("street", "locality", "district", "city")

inkar_sel <- c(
  altersverhältnis = "Verhältnis junge zu alte Erwerbsfähige",
  ohne_berufsabschluss = "Beschäftigte ohne Berufsabschluss",
  akademiker = "Beschäftigte mit akademischem Berufsabschluss",
  unterbeschäftigung = "Unterbeschäftigungsquote",
  industriequote = "Industriequote",
  dienstleistungsquote = "Dienstleistungsquote",
  erwerbstätige_primsek = "Erwerbstätige Primärer Sektor",
  erwerbstätige_seksek = "Erwerbstätige Sekundärer Sektor",
  erwerbstätige_tersek = "Erwerbstätige Tertiärer Sektor",
  kreative_klasse = "Beschäftigte in Kreativbranchen",
  beschäftigte_wissen = "Beschäftigte in wissensintensiven Industrien",
  schutzsuchende = "Schutzsuchende an Bevölkerung",
  lebenserwartung = "Lebenserwartung",
  wahlbeteiligung = "Wahlbeteiligung",
  stimmenanteile_grüne = "Stimmenanteile Grüne",
  stimmenanteile_linke = "Stimmenanteile Die Linke",
  stimmenanteile_afd = "Stimmenanteile AfD",
  abitur = "Schulabgänger mit allgemeiner Hochschulreife",
  schulabbrecher = "Schulabgänger ohne Abschluss",
  haushaltseinkommen = "Haushaltseinkommen",
  erholungsfläche = "Erholungsfläche je Einwohner",
  freifläche = "Freifläche je Einwohner",
  landwirtschaftsfläche = "Landwirtschaftsfläche",
  stickstoffüberschuss = "Stickstoffüberschuss",
  krankenhausbetten = "Krankenhausbetten",
  ärzte = "Ärzte",
  einkommenssteuer = "Einkommensteuer",
  sachinvestitionen = "Ausgaben für Sachinvestitionen",
  städtebauförderung_lang = "Städtebauförderung (langfristig)",
  städtebauförderung_kurz = "Städtebauförderung (kurzfristig)",
  hochschulförderung_lang = "Hochschulförderung (langfristig)",
  hochschulförderung_kurz = "Hochschulförderung (kurzfristig)",
  ländlichkeit = "Ländlichkeit",
  sgb2 = "SGB II - Quote",
  erreichbarkeit_autobahnen = "Erreichbarkeit von Autobahnen",
  erreichbarkeit_fernverkehr = "Erreichbarkeit von IC/EC/ICE-Bahnhöfen",
  erreichbarkeit_versorgung = "Nahversorgung Supermärkte Durchschnittsdistanz",
  erreichbarkeit_öpnv = "Nahversorgung Haltestellen des ÖV Durchschnittsdistanz",
  pendlersaldo = "Pendlersaldo",
  pkwdichte = "Pkw-Dichte",
  bip_pk = "Bruttoinlandsprodukt je Einwohner",
  investitionen_seksek = "Investitionen im Bergbau und Verarb. Gewerbe",
  bws_primsek = "Anteil Bruttowertschöpfung Primärer Sektor",
  bws_seksek = "Anteil Bruttowertschöpfung Sekundärer Sektor",
  bws_tersek = "Anteil Bruttowertschöpfung Tertiärer Sektor",
  versorgung_hausarzt = "Wohnungsnahe Grundversorgung Hausarzt",
  #versorgung_supermarkt = "Wohnungsnahe Grundversorgung Supermarkt",
  versorgung_apotheke = "Wohnungsnahe Grundversorgung Apotheke",
  versorgung_grundschule = "Wohnungsnahe Grundversorgung Grundschule",
  #versorgung_krankenhaus = "Krankenhausversorgung",
  versorgung_breitband = "Breitbandversorgung",
  mietpreise = "Mietpreise"
)

ioer_sel <- c(
  windkraft_pro_10000 = "Anzahl Windkraftanlagen pro 10000 Einwohner",
  bodenversiegelung = "Bodenversiegelung",
  naturbetont = "Naturbetonte Flächen",
  neuinanspruchnahme = "Relative fünfjährliche Flächenneuinanspruchnahme baulich geprägter SuV",
  naturschutz = "Natur- und Artenschutz",
  überschwemmungsgefahr = "Siedlungslast im Überschwemmungsgebiet",
  solarflächenanteil = "Solarflächenanteil",
  windkraftdichte = "Windkraftanlagendichte"
)



# 1
inkar_sel <- c(
  industriequote = "Industriequote",
  kreative_klasse = "Beschäftigte in Kreativbranchen",
  schutzsuchende = "Schutzsuchende an Bevölkerung",
  lebenserwartung = "Lebenserwartung",
  freifläche = "Freifläche je Einwohner",
  einkommenssteuer = "Einkommensteuer",
  städtebauförderung_kurz = "Städtebauförderung (kurzfristig)",
  pkwdichte = "Pkw-Dichte",
  bip_pk = "Bruttoinlandsprodukt je Einwohner",
  bws_primsek = "Anteil Bruttowertschöpfung Primärer Sektor",
  mietpreise = "Mietpreise"
)

ioer_sel <- c(
  windkraft_pro_10000 = "Anzahl Windkraftanlagen pro 10000 Einwohner",
  neuinanspruchnahme = "Relative fünfjährliche Flächenneuinanspruchnahme baulich geprägter SuV",
  naturschutz = "Natur- und Artenschutz",
  überschwemmungsgefahr = "Siedlungslast im Überschwemmungsgebiet",
  solarflächenanteil = "Solarflächenanteil"
)



var_sel <- c(
  industriequote = "Industriequote",
  kreative_klasse = "Beschäftigte in Kreativbranchen",
  bip_pk = "Bruttoinlandsprodukt je Einwohner",
  erwerbstätige_primsek = "Anteil Bruttowertschöpfung Primärer Sektor",
  schutzsuchende = "Schutzsuchende an Bevölkerung",
  lebenserwartung = "Lebenserwartung",
  pkwdichte = "Pkw-Dichte",
  erreichbarkeit_öpnv = "Nahversorgung Haltestellen des ÖV Durchschnittsdistanz",
  naturbetont = "Naturbetonte Flächen",
  einkommenssteuer = "Einkommensteuer",
  städtebauförderung_kurz = "Städtebauförderung (kurzfristig)",
  neuinanspruchnahme = "Relative fünfjährliche Flächenneuinanspruchnahme baulich geprägter SuV",
  naturschutz = "Natur- und Artenschutz",
  windkraft_pro_10000 = "Anzahl Windkraftanlagen pro 10000 Einwohner",
  überschwemmungsgefahr = "Siedlungslast im Überschwemmungsgebiet",
  bodenversiegelung = "Bodenversiegelung"
)


sel_eng <- c(
  industriequote = "Share of industry workers",
  kreative_klasse = "Share of creative workers",
  bip_pk = "GDP per capita",
  erwerbstätige_primsek = "Share of primary sector workers",
  schutzsuchende = "Share of protection seekers",
  lebenserwartung = "Life expectancy",
  pkwdichte = "Car density",
  mietpreise = "Average rent",
  erholungsfläche = "Recreational areas per capita",
  einkommenssteuer = "Income tax per capita",
  städtebauförderung_kurz = "Short-term urban funding",
  neuinanspruchnahme = "Relative land consumption",
  naturschutz = "Share of nature conservation areas",
  windkraft_pro_10000 = "Wind turbines per 10,000 residents",
  überschwemmungsgefahr = "Settlement load in flood plains",
  solarflächenanteil = "Share of solar farms"
)

model_var <- unname(c(names(inkar_sel), names(ioer_sel)))
