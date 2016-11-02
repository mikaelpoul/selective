#' Data in list format used to make the experiment headlines
#'
#' Data in list format used to make the experiment headlines.
#'
#' @param news_type Which news type to get, either \code{hard} (default) or \code{soft}.
#' @keywords headlines prepublication
#'
#' @examples
#'\dontrun{
#'  headlines_data_hard_news <- headlines_data("hard")
#' }
#' @export
headlines_data <- function(news_type = "hard") {
  if (!news_type %in% c("hard", "soft")) {
    stop("News type must be one of 'hard' or 'soft'")
  }
  switch(news_type,
         "hard" = headlines_data_hard_news(),
         "soft" = headlines_data_soft_news())
}


## Hard news ------------------------------------------------------------------------------------

headlines_data_hard_news <- function() {
  parties <- c("Rødt", "Sv", "Ap", "Sp", "Mdg", "Venstre", "Krf", "Høyre", "Frp")

  hard_news <- list(

    ## Reduce income ineq I: Reduce income disparities
    list(opinion = "Regjeringen burde red. inntektsforskjeller",
         base = "%solitiker %s vil %s inntektsforskjellene",
         order = 1:3,
         party = c("P", paste0(parties, "-p")),
         valance = c("med forslag som", "får kjeft for forslag som", "får skryt for forslag som"),
         direction = c("øke", "redusere"),
         link = "direct"),

    ## Reduce income ineq II: Reduce income ineq
    list(opinion = "Regjeringen burde red. inntektsforskjeller",
         base = "%sorslag fra %s-topp som vil %s lønnsforskjeller",
         order = c(2, 1, 3),
         party = c("parti", parties),
         valance = c("F", "Refser f", "Roser f"),
         direction = c("øke", "redusere"),
         link = "indirect"),

    ## Reduce income ineq II: More to the rich
    list(opinion = "Regjeringen burde red. inntektsforskjeller",
         base = "%s-topp vil %s de rike%s",
         order = c(1, 3, 2),
         party = c("Parti", parties),
         valance = c("", " - blir refset ", " - blir roset"),
         direction = c("gi mer til", "ta mer fra"),
         link = "indirect"),

    ## Better with more diversity I: More muslims in schools
    list(opinion = "Det er bedre for et land med flere kulturer/religioner",
         base = "%solitiker %s: «vi %s flere muslimer i norske skoler»",
         order = c(1, 2, 3),
         party = c("P", paste0(parties, "-p")),
         valance = c("med Facebook-innlegg", "fordømmes etter Facebook-innlegg", "lovprises etter Facebook-innlegg"),
         direction = c("trenger ikke", "trenger"),
         link = "indirect"),

    ## Better with more diversity II: Take inn more refugees
    list(opinion = "Det er bedre for et land med flere kulturer/religioner",
         base = "Lokal%solitiker %s vil %s 5000 nye flyktninger",
         order = c(1, 2, 3),
         party = c("p", paste0(" ", parties, "-p")),
         valance = c("–", "skjelles ut –", "hylles –"),
         direction = c("hindre at vi tar imot", "ta imot"),
         link = "indirect"),

    ## Better with more diversity II: Direct
    list(opinion = "Det er bedre for et land med flere kulturer/religioner",
         base = "%solitiker%s mener det er bedre med %s kulturer og religioner i Norge ",
         order = c(1, 2, 3),
         party = c("P", paste0(parties, "-p")),
         valance = c("", " skjelles ut –", " hylles –"),
         direction = c("færre", "flere"),
         link = "direct"),

    ## Refugees same rights to social welfare I: Direct
    list(opinion = "Refugees should have equal rights to social welfare",
         base = "%solitiker mener flyktninger %sbør ha samme rett til sosialhjelp som nordmenn%s",
         order = c(1, 3, 2),
         party = c("P", paste0(parties, "-p")),
         valance = c("", ", blir skjelt ut", ", blir hyllet"),
         direction = c("ikke ", ""),
         link = "direct"),


    ## Refugees same rights to social welfare II: NAV-money for refugees
    list(opinion = "Refugees should have equal rights to social welfare",
         base = "%s-profil i åpent brev: «jeg vil %sat flyktinger skal få NAV-penger»%s",
         order = c(1, 3, 2),
         party = c("Parti", parties),
         valance = c("", ". Blir latterliggjort i sosial medier.", ". Blir hyllet i sosiale medier."),
         direction = c("IKKE ", ""),
         link = "indirect"),


    ## Refugees same rights to social welfare III: NAV-money for Syrian refugees
    list(opinion = "Refugees should have equal rights to social welfare",
         base = "%s-topp %s Syriske flyktinger%s",
         order = c(1, 3, 2),
         party = c("Parti", parties),
         valance = c("", ", får refs", ", får ros"),
         direction = c("presser på for å gi NAV-penger til", "truer med å ta NAV-penger fra"),
         link = "indirect"),

    ## Reduce tax burden I: Wealth tax
    list(opinion = "Skattebyrden burde reduseres",
         base = "Politiker%s vil %s formueskatten%s",
         order = c(1, 3, 2),
         party = c("", paste0(" fra ", parties)),
         valance = c("", ", mottar sterk kritikk", ", blir roset"),
         direction = c("øke", "senke"),
         link = "indirect"),

    ## Reduce tax burden II: Property tax
    list(opinion = "Skattebyrden burde reduseres",
         base = "%s%s-topp: Vil %s i alle norske byer.",
         order = c(2, 1, 3),
         party = c("Parti", parties),
         valance = c("", "Stormer mot ", "Jubler for "),
         direction = c("innføre eiendomsskatt", "avskaffe eiendomsskatten"),
         link = "indirect"),

    ## Reduce tax burden II: Property tax
    list(opinion = "Skattebyrden burde reduseres",
         base = "%s%s-topp mener at skattenivået %sburde reduseres",
         order = c(2, 1, 3),
         party = c("Parti", parties),
         valance = c("", "Flere stormer: ", "Flere jubler: "),
         direction = c("ikke ", ""),
         link = "direct"),


    ## Allow commercial schools I: Allow to profit from private schools
    list(opinion = "Kommersielle skoler burde tillates",
         base = "%srepresentant%s %s private skoler skal kunne ta ut profitt",
         order = c(1, 2, 3),
         party = c("Stortings", paste0(parties, "-")),
         valance = c("", " refses –", " roses –"),
         direction = c("vil sikre at ingen", "vil åpne for at noen"),
         link = "indirect"),

    ## Allow commercial schools II: More/less private schools
    list(opinion = "Kommersielle skoler burde tillates",
         base = "%s-profil %s forslag om %s privatskoler i Norge",
         order = c(1, 2, 3),
         party = c("Parti", parties),
         valance = c("lanserer", "møter proteststorm for", "møter begeistring for"),
         direction = c("færre", "flere"),
         link = "indirect"),

    ## Allow commercial schools III: More/less private schools
    list(opinion = "Kommersielle skoler burde tillates",
         base = "%s-profil%s mener at kommersielle skoler bør %s",
         order = c(1, 2, 3),
         party = c("Parti", parties),
         valance = c("", " møter proteststorm -", " møter begeistring -"),
         direction = c("tillates", "forbys"),
         link = "direct"),

    ## Public activities better by private actors I: Direct
    list(opinion = "Mange offentlige aktiviteter kan gjøres bedre av private aktører",
         base = "%solitiker mener alle offentlige aktiviteter %skan gjøres bedre av private%s",
         order = c(1, 3, 2),
         party = c("P", paste0(parties, "-p")),
         valance = c("", " – får refs", " – får ros"),
         direction = c("ikke ", ""),
         link = "direct"),

    ## Public activities better by private actors I: Elderly care
    list(opinion = "Mange offentlige aktiviteter kan gjøres bedre av private aktører",
         base = "Foreslår å %s eldreomsorgen, ansvarlig %spolitiker %s",
         order = c(3, 1, 2),
         party = c("", paste0(parties, "-")),
         valance = c("forklarer nærmere", "refses", "roses"),
         direction = c("la kommunen ta seg av", "privatisere"),
         link = "indirect"),

    ## Public activities better by private actors II: Privatize Norwegian train service
    list(opinion = "Mange offentlige aktiviteter kan gjøres bedre av private aktører",
         base = "%solitiker %s forslag: %s NSB",
         order = c(1, 2, 3),
         party = c("P", paste0(parties, "-p")),
         valance = c("presenterer", "får pepper for", "får skryt for"),
         direction = c("vil nekte privatisering av", "vil privatisere"),
         link = "indirect"),

    ## Oil/gas extraction in barents I: Direct
    list(opinion = "Ikke tillate oljeboring i LoVe",
         base = "%solitiker %svil %såpne flere oljefelt i nord",
         order = c(1, 2, 3),
         party = c("P", paste0(parties, "-p")),
         valance = c("", "møter massiv motstand - ", "får massiv støtte - "),
         direction = c("", "ikke "),
         link = "indirect"),

    ## Oil/gas extraction in barents II: Fightind for extraction
    list(opinion = "Ikke tillate oljeboring i LoVe",
         base = "%s-topp kjemper %s oljeboring i Lofoten%s",
         order = c(1, 3, 2),
         party = c("Parti", parties),
         valance = c("", " - møter massiv motstand", " - får massiv støtte"),
         direction = c("for", "mot"),
         link = "indirect"),

    ## Oil/gas extraction in barents III: Allow/deny extraction
    list(opinion = "Ikke tillate oljeboring i LoVe",
         base = "%s-profil %s vil %s oljeboring i nord",
         order = c(1, 2, 3),
         party = c("Parti", parties),
         valance = c("på twitter:", "blir latterligjort etter twittermelding –", "blir hedret etter twittermelding –"),
         direction = c("åpne for", "stenge for"),
         link = "direct"),

    ## Equal rights for gay and straight I: FB message
    list(opinion = "Homofile og heterofile bør ha like rettigheter",
         base = '%solitiker %s «Homofile par %s få like rettigheter som heterofile»',
         order = c(1, 2, 3),
         party = c("P", paste0(parties, "-p")),
         valance = c("på Facebook:", "får kritikk etter facebook-status:", "får ros etter facebook-status:"),
         direction = c("MÅ IKKE", "MÅ"),
         link = "direct"),

    ## Equal rights for gay and straight I: gay friendly/hostile policy proposal
    list(opinion = "Homofile og heterofile bør ha like rettigheter",
         case = "[Parti]-topp [med/får kritikk etter/får ros etter] homovennlig/fientlig lovforslag",
         base = "%solitiker %s %s lovforslag",
         order = c(1, 2, 3),
         party = c("P", paste0(parties, "-p")),
         valance = c("med", "hudflettes etter", "lovprises etter"),
         direction = c("homofiendtlig", "homovennlig"),
         link = "indirect"),

    ## Equal rights for gay and straight I: klmgmkøgamkløg
    list(opinion = "Homofile og heterofile bør ha like rettigheter",
         case = "[Parti]-topp [med/får kritikk etter/får ros etter] homovennlig/fientlig lovforslag",
         base = "%solitiker %smener det %sburde være vanskeligere for homofile å kunne gifte seg",
         order = c(1, 2, 3),
         party = c("P", paste0(parties, "-p")),
         valance = c("", "hudflettes: ", "lovprises: "),
         direction = c("", "ikke "),
         link = "indirekt")
  )
  names(hard_news) <- paste0("hard_", 1:length(hard_news))
  class(hard_news) <- c("list", "hard_news_data")
  for (i in 1:length(hard_news)) class(hard_news[[i]]) <- c("list", "hard_news_data")
  return(hard_news)
}


## Soft news ------------------------------------------------------------------------------------

headlines_data_soft_news <- function() {

  soft_news <- list(
    ## Kjendis I: TV-profiler [skiller seg/gifter seg/snakker om livet sammen]
    list(topic = "Kjendis",
         case = "TV-profiler [skiller seg/gifter seg/snakker om livet]",
         base = "TV-profiler %s",
         valance = c("snakker om livet sammen", "skiller seg", "gifter seg")),

    ## Kjendis II: Kjendisfesten [gikk skikkelig galt/var en stor suksess/ble holdt i går]
    list(topic = "Kjendis",
         case = "Kjendisfesten [gikk skikkelig galt/var en stor suksess/ble holdt i går]",
         base = "Kjendisfesten %s",
         valance = c("ble holdt i går", "gikk skikkelig galt", "var en stor suksess")),

    ## Tror Norge [taper/vinner/spiller uavgjort] i morgendagens fotballkamp
    list(topic = "Sport",
         case = "Tror Norge [taper/vinner/spiller uavgjort] i morgendagens fotballkamp",
         base = "Tror Norge %s i morgendagens fotballkamp",
         valance = c("spiller uavgjort", "taper", "vinner")),

    ## Stjernespiller [kampklar til /ute med skade til/snakker om] morgendagens kamp
    list(topic = "Sport",
         case = "Stjernespiller [kampklar til /ute med skade til/snakker om] morgendagens kamp",
         base = "Stjernespiller %s morgendagens kamp",
         valance = c("snakker om", "ute med skade til", "kampklar til")),

    ## Norges store sangstjerne [får ros/ får refs/valances] i fersk anmeldelse
    list(topic = "Anmeldelse",
         case = "Norges store sangstjerne [får ros/ får refs/valances] i fersk anmeldelse",
         base = "Norges store sangstjerne %si fersk anmeldelse",
         valance = c("", "får refs ", "får ros ")),

    ## Denne restauranten [må du prøve mener / må du unngå mener/testes av] våre anmeldere
    list(topic = "Anmeldelse",
         case = "Denne restauranten [må du prøve mener / må du unngå mener/testes av] våre anmeldere",
         base = "Denne restauranten %s våre anmeldere",
         valance = c("er testet av", "må du unngå mener", "må du prøve mener")),

    ## Her er høstens [verste/beste/___] smarttelefoner
    list(topic = "Teknologi",
         base = "Her er vinterens %ssmarttelefoner",
         valance = c("", "verste ", "beste ")),

    ## Disse TV-ene [kommer best ut / kommer vers ut /er med] i vår nye test
    list(topic = "Teknologi",
         case = "Disse TV-ene [kommer best ut / kommer vers ut /er med] i vår nye test",
         base = "Disse TV-ene %s i vår nye test",
         valance = c("er med", "kommer verst ut", "kommer best ut")),

    ## Dette [er det verste/er det beste/kan gjøres] for å gå ned i vekt
    list(topic = "Livsstil",
         case = "Dette [er det verste/er det beste/kan gjøres] for å gå ned i vekt",
         base = "Dette %s ned i vekt",
         valance = c("kan du gjøre for å", "er det verste for å gå", "er det beste for å gå")),

    ## [Suksesshistoriene/Mareritthistoriene/Historier] om høstens treningstrend.
    list(topic = "Livsstil",
         case = "[Suksesshistoriene/Mareritthistoriene/Historier] om høstens treningstrend",
         base = "%s høstens treningstrend",
         valance = c("Dette er", "Mareritthistoriene om", "Suksesshistoriene om")),

    ## Her er helgens filmer [du bør unngå/du bør se/___]
    list(topic = "Konsert/film",
         case = "Her er helgens filmer [du bør unngå/du bør se/___]",
         base = "Her er helgens filmer %s",
         valance = c("", "du bør unngå", "du bør se")),

    ## Her er helgens filmer [du bør unngå/du bør se/___]
    list(topic = "Konsert/film",
         case = "Konsert [gikk skikkelig galt/gikk skikkelig bra/ble holdt] i helgen",
         base = "%s",
         valance = c("Helgens konsert", "Her går konserten fryktelig galt", "Her går konserten skikkelig bra"))
  )
  names(soft_news) <- paste0("soft_", 1:length(soft_news))
  class(soft_news) <- c("list", "soft_news_data")
  for (i in 1:length(soft_news)) class(soft_news[[i]]) <- c("list", "soft_news_data")
  return(soft_news)
}
