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
    ## Reduce income ineq I: high income tax
    list(opinion = "Regjeringen burde red. inntektsforskjeller",
         case = "[Parti]-profil [får kjeft for/får skryt for/lanserer] forslag om å [øke toppskatten/senke toppskatten]",
         base = "%s-politiker %s å %s toppskatten",
         order = 1:3,
         party = parties,
         valance = c("foreslår", "får kjeft for forslag om", "får skryt for forslag om"),
         direction = c("øke", "senke")),

    ## Reduce income ineq II: tax for elderly
    list(opinion = "Regjeringen burde red. inntektsforskjeller",
         case = "[Refser/roser/___] [Parti]-topp forslag om skatte[økning/kutt] for eldre",
         base = "%s%s-topp med nytt forslag om skatte%s for eldre",
         order = c(2, 1, 3),
         party = parties,
         valance = c("", "Refser ", "Roser "),
         direction = c("økning", "kutt")),

    ## Better with more diversity I: More muslims in schools
    list(opinion = "Det er bedre for et land med flere kulturer/religioner",
         case = "[Refser/roser/valancer] [parti]-politiker for å skrive [er negativ til/er positiv til] muslimer i norske skoler",
         base = "%s-politiker %s: «vi %s flere muslimer i norske skoler»",
         order = c(1, 2, 3),
         party = parties,
         valance = c("med Facebook-innlegg", "fordømmes etter Facebook-innlegg", "lovprises etter Facebook-innlegg"),
         direction = c("trenger ikke", "trenger")),

    ## Better with more diversity II: Take inn more refugees
    list(opinion = "Det er bedre for et land med flere kulturer/religioner",
         case = "Lokal [parti]-politiker [får skryt etter uttalelser/får kjeft etter uttalelser/uttaler seg] om å [fjerne/bygge ut] asylmottak",
         base = "Lokal %s-politiker %s vil %s 5000 nye flyktninger",
         order = c(1, 2, 3),
         party = parties,
         valance = c("–", "skjelles ut –", "hylles –"),
         direction = c("hindre at vi tar imot", "ta imot")),

    ## Reduce tax burden I: Wealth tax
    list(opinion = "Skattebyrden burde reduseres",
         case = "[misliker sterkt/liker sterkt/___] [Parti]-politikers forslag om å [øke/senke] formueskatten",
         base = "Politiker fra %s vil %s formueskatten%s",
         order = c(1, 3, 2),
         party = parties,
         valance = c("", ", mottar sterk kritikk", ", blir roset"),
         direction = c("øke", "senke")),

    ## Reduce tax burden II: Property tax
    list(opinion = "Skattebyrden burde reduseres",
         case = "[Stormer mot/Jubler for/___] [Parti]-politikers ønske om å [innføre/avskaffe] eiendomsskatten for samtlige byer i Norge",
         base = "%s%s-topp som vil %s i alle norske byer",
         order = c(2, 1, 3),
         party = parties,
         valance = c("", "Stormer mot ", "Jubler for "),
         direction = c("innføre eiendomsskatt", "avskaffe eiendomsskatten")),

    ## Allow commercial schools I: Allow to profit from private schools
    list(opinion = "Kommersielle skoler burde tillates",
         case = "[Vil åpne for at noen/Vil sikre at ingen] private skoler skal kunne ta ut profitt – [Parti]-representant [refses/roses/uttaler seg]",
         base = "%s-representant%s %s private skoler skal kunne ta ut profitt",
         order = c(1, 2, 3),
         party = parties,
         valance = c("", " refses –", " roses –"),
         direction = c("vil sikre at ingen", "vil åpne for at noen")),

    ## Allow commercial schools II: More/less private schools
    list(opinion = "Kommersielle skoler burde tillates",
         case = "[Parti]-profil [høster jubel for / høster sterk kritikk for /lanserer] forslag om [økning/redusering] av antall privatskoler i Norge",
         base = "%s-profil %s forslag om %s privatskoler i Norge",
         order = c(1, 2, 3),
         party = parties,
         valance = c("lanserer", "møter proteststorm for", "møter begeistring for"),
         direction = c("færre", "flere")),

    ## Public activities better by private actors I: Elderly care
    list(opinion = "Mange offentlige aktiviteter kan gjøres bedre av private aktører",
         case = "Foreslår å [privatisere/la kommunen ta over] eldreomsorgen, ansvarlig [Parti]-representant [får refs/ får ros /forklarer nærmere]",
         base = "Foreslår å %s eldreomsorgen, ansvarlig %s-politiker %s",
         order = c(3, 1, 2),
         party = parties,
         valance = c("forklarer nærmere", "refses", "roses"),
         direction = c("la kommunen ta seg av", "privatisere")),

    ## Public activities better by private actors II: Privatize Norwegian train service
    list(opinion = "Mange offentlige aktiviteter kan gjøres bedre av private aktører",
         case = "[Parti]-politiker [får sterk kritikk for/ får mye ros for/presenterer] forslag om at private [bemanningsbyråer/kommunen] i større grad ta seg av vasking av omsorgsboliger",
         base = "%s-politiker %s forslag: %s NSB",
         order = c(1, 2, 3),
         party = parties,
         valance = c("presenterer", "får pepper for", "får skryt for"),
         direction = c("vil nekte privatisering av", "vil privatisere")),

    ## Oil/gas extraction in barents I: Fightind for extraction
    list(opinion = "Ikke tillate oljeboring i LoVe",
         case = "[Parti]-topp kjemper [for/mot] oljeåpning i Lofoten[BLANK/, - får kritikk/, - får ros]",
         base = "%s-topp kjemper %s oljeboring i Lofoten%s",
         order = c(1, 3, 2),
         party = parties,
         valance = c("", " - møter massiv motstand", " - får massiv støtte"),
         direction = c("for", "mot")),

    ## Oil/gas extraction in barents II: Allow/deny extraction
    list(opinion = "Ikke tillate oljeboring i LoVe",
         case = "[Parti]-profil [på twitter/blir latterligjort etter twittermelding/blir roset etter twittermelding] - vil [åpne for/nekte for] oljeboring i Nord",
         base = "%s-profil %s vil %s oljeboring i nord",
         order = c(1, 2, 3),
         party = parties,
         valance = c("på twitter:", "blir latterligjort etter twittermelding –", "blir hedret etter twittermelding –"),
         direction = c("åpne for", "stenge for")),

    ## Equal rights for gay and straight I: FB message
    list(opinion = "Homofile og heterofile bør ha like rettigheter",
         case = '[Parti]-politiker [på Facebook:/får kritikk etter facebook-melding:/får ros etter facebook-melding:] «Homofile par [MÅ/MÅ IKKE] få like rettigheter som heterofile»',
         base = '%s-politiker %s «Homofile par %s få like rettigheter som heterofile»',
         order = c(1, 2, 3),
         party = parties,
         valance = c("på Facebook:", "får kritikk etter facebook-status:", "får ros etter facebook-status:"),
         direction = c("MÅ IKKE", "MÅ")),

    ## Equal rights for gay and straight I: gay friendly/hostile policy proposal
    list(opinion = "Homofile og heterofile bør ha like rettigheter",
         case = "[Parti]-topp [med/får kritikk etter/får ros etter] homovennlig/fientlig lovforslag",
         base = "%s-politiker %s %s lovforslag",
         order = c(1, 2, 3),
         party = parties,
         valance = c("med", "hudflettes etter", "lovprises etter"),
         direction = c("homofientlig", "homovennlig"))
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
         base = "Norges store sangstjerne %s i fersk anmeldelse",
         valance = c("valances", "får refs", "får ros")),

    ## Denne restauranten [må du prøve mener / må du unngå mener/testes av] våre anmeldere
    list(topic = "Anmeldelse",
         case = "Denne restauranten [må du prøve mener / må du unngå mener/testes av] våre anmeldere",
         base = "Denne restauranten %s våre anmeldere",
         valance = c("er testet av", "må du unngå mener", "må du prøve mener")),

    ## Her er høstens [verste/beste/___] smarttelefoner
    list(topic = "Teknologi",
         case = "Her er høstens [verste/beste/___] smarttelefoner",
         base = "Her er høstens %s smarttelefoner",
         valance = c("", "verste", "beste")),

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
