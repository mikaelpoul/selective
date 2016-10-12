pkgs <- c("tidyr", "dplyr")
for (pkg in pkgs) library(pkg, character.only = TRUE)


kilder <- c("NRK", "TV2", "Klassekampen", "Dagens Næringsliv", "VG", "Aftenposten", "Dagbladet")
partier <- c("Rødt", "Sv", "Ap", "Sp", "Mdg", "Venstre", "Krf", "Høyre", "Frp")
base_omtal <- c("nøytral", "negativ", "positiv")
base_retning <- c("uenig", "enig")

gen_headline <- function(vars) {
  ##kilder <- c("NRK", "TV2", "Klassekampen", "Dagens Næringsliv", "VG", "Aftenposten", "Dagbladet")
  partier <- c("Rødt", "Sv", "Ap", "Sp", "Mdg", "Venstre", "Krf", "Høyre", "Frp")
  base_omtal <- c("nøytral", "negativ", "positiv")
  base_retning <- c("uenig", "enig")
  ords <- c("parti", "omtale", "retning")
  comp <- expand.grid(vars$base, vars[[ords[vars$order[1]]]],
                      vars[[ords[vars$order[2]]]], vars[[ords[vars$order[3]]]],
                      stringsAsFactors = FALSE)
  names(comp) <- c("base", ords[vars$order])
  args_list <- lapply(1:nrow(comp), function(x) as.character(comp[x, ]))
  comp$headline <- sapply(args_list, function(args) do.call(sprintf, as.list(args)))
  return(comp)
}

##
## Hard News --------------------------------------------------------------------------------------
##

## [Parti]-politiker [får kjeft for/får skryt for/presenterer] forslag om å [øke nivået på toppskatten/senke nivå på toppskatten]
c1 <- list(opinion = "Regjeringen burde red. inntektsforskjeller",
           case = "[Parti]-profil [får kjeft for/får skryt for/lanserer] forslag om å [øke toppskatten/senke toppskatten]",
           base = "%s-politiker %s å %s toppskatten",
           order = 1:3,
           parti = partier,
           omtale = c("foreslår", "får kjeft for forslag om", "får skryt for forslag om"),
           retning = c("øke", "senke"))
## gen_headline(c1)$headline

## [Refser/roser/___] [Parti]-politikers forslag om skatte[økning/kutt] for eldre
c2 <- list(opinion = "Regjeringen burde red. inntektsforskjeller",
           case = "[Refser/roser/___] [Parti]-topp forslag om skatte[økning/kutt] for eldre",
           base = "%s%s-topp med nytt forslag om skatte%s for eldre",
           order = c(2, 1, 3),
           parti = partier,
           omtale = c("", "Refser ", "Roser "),
           retning = c("økning", "kutt"))
## gen_headline(c2)$headline

## [Refser/roser/omtaler] [parti]-politiker for Facebook-innlegg som [er veldig negativ til/er veldig positiv til] at vi får flere muslimer inn i skolen
c3 <- list(opinion = "Det er bedre for et land med flere kulturer/religioner",
           case = "[Refser/roser/omtaler] [parti]-politiker for å skrive [er negativ til/er positiv til] muslimer i norske skoler",
           base = "%s-politiker %s: «vi %s flere muslimer i norske skoler»",
           order = c(1, 2, 3),
           parti = partier,
           omtale = c("med Facebook-innlegg", "fordømmes etter Facebook-innlegg", "lovprises etter Facebook-innlegg"),
           retning = c("trenger ikke", "trenger"))
## gen_headline(c3)$headline

## Lokal [parti]-politiker [får skryt etter uttalelser /får kjeft etter uttalelser/uttaler seg] om å [fjerne/bygge ut] asylmottak
c4 <- list(opinion = "Det er bedre for et land med flere kulturer/religioner",
           case = "Lokal [parti]-politiker [får skryt etter uttalelser/får kjeft etter uttalelser/uttaler seg] om å [fjerne/bygge ut] asylmottak",
           base = "Lokal %s-politiker %s vil %s 5000 nye flyktninger",
           order = c(1, 2, 3),
           parti = partier,
           omtale = c("–", "skjelles ut –", "hylles –"),
           retning = c("hindre at vi tar imot", "ta imot"))
## gen_headline(c4)$headline

## [misliker sterkt/liker sterkt/___] [Parti]-politikers forslag om å [øke/senke] formueskatten
c7 <- list(opinion = "Skattebyrden burde reduseres",
           case = "[misliker sterkt/liker sterkt/___] [Parti]-politikers forslag om å [øke/senke] formueskatten",
           base = "Politiker fra %s vil %s formueskatten%s",
           order = c(1, 3, 2),
           parti = partier,
           omtale = c("", ", mottar sterk kritikk", ", blir roset"),
           retning = c("øke", "senke"))
## gen_headline(c7)$headline

## [Stormer mot/Jubler for/___] [Parti]-politikers ønske om å [innføre/avskaffe] eiendomsskatten for samtlige byer i Norge
c8 <- list(opinion = "Skattebyrden burde reduseres",
           case = "[Stormer mot/Jubler for/___] [Parti]-politikers ønske om å [innføre/avskaffe] eiendomsskatten for samtlige byer i Norge",
           base = "%s%s-topp som vil %s eiendomsskatten i alle norske byer",
           order = c(2, 1, 3),
           parti = partier,
           omtale = c("", "Stormer mot ", "Jubler for "),
           retning = c("innføre", "avskaffe"))
## gen_headline(c8)$headline

## [Vil åpne for at noen/Vil sikre at ingen] private skoler skal kunne ta ut profitt – [Parti]-representant [refses/roses/uttaler seg]
## Teste, hvem vil sikre... ?
c9 <- list(opinion = "Kommersielle skoler burde tillates",
           case = "[Vil åpne for at noen/Vil sikre at ingen] private skoler skal kunne ta ut profitt – [Parti]-representant [refses/roses/uttaler seg]",
           base = "%s-representant%s %s private skoler skal kunne ta ut profitt",
           order = c(1, 2, 3),
           parti = partier,
           omtale = c("", " refses –", " roses –"),
           retning = c("vil sikre at ingen", "vil åpne for at noen"))
## gen_headline(c9)$headline

## [Parti]-profil [høster jubel for / høster sterk kritikk for /lanserer] forslag om [økning/redusering] av antall privatskoler i Norge
c10 <- list(opinion = "Kommersielle skoler burde tillates",
           case = "[Parti]-profil [høster jubel for / høster sterk kritikk for /lanserer] forslag om [økning/redusering] av antall privatskoler i Norge",
           base = "%s-profil %s forslag om %s privatskoler i Norge",
           order = c(1, 2, 3),
           parti = partier,
           omtale = c("lanserer", "møter proteststorm for", "møter begeistring for"),
           retning = c("færre", "flere"))
## gen_headline(c10)$headline

## Foreslår å [privatisere/la kommunen ta over] flere tjenester i eldreomsorgen, ansvarlig [Parti]-representant [får refs/ får ros /forklarer nærmere]
c11 <- list(opinion = "Mange offentlige aktiviteter kan gjøres bedre av private aktører",
            case = "Foreslår å [privatisere/la kommunen ta over] eldreomsorgen, ansvarlig [Parti]-representant [får refs/ får ros /forklarer nærmere]",
           base = "Foreslår å %s eldreomsorgen, ansvarlig %s-politiker %s",
           order = c(3, 1, 2),
           parti = partier,
           omtale = c("forklarer nærmere", "refses", "roses"),
           retning = c("la kommunen ta seg av", "privatisere"))
## gen_headline(c11)$headline

## [Parti]-politiker [får sterk kritikk for/ får mye ros for/presenterer] forslag om at [bemanningsbyråer/kommunen] i større grad tar over vasking av omsorgsboliger
c12 <- list(opinion = "Mange offentlige aktiviteter kan gjøres bedre av private aktører",
            case = "[Parti]-politiker [får sterk kritikk for/ får mye ros for/presenterer] forslag om at private [bemanningsbyråer/kommunen] i større grad ta seg av vasking av omsorgsboliger",
            base = "%s-politiker %s forslag: %s NSB",
            order = c(1, 2, 3),
            parti = partier,
            omtale = c("presenterer", "får pepper for", "får skryt for"),
            retning = c("vil nekte privatisering av", "vil privatisere"))
## gen_headline(c12)$headline

## [Parti]-politiker [får sterk kritikk for/ får mye ros for/presenterer] forslag om at [be
c13 <- list(opinion = "Ikke tillate oljeboring i LoVe",
            case = "[Parti]-topp kjemper [for/mot] oljeåpning i Lofoten[BLANK/, - får kritikk/, - får ros]",
            base = "%s-topp kjemper %s oljeboring i Lofoten%s",
            order = c(1, 3, 2),
            parti = partier,
            omtale = c("", " - møter massiv motstand", " - får massiv støtte"),
            retning = c("for", "mot"))
## gen_headline(c13)$headline

## [Parti]-profil [på twitter/blir latterligjort etter twittermelding/blir roset etter twittermelding] - vil [åpne for/nekte for] oljeboring i Nord
c14 <- list(opinion = "Ikke tillate oljeboring i LoVe",
            case = "[Parti]-profil [på twitter/blir latterligjort etter twittermelding/blir roset etter twittermelding] - vil [åpne for/nekte for] oljeboring i Nord",
            base = "%s-profil %s vil %s oljeboring i nord",
            order = c(1, 2, 3),
            parti = partier,
            omtale = c("på twitter:", "blir latterligjort etter twittermelding –", "blir hedret etter twittermelding –"),
            retning = c("åpne for", "stenge for"))
## gen_headline(c14)$headline

## '[Parti]-politiker [på Facebook:/får kritikk etter facebook-melding:/får ros etter facebook-melding:] "Homofile par [MÅ/MÅ IKKE] få like rettigheter som heterofile"'
c15 <- list(opinion = "Homofile og heterofile bør ha like rettigheter",
            case = '[Parti]-politiker [på Facebook:/får kritikk etter facebook-melding:/får ros etter facebook-melding:] «Homofile par [MÅ/MÅ IKKE] få like rettigheter som heterofile»',
            base = '%s-politiker %s «Homofile par %s få like rettigheter som heterofile»',
            order = c(1, 2, 3),
            parti = partier,
            omtale = c("på Facebook:", "får kritikk etter facebook-status:", "får ros etter facebook-status:"),
            retning = c("MÅ IKKE", "MÅ"))
## gen_headline(c15)$headline


## [Parti]-politiker [med/får kritikk etter/får ros etter] homovennlig/fientlig lovforslag
c16 <- list(opinion = "Homofile og heterofile bør ha like rettigheter",
            case = "[Parti]-topp [med/får kritikk etter/får ros etter] homovennlig/fientlig lovforslag",
            base = "%s-politiker %s %s lovforslag",
            order = c(1, 2, 3),
            parti = partier,
            omtale = c("med", "hudflettes etter", "lovprises etter"),
            retning = c("homofientlig", "homovennlig"))
## gen_headline(c16)$headline


##
## Kjendisnyheter ------------------------------------------------------------------------------------
##

gen_soft_headline <- function(vars) {
  base_omtal <- c("nøytral", "negativ", "positiv")
  comp <- expand.grid(type = vars$type, case = vars$case,
                      base = vars$base, omtale = vars$omtale,
                      stringsAsFactors = FALSE)
  args_list <- lapply(1:nrow(comp), function(x) as.character(comp[x, c(3, 4)]))
  comp$headline <- sapply(args_list, function(args) do.call(sprintf, as.list(args)))
  return(comp)
}

headlines_soft_data <- function() {

  ## TV-profiler [skiller seg/gifter seg/snakker om livet sammen]
  s1 <- list(type = "Kjendis",
             case = "TV-profiler [skiller seg/gifter seg/snakker om livet]",
             base = "TV-profiler %s",
             omtale = c("snakker om livet sammen", "skiller seg", "gifter seg"))
  ##gen_soft_headline(s1)$headline

  ## Kjendisfesten [gikk skikkelig galt/var en stor suksess/ble holdt i går]
  s2 <- list(type = "Kjendis",
             case = "Kjendisfesten [gikk skikkelig galt/var en stor suksess/ble holdt i går]",
             base = "Kjendisfesten %s",
             omtale = c("ble holdt i går", "gikk skikkelig galt", "var en stor suksess"))
  ## gen_soft_headline(s2)$headline

  ## Tror Norge [taper/vinner/spiller uavgjort] i morgendagens fotballkamp
  s3 <- list(type = "Sport",
             case = "Tror Norge [taper/vinner/spiller uavgjort] i morgendagens fotballkamp",
             base = "Tror Norge %s i morgendagens fotballkamp",
             omtale = c("spiller uavgjort", "taper", "vinner"))
  ## gen_soft_headline(s3)$headline

  ## Stjernespiller [kampklar til /ute med skade til/snakker om] morgendagens kamp
  s4 <- list(type = "Sport",
             case = "Stjernespiller [kampklar til /ute med skade til/snakker om] morgendagens kamp",
             base = "Stjernespiller %s morgendagens kamp",
             omtale = c("snakker om", "ute med skade til", "kampklar til"))
  ## gen_soft_headline(s4)$headline

  ## Norges store sangstjerne [får ros/ får refs/omtales] i fersk anmeldelse
  s5 <- list(type = "Anmeldelse",
             case = "Norges store sangstjerne [får ros/ får refs/omtales] i fersk anmeldelse",
             base = "Norges store sangstjerne %s i fersk anmeldelse",
             omtale = c("omtales", "får refs", "får ros"))
  ## gen_soft_headline(s5)$headline

  ## Denne restauranten [må du prøve mener / må du unngå mener/testes av] våre anmeldere
  s6 <- list(type = "Anmeldelse",
             case = "Denne restauranten [må du prøve mener / må du unngå mener/testes av] våre anmeldere",
             base = "Denne restauranten %s våre anmeldere",
             omtale = c("er testet av", "må du unngå mener", "må du prøve mener"))
  ## gen_soft_headline(s6)$headline

  ## Her er høstens [verste/beste/___] smarttelefoner
  s7 <- list(type = "Teknologi",
             case = "Her er høstens [verste/beste/___] smarttelefoner",
             base = "Her er høstens %s smarttelefoner",
             omtale = c("", "verste", "beste"))
  ## gen_soft_headline(s7)$headline

  ## Disse TV-ene [kommer best ut / kommer vers ut /er med] i vår nye test
  s8 <- list(type = "Teknologi",
             case = "Disse TV-ene [kommer best ut / kommer vers ut /er med] i vår nye test",
             base = "Disse TV-ene %s i vår nye test",
             omtale = c("er med", "kommer verst ut", "kommer best ut"))
  ## gen_soft_headline(s8)$headline

  ## Dette [er det verste/er det beste/kan gjøres] for å gå ned i vekt
  s9 <- list(type = "Livsstil",
             case = "Dette [er det verste/er det beste/kan gjøres] for å gå ned i vekt",
             base = "Dette %s ned i vekt",
             omtale = c("kan du gjøre for å", "er det verste for å gå", "er det beste for å gå"))
  ## gen_soft_headline(s9)$headline

  ## [Suksesshistoriene/Mareritthistoriene/Historier] om høstens treningstrend.
  s10 <- list(type = "Livsstil",
              case = "[Suksesshistoriene/Mareritthistoriene/Historier] om høstens treningstrend",
              base = "%s høstens treningstrend",
              omtale = c("Dette er", "Mareritthistoriene om", "Suksesshistoriene om"))
  ## gen_soft_headline(s10)$headline

  ## Her er helgens filmer [du bør unngå/du bør se/___]
  s11 <- list(type = "Konsert/film",
              case = "Her er helgens filmer [du bør unngå/du bør se/___]",
              base = "Her er helgens filmer %s",
              omtale = c("", "du bør unngå", "du bør se"))
  ## gen_soft_headline(s11)$headline

  ## Her er helgens filmer [du bør unngå/du bør se/___]
  s12 <- list(type = "Konsert/film",
              case = "Konsert [gikk skikkelig galt/gikk skikkelig bra/ble holdt] i helgen",
              base = "%s",
              omtale = c("Helgens konsert", "Her går konserten fryktelig galt", "Her går konserten skikkelig bra"))
  ## gen_soft_headline(s12)$headline

  return(lapply(as.list(paste0("s", 1:12)), get))
}

soft_headlines <- headlines_soft_data()


headlines_soft_make <- function(soft_headlines) {
  base_omtal <- c("nøytral", "negativ", "positiv")
  headlines <- lapply(soft_headlines, function(vars) {
    comp <- expand.grid(type = vars$type, base = vars$base, omtale = vars$omtale,
                        stringsAsFactors = FALSE)
    args_list <- lapply(1:nrow(comp), function(x) as.character(comp[x, 2:3]))
    comp$headline <- sapply(args_list, function(args) do.call(sprintf, as.list(args)))
    return(comp)
  })
  headlines <- do.call("rbind", headlines)
  return(headlines)
}

headlines_soft_make(soft_headlines)


##
## Make list of headlines
##

hard <- lapply(paste0("c", c(1:4, 7:16)), function(x) {
  x <- get(x)
  return(gen_headline(x))
})
hard <- do.call("rbind", hard)
hardlist <- data.frame(id = 1:nrow(hard),
                       overskrift = hard$headline)
write.csv(hardlist, file = "output/overskrifter-politikk.csv", row.names = FALSE)


soft <- lapply(paste0("s", 1:12), function(x) {
  x <- get(x)
  return(gen_soft_headline(x))
})
soft <- do.call("rbind", soft)
softlist <- data.frame(id = 1:nrow(soft),
                       overskrift = soft$headline)
write.csv(softlist, file = "output/overskrifter-underholdning.csv", row.names = FALSE)


##
## Test functions for sampling headlines
##

## sample_sh <- function(n = 3) {
##   tt <- lapply(paste0("s", 1:12), function(x) {
##     x <- get(x)
##     return(gen_soft_headline(x))
##   })
##   tt <- do.call("rbind", tt)
##   return(tt$headline[sample(1:nrow(tt), n)])
## }

## sample_hh <- function(n = 6) {
##   tt <- lapply(paste0("c", 1:10), function(x) {
##     x <- get(x)
##     return(gen_headline(x))
##   })
##   tt <- do.call("rbind", tt)
##   return(tt$headline[sample(1:nrow(tt), n)])
## }

## lineup <- function(ver = 3) {
##   if (ver == 1) {
##     return(sample_hh(6))
##   } else if (ver == 2) {
##     return(sample_hh(10))
##   } else if (ver == 3) {
##     out <- c(sample_hh(6), sample_sh(4))
##     out <- out[sample(1:10, 10)]
##     return(out)
##   }
## }

## lineup_table <- function() {
##   ## Randomly choose version (hard/hard + hard/hard + soft)
##   version <- sample(1:3, 1)
##   num <- switch(version,
##                 "1" = 6,
##                 "2" = 10,
##                 "3" = 10)
##   ## Possible values:
##   kilder <- c("NRK", "TV2", "Klassekampen", "Dagens Næringsliv", "VG", "Aftenposten", "Dagbladet")
##   anbefalinger <- c(0, 0, 0, 95, 98, 106, 117, 1133, 1211, 1024)
##   ## Make table:
##   out <- data.frame(kilde = kilder[sample(1:length(kilder), num, TRUE)],
##                     anbefalinger = paste0(anbefalinger[sample(1:length(anbefalinger), num, FALSE)], " personer anbefaler"),
##                     overskrift = lineup(version))
##   ## Show/Dont show recommendation
##   show_anb <- sample(c(TRUE, FALSE), 1)
##   if (!show_anb) out <- out[, -c(grep("anbefalinger", names(out)))]
##   ## show_kilder <- sample(c(TRUE, FALSE), 1)
##   ##if (!show_kilder) out <- out[, -c(grep("kilder", names(out)))]
##   return(out)
## }


library(rsconnect)
##setwd("selective_rshiny")
rsconnect::deployApp(getwd())


