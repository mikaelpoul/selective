#' Make all variations of a headline from headline data
#'
#' Make all variations of a headline from headline data in list format (see \code{headlines_data()}). Returns a data frame with the headline, as well as variables identifying the integrated treatment component.
#'
#' @param news_data Headline data in list format (see \code{headlines_data()}).
#' @keywords headlines prepublication
#'
#' @examples
#'\dontrun{
#'  hard_news_data <- headlines_data("hard")[[1]]
#'  hard_news <- headlines_make_from_data(hard_news_data)
#'  print(hard_news$headline[1])
#' }
#' @export
headlines_make_from_data <- function(news_data) {
  if ("hard_news_data" %in% class(news_data)) {
    headlines_make_from_data.hard_news_data(news_data)
  } else {
    headlines_make_from_data.soft_news_data(news_data)
  }
}

headlines_make_from_data.hard_news_data <- function(vars) {

  ## Base values
  base_opinion <- c("reduce_ineq",
                    "rel_div_good",
                    "ref_social_rights",
                    "lower_taxes",
                    "allow_priv",
                    "priv_better",
                    "not_allow_love",
                    "eq_rights")
  base_opinion_label <- c("The government should reduce income disparities",
                          "It is better for a country if there is a diversity of different religions and beliefs",
                          "Refugees should have the same rights to social assistance as Norwegians have, even if they are not Norwegian citizens",
                          "The tax burden should be reduced",
                          "Commercial private schools should be allowed",
                          "Many public activities could be done better and cheaper by the private sector",
                          "We should not allow oil and gas extraction in the Lofoten and Vesterålen areas",
                          "Heterosexual and homosexual couples should have equal rights.")
  base_party <- c("none", "rodt", "sv", "ap", "sp", "mdg", "v", "krf", "h", "frp")
  base_party_label <- c("No party mentioned",
                        "Red Party",
                        "Socialist Left Party",
                        "Labour Party",
                        "Centre Party",
                        "Green Party",
                        "Liberal Party",
                        "Christian Democratic Party",
                        "Conservative Party",
                        "Progress Party")
  base_valance <- c("neutral", "negativ", "positiv")
  base_direction <- c("disagree", "agree")
  base_order <- c("party", "valance", "direction")

  ## Make headlines
  headlines <- expand.grid(vars$base,
                           vars[[base_order[vars$order[1]]]],
                           vars[[base_order[vars$order[2]]]],
                           vars[[base_order[vars$order[3]]]],
                           stringsAsFactors = FALSE)
  names(headlines) <- paste0(c("base", base_order[vars$order]), "_wording")
  args_list <- lapply(1:nrow(headlines), function(x) as.character(headlines[x, ]))
  headlines$headline <- sapply(args_list, function(args) do.call(sprintf, as.list(args)))

  ## Attitude and link
  headlines$opinion <- base_opinion[match(vars$opinion, base_opinion_label)]
  headlines$opinion_label <- vars$opinion
  headlines$opinion_link <- vars$link

  ## Base wording
  fill_1 <- paste0("[", paste0(ifelse(grepl("^ *$", vars[[base_order[vars$order[1]]]]), "BLANK", vars[[base_order[vars$order[1]]]]), collapse = "/"), "]")
  fill_2 <- paste0("[", paste0(ifelse(grepl("^ *$", vars[[base_order[vars$order[2]]]]), "BLANK", vars[[base_order[vars$order[2]]]]), collapse = "/"), "]")
  fill_3 <- paste0("[", paste0(ifelse(grepl("^ *$", vars[[base_order[vars$order[3]]]]), "BLANK", vars[[base_order[vars$order[3]]]]), collapse = "/"), "]")
  headlines$base <- sprintf(headlines$base[1], fill_1, fill_2, fill_3)

  ## Party
  headlines$party <- NA
  for (i in 1:length(base_party)) {
    headlines$party <- ifelse(headlines$party_wording == vars$party[i], base_party[i], headlines$party)
  }
  headlines$party <- factor(headlines$party, levels = base_party)

  ## Party label
  headlines$party_label <- NA
  for (i in 1:length(base_party_label)) {
    headlines$party_label <- ifelse(headlines$party_wording == vars$party[i], base_party_label[i], headlines$party_label)
  }
  headlines$party_label <- factor(headlines$party_label, levels = base_party_label)

  ## Valance
  headlines$valance <- NA
  for (i in 1:length(base_valance)) {
    headlines$valance <- ifelse(headlines$valance_wording == vars$valance[i], base_valance[i], headlines$valance)
  }
  headlines$valance <- factor(headlines$valance, levels = base_valance)

  ## Direction
  headlines$direction <- NA
  for (i in 1:length(base_direction)) {
    headlines$direction <- ifelse(headlines$direction_wording == vars$direction[i], base_direction[i], headlines$direction)
  }
  headlines$direction <- factor(headlines$direction, levels = base_direction)

  ## Sort and return df
  headlines <- headlines[, c("opinion_label", "opinion", "opinion_link", "party_label", "party", "valance", "direction",
                             "base", "party_wording", "valance_wording", "direction_wording", "base_wording", "headline")]
  headlines <- headlines[order(headlines$party, headlines$valance, headlines$direction), ]
  rownames(headlines) <- NULL
  return(as.data.frame(headlines))
}

headlines_make_from_data.soft_news_data <- function(vars) {
  base_valance <- c("neutral", "negative", "positive")

  headlines <- expand.grid(topic = vars$topic,
                           base_wording = vars$base,
                           valance_wording = vars$valance,
                           stringsAsFactors = FALSE)
  args_list <- lapply(1:nrow(headlines), function(x) as.character(headlines[x, c(2, 3)]))
  headlines$headline <- sapply(args_list, function(args) do.call(sprintf, as.list(args)))

  ## Base wording
  fill_1 <- paste0("[", paste0(ifelse(grepl("^ *$", vars$valance), "BLANK", vars$valance), collapse = "/"), "]")
  headlines$base <- sprintf(headlines$base[1], fill_1)

  ## Valance
  headlines$valance <- NA
  for (i in 1:length(base_valance)) {
    headlines$valance <- ifelse(headlines$valance_wording == vars$valance[i], base_valance[i], headlines$valance)
  }
  headlines$valance <- factor(headlines$valance, levels = base_valance)

  ## Sort and return df
  headlines <- headlines[, c("topic", "base", "valance", "valance_wording", "base_wording",
                             "headline")]
  headlines <- headlines[order(headlines$valance), ]
  rownames(headlines) <- NULL
  return(as.data.frame(headlines))
}


#' Make all headlines used in the experiment
#'
#' Make all headlines used in the experiment. Uses \code{headlines_data()} and \code{headlines_make_from_data()}. Returns a data frame with the headline, as well as associated id (linked to the data) and variables identifying the integrated treatment component.
#'
#' @param news_type What news type to get, either \code{hard} (default) or \code{soft}.
#' @keywords headlines prepublication
#'
#' @examples
#'\dontrun{
#'  # Get and print the hard news headlines
#'  hard_news <- headlines_all("hard")
#'  print(hard_news$headline)
#'
#'  # Get and print the soft news headlines
#'  soft_news <- headlines_all("soft")
#'  print(soft_news$headline[1])
#' }
#' @export
headlines <- function(news_type = "hard") {
  headlines_data <- headlines_data(news_type)
  all_headlines <- lapply(headlines_data, headlines_make_from_data)
  all_headlines <- do.call("rbind", all_headlines)
  all_headlines$id <- 1:nrow(all_headlines)
  rownames(all_headlines) <- NULL
  return(all_headlines)
}
