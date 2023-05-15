derive_areas <- function(councilval = NULL){

  checkmate::assert_choice(councilval,
                          choices = c("Highland", "Argyll and Bute"),
                          null.ok = TRUE)

  if (!is.null(councilval)) {

    # determine the parent areas
    if (councilval == "Highland") {
      parents <- c("Badenoch and Strathspey",
                   "Caithness",
                   "East Ross",
                   "Inverness" ,
                   "Lochaber",
                   "Mid Ross",
                   "Nairn and Nairnshire",
                   "Skye, Lochalsh and West Ross",
                   "Sutherland")
     return(parents)
    }


    if (councilval == "Argyll and Bute") {
      parents <- c("Bute and Cowal",
                   "Helensburgh and Lomond",
                   "Mid-Argyll, Kintyre and Islay",
                   "Oban, Lorn and the Isles")
      return(parents)
    }


  }

  if (is.null(councilval)) {

    parents <- c("Badenoch and Strathspey",
                 "Caithness",
                 "East Ross",
                 "Inverness" ,
                 "Lochaber",
                 "Mid Ross",
                 "Nairn and Nairnshire",
                 "Skye, Lochalsh and West Ross",
                 "Sutherland",
                 "Bute and Cowal",
                 "Helensburgh and Lomond",
                 "Mid-Argyll, Kintyre and Islay",
                 "Oban, Lorn and the Isles")
    return(parents)
  }

}
