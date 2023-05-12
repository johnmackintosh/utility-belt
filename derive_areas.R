derive_areas <- function(councilval){
  if (!is.null(councilval)) {

    # determine the parent areas
    if (councilval == "Highland") {
     areas <- c("Badenoch and Strathspey",
                   "Caithness",
                   "East Ross",
                   "Inverness" ,
                   "Lochaber",
                   "Mid Ross",
                   "Nairn and Nairnshire",
                   "Skye, Lochalsh and West Ross",
                   "Sutherland")
    }


    if (!councilval == "Highland") {
      areas <- c("Bute and Cowal",
                   "Helensburgh and Lomond",
                   "Mid-Argyll, Kintyre and Islay",
                   "Oban, Lorn and the Isles")
    }


  }

  if (is.null(councilval)) {

   areas <- c("Badenoch and Strathspey",
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
  }

}
