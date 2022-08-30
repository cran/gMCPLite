## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "#>",
  dev = "ragg_png",
  dpi = 96,
  fig.retina = 1,
  fig.width = 7.2916667,
  fig.asp = 0.618,
  fig.align = "center",
  out.width = "80%"
)

## ---- message=FALSE, warning=FALSE--------------------------------------------
### THERE SHOULD BE NO NEED TO MODIFY THIS CODE SECTION
# Prefer fixed notation
old <- options(scipen = 999)
# Colorblind palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# 2 packages used for data storage and manipulation: dplyr, tibble
library(dplyr)
library(tibble)
# 2 packages used for R Markdown capabilities: knitr, kableExtra
library(knitr)
library(kableExtra)
library(gt)
library(ggplot2) # For plotting
library(gsDesign) # Group sequential design capabilities
library(gMCPLite) # Multiplicity evaluation

## ---- warning=FALSE, message=FALSE--------------------------------------------
### THIS CODE NEEDS TO BE MODIFIED FOR YOUR STUDY
# If needed, see help file for gMCPLite::hGraph() for explanation of parameters below
# Hypothesis names
nameHypotheses <- c(
  "H1: OS\n Subgroup",
  "H2: OS\n All subjects",
  "H3: PFS\n Subgroup",
  "H4: PFS\n All subjects",
  "H5: ORR\n Subgroup",
  "H6: ORR\n All subjects"
)
# Number of hypotheses to be tested
nHypotheses <- length(nameHypotheses)
# Transition weights for alpha reallocation (square matrix)
m <- matrix(c(
  0, 1, 0, 0, 0, 0,
  0, 0, .5, .5, 0, 0,
  0, 0, 0, 1, 0, 0,
  0, 0, 0, 0, .5, .5,
  0, 0, 0, 0, 0, 1,
  .5, .5, 0, 0, 0, 0
), nrow = 6, byrow = TRUE)
# Initial Type I error assigned to each hypothesis (one-sided)
alphaHypotheses <- c(.01, .01, .004, 0.000, 0.0005, .0005)
fwer <- sum(alphaHypotheses)
# Make a ggplot representation of the above specification and display it
g <- gMCPLite::hGraph(6,
  alphaHypotheses = alphaHypotheses, m = m, nameHypotheses = nameHypotheses,
  palette = cbPalette,
  halfWid = 1, halfHgt = .35, xradius = 2.5, yradius = 1, offset = 0, trhw = .15,
  x = c(-1.25, 1.25, -2.5, 2.5, -1.25, 1.25), y = c(2, 2, 1, 1, 0, 0),
  trprop = 0.4, fill = as.character(c(2, 2, 4, 4, 3, 3))
) 
print(g)

## ---- results='asis'----------------------------------------------------------
osmedian <- 12 # Median control survival
# Derive group sequential design for OS in the targeted subgroup
ossub <- gsDesign::gsSurv(
  k = 3, # 3 analyses for OS
  test.type = 1, # Efficacy bound only (no futility)
  alpha = alphaHypotheses[1], # Allocated alpha from design hypothesis group
  beta = 0.1, # Type 2 error (1 - power)
  hr = 0.65, # Assumed hazard ratio for power calculation
  timing = c(0.61, 0.82), # Choose these to match targeted calendar timing of analyses
  sfu = sfLDOF, # Spending function to approximate O'Brien-Fleming bound
  lambdaC = log(2) / osmedian, # Exponential control failure rate
  eta = 0.001, # Exponential dropout rate
  gamma = c(2.5, 5, 7.5, 10), # Relative enrollment rates by time period
  R = c(2, 2, 2, 12), # Duration of time periods for rates in gamma
  T = 42, # Planned study duration for OS
  minfup = 24 # Planned minimum follow-up after end of enrollment
)
tab <- gsDesign::gsBoundSummary(ossub)
rownames(tab) <- 1:nrow(tab)
cat(summary(ossub))

## -----------------------------------------------------------------------------
# tab %>% kable(caption = "Design for OS in the subgroup.") %>% kable_styling()
tab %>%
  gt() %>%
  tab_header(title = "Design for OS in the Subgroup") %>%
  cols_align(align = "left", columns = Value) %>%
  tab_footnote(
    footnote = "Cumulative boundary crossing probability includes crossing probability at earlier analysis.",
    locations = cells_body(columns = "Value", rows = c(9, 10, 14, 15))
  ) %>%
  tab_footnote(
    footnote = "Approximate hazard ratio at bound.",
    locations = cells_body(columns = "Value", rows = c(3, 8, 13))
  )

## ---- results = 'asis'--------------------------------------------------------
hr <- .75
beta <- .14
os <- gsDesign::gsSurv(
  k = 3, test.type = 4, alpha = 0.01, beta = beta, hr = hr,
  timing = c(0.62, 0.83), sfu = sfLDOF,
  sfl = sfHSD, sflpar = -3.25,
  lambdaC = log(2) / 12, eta = 0.001, S = NULL,
  gamma = c(2.5, 5, 7.5, 10), R = c(2, 2, 2, 12),
  T = 42, minfup = 24
)
tab <- gsDesign::gsBoundSummary(os)
rownames(tab) <- 1:nrow(tab)
cat(summary(os))

## -----------------------------------------------------------------------------
tab %>%
  kable(caption = "Design for OS in all subjects") %>%
  kable_styling()

## -----------------------------------------------------------------------------
plot(os, plottype = "HR", xlab = "Events")

## ---- results='asis'----------------------------------------------------------
hr <- .65
beta <- .149
pfssub <- gsDesign::gsSurv(
  k = 2, test.type = 6, astar = 0.1, alpha = 0.004, beta = beta, hr = hr,
  timing = .87, sfu = sfLDOF,
  sfl = sfHSD, sflpar = -8,
  lambdaC = log(2) / 5, eta = 0.02, S = NULL,
  gamma = c(2.5, 5, 7.5, 10), R = c(2, 2, 2, 12),
  T = 32, minfup = 14
)
tab <- gsDesign::gsBoundSummary(pfssub)
rownames(tab) <- 1:nrow(tab)
cat(summary(pfssub))

## -----------------------------------------------------------------------------
tab %>%
  kable(caption = "Design for PFS in the subgroup") %>%
  kable_styling()

## -----------------------------------------------------------------------------
hr <- .74
beta <- .15
pfs <- gsDesign::gsSurv(
  k = 2, test.type = 1, alpha = 0.004, beta = beta, hr = hr,
  timing = .86, sfu = sfLDOF,
  lambdaC = log(2) / 5, eta = 0.02, S = NULL,
  gamma = c(2.5, 5, 7.5, 10), R = c(2, 2, 2, 12),
  T = 32, minfup = 14
)
tab <- gsDesign::gsBoundSummary(pfs)
rownames(tab) <- 1:nrow(tab)
tab %>%
  kable(caption = "Design for PFS in the overall population") %>%
  kable_styling()

## -----------------------------------------------------------------------------
nBinomial(p1 = .35, p2 = .15, alpha = .0005, n = 378)

## -----------------------------------------------------------------------------
nBinomial(p1 = .3, p2 = .15, alpha = .0005, n = 756)

## -----------------------------------------------------------------------------
### THIS NEEDS TO BE MODIFIED TO MATCH STUDY
gsDlist <- list(ossub, os, pfssub, pfs, NULL, NULL)

## -----------------------------------------------------------------------------
### THIS NEEDS TO BE MODIFIED TO MATCH YOUR STUDY
# PFS, overall population
pfs$n.I <- c(675, 750)
# PFS, subgroup
pfssub$n.I <- c(265, 310)
# OS, overall population
os$n.I <- c(529, 700, 800)
# OS, subgroup
ossub$n.I <- c(185, 245, 295)

## ----warnings=FALSE,message=FALSE---------------------------------------------
### THIS NEEDS TO BE MODIFIED TO MATCH YOUR STUDY
inputResults <- tibble(
  H = c(rep(1, 3), rep(2, 3), rep(3, 2), rep(4, 2), 5, 6),
  Pop = c(
    rep("Subgroup", 3), rep("All", 3),
    rep("Subgroup", 2), rep("All", 2),
    "Subgroup", "All"
  ),
  Endpoint = c(rep("OS", 6), rep("PFS", 4), rep("ORR", 2)),
  # Example with some rejections
  nominalP = c(
    .03, .0001, .000001,
    .2, .15, .1,
    .2, .001,
    .3, .2,
    .00001,
    .1
  ),
  # Example with no rejections
  # nominalP = rep(.03, 12),
  Analysis = c(1:3, 1:3, 1:2, 1:2, 1, 1),
  events = c(ossub$n.I, os$n.I, pfssub$n.I, pfs$n.I, NA, NA),
  spendingTime = c(
    ossub$n.I / max(ossub$n.I),
    ossub$n.I / max(ossub$n.I),
    pfssub$n.I / max(pfssub$n.I),
    pfssub$n.I / max(pfssub$n.I),
    NA, NA
  )
)
kable(inputResults, caption = "DUMMY RESULTS FOR IA2.") %>%
  kable_styling() %>%
  add_footnote("Dummy results", notation = "none")

## ----message=FALSE------------------------------------------------------------
### USER SHOULD NOT NEED TO MODIFY THIS CODE
EOCtab <- NULL
EOCtab <- inputResults %>%
  group_by(H) %>%
  slice(1) %>%
  ungroup() %>%
  select("H", "Pop", "Endpoint", "nominalP")
EOCtab$seqp <- .9999
for (EOCtabline in 1:nHypotheses) {
  EOCtab$seqp[EOCtabline] <-
    ifelse(is.null(gsDlist[[EOCtabline]]), EOCtab$nominalP[EOCtabline], {
      tem <- filter(inputResults, H == EOCtabline)
      sequentialPValue(
        gsD = gsDlist[[EOCtabline]], interval = c(.0001, .9999),
        n.I = tem$events,
        Z = -qnorm(tem$nominalP),
        usTime = tem$spendingTime
      )
    })
}
EOCtab <- EOCtab %>% select(-"nominalP")
# kable(EOCtab,caption="Sequential p-values as initially placed in EOCtab") %>% kable_styling()

## ----message=FALSE,warning=FALSE----------------------------------------------
# Make a graph object
rownames(m) <- nameHypotheses
graph <- matrix2graph(m)
# Add weights to the object based on alpha allocation
graph <- setWeights(graph, alphaHypotheses / fwer)
rescale <- 45
d <- g$layers[[2]]$data
rownames(d) <- rownames(m)
# graph@nodeAttr$X <- rescale * d$x * 1.75
# graph@nodeAttr$Y <- -rescale * d$y * 2

## -----------------------------------------------------------------------------
result <- gMCP(graph = graph, pvalues = EOCtab$seqp, alpha = fwer)
result@rejected
# now map back into EOCtable (CHECK AGAIN!!!)
EOCtab$Rejected <- result@rejected
EOCtab$adjPValues <- result@adjPValues

## -----------------------------------------------------------------------------
# Number of graphs is used repeatedly
ngraphs <- length(result@graphs)
# Set up tibble with hypotheses rejected at each stage
rejected <- NULL
for (i in 1:length(result@graphs)) {
  rejected <- rbind(
    rejected,
    tibble(
      H = 1:nHypotheses, Stage = i,
      Rejected = as.logical(result@graphs[[i]]@nodeAttr$rejected)
    )
  )
}
rejected <- rejected %>%
  filter(Rejected) %>%
  group_by(H) %>%
  summarize(graphRejecting = min(Stage) - 1, .groups = "drop") %>% # Last graph with weight>0 where H rejected
  arrange(graphRejecting)

# Get final weights
# for hypotheses not rejected, this will be final weight where
# no hypothesis could be rejected
lastWeights <- as.numeric(result@graphs[[ngraphs]]@weights)
lastGraph <- rep(ngraphs, nrow(EOCtab))

# We will update for rejected hypotheses with last positive weight for each
if (ngraphs > 1) {
  for (i in 1:(ngraphs - 1)) {
    lastWeights[rejected$H[i]] <- as.numeric(result@graphs[[i]]@weights[rejected$H[i]])
    lastGraph[rejected$H[i]] <- i
  }
}
EOCtab$lastAlpha <- fwer * lastWeights
EOCtab$lastGraph <- lastGraph
EOCtabx <- EOCtab
names(EOCtabx) <- c(
  "Hypothesis", "Population", "Endpoint", "Sequential p",
  "Rejected", "Adjusted p", "Max alpha allocated", "Last Graph"
)
# Display table with desired column order
# Delayed following until after multiplicity graph sequence
# EOCtabx %>% select(c(1:4,7,5:6,8)) %>% kable() %>% kable_styling()

## ----message=FALSE,warning=FALSE,results='asis'-------------------------------
### THERE SHOULD BE NO NEED TO MODIFY THIS CODE SECTION
for (i in 1:ngraphs) {
  mx <- result@graphs[[i]]@m
  rownames(mx) <- NULL
  colnames(mx) <- NULL
  g <- gMCPLite::hGraph(
    nHypotheses = nHypotheses,
    alphaHypotheses = result@graphs[[i]]@weights * fwer,
    m = mx,
    nameHypotheses = nameHypotheses,
    palette = cbPalette,
    halfWid = 1, halfHgt = .35, xradius = 2.5, yradius = 1, offset = 0, trhw = .15,
    x = c(-1.25, 1.25, -2.5, 2.5, -1.25, 1.25), y = c(2, 2, 1, 1, 0, 0),
    trprop = .4, fill = as.character(c(2, 2, 4, 4, 3, 3))
  ) 
  cat(" \n")
  cat("####", paste(" Graph", as.character(i), " \n\n"))
  print(g)
  cat(" \n\n\n")
}

## -----------------------------------------------------------------------------
EOCtabx %>%
  select(c(1:4, 7, 5:6, 8)) %>%
  kable() %>%
  kable_styling()

## ----results='asis'-----------------------------------------------------------
for (i in 1:nHypotheses) {
  # Set up tab for hypothesis in output
  cat("####", paste(" Hypothesis", as.character(i), " \n"))
  # Get results for hypothesis
  hresults <- inputResults %>% filter(H == i)
  # Print out max alpha allocated
  xx <- paste("Max alpha allocated from above table: ",
    as.character(EOCtab$lastAlpha[i]),
    sep = ""
  )
  d <- gsDlist[[i]]
  # If not group sequential for this hypothesis, print the max alpha allocated
  # and the nominal p-value
  if (is.null(d)) {
    cat("Maximum alpha allocated: ")
    cat(EOCtab$lastAlpha[i])
    cat("\n\n")
    cat("Nominal p-value for hypothesis test: ")
    cat(hresults$nominalP)
    cat("\n\n")
  }
  # For group sequential tests, print max alpha allocated and
  # corresponding group sequential bounds
  if (!is.null(gsDlist[[i]])) {
    cat("Nominal p-values at each analysis for comparison to bounds in table below:\n\n")
    cat(hresults$nominalP)
    cat("\n\n")
    # Get other info for current hypothesis
    n.I <- hresults$events
    usTime <- hresults$spendingTime
    n.Iplan <- max(d$n.I)
    if (length(n.I) == 1) {
      n.I <- c(n.I, n.Iplan)
      usTime <- c(usTime, 1)
    }
    # If no alpha allocated, just print text line to note this along with the 0 alpha allocated
    if (EOCtab$lastAlpha[i] == 0) {
      cat("Maximum alpha allocated: 0\n\n")
      cat("No testing required\n\n")
    }
    if (EOCtab$lastAlpha[i] > 0) {
      dupdate <- gsDesign::gsDesign(
        alpha = EOCtab$lastAlpha[i],
        k = length(n.I),
        n.I = n.I,
        usTime = usTime,
        maxn.IPlan = n.Iplan,
        n.fix = d$n.fix,
        test.type = 1,
        sfu = d$upper$sf,
        sfupar = d$upper$param
      )
      tabl <- gsDesign::gsBoundSummary(dupdate,
        Nname = "Events",
        exclude = c(
          "B-value", "CP", "CP H1", "Spending",
          "~delta at bound", "P(Cross) if delta=0",
          "PP", "P(Cross) if delta=1"
        )
      )
      kable(tabl, caption = xx, row.names = FALSE) %>%
        kable_styling() %>%
        cat()
      cat("\n\n")
    }
  }
}

## -----------------------------------------------------------------------------
# Restore default options
options(old)

