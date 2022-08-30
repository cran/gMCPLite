## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dev = "ragg_png",
  dpi = 96,
  fig.retina = 1,
  fig.width = 7.2916667,
  fig.asp = 0.618,
  fig.align = "center",
  out.width = "80%"
)

## ---- echo=FALSE, message=FALSE, warning=FALSE--------------------------------
library(gMCPLite)
library(dplyr)
library(gridExtra)

## ---- message=FALSE-----------------------------------------------------------
hGraph()

## -----------------------------------------------------------------------------
hGraph(
  nHypotheses = 3,
  nameHypotheses = c("HA\n First", "HC\n Second", "HB\n Third"),
  alphaHypotheses = c(.2, .3, .5),
  wchar = "w" # Character before weights
)

## ---- message=FALSE, fig.asp=0.5----------------------------------------------
grid.arrange(
  # Left graph in figure
  hGraph(
    nHypotheses = 3,
    size = 5, # Decrease hypothesis text size from default 6
    halfWid = 1.25, # Increase ellipse width from default 0.5
    trhw = 0.25, # Increase transition box sizes from default 0.075
    radianStart = pi / 2, # First hypothesis top middle
    offset = pi / 20, # Decrease offset between transition lines
    arrowsize = .03 # Increase from default 0.02
  ),
  # Right graph in figure
  hGraph(
    nHypotheses = 3,
    x = c(-1, 1, -1), # Custom placement using x and y
    y = c(1, 1, -1),
    halfWid = 0.7, # Increase ellipse width from default 0.5
    boxtextsize = 3, # Decrease box text size from default 4
    trprop = .15 # Slide transition boxes closer to initiating hypothesis
  ),
  nrow = 1
)

## ---- fig.width=9, fig.asp=0.5------------------------------------------------
grid.arrange(
  # Left graph in figure
  hGraph(
    fill = c(1, 1, 2, 2),
    alphaHypotheses = c(.2, .2, .2, .4) * .025
  ),
  # Right graph in figure
  hGraph(
    fill = c(1, 1, 2, 2),
    palette = c("pink", "lightblue"),
    alphaHypotheses = c(.2, .2, .2, .4) * .025
  ),
  nrow = 1
)

## -----------------------------------------------------------------------------
hGraph(
  nHypotheses = 3,
  fill = c(1, 1, 2),
  palette = c("yellow", "lightblue"),
  legend.name = "Color scheme",
  labels = c("Primary", "Secondary"),
  legend.position = c(.75, .25)
)

## -----------------------------------------------------------------------------
hGraph(
  nHypotheses = 3,
  m = matrix(c(
    0, 1, 0,
    .2, 0, .8,
    .3, .7, 0
  ),
  nrow = 3, byrow = TRUE
  ),
)

