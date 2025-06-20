# load required libraries
library(conjoint)
library(dplyr)
library(purrr)
library(readxl)
library(usethis)
library(ggplot2)
library(knitr)

# source custom functions from Shingo-sensei
source("SCRIPTS/conjoint_tool_new.R")

# define the attributes
attribute <- list(
  Price = c("200", "400", "600"),
  Form = c("Bread and Pastries","Wrap","Yougurt Jelly"),
  PurchaseLocation = c("On Route to work","Within 150m", "More than 150m"),
  FunPackaging = c("None", "Attack on Titan", "Doan Ritsu"),
  CognitiveSupport = c("None","Caffeine or Alertness boost",
                       "Natural Mood Stabilizer"),
  DigitalIntegration = c("None", "Loyalty points", "Nutrition Advice")
)


# Full factorial design
profiles <- expand.grid(attribute)

# Orthogonal design
design.o <- caFactorialDesign(data=profiles, type="fractional", cards=13)
print(design.o)
