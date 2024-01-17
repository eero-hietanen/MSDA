library(shiny)
library(dplyr)
library(stringr)
library(MSstats)
library(MSstatsTMT)
library(MSstatsConvert)
library(data.table)
library(UniProt.ws)
library(DT)
library(shinyjs)
library(shinybusy)

options(shiny.maxRequestSize = 40 * 1024^2)

ui <- fluidPage(
  
  useShinyjs(),
  
  add_busy_spinner(spin = "orbit", color = "#f0bc13"),
  
  fluidRow(
    fileInput("evidence", "Upload evidence"),
    fileInput("annotation", "Upload annotation"),
    actionButton("uploadData", "Upload data"),
  )
  
)

