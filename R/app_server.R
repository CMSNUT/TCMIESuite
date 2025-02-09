#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd

app_server <- function(input, output, session) {
  rm(list=ls())
  options(shiny.maxRequestSize=60*1024^2)

  mod_tcm_download_server("tcm")

  mod_protein2gene_server("gene")
}
