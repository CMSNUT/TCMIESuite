#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyBS
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyhelper
#' @import shinyjs
#' @import shinyalert
#' @import shinytoastr
#' @import shinyWidgets
#' @import readxl
#' @import writexl
#' @import Hmisc
#' @import rvest
#' @import httr
#' @import jsonlite
#' @import tidyverse
#' @import DT
#' @import data.table
#' @import BiocManager
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      # golem::golem_welcome_page() # Remove this line to start building your UI
      useShinyjs(),
      useShinyalert(force = TRUE),
      useToastr(),
      HTML(
        "<style type='text/css'> #ResponseVariableORPanel, #LoadDataOptionsExcel, #LoadDataOptionsTxt, #extralabelsPanel {color:white;background-color:rgba(60,141,188,1)}</style>"
      ),
      HTML(
        "<style type='text/css'> #ratioAccordion, #formatAccordion, #decimalsAccordion {color:rgba(60,141,188,1)}</style>"
      ),
      # get window sizes.
      tags$head(tags$script('var dimension = [0, 0];
                        $(document).on("shiny:connected", function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension[0] = window.innerWidth;
                        dimension[1] = window.innerHeight;
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')),

      HTML("<link href='https://fonts.googleapis.com/css?family=Knewave' rel='stylesheet'>"),

      titlePanel(HTML("<p style='margin-top:-20px'></p>"), windowTitle=" | TCM Intelligent Exploration Tools Suite"),

      dashboardPage(
        preloader=NULL,
        skin="blue",
        scrollToTop = TRUE,

        ## header ----
        header = dashboardHeader(
          # title = logo_cg,
          title=tagList(
            span(
              class = "logo-lg",
              HTML(
                "<img src='www/logo.png' width=35 style='margin-right:10px;margin-bottom:10px'/><format style='text-align: left; font-family: Knewave; font-size: 30px; font-style: normal; font-variant: normal;'>TCM-IE<br>xxx</format>"
              )
            ),
            img(src = "www/logo.png", width = 35)
          ),

          fixed = FALSE,

          userOutput("github")
        ),
        ## sidebar ----
        sidebar = dashboardSidebar(
          sidebarMenu(
            id="leftmenu",
            ### Home ----
            menuItem(
              HTML("<format style='font-size:13pt'>中药智鉴</format>"),
              tabName="home",
              icon=icon("home")
            ),
            div(style="border: 1px solid white;height:0px; margin-top:-3px; margin-bottom:0px"),

            ### Data ----

            menuItem(
              HTML("<format style='font-size:13pt'>数据下载</format>"),
              icon=icon("database"),
              menuSubItem(
                HTML("<format style='font-size:13pt'>中药数据</format>"),
                tabName="tcm"
              )
            ),
            div(style="border: 1px solid white;height:0px; margin-top:-3px; margin-bottom:0px"),
            menuItem(
              HTML("<format style='font-size:13pt'>常用工具</format>"),
              icon=icon("toolbox"),
              menuSubItem(
                HTML("<format style='font-size:13pt'>数据探索分析</format>"),
                tabName="eda"
              ),
              menuSubItem(
                HTML("<format style='font-size:13pt'>蛋白名称转基因名称</format>"),
                tabName="gene"
              )
            ),
            div(style="border: 1px solid white;height:0px; margin-top:-3px; margin-bottom:0px")






          )
        ),


        ## body ----
        body = dashboardBody(
          setShadow(class = "dropdown-menu"),
          # cg_theme,
          tags$head(
            tags$style(
              HTML('.content-wrapper, .right-side {background-color: white;}')
            )
          ),

          tabItems(
            ### 主页 ####
            tabItem(
              "home",
              # div(id="homePanel",
              HTML("<p style='text-align: center;><format color:#357CA5; font-family: Knewave; font-size: 40pt; font-style: normal; font-variant: normal;'>中药智鉴  TCM-IE Suite</format></p>"),
              HTML("<h3 style='text-align: center;><format style='text-align: center'><i><strong><code>T</code>raditional <code>C</code>hinese <code>M</code>edicine <code>I</code>ntelligent <code>E</code>xploration Tools <code>Suite</code> in R Language</strong></format></i></h3>"),
              includeMarkdown("./md/home.md")
              # )
            ),

            ### 中药数据库下载 ####
            tabItem(
              "tcm",
              mod_tcm_download_ui("tcm")
            ),

            ### 数据探索分析 ####
            tabItem(
              "eda",
              # mod_data_preparation_ui("data")
            ),

            ### 蛋白名称转基因名称 ####
            tabItem(
              "gene",
              mod_protein2gene_ui("gene")
            )




          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "中药智鉴 | TCM-IE Suite"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
