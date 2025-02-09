#' protein2gene UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_protein2gene_ui <- function(id) {
  ns <- NS(id)
  tagList(
      h2(strong("蛋白名称转基因名称",style="color:blue;")),
      sidebarLayout(
        sidebarPanel(
            fileInput(
              ns("protein_file"),
              HTML("加载蛋白名称文件"),
              multiple = T,
              accept = c("csv", "xlsx", "xls","tsv","sav","txt","bz2","gz"),
              buttonLabel = "浏览...",
              placeholder = "选择文件"
            ),

            fileInput(
              ns("uniprot_file"),
              "加载Uniprot文件",
              multiple = T,
              accept = c("csv", "xlsx", "xls","tsv","sav","txt","bz2","gz"),
              buttonLabel = "浏览...",
              placeholder = "选择文件"
            )
        ),

        mainPanel(
          DT::DTOutput(ns("protein_out")),
          DT::DTOutput(ns("uniprot_out")),
          DT::DTOutput(ns("p2g_out")),
        )
    )
  )
}

#' protein2gene Server Functions
#'
#' @noRd
mod_protein2gene_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    df_protein <- reactive({
      req(input$protein_file)

      file <- input$protein_file
      ext <- tools::file_ext(file$datapath)
      validate(need(ext %in% c("csv", "xlsx", "xls","tsv","sav","txt","bz2","gz"),
                    "不支持的文件格式"))

      data<-importData(file$datapath,ext)
    })

    df_uniprot <- reactive({
      req(input$uniprot_file)

      file <- input$uniprot_file
      ext <- tools::file_ext(file$datapath)
      validate(need(ext %in% c("csv", "xlsx", "xls","tsv","sav","txt","bz2","gz"),
                    "不支持的文件格式"))

      data<-importData(file$datapath,ext) %>% as.data.frame

    })

    output$protein_out <- renderDT(
      df_protein(),
      rownames = FALSE,
      escape = TRUE,
      extensions = 'Buttons',
      options=list(
        pageLength = 5,
        # lengthMenu = c(5, 10, 25, 50,100),
        lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
        dom = 'Bltipr',
        buttons = list(
          # 'copy',
          # 'print',

          list(
            extend = 'copy',
            text = '复制'
          ),

          list(
            extend = 'print',
            text = '打印'
          ),

          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = '下载'
          )
        ),
        scrollX = TRUE,
        language = list(
          info = '显示第 _START_ 至 _END_ 项结果，共 _TOTAL_ 项',
          search = '搜索:',
          paginate = list(previous = '上页', `next` = '下页'),
          lengthMenu = '显示 _MENU_ 项结果'
        )
      )
    )


    output$uniprot_out <- renderDT(
      df_uniprot(),
      rownames = FALSE,
      escape = TRUE,
      extensions = 'Buttons',
      options=list(
        pageLength = 5,
        # lengthMenu = c(5, 10, 25, 50,100),
        lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
        dom = 'Bltipr',
        buttons = list(
          # 'copy',
          # 'print',

          list(
            extend = 'copy',
            text = '复制'
          ),

          list(
            extend = 'print',
            text = '打印'
          ),

          list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = '下载'
          )
        ),
        scrollX = TRUE,
        language = list(
          info = '显示第 _START_ 至 _END_ 项结果，共 _TOTAL_ 项',
          search = '搜索:',
          paginate = list(previous = '上页', `next` = '下页'),
          lengthMenu = '显示 _MENU_ 项结果'
        )
      )
    )


  })
}

## To be copied in the UI
# mod_protein2gene_ui("protein2gene_1")

## To be copied in the server
# mod_protein2gene_server("protein2gene_1")
