#' tcm_download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_tcm_download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    titlePanel(strong("中药数据下载",style="color:blue;font-size=28px;")),

    radioButtons(ns("open_data_src"), "选择中药数据库",
                 choices = c("TCMSP数据库"="tcmsp","BATMAN数据库"="batman","HERB数据库"="herb"),
                 inline = T
                 ),

    tabsetPanel(
      id = ns("hidden_tabs"),
      type = "hidden",

      tabPanelBody(
        "tcmsp",
        "TCMSP数据库",
        sidebarLayout(
          sidebarPanel(
            width = 2,
            # checkboxGroupInput(ns("tcmsp_content"),"下载数据",
            #          choices = c("中药材成分"="mols","活性成分潜在靶标"="targets"),
            #          selected = c("mols","targets")
            #          ),
            checkboxGroupInput(ns("tcmsp_filter_settings"), h4("活性成分筛选设置",style="color:red"),
                               choices = c(
                                 "口服利用度(OB)"="OB",
                                 "类药性指数(DL)"="DL",
                                 "相对分子量(MW)"="MW",
                                 "氢键供体数(Hdon)"="Hdon",
                                 "氢键受体数(Hacc)"="Hacc",
                                 "血脑屏障通透性(BBB)"="BBB"
                               ),
                               selected = c("OB","DL")
            ),
            conditionalPanel(
              ns=ns,
              condition = "input.tcmsp_filter_settings.includes('OB')",
              numericInput(ns("ob_threshold"),"口服利用度(OB) >=",value=30,max=100,min=0)
            ),

            conditionalPanel(
              ns=ns,
              condition = "input.tcmsp_filter_settings.includes('DL')",
              numericInput(ns("dl_threshold"),"类药性指数(DL) >=",value=0.18,max=1,min=0)
            ),

            conditionalPanel(
              ns=ns,
              condition = "input.tcmsp_filter_settings.includes('MW')",
              numericInput(ns("mw_threshold"),"相对分子量(MW) <=",value=500,max=2000,min=10)
            ),

            conditionalPanel(
              ns=ns,
              condition = "input.tcmsp_filter_settings.includes('Hdon')",
              numericInput(ns("hdon_threshold"),"氢键供体数(Hdon) <=",value=10,max=20,min=0)
            ),

            conditionalPanel(
              ns=ns,
              condition = "input.tcmsp_filter_settings.includes('Hacc')",
              numericInput(ns("hacc_threshold"),"氢键受体数(Hacc) <=",value=10,max=20,min=0)
            ),

            conditionalPanel(
              ns=ns,
              condition = "input.tcmsp_filter_settings.includes('BBB')",
              numericInput(ns("bbb_threshold"),"血脑屏障通透性(BBB) >=",value=-0.3)
            )

          ),
          mainPanel(
            width = 10,

            box(
              width = 12,
              status = "primary",
              solidHeader = T,
              collapsible = T,
              title = "中药材信息设置",

              div(
                id="tcmsp",
                actionButton(ns("tcmsp_herb_addBtn"), "增加中药材信息",class = "btn btn-primary"),
                actionButton(ns("tcmsp_herb_removeBtn"), "删除中药材信息",class = "btn btn-danger"),
                actionButton(ns("tcmsp_download_Btn"), "获取成分及其靶点数据",class = "btn btn-success"),
                tags$a(href = "https://old.tcmsp-e.com/tcmspsearch.php",
                       target = "_blank",  # 在新标签页打开
                       class = "btn btn-warning",  # 添加按钮样式
                       "访问TCMSP官网(旧版)"
                       )
              )
            ),

            box(
              width = 12,
              status = "success",
              solidHeader = T,
              title = "数据下载结果",

              h4("下载错误信息:",style="color:blue;"),
              verbatimTextOutput(ns("tcmsp_down_error")),
              tags$hr(),

              h4("中药材成分",style="color:blue;"),
              DT::DTOutput(ns("tcmsp_mols_tab")),
              tags$hr(),

              h4("活性成分潜在靶标",style="color:blue;"),
              DT::DTOutput(ns("tcmsp_targets_tab")),
              tags$hr(),

              h4("各味药材的成分和靶点信息统计:",style="color:blue;"),
              DTOutput(ns("tcmsp_down_report")),
              tags$hr(),

              h4("共有活性分子的药材:",style="color:blue;"),
              DTOutput(ns("tcmsp_down_common_mols")),
              tags$hr(),

              h4("共有靶点的活性分子:",style="color:blue;"),
              DTOutput(ns("tcmsp_down_common_targets"))
            )
          )
        )
      ),
      tabPanelBody(
        "batman",
        "BATMAN数据库"
      ),
      tabPanelBody(
        "herb",
        "HERB数据库"
      )

    )


  )
}

#' tcm_download Server Functions
#'
#' @noRd
mod_tcm_download_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # 切换数据库 ----
    observeEvent(input$open_data_src, {
      updateTabsetPanel(session, "hidden_tabs", selected = input$open_data_src)
    })

    inserted <- c()
    rv <- reactiveValues(
      tcmsp_herb_addBtn_count = 0,
      herbs=c(),
      urls=c(),
      thresholds=list()
      )

    # 添加TCMSP 药材信息 ----
    observeEvent(input$tcmsp_herb_addBtn, {
      # btn <- input$tcmsp_herb_addBtn
      rv$tcmsp_herb_addBtn_count <- rv$tcmsp_herb_addBtn_count + 1
      btn <- rv$tcmsp_herb_addBtn_count
      id <- paste0('herb', btn)
      insertUI(
        selector = '#tcmsp',
        where = "beforeBegin",
        ## wrap element in a div with id for ease of removal
        ui = tags$div(
          id = id,
          column(
            width = 2,
            textInput(ns(paste0("herb_name_zh", btn)),paste0("中药材 ", btn," 中文名称"),
                      placeholder = "例如: 附子")
          ),
          column(
            width = 2,
            textInput(ns(paste0("herb_name_pinyin", btn)),paste0("中药材 ", btn, " 中文拼音"),
                    placeholder = "例如: fuzi")
          ),
          column(
            width = 2,
            textInput(ns(paste0("herb_name_latin", btn)),paste0("中药材 ", btn, " 拉丁名称"),
                    placeholder = "例如: Aconiti Lateralis Radix Praeparata")
          ),
          column(
            width = 6,
            textInput(ns(paste0("herb_query_url", btn)),paste0("中药材 ", btn, " 查询结果网址"),
                      placeholder = "例如: https://old.tcmsp-e.com/tcmspsearch.php?qr=Aconiti%20Lateralis%20Radix%20Praeparata&qsr=herb_en_name&token=90efc5afd9df81ef6fd39ce5aaaed351")
          )
        )
      )
      inserted <<- c(inserted, id)
    })

    # 删除TCMSP 药材信息 ----
    observeEvent(input$tcmsp_herb_removeBtn, {
      req(rv$tcmsp_herb_addBtn_count > 0)
      removeUI(
        selector = paste0('#', inserted[length(inserted)])
      )
      inserted <<- inserted[-length(inserted)]
      rv$tcmsp_herb_addBtn_count <- rv$tcmsp_herb_addBtn_count - 1  # 重置按钮
    })

    # 获取 TCMSP 成分和靶点信息

    observeEvent(input$tcmsp_download_Btn,{
      rv$thresholds <- list()
      if ("OB" %in% input$tcmsp_filter_settings) rv$thresholds$ob <-  input$ob_threshold
      if ("MW" %in% input$tcmsp_filter_settings) rv$thresholds$mw <-  input$mw_threshold
      if ("DL" %in% input$tcmsp_filter_settings) rv$thresholds$dl <-  input$dl_threshold
      if ("Hdon" %in% input$tcmsp_filter_settings) rv$thresholds$hdon <-  input$hdon_threshold
      if ("Hacc" %in% input$tcmsp_filter_settings) rv$thresholds$hacc <-  input$hacc_threshold
      if ("BBB" %in% input$tcmsp_filter_settings) rv$thresholds$bbb <-  input$bbb_threshold

      input_list <- reactiveValuesToList(input)
      btn <- rv$tcmsp_herb_addBtn_count
      if (btn==0) {
        shinyalert(
          title = "请添加中药材信息！",
          # text = "请添加中药材信息！",
          type = "error"  # 弹窗类型（info、warning、error、success等）
        )
      }

      if (btn>0)  {
        rv$herbs <- c()
        rv$urls <- c()
        for (i in 1:btn) {
          if(nchar(trimws(input_list[paste0("herb_name_zh", i)])) == 0 |
             nchar(trimws(input_list[paste0("herb_query_url", i)])) == 0
             ){
            shinyalert(
              title = paste("错误"),
              text = paste("请检查中药材",i,"的中文名称和查询结果网址！"),
              type = "error"  # 弹窗类型（info、warning、error、success等）
            )
            break
          } else {
            rv$herbs <<- c(rv$herbs,trimws(input_list[paste0("herb_name_zh", i)]))
            rv$urls <<- c(rv$urls,trimws(input_list[paste0("herb_query_url", i)]))
          }
        }
        req(rv$herbs,rv$urls)

        if (length(unique(rv$herbs)) != btn ||  length(unique(rv$urls)) != btn) {
          shinyalert(
            title = paste("错误"),
            text = paste("请检查中药材的中文名称和查询结果网址信息！"),
            type = "error"  # 弹窗类型（info、warning、error、success等）
          )
        } else {
          res <- get_ingredients_targets_TCMSP(rv$herbs,rv$urls,rv$thresholds)

          # errors ----
          output$tcmsp_down_error <- renderPrint({
             res$errors
          })

          # report ----
          output$tcmsp_down_report <- renderDT(
            res$report,
            rownames = FALSE,
            escape = FALSE,
            extensions = 'Buttons',
            options = list(
              pageLength = 5,
              # lengthMenu = c(5, 10, 25, 50,100),
              lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
              dom = 'Bltipr',
              buttons = list(
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
              scrollY = TRUE
            )
          )

          # common mols ----
          output$tcmsp_down_common_mols <- renderDT(
            res$common_mols,
            rownames = FALSE,
            escape = FALSE,
            extensions = 'Buttons',
            options = list(
              pageLength = 5,
              # lengthMenu = c(5, 10, 25, 50,100),
              lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
              dom = 'Bltipr',
              buttons = list(
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
              scrollY = TRUE
            )
          )

          # common targets ----
          output$tcmsp_down_common_targets <- renderDT(
            res$common_targets,
            rownames = FALSE,
            escape = FALSE,
            extensions = 'Buttons',
            options = list(
              pageLength = 5,
              # lengthMenu = c(5, 10, 25, 50,100),
              lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
              dom = 'Bltipr',
              buttons = list(
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
              scrollY = TRUE
            )
          )

          # mols -----
          output$tcmsp_mols_tab <- renderDT(
            res$mols,
            rownames = FALSE,
            escape = FALSE,
            extensions = 'Buttons',
            options = list(
              pageLength = 5,
              # lengthMenu = c(5, 10, 25, 50,100),
              lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
              dom = 'Bltipr',
              buttons = list(
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
              scrollY = TRUE
            )
          )

          # targets -----
          output$tcmsp_targets_tab <- renderDT(
            res$targets,
            rownames = FALSE,
            escape = FALSE,
            extensions = 'Buttons',
            options = list(
              pageLength = 5,
              # lengthMenu = c(5, 10, 25, 50,100),
              lengthMenu = list(c(5, 10, 25, 50, 100, -1), c('5', '10', '25', '50', '100', 'All')),
              dom = 'Bltipr',
              buttons = list(
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
              scrollY = TRUE
            )
          )
        }
      }

    })


  })
}

## To be copied in the UI
# mod_tcm_download_ui("tcm_download_1")

## To be copied in the server
# mod_tcm_download_server("tcm_download_1")
