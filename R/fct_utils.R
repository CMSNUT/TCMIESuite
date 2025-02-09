#' importData
#'
#' @description A function for importing Data
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import readxl
#' @import data.table
#' @import Hmisc
#'
importData <- function(datapath,ext) {
  req(datapath)

  if (ext %in% c("csv","tsv","txt","bz2","gz2")) {
    dat <- data.table::fread(
      datapath
    )
  } else if (ext %in% c("xlsx", "xls")) {
    dat <- readxl::read_excel(
      datapath
    )
  } else if (ext == "sav") {
    dat <- Hmisc::spss.get(
      datapath
    )

  }

  return(dat)
}

#' get_duplicated_table
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import data.table
#' @import tidyverse
#'

merge_duplicates <- function(df,group_col,sum_cols=NULL,toString_cols=NULL) {
  res <-data.frame()
  if (!is.null(sum_cols) || !is.null(toString_cols)) {
    try({
      df[,sum_cols] <- df[,sum_cols] %>% sapply(as.numeric)
      res<-df[duplicated(df[,group_col]) | duplicated(df[,group_col], fromLast = TRUE), ] %>%
        dplyr::select(tidyselect::all_of(c(group_col,toString_cols,sum_cols))) %>%
        dplyr::group_by(!!dplyr::sym(group_col)) %>%
        dplyr::summarise(
          dplyr::across(
          .cols = tidyselect::all_of(toString_cols),
          .fns = ~ toString(.x)
          ),
          dplyr::across(
            .cols = tidyselect::all_of(sum_cols),
            .fns = ~ sum(.x)
          ),
          .groups = 'drop'
        )
    })
  }

  return(res)
}




#' get_TCMSP_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import rvest
#' @import httr
#' @import jsonlite
#' @import tidyverse
#' @import data.table
#' @import tibble
#'

get_ingredients_targets_TCMSP <- function(herbs,urls,thresholds=NULL) {
  errors <- c()
  mols <- data.frame()
  targets <- data.frame()
  report <- data.frame(Herb=NA,Mol_count=NA,
                       Active_Mol_count=NA,Target_count=NA,Unique_Target_count=NA, Missing_Target_Mol_Count=NA)
  common_mols <- data.frame()
  common_targets <- data.frame()

  for (i in 1:length(herbs)) {

    name <- herbs[i]
    report[i,1] <- name
    tryCatch({
      # web <- rvest::read_html(GET(urls[i],encoding="UTF-8", config(ssl_verifypeer = FALSE)))

      # read all the script
      tcmsp <- rvest::read_html(GET(urls[i],encoding="UTF-8", config(ssl_verifypeer = FALSE))) %>%
        rvest::html_elements("script") %>%
        rvest::html_text() %>%
        stringr::str_extract_all("data:\\s\\[.*\\]") %>%
        unlist(.[12])

      herb_m <- stringr::str_replace(tcmsp[1], "data:","") %>%
        jsonlite::fromJSON(simplifyVector = TRUE) %>%
        tibble::add_column(Herb = name, .before = 'molecule_ID') %>%
        dplyr::mutate(across(c(ob,mw,bbb,hdon,hacc,dl), as.numeric))

      report[i,2] <- nrow(herb_m)

      ths <- names(thresholds)

      if (length(ths)>0) {
        if ("ob" %in% ths) {
          herb_m <- herb_m %>% dplyr::filter(ob >= thresholds$ob)
        }

        if ("dl" %in% ths) {
          herb_m <- herb_m %>% dplyr::filter(dl >= thresholds$dl)
        }

        if ("mw" %in% ths) {
          herb_m <- herb_m %>% dplyr::filter(mw <= thresholds$mw)
        }

        if ("hdon" %in% ths) {
          herb_m <- herb_m %>% dplyr::filter(hdon <= thresholds$hdon)
        }

        if ("hacc" %in% ths) {
          herb_m <- herb_m %>% dplyr::filter(hacc <= thresholds$hacc)
        }

        if ("bbb" %in% ths) {
          herb_m <- herb_m %>% dplyr::filter(bbb >= thresholds$bbb)
        }
      }
      report[i,3] <- nrow(herb_m)

      herb_t <- stringr::str_replace(tcmsp[2], "data:","") %>%
        jsonlite::fromJSON(simplifyVector = TRUE) %>%
        dplyr::semi_join(herb_m, by = "MOL_ID") %>%
        tibble::add_column(Herb = name, .before = 'molecule_ID')

      report[i,4] <- nrow(herb_t)
      report[i,4] <- length(unique(herb_t$MOL_ID))
      report[i,6] <- nrow(herb_m)-length(unique(herb_t$MOL_ID))

      mols <- dplyr::bind_rows(mols,herb_m)
      targets <- dplyr::bind_rows(targets,herb_t)
    }, error = function(e) {
      errors <<- c(errors,paste0("中药材",name,"的成分及其靶点信息获取失败"))  # 捕获错误信息
    })
  }


  common_mols <- merge_duplicates(mols,"MOL_ID",toString_cols = c("Herb"))

  common_targets <- merge_duplicates(targets,"target_name",toString_cols = c("Herb","MOL_ID"))
  common_targets$Herb <- sapply(
    strsplit(common_targets$Herb, ",\\s*"),  # 分割字符串（支持逗号后可能有空格）
    function(x) paste(unique(x), collapse = ",")
  )

  return(list('mols' = mols,
              "targets" = targets,
              "report" = report,
              "common_mols" = common_mols,
              "common_targets"= common_targets,
              "errors" = errors))
}


#' tcmsp_target_name_to_gene_symbol
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import readxl
#' @import data.table
#' @import Hmisc
#'
tcmsp_target_name_to_gene_symbol <- function() {

}
