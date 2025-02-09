
# horizontal scroll bar on top datatable
css.topScroolBar <-
  "#topScroll > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody {
  transform:rotateX(180deg);
  }
  #topScroll > .dataTables_wrapper.no-footer > .dataTables_scroll > .dataTables_scrollBody table{
  transform:rotateX(180deg);
  }"


options(shiny.maxRequestSize = 10e6) # ~10 Mb
.cGroupsWUIEnv <- new.env(parent=emptyenv())


# color picker
choices_brewer <- list(
  "Blues" = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"),  #brewer.pal(n = 9, name = "Blues"),
  "Greens" = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"), #brewer.pal(n = 9, name = "Greens"),
  "Reds" = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15", "#67000D"), #brewer.pal(n = 9, name = "Reds"),
  "Oranges" = c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704"), #brewer.pal(n = 9, name = "Oranges"),
  "Yellows" = c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704"), #brewer.pal(n = 9, name = "Yellows"),
  "Purples" = c("#FCFBFD", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D"), #brewer.pal(n = 9, name = "Purples"),
  "Greys" = c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525", "#000000") #brewer.pal(n = 9, name = "Greys")
)


# this function has been deprecated from shinyWidgets and code must be sourced
setShadow <- function(id = NULL, class = NULL) {

  # shadow css
  cssShadow <- paste0(
    " box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
      transition: 0.3s;
      border-radius: 5px;
   ")

  cssShadow <- if (!is.null(id)) {
    if (!is.null(class)) {
      paste0("#", id, " .", class, " {", cssShadow, "}")
    } else {
      paste0("#", id, " {", cssShadow, "}")
    }
  } else {
    if (!is.null(class)) {
      paste0(".", class, " {", cssShadow, "}")
    } else {
      NULL
    }
  }

  # hover effect css
  cssHover <- "box-shadow: 0 16px 32px 0 rgba(0,0,0,0.2);"

  cssHover <- if (!is.null(id)) {
    if (!is.null(class)) {
      paste0("#", id, ":hover .", class, ":hover {", cssHover, "}")
    } else {
      paste0("#", id, ":hover", " {", cssHover, "}")
    }
  } else {
    if (!is.null(class)) {
      paste0(".", class, ":hover", " {", cssHover, "}")
    } else {
      NULL
    }
  }

  css <- paste(cssShadow, cssHover)

  # wrap everything in the head
  htmltools::tags$head(
    htmltools::tags$style(css)
  )
}

