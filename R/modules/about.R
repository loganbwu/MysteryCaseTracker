# UI element with no reactivity

aboutUI <- function(id) {
  ns = NS(id)
  tabPanel(
    "About",
    div(
      style = "text-align: center; padding-top: 64px",
      div(
        style = "max-width: 800px; display: inline-block; text-align: left; color: #222",
        includeMarkdown("modules/about.md")
      )
    ),
    tags$footer(
      class = "footer",
      div(
        style = "max-width: 800px; display: inline-block; text-align: left",
        "Department of Health, State Government of Victoria, Australia, \u00A9 2020",
      ),
      align = "center"
    )
  )
}