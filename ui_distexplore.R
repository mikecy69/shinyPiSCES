DistExplPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(id="distexplsidepanel",width=3,
                              tags$h4("Distribution Explorer"),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 3px; margin-bottom: 3px;"),
                              htmlOutput("distexpl_text"),
                              tags$hr(style = "border-color: #2c3e50; margin-top: 6px; margin-bottom: 6px;"),
                              textInput("searchName", "Enter scientific/common name search string:", ""),
                              textInput("searchHUC", "Enter HUC ID/name search string:", ""),
                              tags$div(style = "margin-top: 5px;"),
                              uiOutput("dynamicUI")),

  mainPanel = mainPanel(id="mapmainpanel",width=9,
                        leafletOutput("map"),
                        tags$div(id = "text-box", "Zoom Level: ", textOutput("zoom_level", inline = TRUE)),
                        tags$style(HTML("#text-box {
                              position: absolute;
                              top: 80px;
                              left: 20px;
                              z-index: 1000;
                              background-color: rgba(255, 255, 255, 0.8);
                              padding: 10px;
                              border-radius: 5px;
                              box-shadow: 0 0 5px #888888;}")),
                        tags$style(type = "text/css", "#map {height: calc(100vh - 110px) !important;}")
  ),
)