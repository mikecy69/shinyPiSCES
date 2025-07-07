AssemPredPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(id="aspredsidepanel",width=3,
                              
                        fluidRow(column(3, div(style = "width: 82px;", disabled(textInput("drainage_area", HTML("DA(km<sup>2</sup>)"), value = "0")))),
                            column(3, div(style = "width: 82px;", disabled(textInput("slope", "Slope(%)", value = "0")))),
                            column(3, div(style = "width: 82px;", disabled(textInput("elevation", "Elev(m)", value = "0")))),
                            column(3, div(style = "width: 82px;", disabled(textInput("mean_width", "Width(m)", value = "0"))))),
                        fluidRow(column(3, div(class = "custom-input", numericInput("watershed_integrity", "IWI", value = 0, min = 0, max=1, step=0.01))),
                            column(3, div(class = "custom-input", numericInput("benthic_invert", "BMMI", value = 0, min = 0, max=1, step=0.01))),
                            column(6, selectInput("prob_thresh",label="Probability Threshold",choices = c("P1","Ave(P0,P1)","P1 - 1SD", "P1 - 2SD", "P0"),
                                                     selected = "P1",multiple=F))),

                        fluidRow(column(12, div(style = "text-align: center;", actionButton("update_probs", "Update Predicted Prob")))),
                        tags$hr(style = "border-color: #2c3e50; margin-top: 4px; margin-bottom: 4px;"),
                        fluidRow(column(6, div(style = "text-align: center;", actionButton("add_fish", "Add Selections"))),
                                column(6, div(style = "text-align: center;", actionButton("remove_fish", "Remove Selections")))),
                        fluidRow(column(2),(column(8,uiOutput("color_info")))),
                        tags$hr(style = "border-color: #2c3e50; margin-top: 4px; margin-bottom: 4px;"),
                        fluidRow(column(12,div(style = "text-align: center;", textOutput("SHAP_text")))),
                        fluidRow(DT::dataTableOutput('shap_values'))),
  
  mainPanel = mainPanel(
    width = 9,
    id = "aspredmainpanel",
    tabsetPanel(id = "assemblage_tabs",
                tabPanel("Stream Map",
                         leafletOutput("stream_map"),
                         tags$div(id = "stream-text-box", tags$strong("Stream Segment Info"),uiOutput("segment_info")),
                         tags$style(HTML("#stream-text-box {
                              position: absolute;
                              top: 120px;
                              left: 20px;
                              z-index: 1000;
                              background-color: rgba(255, 255, 255, 0.8);
                              padding: 10px;
                              border-radius: 5px;
                              box-shadow: 0 0 5px #888888;}")),
                         tags$style(type = "text/css", "#stream_map {height: calc(100vh - 110px) !important;}")),
                tabPanel("Fish Assemblage Filtering",fluidRow(
                  column(7,align = "left",DT::dataTableOutput('fish_assem')),
                  column(5,align = "left",DT::dataTableOutput('filtered_fish')))),
                tabPanel("Community Biomass Estimation",
                         
                         fluidRow(numericInput(inputId = "biomass",label = "Biomass (kg)",value = 50,min = 5),
                               numericInput(inputId = "fish_count",label = "Fish Count",value = 1000,min = 10),
                               div(style = "text-align: center;",actionButton(inputId = "comm_calc",label = "Calculate"))),
                        
                         fluidRow(column(12,DT::dataTableOutput('fish_community'))))))
)