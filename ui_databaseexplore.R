DataExplPanel = sidebarLayout(
  sidebarPanel = sidebarPanel(
    id = "exploresidepanel",
    width = 6,
    #tags$style(type = "text/css", "#exploresidepanel {height: calc(100vh - 70px) !important;}"),
    htmlOutput("explore_text"),
    div(style = "width: 350px;",textInput("searchExplore","Search for scientific or common fish names:","")),

    bs_accordion(id="advanced") %>%
      
      bs_set_opts(panel_type = "primary") %>%
      
      bs_append (title="Advanced Search", content = card(
        
        div(style = "height: 800px",
            fluidRow(column(3,div(class = "centered-select",class="big-select",selectizeInput("tribe_choice",label="Tribe",choices = fish_tribes,
                          selected = "None", multiple=T,options = list(optgroupField = 'optgroup',placeholder = 'Selection')))),
                  column(3,div(class = "centered-select",selectizeInput("biogeo_choice",label="Biogeography",choices = c("Native","Introduced"),
                                selected = NULL, multiple=T, options = list(placeholder = 'Selection')))),
                  column(3,div(class = "centered-select",selectizeInput("benuse_choice",label="Beneficial Use",choices = c("Subsistence","Sportfish","NonGame"),
                                selected = NULL, multiple=T, options = list(placeholder = 'Selection')))),
                  column(3,div(class = "centered-select",selectizeInput("sensi_choice",label="Sensitivity",choices = c("Tolerant","Medium","Sensitive","Unclassified"),
                                                                      selected = NULL, multiple=T, options = list(placeholder = 'Selection'))))),
            
            fluidRow(column(4,sliderInput(inputId = "rarity_slider",label = "PFG_Rarity",min = 1,max = 10,value = c(1,10))),
                  column(4,sliderInput(inputId = "max_age",label = "Max Age (yr)",min = 1,max = 25,value = c(1,25))),
                  column(4,sliderInput(inputId = "max_TL",label = "Max Total Length (cm)",min = 0,max = 200,value = c(0,200), step=5))),
            
            fluidRow(column(4,sliderInput(inputId = "mean_length",label = "Mean Length (cm)",min = 0,max = 40,value = c(0,40), step=1)),
                     column(4,tags$div(style = "text-align: center; font-weight: bold;","Lower/Upper Mean Weight (g)"),
                            fluidRow(column(6,div(style = "display: flex; justify-content: flex-end; margin-right: -10px;",
                                        numericInput(inputId = "lower_mean_weight",label = NULL,value = 0, min = 0, max = 100000, step = 1))),
                            column(6,div(style = "display: flex; justify-content: flex-start; margin-left: -10px;",
                                        numericInput(inputId = "upper_mean_weight",label = NULL,value = 200000, min = 0, max = 200000, step = 1))))),
                     column(4,sliderInput(inputId = "girth",label = "Girth Index",min = 0,max = 1,value = c(0,1), step=0.01))),
            
            fluidRow(column(12,
              div(
                style = "display: inline-flex; padding-left: 10px;",
                h5("Include Species with Unestimated Values for Sliders Below?", style = "margin-right: 10px;"),
                switchInput("use_unestimated", label = "", value = TRUE, onLabel = "Yes", offLabel = "No", size = "small")
              ))
            ),
            
            fluidRow(column(6,sliderInput(inputId = "ubiquity_slider",label = "Ubiquity",min = 0,max = 75,value = c(0,75),step=2)),
                column(6,sliderInput(inputId = "extent_slider",label = "Extent",min = 0,max = 80,value = c(0,80),step=2))),
            
            fluidRow(column(6,sliderInput(inputId = "tolerance_slider",label = "Tolerance",min = 0,max = 95,value = c(0,95),step=2)),
                column(6,sliderInput(inputId = "robustness_slider",label = "Robustness",min = 0,max = 50,value = c(0,50),step=2))),
            
            fluidRow(column(4,div(class = "centered-select",class="big-select",selectizeInput("system_choice",label="System",choices = systems, selected = NULL,
                        multiple=T, options = list(placeholder = 'Selection')))),
                column(4,div(class = "centered-select",class="big-select",selectizeInput("waterpos_choice",label="Water Position",choices = c("Benthic","Surface",
                        "Open Water/Pelagic","Nearshore/Littoral"), selected = NULL, multiple=T, options = list(placeholder = 'Selection')))),
                column(4,div(class = "centered-select",class="big-select",selectizeInput("substrate_choice",label="Substrate",choices = c("Mud/Silt","Sand","Gravel",
                        "Rock/Rubble","Vegetation","Woody Debris"), selected = NULL, multiple=T, options = list(placeholder = 'Selection'))))),
            
            fluidRow(column(3,div(class = "centered-select",selectizeInput("habtype_choice",label="Habitat Type",choices = c("Riffles","Runs","Pools"),
                          selected = NULL, multiple=T, options = list(placeholder = 'Selection')))),
                column(3,div(class = "centered-select",selectizeInput("clarity_choice",label="Water Clarity",choices = c("Clear","Turbid"),
                          selected = NULL, multiple=T, options = list(placeholder = 'Selection')))),
                column(3,div(class = "centered-select",selectizeInput("thermal_choice",label="Thermal Regime",choices = c("Cold","Cool","Warm"),
                          selected = NULL, multiple=T, options = list(placeholder = 'Selection')))),
                column(3,div(class = "centered-select",selectizeInput("topo_choice",label="Topography",choices = c("Lowlands","Uplands"),
                          selected = NULL, multiple=T, options = list(placeholder = 'Selection'))))),
            
            fluidRow(column(12,div(class = "centered-select",actionButton("submit_query",label="Submit Query"),actionButton("reset_query",label="Reset Query")))))))),
  
  mainPanel = mainPanel(width=6, id = "explore_main_panel",DT::dataTableOutput('explore_results'),DT::dataTableOutput('selected_species'),uiOutput("species_links"),
                        fluidRow(column(12,uiOutput("lw_inputs"))))
)