source("bs_multi.R")
source("ui_distexplore.R")
source("ui_assemblagepredict.R")
source("ui_databaseexplore.R")

ui = fluidPage(
  
  useShinyjs(),
  
  bsTooltip(id = "ubiquity_slider", title = "How often the species is found in surveys in HUCs where the species is known to occur", placement = "top", trigger = "hover"),
  
  bsTooltip(id = "extent_slider", title = "How many HUCs the species is found within", placement = "top", trigger = "hover"),
  
  bsTooltip(id = "tolerance_slider", title = "How often a statistical model (based on stream characteristics) predicts that the species has a high probability of occurrence", placement = "top", trigger = "hover"),
  
  bsTooltip(id = "robustness_slider", title = "A function of Ubiquity, Extent, and Tolerance: species with high robustness occur in many HUCs across many different stream conditions and are typically seen in fish surveys", placement = "top", trigger = "hover"),
  
  bsTooltip(id = "rarity_slider", title = "1:Abundant 3:Common 6:Uncommon 8:Rare 10:Extinct", placement = "top", trigger = "hover"),
  
  bsTooltip(id = "drainage_area", title = "Drainage Area of the Catchment in square kilometers", placement = "top", trigger = "hover"),
  
  bsTooltip(id = "mean_width", title = "Mean stream width: 75% of estimated bankfull width", placement = "top", trigger = "hover"),
  
  bsTooltip(id = "girth_slider", title = "A relative measure of weight at a standard size. 0:very light 1:very heavy", placement = "top", trigger = "hover"),
  
  bsTooltip(id = "watershed_integrity", title = "Index of Watershed Integrity", placement = "top", trigger = "hover"),
  
  bsTooltip(id = "benthic_invert", title = "Benthic MacroInvertebrate Multimetric Index", placement = "top", trigger = "hover"),
  
  tags$head(
  
    tags$script(HTML(
      "$(document).ready(function() {
        $('#advanced .panel-collapse').collapse('hide');
      });",
      
      "$(document).on('click', '.selectize-input .item', function(e) {
        var selectizeInput = $(this).closest('.selectize-control').prev('select');
        var selectizeInstance = selectizeInput[0].selectize;
        var selectedValue = $(this).attr('data-value');
        selectizeInstance.removeItem(selectedValue);
        selectizeInstance.refreshItems();
        selectizeInstance.refreshOptions(false); // false prevents dropdown from opening
      });"
    )),
    
    tags$style(HTML("
    
    #loading {
      position: absolute;
      z-index: 1000;
      top: 50%;
      left: 50%;
      transform: translate(-50%, -50%);
      display: none;
    }
    
    .custom-input {
      text-align: center;
    }
    
    .custom-input label {
      font-size: 14px;
    }
    
    .custom-input .form-control {
      width: 90px !important;
      height: 35px !important;;
      margin: 0 auto;
    }
    
    #prob_thresh {
      width: 100px !important;
    }
    
    #lower_mean_weight, #upper_mean_weight {
      width: 115px !important;
      text-align: center !important;
    }
    
    #rarity_parm {
      width: 85px !important;
      height: 35px !important;
      text-align: center !important;
    }
    
    #rarity_parm + .tooltip > .tooltip-inner {
      min-width: 250px !important;
    }
    
    #update_probs, #add_fish, #remove_fish, #biomass_calc, #count_calc {
      display: block;
      margin: 0 auto;
      height: 40px !important;
      border-radius: 15px;
      line-height: 15px;
      box-sizing: border-box;
    }
    
    #fish_length, #fish_weight, #calc_lw, #fish_count, #biomass {
      height: 35px;
    }
    
    .irs-grid-text, .irs-min, .irs-max, .irs-single {
      display: none !important;
    }
    
    .irs-grid-pol.small {
      display: none !important;
    }
    
    #fish_assem .dataTable th {
      font-size: 14px;
    }
    
    #fish_assem .dataTable td {
      font-size: 12px;
    }
    
    #filtered_fish .dataTable th {
      font-size: 14px;
    }
    
    #filtered_fish .dataTable td {
      font-size: 12px;
    }
    
    #selected_species table.dataTable tr th:nth-child(1),
    #selected_species table.dataTable tr td:nth-child(1) {
      width: 125px !important;
    }
    
    #selected_species table.dataTable tr th:nth-child(2),
    #selected_species table.dataTable tr td:nth-child(2) {
      width: 150px !important;
    }
    
    #selected_species table.dataTable tr th:nth-child(3),
    #selected_species table.dataTable tr td:nth-child(3) {
      width: 125px !important;
    }
    
    .centered-select {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 100%;
    }
    
    .centered-select label {
      text-align: center;
      width: 100%;
    }
    
    .tooltip {
      width: 275px;
    }
    
    .tooltip-inner {
      max-width: 275px;
    }
    
    .shiny-input-container {
      text-align: center;
    }
    
    .shiny-input-container > label {
      display: block;
      text-align: center;
    }
    
    .selectize-input {
      width: 140px;
      margin: 0px;
      padding: 2px;
    }
    
    .big-select .selectize-input {
      width: 200px !important;
      margin: 0px;
      padding: 2px;
    }
    
    .selectize-control {
      margin-right: 5px;
    }

    .tabbable > .nav > li > a {
      background-color: lightgray;
      color:black;
    }
    
    .tabbable > .nav > li[class=active] > a {
      background-color: #2c3e50 !important;
      color:white !important;
    }
    
    .tab-pane {
      height: calc(100vh - 120px) !important;
    }
    
    .navbar-nav li.disabled {
        pointer-events: none;
        opacity: 0.5;
    }
    
    .btn-default {
       color: black !important;
       background-color: #A9A9A9 !important;
       border-color: #D3D3D3 !important;
       text-align: center !important;
    }
    
    .btn-default:hover {
       background-color: #2c3e50 !important;
       color: #18bc9c !important;
    }
    
    .btn.checkbtn.btn-custom.active {
      background-color: #2c3e50;
      color: white;
      border-color: green;
    }
    
    .btn.checkbtn.btn-custom {
      background-color: #cccccc;
      color: #111111;
      border-color: #2c3e50;
    }
    
    .panel-heading:hover {
      background-color: #cccccc;
      color: #18bc9c;
    }
    
    .panel-body {
      background-color: #ecf0f1;
    }
    
    .shiny-input-panel {
        margin: 0px 0px;
        padding: 0px 0px;
        border: 0px solid #e3e3e3;
        background-color: #ecf0f1;
    }
    
    .shiny-input-container {
        margin-top: 2px;
        margin-bottom: 2px;
    }
    
    .btn-default.btn-file {
      height: 42px;
      width: 80px !important;
      padding: 6px;
    }
    
    .shiny-download-link {
      display: flex;
      justify-content: center;
      align-items: center;
      height: 40px;
      text-align: center;
      padding: 0;
    }
    
    .modal-footer {
      text-align: center;
    }
    
    .align-center {
      display: flex;
      align-items: center;
    }
    
    .custom-inline-elements {
      display: flex;
      align-items: center;
    }
    
    .custom-action-button {
      display: flex;
      justify-content: center;
      align-items: center;
    }
    
  "))),
  
  #div(id = "loading", tags$img(src = "https://jeroen.github.io/images/banana.gif")),
  #div(id = "loading", tags$img(src = "dancing_shark.gif", width=500)),
  div(id = "loading", tags$img(src = "loading_fish.gif", width=500)),
  
  navbarPage(
    title = "PiSCES",
    id="shinyPiSCES",
    theme = shinytheme("flatly"),
    
    tabPanel(title = "Landing",style = "background-color: white;",
             p( 
               h6(".", align = "center"),
               h2("", align = "center"),
               h2("", align = "center"),
               h2("", align = "center"),
               h2("Welcome to PiSCES!", align = "center"),
               fluidRow(column(3),column(6,uiOutput("landing_text")),column(3)),
               h3("Email cyterski.mike@epa.gov with comments and questions.", align = "center")),
             div(align="center",img(src="logo.png",width=1000))),
    
    tabPanel(title = "Distribution Explorer", DistExplPanel),
    
    tabPanel(title = "Assemblage Predictor", AssemPredPanel),
    
    tabPanel(title = "Database Explorer", DataExplPanel)
  )
)