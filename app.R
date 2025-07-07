library(bslib)
library(bsplus)
# library(cachem)
library(dplyr)
library(ggdist)
library(ggplot2)
library(glmnetUtils)
library(grid)
library(hash)
library(httr)
library(jsonlite)
library(later)
library(leaflet)
library(lubridate)
library(Matrix)
# library(memoise)
library(plyr)
library(rlang)
library(RSQLite)
library(sf)
library(SHAPforxgboost)
library(shiny)
library(shinyBS)
library(shinyjs)
library(shinythemes)
library(shinyvalidate)
library(shinyWidgets)
library(stringr)
library(tidyverse)
library(tools)
library(xgboost)
library(DT)

# library(NCmisc)
# library(here)

# load_stream_segments = function(file_path) {
#   st_read(file_path)
# }
# 
# disk_cache = cachem::cache_disk("./stream_segments_cache")
# mem_load_stream_segments = memoise(load_stream_segments, cache = disk_cache)

source("app_variables.R")
source("ui.R")
# source("global_functions.R")
# source("calculate_probs.R")

server= function(input,output,session) {
  
  # stream_segments = mem_load_stream_segments("NHDPlusV21_Flowline.gpkg")
  
  # get_model_params = function(fit) {
  #   alpha = fit$alpha
  #   lambdaMin = sapply(fit$modlist, `[[`, "lambda.min")
  #   lambdaSE = sapply(fit$modlist, `[[`, "lambda.1se")
  #   error = sapply(fit$modlist, function(mod) {min(mod$cvm)})
  #   best = which.min(error)
  #   data.frame(alpha = alpha[best], lambdaMin = lambdaMin[best],
  #              lambdaSE = lambdaSE[best], eror = error[best])
  # }
  
  calculate_probs = function(model_numbers, WA, Slope, Elev, IWI, BMMI) {
    
    probabilities = numeric(length(model_numbers))
    
    for (i in seq_along(model_numbers)) {
      model_number = model_numbers[i]
      
      if (model_number==-999) {
        probabilities[i] = NA
        next
      }
      
      model_file = file.path("Models", paste0("Model_", model_number, ".xgb"))
      model = xgb.load(model_file)
      
      params = c(WA, Slope, Elev, IWI, BMMI)
      
      data_matrix = matrix(params, nrow = 1, dimnames = list(NULL, c("WA", "Slope", "Elev", "IWI", "BMMI")))
      
      prob = predict(model, data_matrix)
      
      probabilities[i] = prob
    }
    
    return(probabilities)
  }
  
  # show_stack <- function() {
  #   cat("#----- Stack containing call to show_stack -----#\n\n")
  #   x <- sys.calls()
  #   lapply(head(x, -1), function(x) {print(x); cat("\n")})
  #   cat("#-----------------------------------------------#\n\n")
  # }

  # Code to show what libraries are in use by the project
  # funcs =
  #   list.files(here::here(), pattern ="\\.R$", recursive = TRUE, full.names = TRUE) |>
  #   map(list.functions.in.file) |>
  #   flatten
  
  # Extract just the unique package names
  # packages <-
  #   funcs |>
  #   names |>
  #   str_extract("package:[[:alnum:]]*") |>
  #   str_split(",") |>
  #   unlist |>
  #   str_remove("package:") |>
  #   unique |>
  #   sort
  # 
  # print(packages)

  output$map = renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = -96, lat = 37, zoom = 5)
  })
  
  output$stream_map = renderLeaflet({
    leaflet() %>%
      addTiles() |>
      setView(lng = -96, lat = 37, zoom = 5)
  })
  
  output$segment_info = renderUI({
    HTML(paste(
      paste("<em>Latitude:</em>", stream_lat()), "<br>",
      paste("<em>Longitude:</em>", stream_lon()), "<br>",
      paste("<em>COM ID:</em>", stream_ID()), "<br>",
      paste("<em>HUC:</em>", stream_HUC()), "<br>",
      paste("<em>Name:</em>", stream_NAME())
    ))
  })
  
  output$color_info = renderUI({
    HTML(paste0(
      "<br>",
      "<div style='background-color: lightgreen; color: black; font-size: 14px; padding: 5px; margin-bottom: 3px;'>Acceptable Stream Width/Occurrence Prob</div>",
      "<div style='background-color: yellow; color: black; font-size: 14px; padding: 5px; margin-bottom: 3px;'>Low Occurrence Probability</div>",
      "<div style='background-color: lightgray; color: black; font-size: 14px; padding: 5px; margin-bottom: 3px;'>Unlikely Presence at this Stream Width</div>",
      "<div style='background-color: gray; color: black; font-size: 14px; padding: 5px; margin-bottom: 3px;'>Unlikely Width and Low Probability</div>"
    ))
  })
  
  output$landing_text = renderUI({
    HTML("<div style='text-align: center; font-size: 20px;'>
    <b>PiSCES</b> forecasts fish assemblages in US streams using HUC-based distribution data for over 1,000 species from NatureServe, USGS, and the Peterson Field Guide. 
    It refines potential communities using species rarity, stream size preferences, and water quality metrics (e.g., pH, conductivity). 
    Additionally, <b>PiSCES</b> can generate abundance distributions based on the inverse relationship between species abundance and maximum body size.
    </div>")
  })
  
  output$zoom_level = renderText({
    input$map_zoom
  })
  
  if (st_crs(HUC_boundaries)$epsg != 4326) {
    HUC_boundaries = st_transform(HUC_boundaries, crs = 4326)
  }
  
  debounced_search_name = debounce(reactive(input$searchName), 300)
  debounced_search_HUC = debounce(reactive(input$searchHUC), 300)
  
  observeEvent(debounced_search_name(), {
    
    selected_huc8(character())
    selected_name(character())
    output$selected_HUC_info = NULL
    
    name_search_term = tolower(trimws(debounced_search_name()))
    
    req(name_search_term)
    
    # Perform the search
    filtered_data = subset(fish_props, 
                          grepl(name_search_term, tolower(fish_props[, 5]), fixed = TRUE) |
                          grepl(name_search_term, tolower(fish_props[, 6]), fixed = TRUE) |
                          grepl(name_search_term, tolower(fish_props[, 7]), fixed = TRUE))
    
    temp_data = data.frame(filtered_data[, c(5, 6, 7)])
    colnames(temp_data) = c("Genus", "Species", "Common Name")
    name_srch_results(temp_data)

    if (is.null(name_srch_results())) {
      output$name_search_results = NULL
      display("none")
      
    } else {
      
      output$name_search_results = DT::renderDataTable(server = T, {
        datatable(
          name_srch_results(),
          rownames = F,
          selection = list(
            selected = NULL,
            target = "row",
            mode = "single"
          ),
          editable = F,
          options = list(
            autoWidth = F,
            dom='tp',
            pageLength = 10,
            scrollX = F,
            scrollY = TRUE,
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        )
      })
      
      display("name_searched")
    }
  })
  
  observeEvent(debounced_search_HUC(), {
    
    selected_huc8(character())
    selected_name(character())
    output$selected_HUC_info = NULL
    
    HUC_search_term = tolower(trimws(debounced_search_HUC()))
    
    req(HUC_search_term)
    
    # Perform the search
    filtered_data = subset(HUC_data, 
                          grepl(HUC_search_term, tolower(HUC_data[, 3]), fixed = TRUE) |
                          grepl(HUC_search_term, tolower(HUC_data[, 4]), fixed = TRUE))
    
    HUC_srch_results(data.frame(filtered_data[,c(3, 4)]))
    
    HUC_srch_results(unique(HUC_srch_results()))
    
    if (is.null(HUC_srch_results())) {
      output$HUC_search_results = NULL
      display("none")
    } else {
      
      output$HUC_search_results = DT::renderDataTable(server = T, {
        datatable(
          HUC_srch_results(),
          rownames = F,
          selection = list(
            selected = NULL,
            target = "row",
            mode = "single"
          ),
          editable = F,
          options = list(
            autoWidth = F,
            dom='tp',
            pageLength = 10,
            scrollX = F,
            scrollY = TRUE,
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        )
      })
      
      display("HUC_searched")
    }
  })
  
  debounced_explore = debounce(reactive(input$searchExplore), 300)
  
  observeEvent(debounced_explore(), {
    
    search_term = tolower(trimws(debounced_explore()))
    
    req(search_term)
    
    # Perform the search
    filtered_data = subset(fish_props, 
                          grepl(search_term, tolower(fish_props[, 5]), fixed = TRUE) |
                          grepl(search_term, tolower(fish_props[, 6]), fixed = TRUE) |
                          grepl(search_term, tolower(fish_props[, 7]), fixed = TRUE))
    
    explore_results(data.frame(filtered_data[, c(5, 6, 7)]))
    
    output$selected_species = NULL
    output$species_links = NULL
    
    if (is.null(explore_results())) {
      output$explore_results = NULL
      
    } else {
      
      output$explore_results = DT::renderDataTable(server = T, {
        datatable(
          explore_results(),
          rownames = F,
          selection = list(
            selected = NULL,
            target = "row",
            mode = "single"
          ),
          editable = F,
          options = list(
            autoWidth = F,
            pageLength = 10,
            dom='tp',
            scrollX = F,
            scrollY = TRUE,
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        )
      })
    }
  })
  
  observeEvent(input$map_zoom, ignoreInit=T, {
    
    current_zoom(input$map_zoom)
    
    if (!is.null(current_zoom()) && previous_zoom() < 5 && current_zoom() >= 5 && is.null(input$HUC_search_results_rows_selected) && is.null(input$name_search_results_rows_selected)) {
      
      shinyjs::disable("map")
      shinyjs::show("loading")
      
      leafletProxy("map") |>
        addPolygons(
          data = HUC_boundaries,
          layerId = ~id,
          color = "#03F",
          weight = 2,
          opacity = 1.0,
          fillColor = "transparent",
          fillOpacity = 0,
          highlightOptions = highlightOptions(
            fillColor = "cadetblue", fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          group = "ordinary"
        )
      
      delay(time_delay(), {
        shinyjs::enable("map")
        shinyjs::hide("loading")
      })
      
      boundaries_added(TRUE)
      
    } else if (!is.null(current_zoom()) && previous_zoom() >= 5 && current_zoom() < 5) {
      leafletProxy("map") |> clearShapes()
      boundaries_added(FALSE)
    }
    
    # Update the previous zoom level
    previous_zoom(current_zoom())
  })
  
  output$dynamicUI = renderUI({
    if (display()=="name_searched") {
      tagList(
        uiOutput("selected_HUC_info"),
        DTOutput("name_search_results")
      )
    }  else if (display()=="HUC_searched") {
      tagList(
        uiOutput("selected_HUC_info"),
        DTOutput("HUC_search_results")
      )
    } else if (display()=="hucfishes") {
      tagList(
        uiOutput("selected_HUC_info"),
        DTOutput("huc_fishes")
      )
    }
  })
  
  observeEvent(input$map_shape_click, {
    
    click_ID = input$map_shape_click
    
    if (!is.null(click_ID$id)) {
      
      selected_huc_polygon = subset(HUC_boundaries, id == click_ID$id)
      
      polygon = st_as_sfc(selected_huc_polygon)
      
      coords = do.call(rbind, lapply(polygon, st_coordinates))
      
      min_lat = min(coords[, 2])
      max_lat = max(coords[, 2])
      min_lon = min(coords[, 1])
      max_lon = max(coords[, 1])
      
      ave_long = (max_lon + min_lon)/2
      ave_lat = (max_lat + min_lat)/2
      
      leafletProxy("map", session) |>
        setView(lng = ave_long, lat = ave_lat, zoom = 7)
      
      if (!boundaries_added()) {
        leafletProxy("map", session) |>
          addPolygons(
            data = HUC_boundaries,
            layerId = ~id,
            color = "#03F",
            weight = 2,
            opacity = 1.0,
            fillColor = "transparent",
            fillOpacity = 0,
            highlightOptions = highlightOptions(
              fillColor = "cadetblue", fillOpacity = 0.5,
              bringToFront = TRUE
            ),
            group = "ordinary"
          )
        
        boundaries_added(TRUE)
        
        shinyjs::disable("map")
        shinyjs::show("loading")
        
        delay(time_delay(), {
          shinyjs::enable("map")
          shinyjs::hide("loading")
        })
      }
      
      leafletProxy("map") |>
        addPolygons(
          data = subset(HUC_boundaries, id == previous_polygon_ID()),
          layerId = ~id,
          color = "#03F",
          weight = 2,
          opacity = 1.0,
          fillColor = "transparent",
          fillOpacity = 0,
          highlightOptions = highlightOptions(
            fillColor = "cadetblue", fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          group = "ordinary"
        ) |>
        addPolygons(
          data = subset(HUC_boundaries, HUC8 %in% previous_matches()),
          layerId = ~id,
          color = "#03F",
          weight = 2,
          opacity = 1.0,
          fillColor = "transparent",
          fillOpacity = 0,
          highlightOptions = highlightOptions(
            fillColor = "cadetblue", fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          group = "ordinary"
        ) |>
        addPolygons(
          data = selected_huc_polygon,
          layerId = ~id,
          color = "#03F",
          weight = 2,
          opacity = 1.0,
          fillColor = "cadetblue",
          fillOpacity = 0.5,
          highlightOptions = highlightOptions(
            fillColor = "cadetblue", fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          group = "highlighted"
        )
      
      previous_polygon_ID(click_ID$id)
      
    } else {
      
      leafletProxy("map", session) |>
        addPolygons(
          data = subset(HUC_boundaries, id == previous_polygon_ID()),
          layerId = ~id,
          color = "#03F",
          weight = 2,
          opacity = 1.0,
          fillColor = "transparent",
          fillOpacity = 0,
          highlightOptions = highlightOptions(
            fillColor = "cadetblue", fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          group = "ordinary"
        ) |>
        addPolygons(
          data = subset(HUC_boundaries, HUC8 %in% previous_matches()),
          layerId = ~id,
          color = "#03F",
          weight = 2,
          opacity = 1.0,
          fillColor = "transparent",
          fillOpacity = 0,
          highlightOptions = highlightOptions(
            fillColor = "cadetblue", fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          group = "ordinary"
        )
      
      previous_matches(character())
      previous_polygon_ID(character())
    }
    
    chosen_HUC8 = HUC_boundaries[HUC_boundaries$id == click_ID$id,"HUC8"][[1]]
    chosen_NAME = HUC_boundaries[HUC_boundaries$id == click_ID$id,"NAME"][[1]]
    
    selected_huc8(chosen_HUC8)
    selected_name(chosen_NAME)
    
    previous_matches(chosen_HUC8)
    
    temp_data = HUC_data[HUC_data$HUC == selected_huc8(),1:2]
    colnames(temp_data) = c("Scientific Name", "Common Name")
    HUC_fishes(temp_data)

    output$huc_fishes = DT::renderDataTable(server = T, {
      datatable(
        HUC_fishes(),
        rownames = F,
        selection = list(
          selected = NULL,
          target = "row",
          mode = "single"
        ),
        editable = F,
        options = list(
          autoWidth = F,
          dom='tp',
          scrollX = F,
          scrollY = TRUE,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
            "}"
          )
        )
      )
    })

    output$selected_HUC_info = renderUI({
      req(selected_huc8(), selected_name())
      HTML(paste("<strong>Name:</strong>", selected_name(), "<br>",
                 "<strong>Selected HUC8:</strong>", selected_huc8()))
    })
    
    display("hucfishes")
    
  })

  observeEvent(input$name_search_results_rows_selected, ignoreInit=T, {
    
    current_selection = input$name_search_results_rows_selected
    
    selected_huc8(character())
    selected_name(character())
    output$selected_HUC_info = NULL
    
    if (is.null(current_selection)) {
      return()
      # highlight_hucs(HUC_data, NULL, current_zoom(), time_delay(), session)
    } else {
      
      selected_common_name = name_srch_results()[current_selection, "Common Name"]
      
      if (!is.null(selected_common_name)) {
        
        matching_hucs = HUC_data[HUC_data$Common_Name == selected_common_name, "HUC"]
        
        if (length(matching_hucs) == 0) {
          
          showModal(modalDialog("No distribution information for this species.",easyClose = TRUE,
                                footer = tagList(modalButton("Close"))))
          
          return()
          
        } else {
          
          relevant_HUCs = subset(HUC_boundaries, HUC8 %in% matching_hucs)
          
          polygons = st_as_sfc(relevant_HUCs)
          
          coords = do.call(rbind, lapply(polygons, st_coordinates))
          
          min_lat = min(coords[, 2])
          max_lat = max(coords[, 2])
          min_lon = min(coords[, 1])
          max_lon = max(coords[, 1])
          
          longitude_range = max_lon - min_lon
          latitude_range = max_lat - min_lat
          US_lon_range = 58
          US_lat_range = 25
          
          #values to convert a species range to the appropriate zoom level
          range_guide = c(0.75,0.4,0.20,0.07)
          
          app_zoom_level = ifelse(
            (longitude_range >= range_guide[1]*US_lon_range |latitude_range >= range_guide[1]*US_lat_range), 5,
            ifelse((longitude_range >= range_guide[2]*US_lon_range |latitude_range >= range_guide[2]*US_lat_range), 6,
                   ifelse((longitude_range >= range_guide[3]*US_lon_range |latitude_range >= range_guide[3]*US_lat_range), 7, 
                          ifelse((longitude_range >= range_guide[4]*US_lon_range |latitude_range >= range_guide[4]*US_lat_range), 8, 9))))
          
          center_lat = (min_lat + max_lat) / 2
          center_lon = (min_lon + max_lon) / 2
          
          leafletProxy("map", session) |>
            setView(lng = center_lon, lat = center_lat, zoom = app_zoom_level)
          
          if (!boundaries_added()) {
            leafletProxy("map", session) |>
              addPolygons(
                data = HUC_boundaries,
                layerId = ~id,
                color = "#03F",
                weight = 2,
                opacity = 1.0,
                fillColor = "transparent",
                fillOpacity = 0,
                highlightOptions = highlightOptions(
                  fillColor = "cadetblue", fillOpacity = 0.5,
                  bringToFront = TRUE
                ),
                group = "ordinary"
              )
            
            boundaries_added(TRUE)
            
            shinyjs::disable("map")
            shinyjs::show("loading")
            
            delay(time_delay(), {
              
              shinyjs::enable("map")
              shinyjs::hide("loading")
              
              leafletProxy("map", session) |>
                addPolygons(
                  data = subset(HUC_boundaries, HUC8 %in% previous_matches()),
                  layerId = ~id,
                  color = "#03F",
                  weight = 2,
                  opacity = 1.0,
                  fillColor = "transparent",
                  fillOpacity = 0,
                  highlightOptions = highlightOptions(
                    fillColor = "cadetblue", fillOpacity = 0.5,
                    bringToFront = TRUE
                  ),
                  group = "ordinary"
                ) |>
                addPolygons(
                  data = subset(HUC_boundaries, id == previous_polygon_ID()),
                  layerId = ~id,
                  color = "#03F",
                  weight = 2,
                  opacity = 1.0,
                  fillColor = "transparent",
                  fillOpacity = 0,
                  highlightOptions = highlightOptions(
                    fillColor = "cadetblue", fillOpacity = 0.5,
                    bringToFront = TRUE
                  ),
                  group = "ordinary"
                ) |>
                addPolygons(
                  data = relevant_HUCs,
                  layerId = ~id,
                  color = "#03F",
                  weight = 2,
                  opacity = 1.0,
                  fillColor = "yellow",
                  fillOpacity = 0.8,
                  highlightOptions = highlightOptions(
                    fillColor = "cadetblue", fillOpacity = 0.5,
                    bringToFront = TRUE
                  ),
                  group = "highlighted"
                )
            })
            
          } else {
            
            leafletProxy("map", session) |>
              addPolygons(
                data = subset(HUC_boundaries, HUC8 %in% previous_matches()),
                layerId = ~id,
                color = "#03F",
                weight = 2,
                opacity = 1.0,
                fillColor = "transparent",
                fillOpacity = 0,
                highlightOptions = highlightOptions(
                  fillColor = "cadetblue", fillOpacity = 0.5,
                  bringToFront = TRUE
                ),
                group = "ordinary"
              ) |>
              addPolygons(
                data = subset(HUC_boundaries, id == previous_polygon_ID()),
                layerId = ~id,
                color = "#03F",
                weight = 2,
                opacity = 1.0,
                fillColor = "transparent",
                fillOpacity = 0,
                highlightOptions = highlightOptions(
                  fillColor = "cadetblue", fillOpacity = 0.5,
                  bringToFront = TRUE
                ),
                group = "ordinary"
              ) |>
              addPolygons(
                data = relevant_HUCs,
                layerId = ~id,
                color = "#03F",
                weight = 2,
                opacity = 1.0,
                fillColor = "yellow",
                fillOpacity = 0.8,
                highlightOptions = highlightOptions(
                  fillColor = "cadetblue", fillOpacity = 0.5,
                  bringToFront = TRUE
                ),
                group = "highlighted"
              )
          }
          previous_matches(matching_hucs)
          previous_polygon_ID(character())
        }
      } else {
        
        leafletProxy("map", session) |>
          setView(lng = -100, lat = 40, zoom = current_zoom())
        
        if (!boundaries_added() && current_zoom() >= 5) {
          leafletProxy("map", session) |>
            addPolygons(
              data = HUC_boundaries,
              layerId = ~id,
              color = "#03F",
              weight = 2,
              opacity = 1.0,
              fillColor = "transparent",
              fillOpacity = 0,
              highlightOptions = highlightOptions(
                fillColor = "cadetblue", fillOpacity = 0.5,
                bringToFront = TRUE
              ),
              group = "ordinary"
            )
          
          boundaries_added(TRUE)
          
          shinyjs::disable("map")
          shinyjs::show("loading")
          
          delay(time_delay(), {
            shinyjs::enable("map")
            shinyjs::hide("loading")
          })
        }
        
        leafletProxy("map", session) |>
          addPolygons(
            data = subset(HUC_boundaries, HUC8 %in% previous_matches()),
            layerId = ~id,
            color = "#03F",
            weight = 2,
            opacity = 1.0,
            fillColor = "transparent",
            fillOpacity = 0,
            highlightOptions = highlightOptions(
              fillColor = "cadetblue", fillOpacity = 0.5,
              bringToFront = TRUE
            ),
            group = "ordinary"
          ) |>
          addPolygons(
            data = subset(HUC_boundaries, id == previous_polygon_ID()),
            layerId = ~id,
            color = "#03F",
            weight = 2,
            opacity = 1.0,
            fillColor = "transparent",
            fillOpacity = 0,
            highlightOptions = highlightOptions(
              fillColor = "cadetblue", fillOpacity = 0.5,
              bringToFront = TRUE
            ),
            group = "ordinary"
          )
        
        previous_matches(character())
        previous_polygon_ID(character())
      }
    }
  }, ignoreNULL = T)
  
  observeEvent(input$HUC_search_results_rows_selected, ignoreInit=T, {
    
    current_selection = input$HUC_search_results_rows_selected
    
    selected_huc8(HUC_srch_results()[current_selection, "HUC"])
    selected_name(HUC_srch_results()[current_selection, "Name"])
    
    selectedHUC = selected_huc8()
    
    if (!is.null(selectedHUC)) {
      
      selected_huc_polygon = subset(HUC_boundaries, HUC8 == selectedHUC)
      
      polygon = st_as_sfc(selected_huc_polygon)
      
      coords = do.call(rbind, lapply(polygon, st_coordinates))
      
      min_lat = min(coords[, 2])
      max_lat = max(coords[, 2])
      min_lon = min(coords[, 1])
      max_lon = max(coords[, 1])
      
      ave_long = (max_lon + min_lon)/2
      ave_lat = (max_lat + min_lat)/2
      
      leafletProxy("map", session) |>
        setView(lng = ave_long, lat = ave_lat, zoom = 7)
      
      if (!boundaries_added()) {
        leafletProxy("map", session) |>
          addPolygons(
            data = HUC_boundaries,
            layerId = ~id,
            color = "#03F",
            weight = 2,
            opacity = 1.0,
            fillColor = "transparent",
            fillOpacity = 0,
            highlightOptions = highlightOptions(
              fillColor = "cadetblue", fillOpacity = 0.5,
              bringToFront = TRUE
            ),
            group = "ordinary"
          )
        
        boundaries_added(TRUE)
        
        shinyjs::disable("map")
        shinyjs::show("loading")
        
        delay(time_delay(), {
          shinyjs::enable("map")
          shinyjs::hide("loading")
        })
      }
      
      leafletProxy("map", session) |>
        addPolygons(
          data = subset(HUC_boundaries, HUC8 %in% previous_matches()),
          layerId = ~id,
          color = "#03F",
          weight = 2,
          opacity = 1.0,
          fillColor = "transparent",
          fillOpacity = 0,
          highlightOptions = highlightOptions(
            fillColor = "cadetblue", fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          group = "ordinary"
        ) |>
        addPolygons(
          data = subset(HUC_boundaries, id == previous_polygon_ID()),
          layerId = ~id,
          color = "#03F",
          weight = 2,
          opacity = 1.0,
          fillColor = "transparent",
          fillOpacity = 0,
          highlightOptions = highlightOptions(
            fillColor = "cadetblue", fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          group = "ordinary"
        ) |>
        addPolygons(
          data = selected_huc_polygon,
          layerId = ~id,
          color = "#03F",
          weight = 2,
          opacity = 1.0,
          fillColor = "cadetblue",
          fillOpacity = 0.8,
          highlightOptions = highlightOptions(
            fillColor = "cadetblue", fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          group = "highlighted"
        )
      
      previous_polygon_ID(selected_huc_polygon$id)
      previous_matches(selected_huc8())
      
    } else {
      
      leafletProxy("map", session) |>
        addPolygons(
          data = subset(HUC_boundaries, HUC8 %in% previous_matches()),
          layerId = ~id,
          color = "#03F",
          weight = 2,
          opacity = 1.0,
          fillColor = "transparent",
          fillOpacity = 0,
          highlightOptions = highlightOptions(
            fillColor = "cadetblue", fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          group = "ordinary"
        ) |>
        addPolygons(
          data = subset(HUC_boundaries, id == previous_polygon_ID()),
          layerId = ~id,
          color = "#03F",
          weight = 2,
          opacity = 1.0,
          fillColor = "transparent",
          fillOpacity = 0,
          highlightOptions = highlightOptions(
            fillColor = "cadetblue", fillOpacity = 0.5,
            bringToFront = TRUE
          ),
          group = "ordinary"
        )
      
      previous_matches(character())
      previous_matches(character())
    }
    
    HUC_fishes(HUC_data[HUC_data$HUC == selected_huc8(),1:2])
    
    output$selected_HUC_info = renderUI({
      req(selected_huc8(), selected_name())
      HTML(paste("<strong>Name:</strong>", selected_name(), "<br>",
                 "<strong>Selected HUC8:</strong>", selected_huc8()))
    })
    
    display("HUC_searched")
    
  }, ignoreNULL=T)
  
  observeEvent(input$huc_fishes_rows_selected,  ignoreInit=T, {
    
    selected_huc8(character())
    selected_name(character())
    output$selected_HUC_info = NULL
    
    current_selection = input$huc_fishes_rows_selected
    
    if (is.null(current_selection)) {
      return()

    } else {
      if (input$map_zoom >= 5) {
        
        selected_common_name = HUC_fishes()[current_selection, "Common Name"]
        
        if (!is.null(selected_common_name)) {
          
          matching_hucs = HUC_data[HUC_data$Common_Name == selected_common_name, "HUC"]
          
          if (length(matching_hucs) == 0) {
            
            showModal(modalDialog("No distribution information for this species.",easyClose = TRUE,
                                  footer = tagList(modalButton("Close"))))
            return()
            
          } else {
            
            relevant_HUCs = subset(HUC_boundaries, HUC8 %in% matching_hucs)
            
            polygons = st_as_sfc(relevant_HUCs)
            
            coords = do.call(rbind, lapply(polygons, st_coordinates))
            
            min_lat = min(coords[, 2])
            max_lat = max(coords[, 2])
            min_lon = min(coords[, 1])
            max_lon = max(coords[, 1])
            
            longitude_range = max_lon - min_lon
            latitude_range = max_lat - min_lat
            US_lon_range = 58
            US_lat_range = 25
            
            #values to convert a species range to the appropriate zoom level
            range_guide = c(0.75,0.4,0.20,0.07)
            
            app_zoom_level = ifelse(
              (longitude_range >= range_guide[1]*US_lon_range |latitude_range >= range_guide[1]*US_lat_range), 5,
              ifelse((longitude_range >= range_guide[2]*US_lon_range |latitude_range >= range_guide[2]*US_lat_range), 6,
                     ifelse((longitude_range >= range_guide[3]*US_lon_range |latitude_range >= range_guide[3]*US_lat_range), 7, 
                            ifelse((longitude_range >= range_guide[4]*US_lon_range |latitude_range >= range_guide[4]*US_lat_range), 8, 9))))
            
            center_lat = (min_lat + max_lat) / 2
            center_lon = (min_lon + max_lon) / 2
            
            leafletProxy("map", session) |>
              setView(lng = center_lon, lat = center_lat, zoom = app_zoom_level)
            
            if (!boundaries_added()) {
              leafletProxy("map", session) |>
                addPolygons(
                  data = HUC_boundaries,
                  layerId = ~id,
                  color = "#03F",
                  weight = 2,
                  opacity = 1.0,
                  fillColor = "transparent",
                  fillOpacity = 0,
                  highlightOptions = highlightOptions(
                    fillColor = "cadetblue", fillOpacity = 0.5,
                    bringToFront = TRUE
                  ),
                  group = "ordinary"
                )
              
              boundaries_added(TRUE)
              
              shinyjs::disable("map")
              shinyjs::show("loading")
              
              delay(time_delay(), {
                
                shinyjs::enable("map")
                shinyjs::hide("loading")
                
                leafletProxy("map", session) |>
                  addPolygons(
                    data = subset(HUC_boundaries, HUC8 %in% previous_matches()),
                    layerId = ~id,
                    color = "#03F",
                    weight = 2,
                    opacity = 1.0,
                    fillColor = "transparent",
                    fillOpacity = 0,
                    highlightOptions = highlightOptions(
                      fillColor = "cadetblue", fillOpacity = 0.5,
                      bringToFront = TRUE
                    ),
                    group = "ordinary"
                  ) |>
                  addPolygons(
                    data = subset(HUC_boundaries, id == previous_polygon_ID()),
                    layerId = ~id,
                    color = "#03F",
                    weight = 2,
                    opacity = 1.0,
                    fillColor = "transparent",
                    fillOpacity = 0,
                    highlightOptions = highlightOptions(
                      fillColor = "cadetblue", fillOpacity = 0.5,
                      bringToFront = TRUE
                    ),
                    group = "ordinary"
                  ) |>
                  addPolygons(
                    data = relevant_HUCs,
                    layerId = ~id,
                    color = "#03F",
                    weight = 2,
                    opacity = 1.0,
                    fillColor = "yellow",
                    fillOpacity = 0.8,
                    highlightOptions = highlightOptions(
                      fillColor = "cadetblue", fillOpacity = 0.5,
                      bringToFront = TRUE
                    ),
                    group = "highlighted"
                  )
              })
              
            } else {
              
              leafletProxy("map", session) |>
                addPolygons(
                  data = subset(HUC_boundaries, HUC8 %in% previous_matches()),
                  layerId = ~id,
                  color = "#03F",
                  weight = 2,
                  opacity = 1.0,
                  fillColor = "transparent",
                  fillOpacity = 0,
                  highlightOptions = highlightOptions(
                    fillColor = "cadetblue", fillOpacity = 0.5,
                    bringToFront = TRUE
                  ),
                  group = "ordinary"
                ) |>
                addPolygons(
                  data = subset(HUC_boundaries, id == previous_polygon_ID()),
                  layerId = ~id,
                  color = "#03F",
                  weight = 2,
                  opacity = 1.0,
                  fillColor = "transparent",
                  fillOpacity = 0,
                  highlightOptions = highlightOptions(
                    fillColor = "cadetblue", fillOpacity = 0.5,
                    bringToFront = TRUE
                  ),
                  group = "ordinary"
                ) |>
                addPolygons(
                  data = relevant_HUCs,
                  layerId = ~id,
                  color = "#03F",
                  weight = 2,
                  opacity = 1.0,
                  fillColor = "yellow",
                  fillOpacity = 0.8,
                  highlightOptions = highlightOptions(
                    fillColor = "cadetblue", fillOpacity = 0.5,
                    bringToFront = TRUE
                  ),
                  group = "highlighted"
                )
            }
            
            previous_matches(matching_hucs)
            previous_polygon_ID(character())
          }
        } else {
          
          leafletProxy("map", session) |>
            setView(lng = -100, lat = 40, zoom = current_zoom())
          
          if (!boundaries_added() && current_zoom() >= 5) {
            leafletProxy("map", session) |>
              addPolygons(
                data = HUC_boundaries,
                layerId = ~id,
                color = "#03F",
                weight = 2,
                opacity = 1.0,
                fillColor = "transparent",
                fillOpacity = 0,
                highlightOptions = highlightOptions(
                  fillColor = "cadetblue", fillOpacity = 0.5,
                  bringToFront = TRUE
                ),
                group = "ordinary"
              )
            
            boundaries_added(TRUE)
            
            shinyjs::disable("map")
            shinyjs::show("loading")
            
            delay(time_delay(), {
              shinyjs::enable("map")
              shinyjs::hide("loading")
            })
          }
          
          leafletProxy("map", session) |>
            addPolygons(
              data = subset(HUC_boundaries, HUC8 %in% previous_matches()),
              layerId = ~id,
              color = "#03F",
              weight = 2,
              opacity = 1.0,
              fillColor = "transparent",
              fillOpacity = 0,
              highlightOptions = highlightOptions(
                fillColor = "cadetblue", fillOpacity = 0.5,
                bringToFront = TRUE
              ),
              group = "ordinary"
            ) |>
            addPolygons(
              data = subset(HUC_boundaries, id == previous_polygon_ID()),
              layerId = ~id,
              color = "#03F",
              weight = 2,
              opacity = 1.0,
              fillColor = "transparent",
              fillOpacity = 0,
              highlightOptions = highlightOptions(
                fillColor = "cadetblue", fillOpacity = 0.5,
                bringToFront = TRUE
              ),
              group = "ordinary"
            )
          
          previous_matches(character())
          previous_polygon_ID(character())
        }
      }
    }
  }, ignoreNULL = T)
  
  output$distexpl_text = renderUI({
    HTML("Search for a fish by its genus, species, or common name. Click on a row in the results table to show the HUCs where that species has been collected.<br><br>
    Click on a HUC to show fish species that have been collected there. HUCs are only shown at zoom level 5 or more.")
  })
  
  output$explore_text = renderUI({
    HTML("<div style='font-size: 18px;'>Query the fish properties database by searching for species of interest.<br>
         Select species properties, habitat characteristics, or other features to find fish that meet your criteria.<br>
         You can remove selected choices by highlighting them and pressing the Delete or Backspace key.<br><br><br></div>")
  })
  
  observeEvent(input$submit_query, {
    
    explore_results(NULL)
    output$selected_species = NULL
    output$species_links = NULL
    output$lw_inputs = NULL
    
    filtered_data = fish_props
    
    if (!is.null(input$tribe_choice)) {
        filtered_data = filtered_data[filtered_data$Tribe %in% input$tribe_choice,]
    }
    
    if (!is.null(input$biogeo_choice)) {
      
      mapping = c("Native" = "Y", "Introduced" = "N")
      mapped_choices = mapping[input$biogeo_choice]
      filtered_data = filtered_data[filtered_data$Native %in% mapped_choices,]
    }
    
    if (!is.null(input$sensi_choice)) {
      
      mapping = c("Tolerant" = "T", "Medium" = "M", "Intolerant" = "S", "Unclassified" = "U")
      mapped_choices = mapping[input$sensi_choice]
      filtered_data = filtered_data[filtered_data$Sensitivity %in% mapped_choices,]
    }
    
    if (!is.null(input$benuse_choice)) {
      condition = rep(FALSE, nrow(filtered_data))
      
      for (col in input$benuse_choice) {
        if (col %in% colnames(filtered_data)) {
          condition = condition | filtered_data[[col]]==1
        }
      }
      
      filtered_data = filtered_data[condition, ]
    }
    
    if (!is.null(input$system_choice)) {
      mapping = c(
        "Small River" = "Small_Rivers",
        "Medium River" = "Medium_Rivers",
        "Large River" = "Large_Rivers",
        "Lake/Impoundment" = "Lake_Impoundment",
        "Swamp/Marsh/Bayou" = "Swamp_Marsh",
        "Coastal/Ocean" = "Coastal_Ocean"
      )
      
      condition = rep(FALSE, nrow(filtered_data))

      for (choice in input$system_choice) {
        if (choice %in% names(mapping)) {
          col = mapping[choice]
        } else {
          col = choice
        }

        if (col %in% colnames(filtered_data)) {
          condition = condition | filtered_data[[col]] != "-999"
        }
      }
      
      filtered_data = filtered_data[condition, ]
    }
    
    if (!is.null(input$waterpos_choice)) {
      
      mapping = c(
        "Nearshore/Littoral" = "Nearshore_Littoral",
        "Open Water/Pelagic" = "OpenWater_Pelagic"
      )
      
      condition = rep(FALSE, nrow(filtered_data))
      
      for (choice in input$waterpos_choice) {
        if (choice %in% names(mapping)) {
          col = mapping[choice]
        } else {
          col = choice
        }
        
        if (col %in% colnames(filtered_data)) {
          condition = condition | filtered_data[[col]] != "-999"
        }
      }
      
      filtered_data = filtered_data[condition, ]
    }
    
    if (!is.null(input$substrate_choice)) {
      
      mapping = c(
        "Mud/Silt" = "Mud_Silt",
        "Rock/Rubble" = "Rock_Rubble",
        "Woody Debris" = "Woody_Debris"
      )
      
      condition = rep(FALSE, nrow(filtered_data))

      for (choice in input$substrate_choice) {
        if (choice %in% names(mapping)) {
          col = mapping[choice]
        } else {
          col = choice
        }
        
        if (col %in% colnames(filtered_data)) {
          condition = condition | filtered_data[[col]] != "-999"
        }
      }

      filtered_data = filtered_data[condition, ]
    }
    
    if (!is.null(input$habtype_choice)) {
      condition = rep(FALSE, nrow(filtered_data))
      
      for (col in input$habtype_choice) {
        if (col %in% colnames(filtered_data)) {
          condition = condition | filtered_data[[col]] != "-999"
        }
      }
      
      filtered_data = filtered_data[condition, ]
    }
    
    if (!is.null(input$clarity_choice)) {
      condition = rep(FALSE, nrow(filtered_data))
      
      for (col in input$clarity_choice) {
        if (col %in% colnames(filtered_data)) {
          condition = condition | filtered_data[[col]] != "-999"
        }
      }
      
      filtered_data = filtered_data[condition, ]
    }
    
    if (!is.null(input$thermal_choice)) {
      condition = rep(FALSE, nrow(filtered_data))
      
      for (col in input$thermal_choice) {
        if (col %in% colnames(filtered_data)) {
          condition = condition | filtered_data[[col]] != "-999"
        }
      }
      
      filtered_data = filtered_data[condition, ]
    }
    
    if (!is.null(input$topo_choice)) {
      condition = rep(FALSE, nrow(filtered_data))
      
      for (col in input$topo_choice) {
        if (col %in% colnames(filtered_data)) {
          condition = condition | filtered_data[[col]] != "-999"
        }
      }
      
      filtered_data = filtered_data[condition, ]
    }
    
    filtered_data = filtered_data[filtered_data$Rarity >= input$rarity_slider[1] & filtered_data$Rarity <= input$rarity_slider[2],]
    filtered_data = filtered_data[filtered_data$Max_Age >= input$max_age[1] & filtered_data$Max_Age <= input$max_age[2],]
    filtered_data = filtered_data[filtered_data$Girth_Index >= input$girth[1] & filtered_data$Girth_Index <= input$girth[2],]
    
    filtered_data = filtered_data[filtered_data$Mean_Weight >= input$lower_mean_weight & filtered_data$Mean_Weight <= input$upper_mean_weight,]
    
    if (input$mean_length[2] == 40) {
      filtered_data = filtered_data[filtered_data$Mean_Length >= input$mean_length[1] & filtered_data$Mean_Length <= 201,]
    } else {
      filtered_data = filtered_data[filtered_data$Mean_Length >= input$mean_length[1] & filtered_data$Mean_Length <= input$mean_length[2],]
    }
    
    if (input$max_TL[2] == 200) {
      filtered_data = filtered_data[filtered_data$Max_Length >= input$max_TL[1] & filtered_data$Max_Length <= 1000,]
    } else {
      filtered_data = filtered_data[filtered_data$Max_Length >= input$max_TL[1] & filtered_data$Max_Length <= input$max_TL[2],]
    }
    
    if (input$use_unestimated) {
      
      filtered_data = filtered_data[filtered_data$Ubiquity == 0 | (filtered_data$Ubiquity >= input$ubiquity_slider[1] & filtered_data$Ubiquity <= input$ubiquity_slider[2]),]
      filtered_data = filtered_data[filtered_data$Extent == 0  | (filtered_data$Extent >= input$extent_slider[1] & filtered_data$Extent <= input$extent_slider[2]),]
      filtered_data = filtered_data[filtered_data$Tolerance == 0  | (filtered_data$Tolerance >= input$tolerance_slider[1] & filtered_data$Tolerance <= input$tolerance_slider[2]),]
      filtered_data = filtered_data[filtered_data$Robustness == 0  | (filtered_data$Robustness >= input$robustness_slider[1] & filtered_data$Robustness <= input$robustness_slider[2]),]
      
    } else {
    
      filtered_data = filtered_data[filtered_data$Ubiquity > 0 & (filtered_data$Ubiquity >= input$ubiquity_slider[1] & filtered_data$Ubiquity <= input$ubiquity_slider[2]),]
      filtered_data = filtered_data[filtered_data$Extent > 0  & (filtered_data$Extent >= input$extent_slider[1] & filtered_data$Extent <= input$extent_slider[2]),]
      filtered_data = filtered_data[filtered_data$Tolerance > 0  & (filtered_data$Tolerance >= input$tolerance_slider[1] & filtered_data$Tolerance <= input$tolerance_slider[2]),]
      filtered_data = filtered_data[filtered_data$Robustness > 0  &  (filtered_data$Robustness >= input$robustness_slider[1] & filtered_data$Robustness <= input$robustness_slider[2]),]
    }

    explore_results(data.frame(filtered_data[, c(5, 6, 7)]))
    
    if (is.null(explore_results())) {
      output$explore_results = NULL
      
    } else {
      
      output$explore_results = DT::renderDataTable(server = T, {
        datatable(
          explore_results(),
          rownames = F,
          selection = list(
            selected = NULL,
            target = "row",
            mode = "single"
          ),
          editable = F,
          options = list(
            autoWidth = F,
            dom='tp',
            scrollX = F,
            scrollY = TRUE,
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
              "}"
            )
          )
        )
      })
    }
  })
  
  observeEvent(input$reset_query, {
    
    output$selected_species = NULL
    output$species_links = NULL
    output$lw_inputs = NULL
    
    updateSelectizeInput(session, "tribe_choice", choices = fish_tribes, selected = NULL)
    updateSelectizeInput(session, "biogeo_choice", choices = c("Native","Introduced"), selected = NULL)
    updateSelectizeInput(session, "benuse_choice", choices = c("Subsistence","Sportfish","NonGame"), selected = NULL)
    updateSelectizeInput(session, "sensi_choice", choices = c("Tolerant","Medium","Sensitive","Unclassified"), selected = NULL)
    updateSelectizeInput(session, "system_choice", choices = systems, selected = NULL)
    updateSelectizeInput(session, "waterpos_choice", choices = c("Benthic","Surface","OpenWater_Pelagic","Nearshore_Littoral"), selected = NULL)
    updateSelectizeInput(session, "substrate_choice", choices = c("Mud_Silt","Sand","Gravel","Rock_Rubble","Vegetation","Woody_Debris"), selected = NULL)
    updateSelectizeInput(session, "habtype_choice", choices = c("Riffles","Runs","Pools"), selected = NULL)
    updateSelectizeInput(session, "clarity_choice", choices = c("Clear","Turbid"), selected = NULL)
    updateSelectizeInput(session, "thermal_choice", choices = c("Cold","Cool","Warm"), selected = NULL)
    updateSelectizeInput(session, "topo_choice", choices = c("Lowlands","Uplands"), selected = NULL)
    
    updateSwitchInput(session, "use_unestimated", value = TRUE)

    updateSliderInput(session, "rarity_slider", value = c(1,10))
    updateSliderInput(session, "max_age", value = c(1,25))
    updateNumericInput(session, "lower_mean_weight", value = 0)
    updateNumericInput(session, "upper_mean_weight", value = 200000)
    updateSliderInput(session, "mean_length", value = c(0,40))
    updateSliderInput(session, "max_TL", value = c(0,200))
    updateSliderInput(session, "girth", value = c(0,1))
    updateSliderInput(session, "ubiquity_slider", value = c(0,75))
    updateSliderInput(session, "extent_slider", value = c(0,80))
    updateSliderInput(session, "tolerance_slider", value = c(0,95))
    updateSliderInput(session, "robustness_slider", value = c(0,50))
    
    output$explore_results = NULL
    output$selected_species = NULL
    output$species_links = NULL

  })
  
  observeEvent(input$unit_toggle, ignoreInit = T, {
    
    units(input$unit_toggle)
    
    if (input$unit_toggle == "english") {
      
      new_weight = (A()*(input$fish_length * 2.54)^B())*0.002205
      
      if (new_weight > 1) {
        new_weight = round(new_weight, 1)
      } else {
        new_weight = round(new_weight, 2)
      }
      
    } else {
      
      new_weight = A()*input$fish_length^B()
      
      if (new_weight > 1) {
        new_weight = round(new_weight, 1)
      } else {
        new_weight = round(new_weight, 2)
      }
    }
    
    updateNumericInput(session, "fish_weight", value = new_weight)
    
    fishy_weight(new_weight)
    fishy_length(input$fish_length)
    
  })
  
  observeEvent(input$explore_results_rows_selected, {
    
    selected_row_index = input$explore_results_rows_selected
    
    if (!is.null(selected_row_index)) {
      
      selected_common_name = explore_results()[selected_row_index, "Common_Name"]
      filtered_fish_props = fish_props[fish_props$Common_Name == selected_common_name, ]
      
      colnames(filtered_fish_props)[7] = HTML("Common Name")
      colnames(filtered_fish_props)[16] = HTML("Rarity (PFG)")
      colnames(filtered_fish_props)[21] = HTML("Max Age (yr)")
      colnames(filtered_fish_props)[22] = HTML("Max Length (cm)")
      colnames(filtered_fish_props)[23] = HTML("Mean Length (cm)")
      colnames(filtered_fish_props)[24] = HTML("Mean Weight (g)")
      colnames(filtered_fish_props)[25] = HTML("Girth Index")
      colnames(filtered_fish_props)[63] = HTML("Location Notes")
      colnames(filtered_fish_props)[64] = HTML("Habitat Notes")
      

      biogeo_feats = names(filtered_fish_props)[10]
      biogeo_feat_value = ifelse(filtered_fish_props[1, biogeo_feats] == "Y", "Native", "Introduced")
      
      sensitivity_feats = names(filtered_fish_props)[15]
      sensitivity_feat_value = switch(filtered_fish_props[1, sensitivity_feats],
                                      "M" = "Medium",
                                      "S" = "Sensitive",
                                      "U" = "Unclassified",
                                      "T" = "Tolerant")
      
      human_feats = names(filtered_fish_props)[12:14]
      human_feat_value = paste(human_feats[filtered_fish_props[1, human_feats] == 1], collapse = ", ")
      
      system_feats = names(filtered_fish_props)[31:40]
      mapped_values1 = vector()
      
      for (feat1 in system_feats) {
        value = filtered_fish_props[1, feat1]
        if (any(filtered_fish_props[[feat1]] != "XXX")) {
          mapped_value1 = switch(feat1,
                                 "Medium_River" = "Medium River",
                                 "Small_River" = "Small River",
                                 "Large_River" = "Large River",
                                 "Lake_Impoundment" = "Lake/Impoundment/Reservoir",
                                 "Swamp_Marsh" = "Swamp/Marsh/Bayou",
                                 "Coastal_Ocean" = "Coastal/Ocean",
                                 feat1)
          mapped_values1 = c(mapped_values1, mapped_value1)
        }
      }
      
      system_feat_value = paste(mapped_values1, collapse = ", ")
      
      hab_feats= names(filtered_fish_props)[41:43]
      hab_feat_value = paste(hab_feats[filtered_fish_props[1, hab_feats] != "XXX"], collapse = ", ")
      
      column_feats= names(filtered_fish_props)[44:47]
      mapped_values2 = vector()
      
      for (feat2 in column_feats) {
        value = filtered_fish_props[1, feat2]
        if (any(filtered_fish_props[[feat2]] != "XXX")) {
          mapped_value2 = switch(feat2,
                                 "Nearshore_Littoral" = "Nearshore/Littoral",
                                 "OpenWater_Pelagic" = "Open Water/Pelagic",
                                 feat2)
          mapped_values2 = c(mapped_values2, mapped_value2)
        }
      }
      
      column_feat_value = paste(mapped_values2, collapse = ", ")
      
      substrate_feats= names(filtered_fish_props)[48:53]
      mapped_values3 = vector()
      
      for (feat3 in substrate_feats) {
        value = filtered_fish_props[1, feat3]
        if (any(filtered_fish_props[[feat3]] != "XXX")) {
          mapped_value3 = switch(feat3,
                                 "Mud_Silt" = "Mud/Silt",
                                 "Rock_Rubble" = "Rock/Rubble",
                                 "Woody_Debris" = "Woody Debris",
                                 feat3)
          mapped_values3 = c(mapped_values3, mapped_value3)
        }
      }
      
      substrate_feat_value = paste(mapped_values3, collapse = ", ")
      
      clarity_feats= names(filtered_fish_props)[54:55]
      clarity_feat_value = paste(clarity_feats[filtered_fish_props[1, clarity_feats] != "XXX"], collapse = ", ")
      
      thermal_feats= names(filtered_fish_props)[56:58]
      therma_feat_value = paste(thermal_feats[filtered_fish_props[1, thermal_feats] != "XXX"], collapse = ", ")
      
      topo_feats= names(filtered_fish_props)[59:60]
      topo_feat_value = paste(topo_feats[filtered_fish_props[1, topo_feats] != "XXX"], collapse = ", ")
      
      filtered_fish_props = filtered_fish_props[,-c(1:3,8,10:15,28:62)]
      
      filtered_fish_props$`Biogeography` = biogeo_feat_value
      filtered_fish_props$`Sensitivity` = sensitivity_feat_value
      filtered_fish_props$`Human Use` = human_feat_value
      filtered_fish_props$`System` = system_feat_value
      filtered_fish_props$`Habitat` = hab_feat_value
      filtered_fish_props$`Water Column` = column_feat_value
      filtered_fish_props$`Substrate` = substrate_feat_value
      filtered_fish_props$`Water Clarity` = clarity_feat_value
      filtered_fish_props$`Thermal` = therma_feat_value
      filtered_fish_props$`Topography` = topo_feat_value
      
      if (filtered_fish_props$Biogeography[1] == "") {
        filtered_fish_props$Biogeography[1] = "-"
      }
      
      if (filtered_fish_props$Sensitivity[1] == "") {
        filtered_fish_props$Sensitivity[1] = "-"
      }
      
      if (filtered_fish_props$`Human Use`[1] == "") {
        filtered_fish_props$`Human Use`[1] = "-"
      }
      
      if (filtered_fish_props$System[1] == "") {
        filtered_fish_props$System[1] = "-"
      }
      
      if (filtered_fish_props$Habitat[1] == "") {
        filtered_fish_props$Habitat[1] = "-"
      }
      
      if (filtered_fish_props$`Water Column`[1] == "") {
        filtered_fish_props$`Water Column`[1] = "-"
      }
      
      if (filtered_fish_props$Substrate[1] == "") {
        filtered_fish_props$Substrate[1] = "-"
      }
      
      if (filtered_fish_props$`Water Clarity`[1] == "") {
        filtered_fish_props$`Water Clarity`[1] = "-"
      }
      
      if (filtered_fish_props$Thermal[1] == "") {
        filtered_fish_props$Thermal[1] = "-"
      }
      
      if (filtered_fish_props$Topography[1] == "") {
        filtered_fish_props$Topography[1] = "-"
      }
      
      if (filtered_fish_props$`Location Notes`[1] == "") {
        filtered_fish_props$`Location Notes`[1] = "-"
      }
      
      if (filtered_fish_props$`Habitat Notes`[1] == "") {
        filtered_fish_props$`Habitat Notes`[1] = "-"
      }
      
      #Rearrange the columns, putting Location and Habitat notes at the end, since new columns added to the dataset
      filtered_fish_props = filtered_fish_props[,c(setdiff(seq_along(filtered_fish_props), 18:19), 18:19)]
      
      field_names = names(filtered_fish_props)
      
      if (filtered_fish_props$Ubiquity[1] == 0) {
        filtered_fish_props$Ubiquity[1] = "-"
      }
      
      if (filtered_fish_props$Extent[1] == 0) {
        filtered_fish_props$Extent[1] = "-"
      }
      
      if (filtered_fish_props$Tolerance[1] == 0) {
        filtered_fish_props$Tolerance[1] = "-"
      }
      
      if (filtered_fish_props$Robustness[1] == 0) {
        filtered_fish_props$Robustness[1] = "-"
      }

      field_values = as.character(filtered_fish_props[1,])
      
      matrix_data = data.frame(
        Col_1 = field_names[seq(1, 15)],
        Col_2 = c(field_names[seq(16, 29)]," ")
      )
      
      matrix_values = data.frame(
        Col_1_Values = field_values[seq(1, 15)],
        Col_2_Values = c(field_values[seq(16, 29)],"")
      )
      
      combined = data.frame(
        Feat_Col_1 = matrix_data$Col_1,
        Col_1_Value = matrix_values$Col_1_Values,
        Feat_Col_2 = matrix_data$Col_2,
        Col_2_Value = matrix_values$Col_2_Values
      )
      
      combined$Feat_Col_2[1] = "A (W=A*L<sup>B</sup>)"
      combined$Feat_Col_2[2] = "B (W=A*L<sup>B</sup>)"
      
      A(as.numeric(field_values[16]))
      B(as.numeric(field_values[17]))
      
      if (units() == "english") {
        fishy_length(filtered_fish_props$`Mean Length (cm)`[1] / 2.54)
        fishy_weight(filtered_fish_props$`Mean Weight (g)`[1] * 0.00205)
      } else {
        fishy_length(filtered_fish_props$`Mean Length (cm)`[1])
        fishy_weight(filtered_fish_props$`Mean Weight (g)`[1])
      }
      
      output$lw_inputs = renderUI({
        length_label = if (units() == "english") "Total Length (in):" else "Total Length (cm):"
        weight_label = if (units() == "english") "Weight (lbs):" else "Weight (g):"
        
        tagList(
          div(style = "display: inline-block; vertical-align: top; margin-right: 3px;",
              div(style = "display: block; width: 120px;", length_label),
              div(style = "display: block;", numericInput("fish_length", NULL, value = fishy_length(), min = 1, width = '110px')),
              div(style = "display: block; width: 100px; margin-top: 10px;", weight_label),
              div(style = "display: block;", numericInput("fish_weight", NULL, value = fishy_weight(), min = 0, width = '110px'))
          ),
          div(style = "display: inline-block; vertical-align: center; margin-left: 10px;",
              div(style = "display: block; width: 75px !important; margin-top: 25px;", actionButton("calc_lw", "Calculate", style = "line-height: 0px; text-align: center;")),
              div(style = "display: block; width: 75px !important; margin-top: 25px;", 
                  radioButtons("unit_toggle", "", choices = c("Metric" = "metric", "English" = "english"), selected = units(), inline = FALSE))
          )
        )
      })
      
      output$selected_species = DT::renderDataTable(server = T, {
        datatable(
          combined,
          escape=FALSE,
          colnames = NULL,
          rownames = F,
          selection = "none",
          editable = F,
          options = list(
            autoWidth = F,
            dom='t',
            pageLength = 15,
            ordering = FALSE,
            scrollX = TRUE,
            scrollY = F,
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).append('<tr><th colspan=\"4\" style=\"text-align:left; background-color: #073744; color: #fff;\">Species Properties</th></tr>');",
              "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});","}"))) |>
          formatStyle(columns = c(1,3),fontWeight = 'bold')
      })
      
      search_string = paste(filtered_fish_props$Genus,filtered_fish_props$Species)
      wiki_link = sprintf('<a href="https://en.wikipedia.org/wiki/%s" target="_blank">Wikipedia</a>',URLencode(search_string))
      google_link = sprintf('<a href="https://www.google.com/search?tbm=isch&q=%s" target="_blank">Google Images</a>',URLencode(search_string))
      
      output$species_links = renderUI({
        tagList(
          div(style = "display: inline-block; margin-right: 20px;", HTML(wiki_link)),
          div(style = "display: inline-block;", HTML(google_link))
        )
      })
    }
  })
  
  observeEvent(input$calc_lw, ignoreInit = TRUE, {
    
    if (input$fish_length != fishy_length()) {
        
        if (input$unit_toggle == "english") {
          
          new_weight = (A()*(input$fish_length * 2.54)^B())*0.002205
          
          if (new_weight > 1) {
            new_weight = round(new_weight, 1)
          } else {
            new_weight = round(new_weight, 2)
          }
          
        } else {
          
          new_weight = A()*input$fish_length^B()
          
          if (new_weight > 1) {
            new_weight = round(new_weight, 1)
          } else {
            new_weight = round(new_weight, 2)
          }
        }
      
        updateNumericInput(session, "fish_weight", value = new_weight)
        
        fishy_weight(new_weight)
        fishy_length(input$fish_length)
        
      } else if (input$fish_weight != fishy_weight()) {
        
        if (input$unit_toggle == "english") {
          
          new_length = ((input$fish_weight*453.5/A())^(1/B()))/2.54
          
          if (new_length > 1) {
            new_length = round(new_length, 1)
          } else {
            new_length = round(new_length, 2)
          }
          
        } else {
          new_length = (input$fish_weight/A())^(1/B())
          
          if (new_length > 1) {
            new_length = round(new_length, 1)
          } else {
            new_length = round(new_length, 2)
          }
        }
        
        updateNumericInput(session, "fish_length", value = new_length)
        
        fishy_weight(input$fish_weight)
        fishy_length(new_length)
        
      } else {
        
        if (input$unit_toggle == "english") {
          
          new_weight = (A()*(input$fish_length * 2.54)^B())*0.002205
          
          if (new_weight > 1) {
            new_weight = round(new_weight, 1)
          } else {
            new_weight = round(new_weight, 2)
          }
          
        } else {
          
          new_weight = A()*input$fish_length^B()
          
          if (new_weight > 1) {
            new_weight = round(new_weight, 1)
          } else {
            new_weight = round(new_weight, 2)
          }
        }
        
        updateNumericInput(session, "fish_weight", value = new_weight)
        
        fishy_weight(new_weight)
        fishy_length(input$fish_length)
      }
  }, ignoreNULL = TRUE)

  observeEvent(input$stream_map_click, {

    # click = input$stream_map_click
    # click_point = st_sfc(st_point(c(click$lng, click$lat)), crs = st_crs(stream_segments))
    # click_point = st_zm(click_point, drop = TRUE, what = "ZM")
    # 
    # radius = 1000
    # 
    # click_bbox = st_bbox(click_point) + c(-radius, -radius, radius, radius)
    # nearby_segments = stream_segments[st_intersects(stream_segments, st_as_sfc(click_bbox), sparse = FALSE), ]
    # 
    # nearest_segment = nearby_segments |>
    #   st_distance(click_point) |>
    #   which.min()
    # 
    # selected_stream = stream_segments[nearest_segment, ]
    # 
    # feature_id = selected_stream$COMID
    # feature_HUC = substr(selected_stream$REACHCODE, 1, 8)
    # 
    # if (is.na(selected_stream$GNIS_NAME)) {
    #   feature_name = ""
    # } else {
    #   feature_name = selected_stream$GNIS_NAME
    # }
    # 
    # stream_ID(feature_id)
    # stream_HUC(feature_HUC)
    # stream_NAME(feature_name)
    # 
    # leafletProxy("stream_map") %>%
    #   clearShapes() %>%
    #   addTiles() %>%
    #   addPolygons(data = selected_stream, color = "blue", weight = 2, opacity = 1)
    # 
    # output$segment_info = renderUI({
    #   HTML(paste(
    #     paste("<em>Latitude:</em>", stream_lat()), "<br>",
    #     paste("<em>Longitude:</em>", stream_lon()), "<br>",
    #     paste("<em>COM ID:</em>", stream_ID()), "<br>",
    #     paste("<em>HUC:</em>", stream_HUC()), "<br>",
    #     paste("<em>Name:</em>", stream_NAME())
    #   ))
    # })
    
    stream_coords$lat = input$stream_map_click$lat
    stream_coords$lng = input$stream_map_click$lng
    
    stream_lat(round(input$stream_map_click$lat,4))
    stream_lon(round(input$stream_map_click$lng,4))

    query_url = paste0(
      "https://api.epa.gov/waters/v3/pointindexing?",
      "p_point={\"type\":\"Point\",\"coordinates\":[", stream_coords$lng, ",", stream_coords$lat, "]}",
      "&p_indexing_engine=DISTANCE",
      "&p_limit_innetwork=FALSE",
      "&p_limit_navigable=TRUE",
      "&p_fallback_limit_innetwork=FALSE",
      "&p_fallback_limit_navigable=TRUE",
      "&p_return_link_path=TRUE",
      "&p_use_simplified_catchments=FALSE",
      "&p_network_resolution=MR",
      "&f=json",
      "&api_key=8MHCinyNAGQh6ypgCW1mD2S6yhJ888dPT1RVKzRM"
    )
    
    response = GET(query_url)
    
    if (status_code(response) == 200) {
      content = content(response, as = "text", encoding = "UTF-8")
      json_data = fromJSON(content)
      
      if ("output_flowlines" %in% names(json_data)) {
        
        flowline = json_data$output_flowlines
        
        # print(flowline$properties)
        
        feature_id = flowline$properties$permanent_identifier
        feature_HUC = substr(flowline$properties$reachcode, 1, 8)
        
        if (is.na(flowline$properties$gnis_name)) {
          feature_name = ""
        } else {
          feature_name = flowline$properties$gnis_name
        }
        
        stream_ID(feature_id)
        stream_HUC(feature_HUC)
        stream_NAME(feature_name)
        
        coordinates = flowline$geometry$coordinates[[1]]
        
        leafletProxy("stream_map") |>
          clearShapes() |>
          addPolylines(lng = coordinates[, 1], lat = coordinates[, 2])
        
        output$segment_info = renderUI({
          HTML(paste(
            paste("<em>Latitude:</em>", stream_lat()), "<br>",
            paste("<em>Longitude:</em>", stream_lon()), "<br>",
            paste("<em>COM ID:</em>", stream_ID()), "<br>",
            paste("<em>HUC:</em>", stream_HUC()), "<br>",
            paste("<em>Name:</em>", stream_NAME())
          ))
        })
        
        con = dbConnect(RSQLite::SQLite(), "stream_bbd.db")
        
        query = sprintf("SELECT wsareasqkm, slope, elevation, bmmi, iwi, bankfull_width FROM catchment WHERE comid = '%s'", feature_id)
        result = dbGetQuery(con, query)
        
        if (is.null(result$wsareasqkm) || result$wsareasqkm == -9999 || is.na(result$wsareasqkm)) {
          WA(NA)
        } else {
          WA(result$wsareasqkm)
        }
        
        if (is.null(result$slope) || result$slope == -9999 || is.na(result$slope)) {
          Slope(NA)
        } else {
          Slope(result$slope)
        }
        
        if (is.null(result$elevation) || result$elevation == -9999 || is.na(result$elevation)) {
          Elev(NA)
        } else {
          Elev(result$elevation)
        }
        
        if (is.null(result$bankfull_width) || result$bankfull_width == -9999 || is.na(result$bankfull_width)) {
          Mean_Width(NA)
        } else {
          Mean_Width(0.75*result$bankfull_width)
        }
        
        if (is.null(result$IWI) || result$IWI == -9999 || is.na(result$IWI)) {
          IWI(NA)
        } else {
          IWI(result$IWI)
        }
        
        if (is.null(result$BMMI) || result$BMMI == -9999 || is.na(result$BMMI)) {
          BMMI(NA)
        } else {
          BMMI(result$BMMI)
        }
        
        dbDisconnect(con)
        
        updateDA = round(WA(), 1)
        updateSlope = 100*round(Slope(), 5)
        updateElev = round(Elev(), 1)
        updateWidth = round(Mean_Width(), 1)
        
        updateTextInput(session, "drainage_area", value = as.character(updateDA))
        updateTextInput(session, "slope", value = as.character(updateSlope))
        updateTextInput(session, "elevation", value = as.character(updateElev))
        updateTextInput(session, "mean_width", value = as.character(updateWidth))
        updateNumericInput(session, "benthic_invert", value = round(BMMI(), 3))
        updateNumericInput(session, "watershed_integrity", value = round(IWI(), 3))
        
        fish_data1 = HUC_data[HUC_data$HUC == stream_HUC(),1:2]
        colnames(fish_data1) = c("Scientific Name", "Common Name")
        
        fish_data2 = fish_props[fish_props[,7] %in% fish_data1[,2],c(1,3,24,28:29,61:62)]
        
        fish_data3 = cbind(fish_data1,fish_data2)
        
        fish_data3$Predicted_Prob = round(calculate_probs(as.numeric(fish_data3$Model_ID), WA(),Slope(),Elev(),IWI(),BMMI()),3)
        
        CritCol = switch(input$prob_thresh,
                         "P1" = "Crit_P1",
                         "Ave(P0,P1)" = "Crit_Ave",
                         "P1 - 1SD" = "Crit_1SD",
                         "P1 - 2SD" = "Crit_2SD",
                         "P0" = "Crit_P0")
        
        selected_modelstats = modelstats[, c("Model", CritCol)]
        fish_data = merge(fish_data3, selected_modelstats, by.x = "Model_ID", by.y = "Model", all.x = TRUE)
        
        print(fish_data)
        
        STREAM_fishes(fish_data)
        
        new_fish_data = fish_data
        
        colnames(new_fish_data)[ncol(new_fish_data)] = input$prob_thresh
        
        new_fish_data$row_color = ifelse(
          !is.na(new_fish_data[[input$prob_thresh]]) & new_fish_data$Predicted_Prob < new_fish_data[[input$prob_thresh]] &
            (Mean_Width() < new_fish_data$Lower_Width | Mean_Width() > new_fish_data$Upper_Width), "gray",
          ifelse(
            !is.na(new_fish_data[[input$prob_thresh]]) & new_fish_data$Predicted_Prob < new_fish_data[[input$prob_thresh]], "yellow",
            ifelse(
              Mean_Width() < new_fish_data$Lower_Width | Mean_Width() > new_fish_data$Upper_Width, "lightgray",
              "lightgreen"
            )
          )
        )
        
        output$fish_assem = DT::renderDataTable(server = TRUE, {
          datatable(
            new_fish_data,
            rownames = FALSE,
            selection = list(
              selected = NULL,
              target = "row",
              mode = "multiple"
            ),
            editable = FALSE,
            options = list(
              dom = 't',
              autoWidth = TRUE,
              pageLength = 200,
              scrollY = TRUE,
              columnDefs = list(
                list(visible = FALSE, targets = c(0,3:8,11)),
                list(width = '80px', targets = 9),
                list(width = '80px', targets = 10),
                list(className = 'dt-center', targets = 9)
              ),
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
                "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
                "}"
              )
            )
          ) |>
            formatStyle(columns = names(new_fish_data),target = 'row',backgroundColor = styleEqual(unique(new_fish_data$row_color), unique(new_fish_data$row_color))) |>
            formatStyle(0, target= 'row', lineHeight='25%') |>
            formatStyle(columns = c(9,10),`text-align` = 'center')
        })
        
        filtered_fish_data  = fish_data[Mean_Width() >= fish_data$Lower_Width & Mean_Width() <= fish_data$Upper_Width &
                                          (fish_data$Predicted_Prob > fish_data[[CritCol]] | is.na(fish_data[[CritCol]])),]
        
        filtered_STREAM_fishes(filtered_fish_data)
        
        output$filtered_fish = DT::renderDataTable(server = T, {
          datatable(
            filtered_fish_data[,2:3],
            rownames = F,
            selection = list(
              selected = NULL,
              target = "row",
              mode = "multiple"
            ),
            editable = F,
            options = list(
              dom='t',
              autoWidth = T,
              pageLength = 200,
              scrollY = T,
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
                "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
                "}"
              )
            )
          ) |>
            formatStyle(0, target= 'row', lineHeight='25%')
        })
      }
    }
    
    #updateTabsetPanel(session, inputId = 'shinyPiSCES', selected = 'Assemblage Predictor')
    #updateTabsetPanel(session, inputId = 'assemblage_tabs', selected = 'Fish Assemblage Filtering')
    
  })
  
  observeEvent(input$update_probs, ignoreInit=T, {
    
    BMMI(input$benthic_invert)
    IWI(input$watershed_integrity)
    
    CritCol = switch(input$prob_thresh,
                     "P1" = "Crit_P1",
                     "Ave(P0,P1)" = "Crit_Ave",
                     "P1 - 1SD" = "Crit_1SD",
                     "P1 - 2SD" = "Crit_2SD",
                     "P0" = "Crit_P0")
    
    new_data = STREAM_fishes()[,-ncol(STREAM_fishes())]
    
    selected_modelstats = modelstats[, c("Model", CritCol)]
    new_data = merge(new_data, selected_modelstats, by.x = "Model_ID", by.y = "Model", all.x = TRUE)
    
    data = new_data
    colnames(data)[ncol(data)] = CritCol
    
    data$Predicted_Prob = round(calculate_probs(as.numeric(data$Model_ID), WA(),Slope(),Elev(),IWI(),BMMI()),3)
    
    STREAM_fishes(data)
    
    data$row_color = ifelse(
      !is.na(data[[CritCol]]) & data$Predicted_Prob < data[[CritCol]] &
        (Mean_Width() < data$Lower_Width | Mean_Width() > data$Upper_Width), "gray",
      ifelse(
        !is.na(data[[CritCol]]) & data$Predicted_Prob < data[[CritCol]], "yellow",
        ifelse(
          Mean_Width() < data$Lower_Width | Mean_Width() > data$Upper_Width, "lightgray",
          "lightgreen"
        )
      )
    )
    
    output$fish_assem = DT::renderDataTable(server = TRUE, {
      datatable(
        data,
        rownames = FALSE,
        selection = list(
          selected = NULL,
          target = "row",
          mode = "multiple"
        ),
        editable = FALSE,
        options = list(
          dom = 't',
          autoWidth = TRUE,
          pageLength = 200,
          scrollY = TRUE,
          columnDefs = list(
            list(visible = FALSE, targets = c(0,3:8,11)),
            list(width = '80px', targets = 9),
            list(width = '80px', targets = 10),
            list(className = 'dt-center', targets = 9)
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
            "}"
          )
        )
      ) |>
        formatStyle(columns = names(data),target = 'row',backgroundColor = styleEqual(unique(data$row_color), unique(data$row_color))) |>
        formatStyle(0, target= 'row', lineHeight='25%') |>
        formatStyle(columns = c(9,10),`text-align` = 'center')
    })
    
    filtered_fish_data  = data[Mean_Width() >= data$Lower_Width & Mean_Width() <= data$Upper_Width &
      (data$Predicted_Prob > data[[CritCol]] | is.na(data[[CritCol]])),]
    
    filtered_STREAM_fishes(filtered_fish_data)
    
    output$filtered_fish = DT::renderDataTable(server = T, {
      datatable(
        filtered_fish_data[,2:3],
        rownames = F,
        selection = list(
          selected = NULL,
          target = "row",
          mode = "multiple"
        ),
        editable = F,
        options = list(
          dom='t',
          autoWidth = T,
          pageLength = 200,
          scrollY = T,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
            "}"
          )
        )
      ) |>
        formatStyle(0, target= 'row', lineHeight='25%')
    })
    
    updateTabsetPanel(session, inputId = 'shinyPiSCES', selected = 'Assemblage Predictor')
    updateTabsetPanel(session, inputId = 'assemblage_tabs', selected = 'Fish Assemblage Filtering')
  })
  
  observeEvent(input$prob_thresh, ignoreInit=T, {
    
    CritCol = switch(input$prob_thresh,
                     "P1" = "Crit_P1",
                     "Ave(P0,P1)" = "Crit_Ave",
                     "P1 - 1SD" = "Crit_1SD",
                     "P1 - 2SD" = "Crit_2SD",
                     "P0" = "Crit_P0")
    
    temp_data = STREAM_fishes()[,-ncol(STREAM_fishes())]
    
    selected_modelstats = modelstats[, c("Model", CritCol)]
      temp_data = merge(temp_data, selected_modelstats, by.x = "Model_ID", by.y = "Model", all.x = TRUE)
    
    temp_table_data = temp_data
    colnames(temp_table_data)[ncol(temp_table_data)] = CritCol
    
    temp_table_data$row_color = ifelse(
      !is.na(temp_table_data[[CritCol]]) & temp_table_data$Predicted_Prob < temp_table_data[[CritCol]] &
        (Mean_Width() < temp_table_data$Lower_Width | Mean_Width() > temp_table_data$Upper_Width), "gray",
      ifelse(
        !is.na(temp_table_data[[CritCol]]) & temp_table_data$Predicted_Prob < temp_table_data[[CritCol]], "yellow",
        ifelse(
          Mean_Width() < temp_table_data$Lower_Width | Mean_Width() > temp_table_data$Upper_Width, "lightgray",
          "lightgreen"
        )
      )
    )
    
    output$fish_assem = DT::renderDataTable(server = TRUE, {
      datatable(
        temp_table_data,
        rownames = FALSE,
        selection = list(
          selected = NULL,
          target = "row",
          mode = "multiple"
        ),
        editable = FALSE,
        options = list(
          dom = 't',
          autoWidth = TRUE,
          pageLength = 200,
          scrollY = TRUE,
          columnDefs = list(
            list(visible = FALSE, targets = c(0,3:8,11)),
            list(width = '80px', targets = 9),
            list(width = '80px', targets = 10),
            list(className = 'dt-center', targets = 9)
          ),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
            "}"
          )
        )
      ) |>
        formatStyle(columns = names(temp_table_data),target = 'row',backgroundColor = styleEqual(unique(temp_table_data$row_color), unique(temp_table_data$row_color))) |>
        formatStyle(0, target= 'row', lineHeight='25%') |>
        formatStyle(columns = c(9,10),`text-align` = 'center')
    })
    
    filtered_temp = temp_data[Mean_Width() >= temp_data$Lower_Width & Mean_Width() <= temp_data$Upper_Width &
      (temp_data$Predicted_Prob > temp_data[[CritCol]] | is.na(temp_data[[CritCol]])),]
    
    filtered_STREAM_fishes(filtered_temp)
    
    output$filtered_fish = DT::renderDataTable(server = T, {
      datatable(
        filtered_temp[,2:3],
        rownames = F,
        selection = list(
          selected = NULL,
          target = "row",
          mode = "multiple"
        ),
        editable = F,
        options = list(
          dom='t',
          autoWidth = T,
          pageLength = 200,
          scrollY = T,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
            "}"
          )
        )
      ) |>
        formatStyle(0, target= 'row', lineHeight='25%')
    })
  })
  
  observeEvent(input$remove_fish, ignoreInit=T, {
    
    req(input$filtered_fish_rows_selected)
    
    new_filtered = filtered_STREAM_fishes()[-input$filtered_fish_rows_selected,]
    
    filtered_STREAM_fishes(new_filtered)
    
    output$filtered_fish = DT::renderDataTable(server = T, {
      datatable(
        new_filtered[,2:3],
        rownames = F,
        selection = list(
          selected = NULL,
          target = "row",
          mode = "multiple"
        ),
        editable = F,
        options = list(
          dom='t',
          autoWidth = T,
          pageLength = 200,
          scrollY = T,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
            "}"
          )
        )
      ) |>
        formatStyle(0, target= 'row', lineHeight='25%')
    })
  })
  
  observeEvent(input$add_fish, ignoreInit=T, {
    
    req(input$fish_assem_rows_selected)
    
    which_rows = STREAM_fishes()[input$fish_assem_rows_selected,]
    
    rows_to_add = which_rows %>%
      anti_join(filtered_STREAM_fishes(), by = c("Scientific Name" = "Scientific Name"))
    
    updated_filtered = bind_rows(filtered_STREAM_fishes(), rows_to_add)
    
    filtered_STREAM_fishes(updated_filtered)
    
    output$filtered_fish = DT::renderDataTable(server = T, {
      datatable(
        updated_filtered[,2:3],
        rownames = F,
        selection = list(
          selected = NULL,
          target = "row",
          mode = "multiple"
        ),
        editable = F,
        options = list(
          dom='t',
          autoWidth = T,
          pageLength = 200,
          scrollY = T,
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
            "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
            "}"
          )
        )
      ) |>
        formatStyle(0, target= 'row', lineHeight='25%')
    })
    
    proxy = dataTableProxy("fish_assem")
    selectRows(proxy, NULL)
  })
  
  observeEvent(input$assemblage_tabs, ignoreInit = T, {

    if (input$assemblage_tabs == "Community Biomass Estimation") {
      
      if (is.null(filtered_STREAM_fishes())) {
        return()
      } else {
        
        data = filtered_STREAM_fishes()
        
        data$`Biomass (kg)` = NA
        data$Count = NA
        
        output$fish_community = DT::renderDataTable(server = T, {
          datatable(
            data[,c(2:3,5:7,ncol(data)-1,ncol(data))],
            rownames = F,
            selection = list(
              selected = NULL,
              target = "row",
              mode = "multiple"
            ),
            editable = F,
            options = list(
              dom='t',
              autoWidth = T,
              pageLength = 200,
              scrollY = T,
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff','text-align': 'center'});",
                "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
                "}"
              )
            )
          ) |>
            formatStyle(0, target= 'row', lineHeight='25%') |>
            formatStyle(columns = c(3:ncol(data)),textAlign = 'center')
        })
      }
    }
  })
  
  observeEvent(input$fish_assem_rows_selected,  ignoreInit=T, {
    
    current_selection = input$fish_assem_rows_selected
    last_selected = setdiff(current_selection, previous_SHAP_selection())
    last_deselected = setdiff(previous_SHAP_selection(), current_selection)
    
    if (length(last_selected) > 0) {
      last_affected = last_selected
    } else if (length(last_deselected) > 0) {
      last_affected = last_deselected
    } else {
      last_affected = NULL
    }
    
    previous_SHAP_selection(current_selection)
    
    if (!is.null(last_affected)) {
      
      fish_name = STREAM_fishes()[last_affected, "Common Name"]
      
      output$SHAP_text = renderUI({
        HTML(paste("Model Information for ", "<b><i>", fish_name, "</i></b>"))
      })
      
      model_num = as.numeric(STREAM_fishes()[last_affected, "Model_ID"])
      
      if (!is.na(model_num) && !is.null(model_num)) {
        
        values = round(as.numeric(modelstats[modelstats$Model == model_num, c("WA_SHAP", "Slope_SHAP", "Elev_SHAP", "IWI_SHAP", "BMMI_SHAP")]), 3)
        
        dataframe = data.frame(Feature = c("Drainage Area", "Slope", "Elevation", "IWI", "BMMI"),`SHAP Value` = values, check.names = FALSE)
        
        output$shap_values = DT::renderDataTable(server = T, {
          datatable(
            dataframe,
            rownames = F,
            selection = 'none',
            editable = F,
            options = list(
              dom='t',
              autoWidth = T,
              pageLength = 5,
              scrollY = T,
              initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#073744', 'color': '#fff'});",
                "Shiny.setInputValue('tableRendered', 'data', {priority: 'event'});",
                "}"
              )
            )
          )
        })
      } else {
        output$shap_values = NULL
      }
    } else {
      output$shap_values = NULL
    }
  }, ignoreNULL = FALSE)

}
shinyApp(ui, server)