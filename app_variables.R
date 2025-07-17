Version = "1.0.0"

fish_props = data.frame(read.csv("fish_properties.csv"))
species_models = data.frame(read.csv("species_models.csv"))
modelstats = data.frame(read.csv("modelstats.csv"))
HUC_data = read.csv("HUC_table.csv", colClasses = c("character", "character", "NULL", "character", "character", "character"))
HUC_boundaries = st_read("huc8.geojson", quiet = TRUE)

boundaries_added = reactiveVal(FALSE)
fish_dist_showing = reactiveVal(FALSE)
time_delay = reactiveVal(2500)

fish_tribes = list(
  "Darters" = c("Etheostoma", "Logperch", "Other Darter"),
  "Minnows" = c("Chub", "Dace", "Shiner", "Stoneroller", "Other Minnow"),
  "Suckers" = c("Catostomus", "Redhorse", "Other Sucker"),
  "Other - Primary" = c(
    "Black Bass",
    "Carp",
    "Catfish",
    "Cavefish",
    "Cichlid",
    "Gar",
    "Goby",
    "Lamprey",
    "Livebearer",
    "Madtom",
    "Mudminnow",
    "Perch",
    "Pike",
    "Pipefish",
    "Pupfish",
    "Pygmy Sunfish",
    "Salmon/Trout",
    "Sculpin",
    "Shad/Herring",
    "Silverside",
    "Smelt",
    "Snakehead",
    "Splitfin",
    "Stickleback",
    "Sturgeon",
    "Sunfish",
    "Topminnow",
    "Trout-Perch",
    "White Bass",
    "Whitefish"),
    "Oddballs" = c("Anchovy","Bowfin","Burbot","Cod","Drum","Eel","Flounder","Knifefish","Loach","Mullet","Paddlefish","Skipjack","Stingray","Surfperch","Tetra"))

systems = c("Cave",
            "Spring",
            "Headwater",
            "Creek",
            "Small River",
            "Medium River",
            "Large River",
            "Lake/Impoundment",
            "Swamp/Marsh/Bayou",
            "Coastal/Ocean")

map_clicks = reactiveValues(points = data.frame())
name_srch_results = reactiveVal()
HUC_srch_results = reactiveVal()
explore_results = reactiveVal()
previous_zoom = reactiveVal(4)
current_zoom = reactiveVal(4)
selected_polygon_ID = reactiveVal(NULL)
previous_polygon_ID = reactiveVal(NULL)
previous_matches = reactiveVal()
previous_selection = reactiveVal()
previous_SHAP_selection = reactiveVal()
selected_huc8 = reactiveVal(NULL)
selected_name = reactiveVal(NULL)
A = reactiveVal()
B = reactiveVal()
units = reactiveVal("metric")
fishy_length = reactiveVal(10)
fishy_weight = reactiveVal(6)
HUC_fishes = reactiveVal()
STREAM_fishes = reactiveVal()
EST_FISH_COMM = reactiveVal()
filtered_STREAM_fishes = reactiveVal()
display = reactiveVal("none")

comm_numbers = reactiveVal(5000)
comm_biomass = reactiveVal(50)
rarity_value = reactiveVal(0.8)

stream_lat = reactiveVal()
stream_lon = reactiveVal()
stream_ID = reactiveVal()
stream_HUC = reactiveVal()
stream_NAME = reactiveVal()

WA = reactiveVal()
BMMI = reactiveVal()
IWI = reactiveVal()
Elev = reactiveVal()
Slope = reactiveVal()
Mean_Width = reactiveVal()

stream_coords = reactiveValues(lat = NULL, lng = NULL)