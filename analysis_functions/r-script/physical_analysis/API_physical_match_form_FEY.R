source("EXE_physical_match_form_FEY.R")
parser <- OptionParser()

parser <- add_option(
  parser,
  "--output",
  type = "character",
  action = "store",
  default = "./outputs"
)

parser <- add_option(
  parser,
  "--benchmark_file",
  type = "character",
  action = "store",
  default = "data/myfile.csv"
)

parser <- add_option(
  parser,
  "--game_data_file",
  type = "character",
  action = "store",
  default = "data/myfile.csv"
)

parser <- add_option(
  parser,
  "--team_name",
  type = "character",
  action = "store",
  default = "Feyenoord"
)

parser <- add_option(
  parser,
  "--competition_name",
  type = "character",
  action = "store",
  default = "Eredivisie"
)

parser <- add_option(
  parser,
  "--gata_data",
  type = "character",
  action = "store",
  default = "Feyenoord"
)

parser <- add_option(
  parser,
  "--chyron_hego_data",
  type = "character",
  action = "store",
  default = "Feyenoord"
)

parser <- add_option(
  parser,
  "--ch_competition_data",
  type = "character",
  action = "store",
  default = "Feyenoord"
)
args <- parse_args(parser)

####################
# create the ./outputs directory
if (!dir.exists(args$output)) {
  dir.create(args$output)
}

## The container script
fun_physical_match_form_FEY(
  benchmark_path = args$benchmark_file,
  game_data_path = args$game_data_file, # "/Users/folkert.boer@feyenoord.nl/Library/CloudStorage/OneDrive-FeyenoordRotterdam/Medical & Performance Feyenoord 1/Performance/Data/Match Volumes.xlsx",
  gata_path = args$gata_data, # "/Users/folkert.boer@feyenoord.nl/Documents/Feyenoord/SportsDynamic/Raw Data/",
  chyron_hego_path = args$chyron_hego_data, # "/Users/folkert.boer@feyenoord.nl/Dropbox/ChyronHego data/2023-2024/Physical data/physical_data_Eredivisie_2324.RData",
  ch_competition_data_path = args$ch_competition_data, # "/Users/folkert.boer@feyenoord.nl/Dropbox/ChyronHego data/Data Bases/CH_competition_data_2324.RData",
  competition_name = args$competition_name, team_name = args$team_name,
  output_path = args$output
)

###################
