#####################################################################
#####################################################################
source("EXE_plot_first_contact_corner.R")
#####################################################################
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
  "--team_name",
  type = "character",
  action = "store",
  default = "Feyenoord"
)

args <- parse_args(parser)

#####################################################################
# create the ./outputs directory
if (!dir.exists(args$output)) {
  dir.create(args$output)
}

## The overall function
fun_plot_first_contact_corner(
  input_team_name = args$team_name,
  output_path = args$output
)


#####################################################################
