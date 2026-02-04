#S4 methods for hmde_data_template

# Class definition
setClass(
  Class = "hmde_data_template",
  representation = representation(
    model_name = "character", # Name of model DE
    model_level = "character", # Single or multi-individual
    obs_data = "list", # List of input data for model
    prior_pars = "list" # List of prior parameters for model
  ),
  prototype = prototype(
    model_name = NA_character_,
    model_level = NA_character_,
    obs_data = NULL,
    prior_pars = NULL
  )
)

# Class constructor
