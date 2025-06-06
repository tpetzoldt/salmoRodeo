#' Set up and compile a `rodeo` model.
#'
#' This function orchestrates the setup of a `rodeo` model by reading
#' various definition files (variables, parameters, functions, processes,
#' stoichiometry), applying replacements to process expressions,
#' generating forcing functions, and finally compiling the model.
#' @param base_path Path to the model files. Can be empty, if vars, pars,
#'   funs, pros, soi and repl contain full path and file names.
#' @param vars File name of the variables definition TSV file.
#' @param pars File name of the parameters definition TSV file.
#' @param funs File name of the functions definition TSV file.
#' @param pros File name of the processes definition TSV file.
#' @param stoi File name of the stoichiometry definition TSV file.
#' @param repl File name of the replacements definition TSV file.
#' @param datafile Path and file name of the input data TSV file used for forcings.
#' @param input_columns A character vector of column names from `datafile`
#'   to be used as input forcings.
#' @param sources File name to additional Fortran source files to be compiled
#'   with the model (e.g., `functions_extended.f95`).
#' @param phyto_groups An integer (2 or 3) indicating the number of
#'   phytoplankton groups, affecting how process and replacement files are read.
#' @return A `rodeo` model object, compiled and ready for simulation.
#'
#' @examples
#' \dontrun{
#' # This example requires dummy files and the 'rodeo' package
#' # and is skipped in automatic checks.
#' # Create dummy files
#' dir.create("model_files", showWarnings = FALSE)
#' cat("name\tmin\tmax\tunit\tdescription\n",
#'     "N\t0\tInf\tmmol/m3\tNitrogen\n", file = "model_files/vars.tsv")
#' cat("name\tdefault\tunit\tdescription\n",
#'     "KN\t0.1\tmmol/m3\tHalf-saturation constant for N\n", file = "model_files/pars.tsv")
#' cat("name\texpression\n",
#'     "f_N\tN/(KN+N)\n", file = "model_files/funs.tsv")
#' cat("name\texpression\ttwoPhytoGroups\tthreePhytoGroups\n",
#'     "uptake\tN*f_N\tN*f_N_2\tN*f_N_3\n", file = "model_files/pros.tsv")
#' cat("process\tN\n",
#'     "uptake\t-1\n", file = "model_files/stoi.tsv")
#' cat("pattern\treplacement\ttwoPhytoGroups\tthreePhytoGroups\n",
#'     "N\tNO3\tN_two\tN_three\n", file = "model_files/replacements.tsv")
#' cat("stagnation\tVE\n",
#'     "1\t10\n",
#'     "2\t20\n",
#'     file = "model_files/inputs.tsv")
#'
#' # Define a dummy function_extended.f95 (can be empty for this example)
#' cat("SUBROUTINE dummy_ext()\nEND SUBROUTINE\n", file = "model_files/functions_extended.f95")
#'
#' # Run model setup
#' model_obj <- model_setup(
#'   vars = "model_files/vars.tsv",
#'   pars = "model_files/pars.tsv",
#'   funs = "model_files/funs.tsv",
#'   pros = "model_files/pros.tsv",
#'   stoi = "model_files/stoi.tsv",
#'   repl = "model_files/replacements.tsv",
#'   datafile = "model_files/inputs.tsv",
#'   input_columns = c("stagnation", "VE"),
#'   sources = "model_files/functions_extended.f95",
#'   phyto_groups = 3
#' )
#'
#' # Check if model is compiled (it should be)
#' print(model_obj$is_compiled())
#'
#' # Clean up dummy files
#' unlink("model_files", recursive = TRUE)
#' }
#' @export
model_setup <- function(base_path = NULL,
                        vars = "vars.tsv",
                        pars = "pars.tsv",
                        funs = "funs.tsv",
                        pros = "pros.tsv",
                        stoi = "stoi.tsv",
                        repl = "replacements.tsv",
                        datafile = paste0(system.file("models", package="salmoRodeo"), "/inputs.tsv"),
                        input_columns = c("stagnation", "VE", "ZMIX", "QIN",
                                          "IIN", "TE", "PIN", "NIN", "POMIN"),
                        sources = "functions_extended.f95",
                        phyto_groups = 3) {

  if (is.null(base_path)) base_path <- paste0(system.file("models", package="salmoRodeo"), "/")

  ## create a rodeo model object
  model <- rodeo::rodeo$new(
    vars = rd(paste0(base_path, vars)),
    pars = rd(paste0(base_path, pars)),
    funs = rd(paste0(base_path, funs)),
    pros = replace_abbreviations(read_pros(paste0(base_path, pros), phyto_groups),
             read_replacements(paste0(base_path, repl), phyto_groups), npass = 5),
    stoi = as.matrix(
      rd(paste0(base_path, stoi), row.names = "process")
    ), asMatrix = TRUE, dim = 1)

  ## set default values of parameters
  model$setPars(setNames(model$getParsTable()$default, model$getParsTable()$name))

  ## create forcings file
  # Note: generate_forcings writes to "forc.f95" directly due to rodeo's internal workings
  # Note2: datafile can be on a different path
  code <- generate_forcings(datafile = datafile, columns = input_columns)
  write(x = code, file = paste0(base_path, "forc.f95")) # Ensure this file is handled correctly by rodeo

  ## run fortran compiler
  # The 'sources' argument adds custom Fortran code to the compilation process
  model$compile(sources = paste0(base_path, sources), fortran = TRUE)
  model
}
