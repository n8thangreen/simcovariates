
#' @noRd
#' Helper function to assign default names to unnamed covariate definitions
#'
assign_default_covariate_names <- function(covariate_defns) {

  covariate_names_defined <- names(covariate_defns)

  # Check if the list is named; if not, generate default names
  if (is.null(covariate_names_defined) || any(covariate_names_defined == "")) {
    message("`covariate_defns` is not fully named. Generating default covariate
            names (e.g., 'Cont_1', 'Bin_1', 'Cat_1').")

    generated_names <- character(length(covariate_defns))
    cont_counter <- 1
    bin_counter <- 1
    cat_counter <- 1

    for (i in seq_along(covariate_defns)) {
      cov_def <- covariate_defns[[i]]
      cov_type_obj <- cov_def$type

      if (inherits(cov_type_obj, "cts_var_type")) {
        generated_names[i] <- paste0("Cont_", cont_counter)
        cont_counter <- cont_counter + 1
      } else if (inherits(cov_type_obj, "bin_var_type")) {
        generated_names[i] <- paste0("Bin_", bin_counter)
        bin_counter <- bin_counter + 1
      } else {
        stop(paste("Unsupported covariate type object found in unnamed list at position ", i, ".", sep=""))
      }
    }
    names(covariate_defns) <- generated_names # Assign the generated names
  }

  return(covariate_defns)
}
