#' Scaling Numeric Data
#'
#' `step_winsorize` creates a *specification* of a recipe
#'  step that will normalize numeric data to have a standard
#'  deviation of one and a mean of zero, however leaving default value out of preprocessing.
#'
#' @inheritParams step_center
#' @param ... One or more selector functions to choose which
#'  variables are affected by the step. See [selections()]
#'  for more details. For the `tidy` method, these are not
#'  currently used.
#' @param role Not used by this step since no new variables are
#'  created.
#' @param sds A named numeric vector of standard deviations This
#'  is `NULL` until computed by [prep.recipe()].
#' @param na_rm A logical value indicating whether `NA`
#'  values should be removed when computing the standard deviation.
#' @return An updated version of `recipe` with the new step
#'  added to the sequence of existing steps (if any). For the
#'  `tidy` method, a tibble with columns `terms` (the
#'  selectors or variables selected) and `value` (the
#'  standard deviations).
#' @keywords datagen
#' @concept preprocessing normalization_methods
#' @export
#' @details Scaling data means that the standard deviation of a
#'  variable is divided out of the data. `step_scale_except_zero` estimates
#'  the variable standard deviations from the data used in the
#'  `training` argument of `prep.recipe`.
#'  `bake.recipe` then applies the scaling to new data sets
#'  using these standard deviations.
#' @examples
#' data(biomass)
#'
#' biomass_tr <- biomass[biomass$dataset == "Training",]
#' biomass_te <- biomass[biomass$dataset == "Testing",]
#'
#' rec <- recipe(HHV ~ carbon + hydrogen + oxygen + nitrogen + sulfur,
#'               data = biomass_tr)
#'
#' scaled_trans <- rec %>%
#'   step_scale(carbon, hydrogen)
#'
#' scaled_obj <- prep(scaled_trans, training = biomass_tr)
#'
#' transformed_te <- bake(scaled_obj, biomass_te)
#'
#' biomass_te[1:10, names(transformed_te)]
#' transformed_te
#' tidy(scaled_trans, number = 1)
#' tidy(scaled_obj, number = 1)
#'
step_winsorize <- function(
  recipe, ...,
  role = NA,
  trained = FALSE,
  cut = 0.02,
  columns = NULL,
  skip = FALSE,
  id = rand_id("scale_except_zero")
) {

  ## The variable selectors are not immediately evaluated by using
  ##  the `quos` function in `rlang`. `ellipse_check` captures the
  ##  values and also checks to make sure that they are not empty.
  terms <- ellipse_check(...)

  add_step(
    recipe,
    step_winsorize_new(
      terms = terms,
      trained = trained,
      role = role,
      cut = cut,
      columns = columns,
      skip = skip,
      id = id
    )
  )
}

#' @export
winsorize = function(x, cut = 0.01){
  cut_point_top <- quantile(x, 1 - cut, na.rm = T)
  cut_point_bottom <- quantile(x, cut, na.rm = T)
  i = which(x >= cut_point_top)
  x[i] = cut_point_top
  j = which(x <= cut_point_bottom)
  x[j] = cut_point_bottom
  return(x)
}


step_winsorize_new <-
  function(terms, role, trained, cut, skip, columns, id) {
    step(
      subclass = "winsorize",
      terms = terms,
      role = role,
      trained = trained,
      cut = cut,
      columns = columns,
      skip = skip,
      id = id
    )
  }

prep.step_winsorize <- function(x, training, info = NULL, ...) {
  col_names <- terms_select(terms = x$terms, info = info)
  ## You can add error trapping for non-numeric data here and so on. See the
  ## `check_type` function to do this for basic types.

  ## Use the constructor function to return the updated object.
  ## Note that `trained` is set to TRUE

  step_winsorize_new(
    terms = x$terms,
    trained = TRUE,
    role = x$role,
    cut = x$cut,
    skip = x$skip,
    columns = col_names,
    id = x$id
  )
}

bake.step_winsorize <- function(object, new_data, ...) {

  col_names <- object$columns



  for (i in seq_along(col_names)) {
    col <- new_data[[ col_names[i] ]]
    new_data[, col_names[[i]]] <- winsorize(col, object$cut)

  }
  ## Always convert to tibbles on the way out
  as_tibble(new_data)
}

print.step_winsorize <-
  function(x, width = max(20, options()$width - 30), ...){
    cat("Winsorizing the data", sep = " ")
    printer(x$columns, x$terms, x$trained, width = width)
    invisible(x)
  }

#' @rdname step_winsorize
#' @param x A `step_winsorize` object.
#' @export
#'
tidy.step_winsorize <- function(x, ...){
  if(is_trained(x)) {
    res <- tibble(terms = x$columns)
  } else {
    res <- tibble(terms = sel2char(x$terms))
  }
  res
}
