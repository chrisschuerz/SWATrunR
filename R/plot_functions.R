#' Dotty for an objective criterion over all parameters
#'
#' @param parameter data.frame that provides all paramete sets used for simulation
#' @param objective Vector or data.frame providing all calculated objective criteria
#'
#' @importFrom dplyr bind_cols %>%
#' @importFrom ggplot2 aes facet_wrap geom_point ggplot theme_bw ylab
#' @importFrom tibble as_tibble
#' @importFrom tidyr gather
#' @importFrom purrr map map2 set_names
#'
#' @return Interactive visualization of observed (blue) and simulation
#' results (red). The red ribbon gives the upper and lower boundary for each
#' time step from the selected model runs.
#' @export
plot_dotty <- function(parameter, objective) {
  objective <- as_tibble(objective) %>%
    map(., ~as_tibble(.x)) %>%
    map(., ~set_names(.x, nm = "obj"))

  plot_data <- objective %>%
    map(., ~ bind_cols(param, .x)) %>%
    map(., ~gather(.x, key = "parameter", value = "par_value", -obj))

  gg_dotty <- map(plot_data, ~ ggplot(data = .x) +
                    geom_point(aes(x = par_value, y = obj)) +
                    facet_wrap(~parameter, scales = "free_x") +
                    theme_bw()) %>%
    map2(., names(.), ~.x + ylab(.y))

  return(gg_dotty)
}

#' Interactive timeseries plot of the simulation results filtered by a criterion
#'
#' @param sim Simulation results for the SWAT model runs
#' @param obs data.frame holding the observed data. First column must be of
#' type date. Second column must hold numeric observations for respective dates.
#' @param obj Vector giving the values of the objective calculated for the
#' individual model runs. Must be of same length as the number of simulations.
#' @param crit Criterion applied. Either ">" for model runs with objective
#' values greater than thres or "<" for smaller values to be selected.
#' @param thres Threshold value that is applied for selection.
#'
#' @import dplyr
#' @importFrom dygraphs dygraph dySeries dyRangeSelector dyOptions
#' @importFrom xts as.xts
#'
#' @return Interactive visualization of observed (blue) and simulation
#' results (red). The red ribbon gives the upper and lower boundary for each
#' time step from the selected model runs.
#' @export

plot_timeseries <- function(sim, obs, obj, crit, thres) {

  # if(!is_date(obs[1,1])){ stop("First column of obs must be of type
  #                              'Date'")}
  # if(!is.double(obs[1,2])){ stop("Second column of obs must be of type
  #                                'double'")}
  # colnames(obs) <- c("date", "obs")

  sim <-  as.data.frame(sim)

  obj_sel <- do.call(crit, list(obj, thres)) %>%
    which %>%
    .[] + 1
  if(crit == ">"){
    which_best <- which.max(obj)
  } else if(crit == "<"){
    which_best <- which.min(obj)
  }

  colnames(obs) <- c("date", "obs")

  sim_stat <- sim%>%
    mutate(best_sim = sim[,which_best + 1]) %>%
    select(obj_sel, best_sim) %>%
    mutate(lwr_bnd = apply(.[,1:ncol(.)], 1, function(x) min(x)),
           upr_bnd = apply(.[,1:ncol(.)], 1, function(x) max(x))) %>%
    select( best_sim, lwr_bnd, upr_bnd) %>%
    cbind.data.frame(date = sim$date, .)

  timeseries <- left_join(sim_stat, obs, by = "date") %>%
    select(-date) %>%
    as.xts(., order.by = sim_stat$date)

  dygraph(timeseries) %>%
    dySeries(c("lwr_bnd","best_sim","upr_bnd"),
             color = "#CC4F38", label = "sim", strokeWidth = 1.2) %>%
    dySeries("obs", label = "obs", color = "#36648B", strokeWidth = 1.2) %>%
    dyRangeSelector() %>%
    dyOptions(fillAlpha = 0.5)
  }
