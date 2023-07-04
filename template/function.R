# Function definitions ----------------------------------------------------
sample_lhs <- function(par, n) {
  n_par <- ncol(par)

  randomLHS(n = n, k = n_par) %>% # Perform sampling
    as_tibble(., .name_repair = 'minimal') %>% # Convert to a tibble
    set_names(names(par)) %>% # Assign the parameter names with purrr
    map2_df(., par, ~ (.x * (.y[2] - .y[1]) + .y[1])) # Scale parameter ranges
}


read_crop_yld_aa <- function(file_path) {
  read_lines(file_path, skip = 2, lazy = FALSE) %>%
    str_trim(.) %>%
    str_split(., '[:space:]+', simplify = T) %>%
    .[,c(3,6)] %>%
    as_tibble(., .name_repair = 'minimal') %>%
    set_names(c('crop', 'yield')) %>%
    mutate(yield = as.numeric(yield))
}

plot_dotty <- function(par, var, y_label = 'y', n_col = 3, y_lim = NULL) {
  dotty_tbl <- par %>%
    mutate(var = var) %>%
    pivot_longer(., cols = -var, names_to = 'parameter')

  gg <- ggplot(data = dotty_tbl) +
    geom_point(aes(x = value, y = var)) +
    facet_wrap(. ~ parameter, ncol = n_col, scales = "free_x") +
    labs(x = 'Change of parameter value', y = y_label) +
    theme_bw()

  if (!is.null(y_lim)) {
    gg <- gg + ylim(y_lim)
  }

  return(gg)
}

get_par_values <- function(rlist, rname){
  rlist$simulation[[rname]] %>%
    pivot_longer(!date, names_to = "run", values_to = rname) %>%
    mutate(run = as.numeric(str_replace(run, "run_", ""))) %>%
    left_join(rlist$parameter$values %>%
                mutate(run = as.numeric(rownames(.))), by = c('run')) %>%
    select(-any_of(c("run", "date"))) %>%
    pivot_longer(!c(rname), names_to = "Parameter", values_to = "Values") %>%
    group_by(Parameter, Values) %>%
    summarise_all(mean)
}

##Function for a single parameter
plot_dot <- function(df, y_value, facet, obs_df = NULL){
  gg <- ggplot(df, aes(x = Values, y = .data[[y_value]]))+
    geom_point()+
    facet_wrap(~.data[[facet]], scales = "free")+
    theme_gray()+
    theme(axis.text.x = element_text(angle = 30, hjust=1))
  if(!is.null(obs_df)){
    gg <- gg +
      geom_hline(data = obs_df, aes(yintercept = value), color = "red", linetype = "dashed")+
      labs(caption = "Red dashed line presents observed values.")
  }
  return(gg)
}


##Function to plot all parameters vs something
plot_pars <- function(df, y_value, facet, col_group = NULL, obs_df = NULL){
  if(!is.null(col_group)){
    gr <- unique(df[[col_group]])
    if(!is.null(obs_df)){
      fig_list <- map(gr, ~plot_dot(df[df[[col_group]] == .x,], y_value, facet,
                                    obs_df[obs_df$crop == .x,])) %>%
        set_names(gr)
    } else {
      fig_list <- map(gr, ~plot_dot(df[df[[col_group]] == .x,], y_value, facet)) %>%
        set_names(gr)
    }
  } else {
    if(!is.null(obs_df)){
      fig_list <- list(fig = plot_dot(df[df$.data[[col_group]] == .x,], y_value, facet))
    } else {
      fig_list <- list(fig = plot_dot(df[df$.data[[col_group]] == .x,], y_value, facet, obs_df))
    }
  }
  return(fig_list)
}
