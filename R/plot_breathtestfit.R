#' A description of plotting
#' 
#' @description Plots 13C data and fits.
#' 
#' @title S3 plot method for breathtestfit
#' @param x object of class breathtestfit, as returned by nls_fit or nlme_fit
#' @param inc increment for fitted curve plot in minutes
#' @param method_t50 method for t50: \code{maes_ghoos}, \code{bluck_coward} or 
#' \code{maes_ghoos_scint}
#' @param ... other parameters passed to methods
#' @examples
#' data = list(
#'   A = simulate_breathtest_data(n_records = 6, seed = 100)$data,
#'   B = simulate_breathtest_data(n_records = 4, seed = 187)$data 
#' )
#' # cleanup_data combines the list into a data frame
#' x = nls_fit(cleanup_data(data))
#' plot(x)
#' @export 
plot.breathtestfit = function(x, inc = 5, method_t50 = "maes_ghoos", ...){
  # Make CRAN happy
  pdr = parameter = value = method = minute = fitted = group = NULL
  # Only plot data if there are no coefficients
  has_fit = !is.null(coef(x))
  if (has_fit) {
    dd = broom::augment(x, by = inc) 
    # Mark t50
    t50 = coef(x) %>%
      filter(parameter == "t50",  method == method_t50) %>% 
      select(-parameter)
  }
  # Avoid ugly ggplot shading
  theme_set(theme_bw() + theme(panel.spacing = grid::unit(0,"lines")))
  if (length(unique(x$data$group)) > 1) {
    # With grouping
    p = ggplot(x$data, aes(x = minute, y = pdr, color = group)) + 
      geom_point(size = 1)
    if (has_fit)  {
      p = p + geom_line(aes(x = minute, y = fitted, color = group), data = dd) + 
        geom_vline(aes(xintercept = value, color = group),  t50) 
    }  
  } else {
    # without grouping
    p = ggplot(data = x$data, aes(x = minute, y = pdr)) +
                 geom_point(size = 1)
    if (has_fit)    {
      p = p + 
        geom_line(aes(x = minute, y = fitted), data = dd)  +
        geom_vline(aes(xintercept = value, color = "red" ),  t50) +
        theme(legend.position = "none")
        
    }
  } 
  subtitle = ifelse(has_fit, 
        paste("Half-emptying t50 by method", method_t50),
        paste0("No successful fit"))
  p + facet_wrap(~patient_id) +
    scale_colour_brewer(type = "seq", palette = "Set1") + 
    ylab("pdr") +
    ggtitle(label = NULL, subtitle = subtitle)
}


