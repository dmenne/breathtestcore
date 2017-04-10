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
  dd = broom::augment(x, by = inc) 
  # Mark t50
  t50 = coef(x) %>%
    filter(parameter == "t50",  method == method_t50) %>% 
    select(-parameter)
  # Avoid ugly ggplot shading
  theme_set(theme_bw() + theme(panel.spacing = grid::unit(0,"lines")))
  if (length(unique(dd$group)) > 1) {
    p = ggplot(dd, aes(x = minute, y = fitted, color = group)) + geom_line(size = 1) +
      geom_point(data = x$data, aes(x = minute, y = pdr, color = group), size = 1) 
    if (nrow(t50) > 0 )    
      p = p + geom_vline(aes(xintercept = value, color = group),  t50) 
  } else {
    p = ggplot(dd, aes(x = minute, y = fitted)) + geom_line(size = 1) +
      geom_point(data = x$data, aes(x = minute, y = pdr), size = 1) 
    if (nrow(t50) > 0 )    
      geom_vline(aes(xintercept = value, color = "red" ),  t50) 
  } 
  p + facet_wrap(~patient_id) +
    scale_colour_brewer(type = "seq", palette = "Set1") + 
    ylab("pdr") +
    ggtitle(label = NULL, subtitle = paste("Half-emptying t50 by method", method_t50))
}


