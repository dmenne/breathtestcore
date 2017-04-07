#' A description of plotting
#' 
#' @description Plots 13C data and fits.
#' 
#' @title S3 plot method for breathtestfit
#' @param x object of class breathtestfit, as returned by nls_fit or nlme_fit
#' @param inc increment for fitted curve plot in minutes
#' @param ... other parameters passed to methods
#' @importFrom graphics plot

plot.breathtestfit = function(x, inc = 5, ...){
  # Make CRAN happy
  pdr = parameter = value = method = minute = fitted = group = NULL
  # use library(broom) for debugging of augment
  dd = augment(x, by = inc) 
  # Avoid ugly ggplot shading
  theme_set(theme_bw() + theme(panel.spacing = grid::unit(0,"lines")))
  if (length(unique(dd$group)) > 1) {
    p = ggplot(dd, aes(x = minute, y = fitted, color = group)) + geom_line() +
      geom_point(data = x$data, aes(x = minute, y = pdr, color = group)) 
  } else {
    p = ggplot(dd, aes(x = minute, y = fitted)) + geom_line() +
      geom_point(data = x$data, aes(x = minute, y = pdr)) 
  } 
  # Mark t50
  t50 = coef(x) %>% 
    filter(parameter == "t50") %>% 
    select(-parameter)
  p +  
    geom_vline(aes(xintercept = value, color = method), size = 1, t50) +
    facet_wrap(~patient_id) 
}

