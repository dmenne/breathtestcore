#' @title S3 plot method for breathtestfit
#' 
#' @description Plots 13C data and fits.
#' 
#' @param x object of class \code{breathtestfit}, as returned by \code{\link{nls_fit}}, 
#' \code{\link{nlme_fit}}, \code{\link{null_fit}} or \code{stan_fit}; stan_fit is
#' in package \code{breathteststan},
#' @param inc Increment for fitted curve plot in minutes
#' @param method_t50 Method for t50: "\code{maes_ghoos}", "\code{bluck_coward}" or 
#' "\code{maes_ghoos_scintigraphy}"
#' @param ... other parameters passed to methods. Not used
#' @examples
#' data = list(
#'   A = simulate_breathtest_data(n_records = 6, seed = 100),
#'   B = simulate_breathtest_data(n_records = 4, seed = 187) 
#' )
#' # cleanup_data combines the list into a data frame
#' x = nls_fit(cleanup_data(data))
#' plot(x)
#' @importFrom stats quantile
#' @importFrom tidyr spread
#' @importFrom ggfittext geom_fit_text
#' @export 
plot.breathtestfit = function(x, inc = 5, method_t50 = "maes_ghoos", ...){
  # Make CRAN happy
  pdr = parameter = value = method = minute = fitted = group = NULL
  pat_group = patient_id = s = NULL
  # Plot data only if there are no coefficients
  has_fit = !is.null(coef(x))
  sep = max(x$data$pdr)/15 # separation between annotations
  if (has_fit) {
    dd = broom::augment(x, by = inc) 
    # Mark t50
    ann = coef(x) %>%
      filter(parameter %in% c("t50", "tlag"),  method == method_t50) %>% 
      tidyr::spread(parameter, value) %>% 
      mutate(
        annotate_g = paste0("group = ", group, ", t50 = ", round(t50), 
                          " min\ntlag = ", round(tlag), " min"),
        annotate = paste0("t50 = ", round(t50), 
                            " min\ntlag = ", round(tlag), " min"),
        xmin = 0,
        xmax = max(x$data$minute),
        y_index = as.integer(as.factor(group)), 
        ymin = (y_index - 1)*sep,
        ymax = y_index*sep
      ) %>% 
      select(-method,  -y_index)
  }
  # Compute point size dynamically
  size = x$data %>%
    mutate(
      pat_group = paste(patient_id, group, sep = "/")
    ) %>% 
    group_by(pat_group) %>%
    summarize(
      s = max(min(mean(diff(minute))/12, 2), 0.1)
    ) %>%
    ungroup() %>% 
    summarize(
      s = quantile(s, 0.1)[1]
    ) %>% 
    unlist()
  # When too dense, make it transparentish
  alpha = max(min(size, 1), 0.6)
  
  # Avoid ugly ggplot shading
  theme_set(theme_bw() + theme(panel.spacing = grid::unit(0,"lines")))
  has_groups = length(unique(x$data$group)) > 1
  if (has_groups) {
    # With grouping
    p = ggplot(x$data, aes(x = minute, y = pdr, color = group)) + 
      geom_point(size = size, alpha = alpha)
    if (has_fit)  {
      p = p + geom_line(aes(x = minute, y = fitted, color = group), data = dd) + 
        geom_vline(aes(xintercept = t50, color = group),  data = ann) +
        geom_fit_text(aes(xmin = 0, xmax = 200, ymin = 0, ymax = 10,
                          label = annotate), 
                      data = ann, min.size = 8,
                      inherit.aes = FALSE, reflow = TRUE,
                      grow = TRUE) 
    }  
  } else {
    # without grouping
    p = ggplot(data = x$data, aes(x = minute, y = pdr)) +
                 geom_point(size = size, alpha = alpha)
    if (has_fit)    {
      p = p + 
        geom_line(aes(x = minute, y = fitted), data = dd)  +
        theme(legend.position = "none")  +
        geom_vline(aes(xintercept = t50, color = "red" ), data = ann) +
        geom_fit_text(aes(xmin = xmin, xmax = xmax, ymin = xmin, ymax = ymax,
                      label = annotate), 
                      data = ann, min.size = 8, reflow = TRUE,
                      inherit.aes = FALSE,
                      grow = TRUE) 
      }

  } 
  if (is(x, "breathtestnlsfit"))
    fit = "Single curve fit (nls)." else
  if (is(x, "breathtestnlmefit"))
    fit = "Population fit (nlme)." else
  if (is(x, "breathteststanfit"))
    fit = "Bayesian fit (Stan)." else
  fit = ""
  subtitle = ifelse(has_fit, 
        paste(fit, "Half-emptying t50 by method", method_t50),
        paste("No fit ", comment(x$data)))
  
  p + facet_wrap(~patient_id) +
    scale_colour_brewer(type = "seq", palette = "Set1") + 
    ylab("pdr") +
    ggtitle(label = NULL, subtitle = subtitle)
}

if (FALSE){
library(breathtestcore)
library(dplyr)
library(ggfittext)
data = list(
  A = simulate_breathtest_data(n_records = 2, seed = 100)
)
# cleanup_data combines the list into a data frame
inc = 5
method_t50 = "maes_ghoos"
x = nls_fit(cleanup_data(data))

plot(x)
}