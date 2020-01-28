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
#' @param line_size optional line width; can improve look for printouts
#' @param point_size optional point size; determined dynamically when NULL
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
#' @import dplyr
#' @importFrom ggfittext geom_fit_text
#' @export 
plot.breathtestfit = function(x, inc = 5, method_t50 = "maes_ghoos", 
                              line_size = 1, point_size = NULL, ...){
  # Make CRAN happy
  pdr = parameter = value = method = minute = fitted = group = fac = NULL
  t50 = tlag = y_index = annotate_g = xmin = xmax = ymin = ymax = NULL
  pat_group = patient_id = s = NULL
  # Plot data only if there are no coefficients
  has_fit = !is.null(coef(x))
  if (has_fit) {
    has_repeats = (max(with(
      coef(x)  %>% 
      dplyr::filter(parameter == "t50", method == "maes_ghoos") , 
      table(patient_id)))  ) > 1
  } else has_repeats = FALSE
  n_groups = length(unique(x$data$group))
  has_groups =  n_groups > 1

  sep = max(x$data$pdr)/10 # separation between annotations
  if (has_fit) {
    dd = broom::augment(x, by = inc) 
    # Mark t50
    ann = coef(x) %>%
      dplyr::filter(parameter %in% c("t50", "tlag"),  method == method_t50) %>% 
      tidyr::spread(parameter, value) %>% 
      dplyr::mutate(
        annotate_g = paste0(group, ": t50 ", round(t50), 
                          " min, tlag ", round(tlag), " min" ),
        annotate = paste0("t50 ", round(t50), 
                            " min,  tlag ", round(tlag), " min"),
        xmin = 0,
        xmax = max(c(x$data$minute, t50)),
        fac = as.integer(as.factor(group)),
        y_index = if_else(rep(has_repeats, n()), fac, 1L), 
        ymin = (y_index - 1)*sep,
        ymax = (y_index )*sep
      ) %>%   
      dplyr::select(-method,  -y_index, -fac)
  }
  # Compute point size dynamically
  if (! is.null(point_size)) {
    size = point_size 
  }
  else {
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
  } 
  # When too dense, make it transparentish
  alpha = max(min(size, 1), 0.6)
  
  # Avoid ugly ggplot shading
  theme_set(theme_bw() + theme(panel.spacing = grid::unit(0,"lines")))
  if (has_groups) {
    # With grouping
    p = ggplot(x$data, aes(x = minute, y = pdr, color = group)) + 
      geom_point(size = size, alpha = alpha)
    if (has_fit)  {
      p = p + geom_line(aes(x = minute, y = fitted, color = group), 
                        size = line_size, data = dd, show.legend = FALSE) + 
        geom_vline(aes(xintercept = t50, color = group),  data = ann) +
        ggfittext::geom_fit_text(
          aes(xmin = xmin, xmax = xmax, 
              ymin = ymin, ymax = ymax,
              label = annotate_g,
              color = group), 
          show.legend = FALSE,
          inherit.aes = FALSE, 
          data = ann, min.size = 4,
          grow = TRUE)  +
        scale_x_continuous(expand = c(0.02, 0.02, 0.02, 0.02))
    }  
  } else {
    # without grouping
    p = ggplot(data = x$data, aes(x = minute, y = pdr)) +
                 geom_point(size = size, alpha = alpha)
    if (has_fit)    {
      p = p + 
        geom_line(aes(x = minute, y = fitted), size = line_size, data = dd, 
                  show.legend = FALSE)  +
        theme(legend.position = "none")  +
        geom_vline(aes(xintercept = t50, color = "red" ), data = ann) +
        ggfittext::geom_fit_text(
          aes(xmin = xmin, xmax = xmax, 
              ymin = ymin, ymax = ymax,
              label = annotate), 
              data = ann, min.size = 4, 
              inherit.aes = FALSE,
              grow = TRUE) +
        scale_x_continuous(expand = c(0.02, 0.02, 0.02, 0.02))
      }

  } 
  if (is(x, "breathtestnlsfit"))
    fit = "Individual curve fit (nls)." else
  if (is(x, "breathtestnlmefit"))
    fit = "Population fit (nlme)." else
  if (is(x, "breathteststanfit"))
    fit = "Bayesian fit (Stan)." else
  fit = ""
  subtitle = ifelse(has_fit, 
        paste(fit, "Half-emptying t50 by method", method_t50),
        paste("No fit ", comment(x$data)))
  
  p = p + facet_wrap(~patient_id) +
    ylab("pdr") +
    ggtitle(label = NULL, subtitle = subtitle)
  if (ngroups <= 5)
    p = p + scale_colour_brewer(type = "seq", palette = "Set1")  
  p
}

