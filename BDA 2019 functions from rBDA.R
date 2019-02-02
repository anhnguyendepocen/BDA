# Functions in the rBDA package #

# There is always a chance that due to system updates and dependency changes, parts of the rBDA package might not be working for you. But never fear! The code for each function is available here.
  # All you need to do is run this code. The functions will then appear in the 'Global Environment' (on the right), under 'Functions'. Problem solved.

# Code for:
  # error_bars() # week 3
  # cor_fun() # week 4
  # cor_scatter() # week 4
  # cor_plot() # week 4

# error_bars()
error_bars = function (df, s = NULL, draw = "both") 
{
  library(ggplot2)
  library(dplyr)
  if (!missing(s)) {
    if (!is.numeric(s)) 
      stop("'s' must be a numeric vector")
    if (length(s) < 3) 
      stop("'s' must be at least 3 values long")
  }
  if (class(df) != "data.frame") {
    stop("df must be a dataframe object and must have only two columns (x,y). If your input looks like a dataframe, check it by using class() and as.data.frame() functions ")
  }
  df = as.data.frame(df)
  if (ncol(df) != 2) {
    stop("df must have only two columns: (x,y)")
  }
  app = apply(df, 2, function(x) any(is.na(x) | is.infinite(x)))
  if (app[1] == TRUE) {
    stop("the data frame contains either Infinite or NA values. These must be removed.")
  }
  if (app[2] == TRUE) {
    stop("the data frame contains either Infinite or NA values. These must be removed.")
  }
  rm(app)
  if (draw == "standard error") {
    cont = 1
  }
  else if (draw == "standard deviation") {
    cont = 1
  }
  else if (draw == "both") {
    cont = 1
  }
  else {
    cont = 0
  }
  if (cont == 0) {
    stop("'draw can only take the values 'standard error', 'standard deviation', or 'both'.")
  }
  x = df[, 1]
  if (missing(s)) {
    a = min(floor(x))
    b = max(ceiling(x))
    c = (b - a)/10
    d = seq(a, b, c)
    s = d[2:length(d)]
  }
  df.copy = df
  colnames(df.copy) = c("x", "y")
  df.copy$bin_x = cut(x, breaks = c(-Inf, s, Inf), labels = c(s, 
                                                              Inf))
  df.copy$bin_x <- as.numeric(as.character(df.copy$bin_x))
  df.summary <- df.copy %>% group_by(bin_x) %>% summarize(xmean = mean(x), 
                                                          ymin = min(y), ymax = max(y), ymean = mean(y), ysd = sd(y), 
                                                          ymean_plus_sd = mean(y) + sd(y), ymean_minus_sd = mean(y) - 
                                                            sd(y), yse = sd(y)/sqrt(length(y)), ymean_plus_se = mean(y) + 
                                                            sd(y)/sqrt(length(y)), ymean_minus_se = mean(y) - 
                                                            sd(y)/sqrt(length(y)))
  df.summary$bin_x <- as.numeric(as.character(df.summary$bin_x))
  df.summary$num = dplyr::count(df.copy, bin_x)$n
  if (draw == "both") {
    errorbars_plot = ggplot(df.summary, aes(bin_x, ymean)) + 
      geom_point(size = 2) + geom_errorbar(aes(ymin = ymean_minus_sd, 
                                               ymax = ymean_plus_sd, width = 0.75, color = "Standard Deviation")) + 
      geom_errorbar(aes(ymin = ymean_minus_se, ymax = ymean_plus_se, 
                        width = 0.75, color = "Standard Error")) + labs(x = "x bin", 
                                                                        y = "y values") + ggtitle("Plot of error bars") + 
      scale_colour_discrete(name = "Measures") + theme_bw()
  }
  if (draw == "standard error") {
    errorbars_plot = ggplot(df.summary, aes(bin_x, ymean)) + 
      geom_point(size = 2) + geom_errorbar(aes(ymin = ymean_minus_se, 
                                               ymax = ymean_plus_se, width = 0.75, color = "Standard Error")) + 
      labs(x = "x bin", y = "y values") + ggtitle("Plot of error bars") + 
      scale_colour_discrete(name = "Measures") + theme_bw()
  }
  if (draw == "standard deviation") {
    errorbars_plot = ggplot(df.summary, aes(bin_x, ymean)) + 
      geom_point(size = 2) + geom_errorbar(aes(ymin = ymean_minus_sd, 
                                               ymax = ymean_plus_sd, width = 0.75, color = "Standard Deviation")) + 
      labs(x = "x bin", y = "y values") + ggtitle("Plot of error bars") + 
      scale_colour_discrete(name = "Measures") + theme_bw()
  }
  error_bars.out = structure(list(plot = errorbars_plot, df.summary = df.summary, 
                                  df = df.copy), class = "error_bars.object")
  return(error_bars.out)
}


# cor_fun()
cor_fun = function (x, y = NULL, lag.max = 200, wrap = F, level = "sample") 
{
  if (missing(y)) {
    y = x
    type = "ACF"
  }
  else {
    type = "CCF"
  }
  series_x = deparse(substitute(x))
  series_y = deparse(substitute(y))
  if (!is.numeric(x) || !is.numeric(y)) 
    stop("both 'x' and 'y' must be numeric")
  if (length(x) != length(y)) 
    stop("'x' and 'y' must be of equal lengths")
  if (length(x) < 15) 
    stop("there must be at least 15 values in the vectors passed to cor_fun")
  if (level != "population" && level != "sample") 
    stop("'level' can only take either 'population' or 'sample'. Please note that by default it takes 'sample'")
  x = as.matrix(x)
  y = as.matrix(y)
  vector_length = as.integer(nrow(x))
  nser <- as.integer(ncol(x))
  if (is.na(vector_length) || is.na(nser)) 
    stop("the length of the vectors must be greater than 1")
  lag.max = as.integer(min(lag.max, (vector_length - 20), (vector_length/2)))
  if (is.na(lag.max) || lag.max < 4) 
    stop("'lag.max' must be at least 5.")
  if (level == "sample") {
    lower_bound = cor.test(x, y)$conf.int[1]
    upper_bound = cor.test(x, y)$conf.int[2]
    conf = data.frame(lower_bound = lower_bound, upper_bound = upper_bound)
  }
  else {
    conf = "confidence intervals are not reported for populations"
  }
  if (wrap == F) {
    result_pos = rep(NA, lag.max + 1)
    result_neg = rep(NA, lag.max)
    if (level == "population") {
      for (j in seq(0, lag.max)) {
        dist = length(y) - j
        result_pos[[j + 1]] = cor_pop(x[1:dist], y[(1 + 
                                                      j):length(y)])
      }
      for (j in seq(1, lag.max)) {
        dist = length(y) - j
        result_neg[[j]] = cor_pop(x[(1 + j):length(x)], 
                                  y[1:dist])
      }
    }
    if (level == "sample") {
      for (j in seq(0, lag.max)) {
        dist = length(y) - j
        result_pos[[j + 1]] = cor_sample(x[1:dist], y[(1 + 
                                                         j):length(y)])
      }
      for (j in seq(1, lag.max)) {
        dist = length(y) - j
        result_neg[[j]] = cor_sample(x[(1 + j):length(x)], 
                                     y[1:dist])
      }
    }
    result = c(result_neg, result_pos)
    names(result) <- paste0("lag", seq.int(-length(result_neg), 
                                           lag.max))
    result = as.matrix(result)
    lag = as.matrix(seq.int(-length(result_neg), lag.max))
    dimnames(lag) = dimnames(result)
    if (type == "CCF") {
      index = 1
    }
    if (type == "ACF") {
      index = 2
    }
    result_df = as.data.frame(result)
    cor_value = result_df$V1[order(abs(result_df$V1), decreasing = T)[index]]
    row_index = order(abs(result_df$V1), decreasing = T)[index]
    row_index = as.integer(row_index)
    lag_best = row.names(lag)[row_index]
    best = data.frame(lag_best = lag_best, cor_value = cor_value, 
                      row_index = row_index)
  }
  if (wrap == T) {
    bound = length(y)
    result_pos = rep(NA, bound)
    if (level == "population") {
      for (j in seq(0, (bound - 1))) {
        dist = bound - j
        if (j == 0) {
          x1 = x
          y1 = y
        }
        else {
          x1 = x
          y1 = y[c((1 + j):bound, 1:j)]
        }
        result_pos[[j + 1]] = cor_pop(x1, y1)
      }
    }
    if (level == "sample") {
      for (j in seq(0, (bound - 1))) {
        dist = bound - j
        if (j == 0) {
          x1 = x
          y1 = y
        }
        else {
          x1 = x[1:1000]
          y1 = y[c((1 + j):bound, 1:j)]
        }
        result_pos[[j + 1]] = cor_sample(x1, y1)
      }
    }
    result = result_pos
    names(result) <- paste0("lag", seq.int(0, length(result) - 
                                             1))
    result = as.matrix(result)
    lag = as.matrix(seq(1, length(result)))
    dimnames(lag) = dimnames(result)
    if (type == "CCF") {
      index = 1
    }
    if (type == "ACF") {
      index = 2
    }
    result_df = as.data.frame(result)
    cor_value = result_df$V1[order(abs(result_df$V1), decreasing = T)[index]]
    row_index = order(abs(result_df$V1), decreasing = T)[index]
    row_index = as.integer(row_index)
    lag_best = row.names(lag)[row_index]
    best = data.frame(lag_best = lag_best, cor_value = cor_value, 
                      row_index = row_index)
    result = as.matrix(rep(result, 10))
  }
  if (wrap == T) {
    wrap1 = "wrapped"
  }
  if (wrap == F) {
    wrap1 = "no wrap"
  }
  cor.out <- structure(list(cor = result, lag = lag, fit_best = best, 
                            x = x, y = y, wrap = wrap1, type = type, level = level, 
                            conf.int = conf), class = "cor.out")
  return(cor.out)
}




# cor_plot()
cor_plot = function (cor.out, results = length(cor.out$cor), graph = "bar") 
{
  if (!"ggplot2" %in% installed.packages()) 
    stop("ggplot2 must be installed and loaded to continue")
  if (!"package:ggplot2" %in% search()) 
    library(ggplot2)
  if (class(cor.out) != "cor.out") {
    stop("input must be an object of type \"cor.out\", as produced by the cor_fun() function.")
  }
  if (results > length(cor.out$cor)) {
    stop("the length of 'results' cannot be more than the number of correlated values. Check:: length(cor.out$cor)")
  }
  if (graph != "line" && graph != "bar") {
    stop("'type' can only take either 'line' or 'bar'. Please note that by default it takes 'bar'")
  }
  if (cor.out$wrap == "no wrap") {
    if (results > 1000) {
      results = 1000
      print("if there is no wrap, the maximum number of values that can be printed is 1000")
    }
  }
  if (cor.out$wrap == "wrapped") {
    if (results > 5000) {
      results = 5000
      print("if there is a wrap, the maximum number of values that can be printed is 5000")
    }
  }
  type = cor.out$type
  wrap = cor.out$wrap
  level = cor.out$level
  if (level == "sample") {
    conf = cor.out$conf.int
    upper_bound = conf$upper_bound
    lower_bound = conf$lower_bound
  }
  if (results < length(cor.out$cor)) {
    ccf = cor.out$cor[1:results]
  }
  else {
    ccf = cor.out$cor
  }
  if (wrap == "no wrap") {
    if (results < length(cor.out$cor)) {
      lag = cor.out$lag[1:results]
    }
    else {
      lag = cor.out$lag
    }
  }
  if (wrap == "wrapped") {
    lag = seq(1, length(ccf))
  }
  df = data.frame(lag, ccf)
  df = df[complete.cases(df), ]
  if (level == "sample") {
    if (type == "CCF") {
      if (graph == "bar") {
        cor_plot = ggplot(df, aes(lag, ccf)) + geom_hline(aes(yintercept = 0)) + 
          geom_segment(mapping = aes(xend = lag, yend = 0)) + 
          geom_hline(aes(yintercept = upper_bound), color = "red", 
                     linetype = 2) + geom_hline(aes(yintercept = lower_bound), 
                                                color = "red", linetype = 2) + ggtitle(paste(type, 
                                                                                             " bar plot")) + ylab(type) + xlab("lag")
      }
      if (graph == "line") {
        cor_plot = ggplot(df, aes(lag, ccf)) + geom_hline(aes(yintercept = 0)) + 
          geom_line() + ggtitle(paste(type, " line plot")) + 
          geom_hline(aes(yintercept = upper_bound), color = "red", 
                     linetype = 2) + geom_hline(aes(yintercept = lower_bound), 
                                                color = "red", linetype = 2) + ylab(type) + 
          xlab("lag")
      }
    }
    if (type == "ACF") {
      if (graph == "bar") {
        cor_plot = ggplot(df, aes(lag, ccf)) + geom_hline(aes(yintercept = 0)) + 
          geom_segment(mapping = aes(xend = lag, yend = 0)) + 
          ggtitle(paste(type, " bar plot")) + ylab(type) + 
          xlab("lag")
      }
      if (graph == "line") {
        cor_plot = ggplot(df, aes(lag, ccf)) + geom_hline(aes(yintercept = 0)) + 
          geom_line() + ggtitle(paste(type, " line plot")) + 
          ylab(type) + xlab("lag")
      }
    }
  }
  if (level == "population") {
    if (graph == "bar") {
      cor_plot = ggplot(df, aes(lag, ccf)) + geom_hline(aes(yintercept = 0)) + 
        geom_segment(mapping = aes(xend = lag, yend = 0)) + 
        ggtitle(paste(type, " bar plot")) + ylab(type) + 
        xlab("lag")
    }
    if (graph == "line") {
      cor_plot = ggplot(df, aes(lag, ccf)) + geom_hline(aes(yintercept = 0)) + 
        geom_line() + ggtitle(paste(type, " line plot")) + 
        ylab(type) + xlab("lag")
    }
  }
  cor_plot
}




# cor_scatter()
cor_scatter = function (x, y = NULL, lags, period = "", x.lab = "x", y.lab = "y") 
{
  if (missing(y)) {
    y = x
    type = "ACF"
  }
  else {
    type = "CCF"
  }
  if (!is.list(lags)) 
    stop("'lags' must be formatted as a list: e.g. lags = list(10,20,30,40,50)")
  if (length(lags) < 3) 
    stop("'lags' must include at least 3 values")
  max_lag = max(unlist(lags))
  if (max_lag > (length(x) - 2)) 
    stop("all values within 'lags' must be smaller (by at least 2) than the length of 'x'")
  series_x = deparse(substitute(x))
  series_y = deparse(substitute(y))
  if (!is.numeric(x) || !is.numeric(y)) 
    stop("both 'x' and 'y' must be numeric")
  if (length(x) != length(y)) 
    stop("'x' and 'y' must be of equal lengths")
  if (length(x) < 5) 
    stop("'x' must be at least 5 values long")
  x = as.matrix(x)
  y = as.matrix(y)
  df = as.data.frame(cbind(x, y))
  vector_length <- as.integer(nrow(x))
  nser <- as.integer(ncol(x))
  if (is.na(vector_length) || is.na(nser)) 
    stop("'vector_length' must be integer")
  result = replicate(length(lags), data.frame())
  
  for (j in seq(1, length(lags))) {
    lag_len = lags[[j]]
    total = nrow(df)
    start = df[1:(total - lag_len), 1]
    end = df[(1 + lag_len):total, 2]
    df_temp = as.data.frame(cbind(start, end))
    title = title = paste("time lag of", lag_len, period)
    plot = ggplot(df_temp,
                  aes(start, end)) +
      geom_point() + 
      ggtitle(title) +
      xlab(x.lab) +
      ylab(y.lab) +
      geom_smooth(method = "lm",
                  se = FALSE,
                  col = "red",
                  size = 0.3)
    result[[j]] = plot
  }
  n <- length(result)
  nCol <- floor(sqrt(n))
  do.call("grid.arrange", c(result, ncol = nCol))
}

