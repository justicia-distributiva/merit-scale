library(sjPlot)
poly<- function (data, na.deletion = c("listwise", "pairwise"), corr.method = c("pearson", 
                                                                         "spearman", "kendall"), title = NULL, var.labels = NULL, 
          wrap.labels = 40, show.p = TRUE, p.numeric = FALSE, fade.ns = TRUE, 
          val.rm = NULL, digits = 3, triangle = "both", string.diag = NULL, 
          CSS = NULL, file = NULL,encoding=NULL, use.viewer = TRUE, 
          remove.spaces = TRUE) 
{
  opt <- getOption("p_zero")
  if (is.null(opt) || opt == FALSE) {
    p_zero <- ""
  }
  else {
    p_zero <- "0"
  }
  na.deletion <- match.arg(na.deletion)
  corr.method <- match.arg(corr.method)
  encoding <- get.encoding(encoding, data)
  if (is.null(triangle)) {
    triangle <- "both"
  }
  else if (triangle == "u" || triangle == "upper") {
    triangle <- "upper"
  }
  else if (triangle == "l" || triangle == "lower") {
    triangle <- "lower"
  }
  else triangle <- "both"
  if (is.null(var.labels) && is.data.frame(data)) {
    var.labels <- sjlabelled::get_label(data, def.value = colnames(data))
  }
  if (corr.method != "pearson" && corr.method != "spearman" && 
      corr.method != "kendall") {
    stop("argument 'corr.method' must be one of: pearson, spearman or kendall")
  }
  if (is.matrix(data)) {
    corr <- data
    cpvalues <- NULL
  }
  else {
    if (na.deletion == "listwise") {
      data <- stats::na.omit(data)
      # corr <- stats::cor(data, method = corr.method)
      corr <- psych::polychoric(data,  na.rm = TRUE)$rho
    }
    else {
      corr <- stats::cor(data, method = corr.method, use = "pairwise.complete.obs")
      # corr <- psych::polychoric(data,  na.rm = TRUE)
    }
    computePValues <- function(df) {
      cp <- c()
      for (i in 1:ncol(df)) {
        pv <- c()
        for (j in 1:ncol(df)) {
          test <- suppressWarnings(stats::cor.test(df[[i]], 
                                                   df[[j]], alternative = "two.sided", method = corr.method))
          
          # test <- suppressWarnings(psyc(df[[i]], 
          #                                          df[[j]], alternative = "two.sided", method = corr.method))
          pv <- cbind(pv, round(test$p.value, 5))
        }
        cp <- rbind(cp, pv)
      }
      return(cp)
    }
    cpvalues <- computePValues(data)
  }
  cpv <- cpvalues
  if (!is.null(cpvalues)) {
    if (!p.numeric) {
      fun.star <- function(x) {
        x <- get_p_stars(x)
      }
    }
    else {
      fun.star <- function(x) {
        round(x, digits)
      }
    }
    cpvalues <- apply(cpvalues, c(1, 2), fun.star)
    if (p.numeric) {
      cpvalues <- apply(cpvalues, c(1, 2), function(x) {
        if (x < 0.001) 
          x <- sprintf("&lt;%s.001", p_zero)
        else x <- sub("0", p_zero, sprintf("%.*f", digits, 
                                           x))
      })
    }
  }
  else {
    show.p <- FALSE
  }
  if (is.null(var.labels)) {
    var.labels <- row.names(corr)
  }
  var.labels <- sjmisc::word_wrap(var.labels, wrap.labels, 
                                  "<br>")
  toWrite <- table.header <- sprintf("<html>\n<head>\n<meta http-equiv=\"Content-type\" content=\"text/html;charset=%s\">\n", 
                                     encoding)
  tag.table <- "table"
  tag.caption <- "caption"
  tag.thead <- "thead"
  tag.tdata <- "tdata"
  tag.notsig <- "notsig"
  tag.pval <- "pval"
  tag.valueremove <- "valueremove"
  tag.summary <- "summary"
  tag.centeralign <- "centeralign"
  tag.firsttablecol <- "firsttablecol"
  css.table <- "border-collapse:collapse; border:none;"
  css.thead <- "font-style:italic; font-weight:normal; border-top:double black; border-bottom:1px solid black; padding:0.2cm;"
  css.tdata <- "padding:0.2cm;"
  css.caption <- "font-weight: bold; text-align:left;"
  css.valueremove <- "color:white;"
  css.centeralign <- "text-align:center;"
  css.firsttablecol <- "font-style:italic;"
  css.notsig <- "color:#999999;"
  css.summary <- "border-bottom:double black; border-top:1px solid black; font-style:italic; font-size:0.9em; text-align:right;"
  css.pval <- "vertical-align:super;font-size:0.8em;"
  if (p.numeric) 
    css.pval <- "font-style:italic;"
  if (!is.null(CSS)) {
    if (!is.null(CSS[["css.table"]])) 
      css.table <- ifelse(substring(CSS[["css.table"]], 
                                    1, 1) == "+", paste0(css.table, substring(CSS[["css.table"]], 
                                                                              2)), CSS[["css.table"]])
    if (!is.null(CSS[["css.thead"]])) 
      css.thead <- ifelse(substring(CSS[["css.thead"]], 
                                    1, 1) == "+", paste0(css.thead, substring(CSS[["css.thead"]], 
                                                                              2)), CSS[["css.thead"]])
    if (!is.null(CSS[["css.tdata"]])) 
      css.tdata <- ifelse(substring(CSS[["css.tdata"]], 
                                    1, 1) == "+", paste0(css.tdata, substring(CSS[["css.tdata"]], 
                                                                              2)), CSS[["css.tdata"]])
    if (!is.null(CSS[["css.caption"]])) 
      css.caption <- ifelse(substring(CSS[["css.caption"]], 
                                      1, 1) == "+", paste0(css.caption, substring(CSS[["css.caption"]], 
                                                                                  2)), CSS[["css.caption"]])
    if (!is.null(CSS[["css.summary"]])) 
      css.summary <- ifelse(substring(CSS[["css.summary"]], 
                                      1, 1) == "+", paste0(css.summary, substring(CSS[["css.summary"]], 
                                                                                  2)), CSS[["css.summary"]])
    if (!is.null(CSS[["css.centeralign"]])) 
      css.centeralign <- ifelse(substring(CSS[["css.centeralign"]], 
                                          1, 1) == "+", paste0(css.centeralign, substring(CSS[["css.centeralign"]], 
                                                                                          2)), CSS[["css.centeralign"]])
    if (!is.null(CSS[["css.firsttablecol"]])) 
      css.firsttablecol <- ifelse(substring(CSS[["css.firsttablecol"]], 
                                            1, 1) == "+", paste0(css.firsttablecol, substring(CSS[["css.firsttablecol"]], 
                                                                                              2)), CSS[["css.firsttablecol"]])
    if (!is.null(CSS[["css.notsig"]])) 
      css.notsig <- ifelse(substring(CSS[["css.notsig"]], 
                                     1, 1) == "+", paste0(css.notsig, substring(CSS[["css.notsig"]], 
                                                                                2)), CSS[["css.notsig"]])
    if (!is.null(CSS[["css.pval"]])) 
      css.pval <- ifelse(substring(CSS[["css.pval"]], 
                                   1, 1) == "+", paste0(css.pval, substring(CSS[["css.pval"]], 
                                                                            2)), CSS[["css.pval"]])
    if (!is.null(CSS[["css.valueremove"]])) 
      css.valueremove <- ifelse(substring(CSS[["css.valueremove"]], 
                                          1, 1) == "+", paste0(css.valueremove, substring(CSS[["css.valueremove"]], 
                                                                                          2)), CSS[["css.valueremove"]])
  }
  page.style <- sprintf("<style>\nhtml, body { background-color: white; }\n%s { %s }\n%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n.%s { %s }\n</style>", 
                        tag.table, css.table, tag.caption, css.caption, tag.thead, 
                        css.thead, tag.tdata, css.tdata, tag.firsttablecol, 
                        css.firsttablecol, tag.centeralign, css.centeralign, 
                        tag.notsig, css.notsig, tag.pval, css.pval, tag.summary, 
                        css.summary, tag.valueremove, css.valueremove)
  toWrite <- paste0(toWrite, page.style)
  toWrite = paste(toWrite, "\n</head>\n<body>", "\n")
  page.content <- "<table>\n"
  if (!is.null(title)) 
    page.content <- paste0(page.content, sprintf("  <caption>%s</caption>\n", 
                                                 title))
  page.content <- paste0(page.content, "  <tr>\n")
  page.content <- paste0(page.content, "    <th class=\"thead\">&nbsp;</th>\n")
  for (i in 1:ncol(corr)) {
    page.content <- paste0(page.content, sprintf("    <th class=\"thead\">%s</th>\n", 
                                                 var.labels[i]))
  }
  page.content <- paste0(page.content, "  </tr>\n")
  for (i in 1:nrow(corr)) {
    page.content <- paste0(page.content, "  <tr>\n")
    page.content <- paste0(page.content, sprintf("    <td class=\"firsttablecol\">%s</td>\n", 
                                                 var.labels[i]))
    for (j in 1:ncol(corr)) {
      if (j == i) {
        if (is.null(string.diag) || length(string.diag) > 
            ncol(corr)) {
          page.content <- paste0(page.content, "    <td class=\"tdata centeralign\">&nbsp;</td>\n")
        }
        else {
          page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign\">%s</td>\n", 
                                                       string.diag[j]))
        }
      }
      else {
        if ((triangle == "upper" && j > i) || (triangle == 
                                               "lower" && i > j) || triangle == "both") {
          cellval <- sprintf("%.*f", digits, corr[i, 
                                                  j])
          if (show.p) {
            if (p.numeric) {
              cellval <- sprintf("%s<br><span class=\"pval\">(%s)</span>", 
                                 cellval, cpvalues[i, j])
            }
            else {
              cellval <- sprintf("%s<span class=\"pval\">%s</span>", 
                                 cellval, cpvalues[i, j])
            }
          }
          notsig <- ""
          if (fade.ns && !is.null(cpv)) {
            if (cpv[i, j] >= 0.05) 
              notsig <- " notsig"
          }
          value.remove <- ""
          if (!is.null(val.rm) && abs(corr[i, j]) < 
              abs(val.rm)) {
            value.remove <- " valueremove"
          }
          page.content <- paste0(page.content, sprintf("    <td class=\"tdata centeralign%s%s\">%s</td>\n", 
                                                       notsig, value.remove, cellval))
        }
        else {
          page.content <- paste0(page.content, "    <td class=\"tdata centeralign\">&nbsp;</td>\n")
        }
      }
    }
    page.content <- paste0(page.content, "  </tr>\n")
  }
  page.content <- paste0(page.content, "  <tr>\n")
  page.content <- paste0(page.content, sprintf("    <td colspan=\"%i\" class=\"summary\">", 
                                               ncol(corr) + 1))
  page.content <- paste0(page.content, sprintf("Computed correlation used %s-method with %s-deletion.", 
                                               corr.method, na.deletion))
  page.content <- paste0(page.content, "</td>\n  </tr>\n")
  page.content <- paste(page.content, "\n</table>")
  toWrite <- paste(toWrite, page.content, "\n")
  toWrite <- paste0(toWrite, "</body></html>")
  knitr <- page.content
  knitr <- gsub("class=", "style=", knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub("<table", sprintf("<table style=\"%s\"", css.table), 
                knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub("<caption", sprintf("<caption style=\"%s\"", 
                                    css.caption), knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.tdata, css.tdata, knitr, fixed = TRUE, 
                useBytes = TRUE)
  knitr <- gsub(tag.thead, css.thead, knitr, fixed = TRUE, 
                useBytes = TRUE)
  knitr <- gsub(tag.centeralign, css.centeralign, knitr, fixed = TRUE, 
                useBytes = TRUE)
  knitr <- gsub(tag.notsig, css.notsig, knitr, fixed = TRUE, 
                useBytes = TRUE)
  knitr <- gsub(tag.pval, css.pval, knitr, fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.summary, css.summary, knitr, fixed = TRUE, 
                useBytes = TRUE)
  knitr <- gsub(tag.firsttablecol, css.firsttablecol, knitr, 
                fixed = TRUE, useBytes = TRUE)
  knitr <- gsub(tag.valueremove, css.valueremove, knitr, fixed = TRUE, 
                useBytes = TRUE)
  if (remove.spaces) {
    knitr <- sju.rmspc(knitr)
    toWrite <- sju.rmspc(toWrite)
    page.content <- sju.rmspc(page.content)
  }
  structure(class = c("sjTable", "sjtcorr"), list(page.style = page.style, 
                                                  page.content = page.content, page.complete = toWrite, 
                                                  header = table.header, knitr = knitr, file = file, viewer = use.viewer))
}

x <- data.frame(..., stringsAsFactors = FALSE)
rownames(x) <- NULL
x
}

# do we have a stan-model?
is.stan <- function(x) inherits(x, c("stanreg", "stanfit", "brmsfit"))


#' @importFrom sjmisc is_empty
#' @importFrom dplyr n_distinct
stan.has.multiranef <- function(x) {
  if (obj_has_name(x, "facet")) {
    ri <- string_starts_with("(Intercept", x = x$facet)
    if (!sjmisc::is_empty(ri)) {
      return(dplyr::n_distinct(x$facet[ri]) > 1)
    }
  }
  FALSE
}

has_value_labels <- function(x) {
  !(is.null(attr(x, "labels", exact = T)) && is.null(attr(x, "value.labels", exact = T)))
}


#' @importFrom grDevices axisTicks
#' @importFrom dplyr if_else
#' @importFrom sjmisc is_empty
axis_limits_and_ticks <- function(axis.lim, min.val, max.val, grid.breaks, exponentiate, min.est, max.est) {
  
  # factor to multiply the axis limits. for exponentiated scales,
  # these need to be large enough to find appropriate pretty numbers
  
  fac.ll <- dplyr::if_else(exponentiate, .3, .95)
  fac.ul <- dplyr::if_else(exponentiate, 3.3, 1.05)
  
  
  # check for correct boundaries
  
  if (is.infinite(min.val) || is.na(min.val)) min.val <- min.est
  if (is.infinite(max.val) || is.na(max.val)) max.val <- max.est
  
  
  # for negative signs, need to change multiplier
  
  if (min.val < 0) fac.ll <- 1 / fac.ll
  if (max.val < 0) fac.ul <- 1 / fac.ul
  
  
  # axis limits
  
  if (is.null(axis.lim)) {
    lower_lim <- min.val * fac.ll
    upper_lim <- max.val * fac.ul
  } else {
    lower_lim <- axis.lim[1]
    upper_lim <- axis.lim[2]
  }
  
  
  # determine gridbreaks
  
  if (is.null(grid.breaks)) {
    if (exponentiate) {
      
      # make sure we have nice x-positions for breaks
      lower_lim <- round(lower_lim, 2)
      upper_lim <- round(upper_lim, 2)
      
      # for *very* small values, lower_lim might be zero, so
      # correct value here. else we have Inf as limit
      if (lower_lim == 0) lower_lim <- min.val * fac.ll / 10
      
      # use pretty distances for log-scale
      ls <- log10(c(lower_lim, upper_lim))
      ticks <- grDevices::axisTicks(c(floor(ls[1]), ceiling(ls[2])), log = TRUE)
      
      # truncate ticks to highest value below lower lim and
      # lowest value above upper lim
      
      ll <- which(ticks < lower_lim)
      if (!sjmisc::is_empty(ll) && length(ll) > 1) ticks <- ticks[ll[length(ll)]:length(ticks)]
      
      ul <- which(ticks > upper_lim)
      if (!sjmisc::is_empty(ul) && length(ul) > 1) ticks <- ticks[1:ul[1]]
      
    } else {
      ticks <- pretty(c(floor(lower_lim), ceiling(upper_lim)))
    }
  } else {
    if (length(grid.breaks) == 1)
      ticks <- seq(floor(lower_lim), ceiling(upper_lim), by = grid.breaks)
    else
      ticks <- grid.breaks
  }
  
  # save proper axis limits
  list(axis.lim = c(min(ticks), max(ticks)), ticks = ticks)
}


#' @importFrom insight model_info
#' @importFrom dplyr case_when
estimate_axis_title <- function(fit, axis.title, type, transform = NULL, multi.resp = NULL, include.zeroinf = FALSE) {
  
  # no automatic title for effect-plots
  if (type %in% c("eff", "pred", "int")) return(axis.title)
  
  # check default label and fit family
  if (is.null(axis.title)) {
    
    fitfam <- insight::model_info(fit)
    
    if (!is.null(multi.resp))
      fitfam <- fitfam[[multi.resp]]
    else if (insight::is_multivariate(fit))
      fitfam <- fitfam[[1]]
    
    axis.title <- dplyr::case_when(
      !is.null(transform) && transform == "plogis" ~ "Probabilities",
      is.null(transform) && fitfam$is_binomial ~ "Log-Odds",
      is.null(transform) && fitfam$is_ordinal ~ "Log-Odds",
      is.null(transform) && fitfam$is_multinomial ~ "Log-Odds",
      is.null(transform) && fitfam$is_categorical ~ "Log-Odds",
      is.null(transform) && fitfam$is_count ~ "Log-Mean",
      fitfam$is_count ~ "Incidence Rate Ratios",
      fitfam$is_ordinal ~ "Odds Ratios",
      fitfam$is_multinomial ~ "Odds Ratios",
      fitfam$is_categorical ~ "Odds Ratios",
      fitfam$is_binomial && !fitfam$is_logit ~ "Risk Ratios",
      fitfam$is_binomial ~ "Odds Ratios",
      TRUE ~ "Estimates"
    )
    
    if (fitfam$is_zero_inflated && isTRUE(include.zeroinf)) {
      if (is.null(transform))
        axis.title <- c(axis.title, "Log-Odds")
      else
        axis.title <- c(axis.title, "Odds Ratios")
    }
    
  }
  
  axis.title
}


#' @importFrom dplyr case_when
get_p_stars <- function(pval, thresholds = NULL) {
  
  if (is.null(thresholds)) thresholds <- c(.05, .01, .001)
  
  dplyr::case_when(
    is.na(pval) ~ "",
    pval < thresholds[3] ~ "***",
    pval < thresholds[2] ~ "**",
    pval < thresholds[1] ~ "*",
    TRUE ~ ""
  )
}


is_merMod <- function(fit) {
  inherits(fit, c("lmerMod", "glmerMod", "nlmerMod", "merModLmerTest"))
}


is_brms_mixed <- function(fit) {
  inherits(fit, "brmsfit") && !sjmisc::is_empty(fit$ranef)
}


# short checker so we know if we need more summary statistics like ICC
#' @importFrom insight model_info is_multivariate
is_mixed_model <- function(fit) {
  mi <- insight::model_info(fit)
  if (insight::is_multivariate(fit))
    mi[[1]]$is_mixed
  else
    mi$is_mixed
}


nulldef <- function(x, y, z = NULL) {
  if (is.null(x)) {
    if (is.null(y))
      z
    else
      y
  } else
    x
}


geom_intercept_line <- function(yintercept, axis.scaling, vline.color) {
  if (yintercept > axis.scaling$axis.lim[1] && yintercept < axis.scaling$axis.lim[2]) {
    t <- theme_get()
    if (is.null(t$panel.grid.major)) t$panel.grid.major <- t$panel.grid
    color <- nulldef(vline.color, t$panel.grid.major$colour, "grey90")
    minor_size <- nulldef(t$panel.grid.minor$size, .125)
    major_size <- nulldef(t$panel.grid.major$size, minor_size * 1.5)
    size <- major_size * 1.5
    geom_hline(yintercept = yintercept, color = color, size = size)
  } else {
    NULL
  }
}

# same as above, but no check if intercept is within boundaries or not
geom_intercept_line2 <- function(yintercept, vline.color) {
  t <- theme_get()
  if (is.null(t$panel.grid.major)) t$panel.grid.major <- t$panel.grid
  color <- nulldef(vline.color, t$panel.grid.major$colour, "grey90")
  minor_size <- nulldef(t$panel.grid.minor$size, .125)
  major_size <- nulldef(t$panel.grid.major$size, minor_size * 1.5)
  size <- major_size * 1.5
  geom_hline(yintercept = yintercept, color = color, size = size)
}


check_se_argument <- function(se, type = NULL) {
  if (!is.null(se) && !is.null(type) && type %in% c("std", "std2")) {
    warning("No robust standard errors for `type = \"std\"` or `type = \"std2\"`.")
    se <- NULL
  }
  
  if (!is.null(se) && !is.null(type) && type == "re") {
    warning("No robust standard errors for `type = \"re\"`.")
    se <- NULL
  }
  
  se
}


list.depth <- function(this, thisdepth = 0) {
  # http://stackoverflow.com/a/13433689/1270695
  if (!is.list(this)) {
    return(thisdepth)
  } else {
    return(max(unlist(lapply(this, list.depth, thisdepth = thisdepth + 1))))
  }
}


#' @importFrom purrr map flatten_chr
#' @importFrom sjmisc is_empty trim
parse_terms <- function(x) {
  if (sjmisc::is_empty(x)) return(x)
  
  # get variable with suffix
  vars.pos <-
    which(as.vector(regexpr(
      pattern = " ([^\\]]*)\\]",
      text = x,
      perl = T
    )) != -1)
  
  # is empty?
  if (sjmisc::is_empty(vars.pos)) return(x)
  
  # get variable names. needed later to set as
  # names attributes
  vars.names <- clear_terms(x)[vars.pos]
  
  # get levels inside brackets
  tmp <- unlist(regmatches(
    x,
    gregexpr(
      pattern = " ([^\\]]*)\\]",
      text = x,
      perl = T
    )
  ))
  
  # remove brackets
  tmp <- gsub("(\\[*)(\\]*)", "", tmp)
  
  # see if we have multiple values, split at comma
  tmp <- sjmisc::trim(strsplit(tmp, ",", fixed = T))
  
  parsed.terms <- seq_len(length(tmp)) %>%
    purrr::map(~ sprintf("%s%s", vars.names[.x], tmp[[.x]])) %>%
    purrr::flatten_chr()
  
  c(x[-vars.pos], parsed.terms)
}


#' @importFrom sjmisc trim
clear_terms <- function(x) {
  # get positions of variable names and see if we have
  # a suffix for certain values
  cleaned.pos <- regexpr(pattern = "\\s", x)
  
  # position "-1" means we only had variable name, no suffix
  replacers <- which(cleaned.pos == -1)
  # replace -1 with number of chars
  cleaned.pos[replacers] <- nchar(x)[replacers]
  
  # get variable names only
  sjmisc::trim(substr(x, 0, cleaned.pos))
}


#' @importFrom purrr map_lgl
#' @importFrom sjmisc is_empty
is_empty_list <- function(x) {
  all(purrr::map_lgl(x, sjmisc::is_empty))
}


model_deviance <- function(x) {
  tryCatch(
    {
      m_deviance(x)
    },
    error = function(x) { NULL }
  )
}


#' @importFrom performance performance_aic
model_aic <- function(x) {
  performance::performance_aic(x)
}


#' @importFrom performance performance_aicc
model_aicc <- function(x) {
  tryCatch(
    {
      performance::performance_aicc(x)
    },
    error = function(x) { NULL }
  )
}


#' @importFrom stats logLik
model_loglik <- function(x) {
  tryCatch(
    {
      stats::logLik(x)
    },
    error = function(x) { NULL }
  )
}


#' @importFrom stats deviance
m_deviance <- function(x) {
  if (is_merMod(x)) {
    if (!requireNamespace("lme4", quietly = TRUE)) {
      stop("Package 'lme4' required for this function to work, please install it.")
    }
    d <- lme4::getME(x, "devcomp")$cmp["dev"]
    if (is.na(d)) d <- stats::deviance(x, REML = FALSE)
  } else {
    d <- stats::deviance(x)
  }
  
  d
}


#' @importFrom purrr map as_vector
tidy_label <- function(labs, sep = ".") {
  # create table, and check if any value label is duplicated
  duped.val <- names(which(table(labs) > 1))
  
  # find position of duplicated labels
  dupes <- duped.val %>%
    purrr::map(~which(labs == .x)) %>%
    purrr::as_vector(.type = "double")
  
  # prefix labels with value
  labs[dupes] <- sprintf("%s%s%s", labs[dupes], sep, dupes)
  
  labs
}


#' @importFrom purrr map_df
#' @importFrom insight find_random
se_ranef <- function(object) {
  if (!requireNamespace("lme4", quietly = TRUE)) {
    stop("Package 'lme4' required for this function to work, please install it.")
  }
  if (inherits(object, "MixMod")) {
    se.bygroup <- lme4::ranef(object, post_vars = TRUE)
    vars.m <- attr(se.bygroup, "post_vars")
    
    if (dim(vars.m[[1]])[1] == 1)
      se.bygroup <- sqrt(unlist(vars.m))
    else {
      se.bygroup <- do.call(
        rbind,
        purrr::map_df(vars.m, ~ t(as.data.frame(sqrt(diag(.x)))))
      )
      
      dimnames(se.bygroup)[[2]] <- dimnames(vars.m[[1]])[[1]]
      se.bygroup <- list(se.bygroup)
      names(se.bygroup) <- insight::find_random(object, flatten = TRUE)
    }
  } else {
    se.bygroup <- lme4::ranef(object, condVar = TRUE)
    n.groupings <- length(se.bygroup)
    
    for (m in 1:n.groupings) {
      
      vars.m <- attr(se.bygroup[[m]], "postVar")
      
      K <- dim(vars.m)[1]
      J <- dim(vars.m)[3]
      
      names.full <- dimnames(se.bygroup[[m]])
      se.bygroup[[m]] <- array(NA, c(J, K))
      
      for (j in 1:J) {
        se.bygroup[[m]][j, ] <- sqrt(diag(as.matrix(vars.m[, , j])))
      }
      dimnames(se.bygroup[[m]]) <- list(names.full[[1]], names.full[[2]])
    }
  }
  
  se.bygroup
}


#' @importFrom insight n_obs
get_observations <- function(model) {
  tryCatch(
    {
      insight::n_obs(model)
    },
    error = function(x) { NULL }
  )
}


#' @importFrom insight find_predictors get_data
.labelled_model_data <- function(models) {
  # to be generic, make sure argument is a list
  if (!inherits(models, "list")) models <- list(models)
  
  # get model terms and model frame
  mf <- try(lapply(models, function(.x) insight::get_data(.x)[, -1, drop = FALSE]), silent = TRUE)
  
  # return NULL on error
  if (inherits(mf, "try-error")) {
    return(FALSE)
  }
  
  
  # get all variable labels for predictors
  lbs <- unlist(lapply(mf, function(x) {
    any(sapply(x, function(i) !is.null(attributes(i)$label)))
  }))
  
  any(lbs)
}



# evaluates arguments
get_dot_data <- function(data, dots) {
  # any dots?
  if (length(dots) > 0)
    # get variable names
    vars <- dot_names(dots)
  else
    vars <- NULL
  
  # check if data is a data frame
  if (is.data.frame(data)) {
    # get valid variable names
    vars <- vars[vars %in% colnames(data)]
    vars.is.empty <- sjmisc::is_empty(vars)
    if (!is.null(vars) && !vars.is.empty)
      # select variables, if any
      x <- data[, vars, drop = FALSE]
    else
      # else return complete data frame
      x <- data
  }
  
  x
}

# return names of objects passed as ellipses argument
dot_names <- function(dots) unname(unlist(lapply(dots, as.character)))


#' @importFrom dplyr quos select
get_dplyr_dot_data <- function(x, qs) {
  if (sjmisc::is_empty(qs))
    x
  else
    suppressMessages(dplyr::select(x, !!!qs))
}


# add annotations with table summary
# here we print out total N of cases, chi-square and significance of the table
print.table.summary <- function(baseplot,
                                modsum,
                                summary.pos = "r") {
  if (!is.null(modsum)) {
    # add annotations with table summary
    # here we print out total N of cases, chi-square and significance of the table
    if (summary.pos == "r") {
      t.hjust <- "top"
      x.x <- Inf
    } else {
      t.hjust <- "bottom"
      x.x <- -Inf
    }
    baseplot <- baseplot +
      annotate(
        "text",
        label = modsum,
        parse = TRUE,
        x = x.x,
        y = Inf,
        vjust = "top",
        hjust = t.hjust
      )
  }
  
  baseplot
}


get_var_name <- function(x) {
  if (is.null(x)) return(NULL)
  # remove "data frame name"
  dollar_pos <- regexpr("$", x, fixed = T)[1]
  if (dollar_pos != -1)
    x <- substr(x, start = dollar_pos + 1, stop = nchar(x))
  
  x
}


# Create frequency data frame of a variable
# for sjp and sjt frq functions
#' @importFrom stats na.omit ftable na.pass
#' @importFrom tidyr spread
create.xtab.df <- function(x,
                           grp,
                           round.prz = 2,
                           na.rm = FALSE,
                           weight.by = NULL) {
  # ------------------------------
  # convert to labels
  # ------------------------------
  x_full <- suppressWarnings(sjmisc::to_label(x, add.non.labelled = T))
  grp_full <- suppressWarnings(sjmisc::to_label(grp, add.non.labelled = T))
  # ------------------------------
  # create frequency crosstable. we need to convert
  # vector to labelled factor first.
  # ------------------------------
  if (is.null(weight.by)) {
    if (na.rm) {
      mydat <- stats::ftable(table(x_full, grp_full))
    } else {
      mydat <- stats::ftable(table(x_full, grp_full, useNA = "always"))
    }
  } else {
    if (na.rm)
      mydat <- stats::ftable(round(stats::xtabs(weight.by ~ x_full + grp_full)), 0)
    else
      mydat <- stats::ftable(round(stats::xtabs(weight.by ~ x_full + grp_full,
                                                exclude = NULL,
                                                na.action = stats::na.pass)), 0)
  }
  
  # create proportional tables, cell values
  ori.cell.values <- 100 * prop.table(mydat)
  proptab.cell <- round(100 * prop.table(mydat), round.prz)
  
  # create proportional tables, row percentages, including total row
  proptab.row <- rbind(
    as.data.frame(as.matrix(round(100 * prop.table(mydat, 1), round.prz))),
    round(colSums(ori.cell.values), round.prz)
  )
  
  rownames(proptab.row)[nrow(proptab.row)] <- "total"
  proptab.row <- as.data.frame(apply(proptab.row, c(1, 2), function(x) if (is.na(x)) x <- 0 else x))
  
  # create proportional tables, column  percentages, including total row
  proptab.col <- cbind(
    as.data.frame(as.matrix(round(100 * prop.table(mydat, 2), round.prz))),
    round(rowSums(ori.cell.values), round.prz)
  )
  
  colnames(proptab.col)[ncol(proptab.col)] <- "total"
  proptab.col <- as.data.frame(apply(proptab.col, c(1, 2), function(x) if (is.na(x)) x <- 0 else x))
  
  # add total row and column to cell percentages afterwards
  proptab.cell <- rbind(
    as.data.frame(as.matrix(proptab.cell)),
    round(colSums(ori.cell.values), round.prz)
  )
  
  proptab.cell <- cbind(
    as.data.frame(as.matrix(proptab.cell)),
    rowSums(proptab.cell)
  )
  
  # due to roundings, total might differ from 100%, so clean this here
  proptab.cell[nrow(proptab.cell), ncol(proptab.cell)] <- 100
  colnames(proptab.cell)[ncol(proptab.cell)] <- "total"
  rownames(proptab.cell)[nrow(proptab.cell)] <- "total"
  
  # convert to data frame
  mydat <- data.frame(mydat)
  colnames(mydat)[2] <- "Var2"
  
  # spread variables back, so we have a table again
  mydat <- tidyr::spread(mydat, .data$Var2, .data$Freq)
  
  # rename column names
  colnames(mydat)[1] <- "label"
  colnames(mydat)[is.na(colnames(mydat))] <- "NA"
  colnames(mydat)[colnames(mydat) == "<NA>"] <- "NA"
  
  # label must be character
  mydat$label <- as.character(mydat$label)
  mydat$label[is.na(mydat$label)] <- "NA"
  
  # save labels to extra vector
  labels.cnt <- mydat$label
  labels.grp <- colnames(mydat)[-1]
  
  # return result
  invisible(structure(list(mydat = mydat,
                           proptab.cell = proptab.cell,
                           proptab.col = proptab.col,
                           proptab.row = proptab.row,
                           labels.cnt = labels.cnt,
                           labels.grp = labels.grp)))
}


# check character encoding for HTML-tables
# (sjt-functions)
get.encoding <- function(encoding, data = NULL) {
  if (is.null(encoding)) {
    if (!is.null(data) && is.data.frame(data)) {
      # get variable label
      labs <- sjlabelled::get_label(data[[1]])
      # check if vectors of data frame have
      # any valid label. else, default to utf-8
      if (!is.null(labs) && is.character(labs))
        encoding <- Encoding(sjlabelled::get_label(data[[1]]))
      else
        encoding <- "UTF-8"
      # unknown encoding? default to utf-8
      if (encoding == "unknown") encoding <- "UTF-8"
    } else if (.Platform$OS.type == "unix")
      encoding <- "UTF-8"
    else
      encoding <- "Windows-1252"
  }
  return(encoding)
}


# Calculate statistics of cross tabs
#' @importFrom sjstats cramer phi table_values
#' @importFrom stats chisq.test fisher.test xtabs
crosstabsum <- function(x, grp, weight.by) {
  # --------------------------------------------------------
  # check p-value-style option
  # --------------------------------------------------------
  opt <- getOption("p_zero")
  if (is.null(opt) || opt == FALSE) {
    p_zero <- ""
  } else {
    p_zero <- "0"
  }
  if (is.null(weight.by)) {
    ftab <- table(x, grp)
  } else {
    ftab <- round(stats::xtabs(weight.by ~ x + grp), 0)
  }
  # calculate chi square value
  chsq <- stats::chisq.test(ftab)
  p.value <- chsq$p.value
  tab <- sjstats::table_values(ftab)
  # do we have cells with less than 5 observations?
  if (min(tab$expected) < 5 || (min(tab$expected) < 10 && chsq$parameter == 1)) {
    fish <- stats::fisher.test(ftab, simulate.p.value = (nrow(ftab) > 2 || ncol(ftab) > 2))
    p.value <- fish$p.value
  } else {
    fish <- NULL
  }
  # pvalue in string
  if (p.value < 0.001)
    pvas <- sprintf("%s.001", p_zero)
  else
    pvas <- sub("0", p_zero, sprintf("%.3f", p.value))
  # check whether variables are dichotome or if they have more
  # than two categories. if they have more, use Cramer's V to calculate
  # the contingency coefficient
  if (nrow(ftab) > 2 || ncol(ftab) > 2) {
    # check whether fisher's test or chi-squared should be printed
    if (is.null(fish)) {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "p" == pva,
                   list(tn = summary(ftab)$n.cases,
                        c2 = sprintf("%.2f", chsq$statistic),
                        dft = c(chsq$parameter),
                        kook = sprintf("%.2f", sjstats::cramer(ftab)),
                        pva = pvas))))
    } else {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi[c] == kook * "," ~~ "Fisher's p" == pva,
                   list(tn = summary(ftab)$n.cases,
                        dft = c(chsq$parameter),
                        kook = sprintf("%.2f", sjstats::cramer(ftab)),
                        pva = pvas))))
    }
    # if variables have two categories (2x2 table), use phi to calculate
    # the degree of association
  } else {
    # check whether fisher's test or chi-squared should be printed
    if (is.null(fish)) {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ chi^2 == c2 * "," ~~ "df" == dft * "," ~~ phi == kook * "," ~~ "p" == pva,
                   list(tn = summary(ftab)$n.cases,
                        c2 = sprintf("%.2f", chsq$statistic),
                        dft = c(chsq$parameter),
                        kook = sprintf("%.2f", sjstats::phi(ftab)),
                        pva = pvas))))
    } else {
      modsum <- as.character(as.expression(
        substitute("N" == tn * "," ~~ "df" == dft * "," ~~ phi == kook * "," ~~ "Fisher's p" == pva,
                   list(tn = summary(ftab)$n.cases,
                        dft = c(chsq$parameter),
                        kook = sprintf("%.2f", sjstats::phi(ftab)),
                        pva = pvas))))
    }
  }
  return(modsum)
}


# Erzeugt eine rotierte Faktorladungen einer Hauptkomponentenanalyse
# (Paramter "data") mit einer bestimmten Anzahl an Faktoren (Parameter "factors")
# auf Grundlage der Varimax-Rotation
#
# Parameter:
# - data: the results (object) from a principal component analysis
#         (prcomp(myData...))
# - factors: the amount of factors. can be calculated from the
#            below function "factorcount"
#' @importFrom stats varimax
varimaxrota <- function(data, factors) {
  # Faktorladungen berechnen
  # Die Faktorladungen erhält man durch Multiplikation der Eigenvektoren
  # mit der Diagonalmatrix der ausgewiesenen Standardabweichungen
  ladungen <- data$rotation %*% diag(data$sdev)
  # Zur Durchführung der VARIMAX-Rotation erzeugen wir eine Matrix
  # mit den Faktorladungen der ausgewählten Faktoren (Anzahl = Parameter "factors")
  # Varimax Rotation durchführen
  varib <- stats::varimax(ladungen[, seq_len(factors)])
  varib
}


# unlist labels
# Help function that unlists a list into a vector
unlistlabels <- function(lab) {
  dummy <- unlist(lab)
  labels <- c()
  labels <- c(labels, as.character(dummy))
  return(labels)
}


sju.rmspc <- function(html.table) {
  cleaned <- gsub("      <", "<", html.table, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("    <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("  <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  return(cleaned)
}

.is_false <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x) && !x
}