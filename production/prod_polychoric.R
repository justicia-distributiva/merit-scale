# library(sjPlot)
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
      corr <- psych::polychoric(data,  na.rm = TRUE)$rho #fixed correlation type to polychoric
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
          # test <- suppressWarnings(stats::cor.test(df[[i]], 
          #                                          df[[j]], alternative = "two.sided", method = corr.method))
          test <- suppressWarnings(psych::corr.test(df[[i]], 
                                                    df[[j]])) # hypothesis test polychoric correlation
          
          
          pv <- cbind(pv, round(test$p, 5))
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
  # } #---------------------------------------- from here starts de html stuff----------#
 } #- END OF CODE----- To get the correlation matrix with the rho values and p values
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
  # page.content <- paste0(page.content, sprintf("",
  #                                              corr.method, na.deletion))
  # page.content <- paste0(page.content, "</td>\n  </tr>\n")
  # page.content <- paste(page.content, "\n</table>")
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
# print(cpvalues)
# print(corr)

}


# part II -----------------------------------------------------------------

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

sju.rmspc <- function(html.table) {
  cleaned <- gsub("      <", "<", html.table, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("    <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  cleaned <- gsub("  <", "<", cleaned, fixed = TRUE, useBytes = TRUE)
  return(cleaned)
}
