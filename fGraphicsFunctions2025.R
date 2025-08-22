# ========================================================================
# fGraphicsFunctionsNew.R
# Enhanced PSTricks Graphics Functions for R
# 
# This file contains functions to generate PSTricks LaTeX code for
# high-quality economic charts and visualizations.
# All functions now have smart defaults and optional parameters.
# ========================================================================

# Load required libraries
library(readr)

# ========================================================================
# fPSTAxes: Create chart axes with tick marks and labels
# ========================================================================
fPSTAxes <- function(pst_root,
                     pst_xlims,
                     pst_ylims,
                     pst_ylimsRight = NULL,           # Default no right axis
                     pst_xticks,
                     pst_xlines,
                     pst_yticks,
                     pst_yticksRight,
                     pst_ytickLabels,
                     pst_yticksRightLabels,
                     factor = 0.8,                    # Default factor
                     pst_xticks_main,
                     pst_yticks_main,
                     pst_xticks_decimal = 0,          # Default decimal places
                     pst_yticks_decimal = 0,          # Default decimal places
                     aspect = sqrt(2),                # Default aspect ratio
                     DateFormat,
                     ylabel,
                     ylabel_sep = 100,                # Default label separation
                     YNumberSide = "left",            # Default side
                     YNumberSize = "\\footnotesize",  # Default size
                     YNumberSideRight = "left",       # Default right side
                     ylabelRight,
                     ylabel_sepRight = 100,           # Default right label separation
                     pst_yticks_decimalRight,
                     XTickLabels,
                     pst_yticks_mainRight,
                     DateOffset = 0,                  # Default date offset
                     TickLineColor = "lightgray",     # Default tick color
                     TickLineColorRight,
                     XLabel,
                     XLabel_sep,
                     pst_box_xlims,
                     pst_box_ylims,
                     axes_filename) {
  
  # Set dependent defaults
  if (missing(TickLineColorRight)) {TickLineColorRight = TickLineColor}
  if (missing(pst_yticks_decimalRight)) {pst_yticks_decimalRight = pst_yticks_decimal}
  if (missing(pst_yticks_mainRight)) {pst_yticks_mainRight = 9 * exp(pi)}
  
  pst_xlims <- as.numeric(pst_xlims)
  pst_xticks <- as.numeric(pst_xticks)
  pst_xticksLim <- pst_xticks[pst_xticks >= pst_xlims[1] & pst_xticks <= pst_xlims[2]]
  
  if (missing(pst_box_xlims)){pst_box_xlims <- c(0, factor*10*aspect)}
  pst_x_ratio <- (pst_box_xlims[2] - pst_box_xlims[1]) / (pst_xlims[2] - pst_xlims[1])
  if (missing(pst_box_ylims)){pst_box_ylims <- c(0, factor*10)}
  pst_y_ratio <- (pst_box_ylims[2] - pst_box_ylims[1]) / (pst_ylims[2] - pst_ylims[1])
  if (missing(pst_xlines)) {pst_xlines = pst_xticks}
  pst_xlinesLim <- pst_xlines[pst_xlines >= pst_xlims[1] & pst_xlines <= pst_xlims[2]]

  if (missing(pst_ytickLabels)){pst_yticks = pst_yticks[pst_yticks >= pst_ylims[1] & pst_yticks <= pst_ylims[2]]
  pst_ytickLabels = pst_yticks} else {
  pst_yticks <- pst_yticks[pst_yticks >= pst_ylims[1] & pst_yticks <= pst_ylims[2]]
  pst_ytickLabels <- pst_ytickLabels[pst_yticks >= pst_ylims[1] & pst_yticks <= pst_ylims[2]]}

  if (!is.null(pst_ylimsRight)) {
    if (missing(pst_yticksRightLabels)){
      pst_yticksRight = pst_yticksRight[pst_yticksRight >= pst_ylimsRight[1] & pst_yticksRight <= pst_ylimsRight[2]]
      pst_yticksRightLabels = pst_yticksRight} else {
      pst_yticksRight = pst_yticksRight[pst_yticksRight >= pst_ylimsRight[1] & pst_yticksRight <= pst_ylimsRight[2]]
      pst_yticksRightLabels <- pst_yticksRightLabels[pst_yticksRight >= pst_ylimsRight[1] & pst_yticksRight <= pst_ylimsRight[2]]
    }
    
    pst_y_ratioRight <- (pst_box_ylims[2] - pst_box_ylims[1])/(pst_ylimsRight[2] - pst_ylimsRight[1])
    
  } else {
    pst_y_ratioRight <- NULL  # Set to NULL when no right axis
  }
  
  if (missing(XLabel_sep)){XLabel_sep = .06*(pst_ylims[2] - pst_ylims[1])}
  else{XLabel_sep = XLabel_sep*(pst_ylims[2] - pst_ylims[1])}

  if (missing(axes_filename)){FileName <- paste0(pst_root, "_PSTAxes.tex")}
  else{FileName <- paste0(pst_root,'_',axes_filename,'.tex')}

  cat("\\definecolor{lightblue}{rgb}{.4,0.25,1}", file = FileName)
  
  # Create axis box
  cat(sprintf("%s %6.5f %s %6.5f %s  %6.5f %s  %6.5f %s \n",
              "\\psline[linewidth=1pt](",
              pst_box_xlims[1], ",", pst_box_ylims[1], ")(",
              pst_box_xlims[1], ",", pst_box_ylims[2], ")"),
      file = FileName, append = TRUE)
  
  cat(sprintf("%s %6.5f %s %6.5f %s  %6.5f %s  %6.5f %s \n",
              "\\psline[linewidth=1pt](",
              pst_box_xlims[1], ",", pst_box_ylims[1], ")(",
              pst_box_xlims[2], ",", pst_box_ylims[1], ")"),
      file = FileName, append = TRUE)
  
  if (YNumberSide != "left" | YNumberSideRight != "left") {
    cat(sprintf("%s %6.5f %s %6.5f %s  %6.5f %s  %6.5f %s \n",
                "\\psline[linewidth=1pt](",
                pst_box_xlims[2], ",", pst_box_ylims[1], ")(",
                pst_box_xlims[2], ",", pst_box_ylims[2], ")"),
        file = FileName, append = TRUE)
  }

  # Add x-axis tick marks and labels
  x_i <- 1
  for (i in pst_xticks) {
    if (missing(DateFormat)) {
      if (missing(XTickLabels)) {
        cat(sprintf(paste0("%s %6.5f %s %6.5f %s%s%1.",toString(pst_xticks_decimal),"f%s \n"),
                    "\\uput{0.075}[270](",
                    (i-pst_xlims[1])*pst_x_ratio+pst_box_xlims[1], ",",
                    pst_box_ylims[1], "){", YNumberSize, i, "}"),
            file=FileName, append=TRUE)
      } else {
        cat(sprintf("%s %6.5f %s %6.5f %s%s%s%s \n",
                    "\\uput{0.075}[270](",
                    (i-pst_xlims[1])*pst_x_ratio+pst_box_xlims[1], ",",
                    pst_box_ylims[1], "){", YNumberSize,
                    XTickLabels[x_i], "}"),
            file=FileName, append=TRUE)
        x_i <- x_i + 1
      }
    } else {
      cat(sprintf("%s %6.5f %s %6.5f %s%s%s%s \n",
                  "\\uput{0.075}[270](",
                  (i-pst_xlims[1])*pst_x_ratio+pst_box_xlims[1], ",",
                  pst_box_ylims[1], "){", YNumberSize,
                  format(as.Date(i, origin="1970-01-01") + DateOffset, DateFormat), "}"),
          file=FileName, append=TRUE)
    }
  }

  # Add vertical grid lines
  for (i in pst_xlinesLim) {
    if (!missing(pst_xticks_main) && i == pst_xticks_main) {
      cat(sprintf("%s %6.5f %s %6.5f %s  %6.5f %s  %6.5f %s \n",
                  "\\psline[linewidth=.5pt,linecolor=black,linestyle=dashed,dash=4pt 2pt](",
                  (i-pst_xlims[1])*pst_x_ratio+pst_box_xlims[1], ",", pst_box_ylims[1], ")(",
                  (i-pst_xlims[1])*pst_x_ratio+pst_box_xlims[1], ",", pst_box_ylims[2], ")"),
          file = FileName, append = TRUE)
    } else {
      cat(sprintf("%s%s%s %6.5f %s %6.5f %s  %6.5f %s  %6.5f %s \n",
                  "\\psline[linewidth=.25pt,linecolor=", TickLineColor, ",linestyle=dashed,dash=4pt 2pt](",
                  (i-pst_xlims[1])*pst_x_ratio+pst_box_xlims[1], ",", pst_box_ylims[1], ")(",
                  (i-pst_xlims[1])*pst_x_ratio+pst_box_xlims[1], ",", pst_box_ylims[2], ")"),
          file = FileName, append = TRUE)
    }
  }

  # Add y-axis labels and ticks (left side)
  if (YNumberSide == "both") {
    for (i in seq(1, NROW(pst_yticks), by=1)) {
      cat(sprintf(paste0("%s %6.5f %s %6.5f %s%s%1.",toString(pst_yticks_decimal),"f%s \n"),
                  "\\uput{0.075}[180](", pst_box_xlims[1], ",",
                  (i-pst_ylims[1])*pst_y_ratio+pst_box_ylims[1],
                  "){", YNumberSize, i, "}"),
          file=FileName, append=TRUE)
      cat(sprintf(paste0("%s %6.5f %s %6.5f %s%s%1.",toString(pst_yticks_decimal),"f%s \n"),
                  "\\uput{0.075}[0](", pst_box_xlims[2], ",",
                  (i-pst_ylims[1])*pst_y_ratio+pst_box_ylims[1],
                  "){", YNumberSize, i, "}"),
          file=FileName, append=TRUE)
    }
    if (!missing(ylabel)) {
      cat(sprintf(paste0("%s %6.5f %s %6.5f %s %s %s \n"),
                  "\\uput{0.45}[180]{90}(", (-ylabel_sep)*pst_x_ratio+pst_box_xlims[1], ",",
                  (mean(pst_yticks)-pst_ylims[1])*pst_y_ratio+pst_box_ylims[1],
                  "){\\bf ", ylabel, "}"),
          file=FileName, append=TRUE)
    }
  } else if (YNumberSide == "right") {
    for (i in seq(1, NROW(pst_yticks), by=1)) {
      cat(sprintf(paste0("%s %6.5f %s %6.5f %s%s%1.",toString(pst_yticks_decimal),"f%s \n"),
                  "\\uput{0.075}[0](", pst_box_xlims[2], ",",
                  (pst_yticks[i]-pst_ylims[1])*pst_y_ratio+pst_box_ylims[1],
                  "){", YNumberSize, pst_ytickLabels[i], "}"),
          file=FileName, append=TRUE)
    }
    if (!missing(ylabel)) {
      cat(sprintf(paste0("%s %6.5f %s %6.5f %s %s %s \n"),
                  "\\uput{0.45}[0]{90}(", (ylabel_sep)*pst_x_ratio+pst_box_xlims[2], ",",
                  (mean(pst_yticks)-pst_ylims[1])*pst_y_ratio+pst_box_ylims[1],
                  "){\\bf ", ylabel, "}"),
          file=FileName, append=TRUE)
    }
  } else {
    for (i in seq(1, NROW(pst_yticks), by=1)) {
      cat(sprintf(paste0("%s %6.5f %s %6.5f %s%s%1.",toString(pst_yticks_decimal),"f%s \n"),
                  "\\uput{0.075}[180](", pst_box_xlims[1], ",",
                  (pst_yticks[i]-pst_ylims[1])*pst_y_ratio+pst_box_ylims[1],
                  "){", YNumberSize, pst_ytickLabels[i], "}"),
          file=FileName, append=TRUE)
    }
    if (!missing(ylabel)) {
      cat(sprintf(paste0("%s %6.5f %s %6.5f %s %s %s \n"),
                  "\\uput{0.45}[180]{90}(", (-ylabel_sep)*pst_x_ratio+pst_box_xlims[1], ",",
                  (mean(pst_yticks)-pst_ylims[1])*pst_y_ratio+pst_box_ylims[1],
                  "){\\bf ", ylabel, "}"),
          file=FileName, append=TRUE)
    }
  }

  # Add horizontal grid lines
  for (i in pst_yticks) {
    if (!missing(pst_yticks_main) && i == pst_yticks_main) {
      cat(sprintf("%s %6.5f %s %6.5f %s  %6.5f %s  %6.5f %s \n",
                  "\\psline[linewidth=.5pt,linecolor=black,linestyle=dashed,dash=4pt 2pt](",
                  pst_box_xlims[1], ",", (i - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1], ")(",
                  pst_box_xlims[2], ",", (i - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1], ")"),
          file = FileName, append = TRUE)
    } else {
      cat(sprintf("%s%s%s %6.5f %s %6.5f %s  %6.5f %s  %6.5f %s \n",
                  "\\psline[linewidth=.25pt,linecolor=", TickLineColor, ",linestyle=dashed,dash=4pt 2pt](",
                  pst_box_xlims[1], ",", (i - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1], ")(",
                  pst_box_xlims[2], ",", (i - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1], ")"),
          file = FileName, append = TRUE)
    }
  }

  # Handle right y-axis if present
  if (!is.null(pst_ylimsRight)) {
    # Add right y-axis labels and ticks
    if (YNumberSideRight == "both") {
      for (i in seq(1, NROW(pst_yticksRight), by=1)) {
        cat(sprintf(paste0("%s %6.5f %s %6.5f %s%s%1.",toString(pst_yticks_decimalRight),"f%s \n"),
                    "\\uput{0.075}[180](", pst_box_xlims[1], ",",
                    (i-pst_ylimsRight[1])*pst_y_ratioRight+pst_box_ylims[1],
                    "){", YNumberSize, i, "}"),
            file=FileName, append=TRUE)
        cat(sprintf(paste0("%s %6.5f %s %6.5f %s%s%1.",toString(pst_yticks_decimalRight),"f%s \n"),
                    "\\uput{0.075}[0](", pst_box_xlims[2], ",",
                    (i-pst_ylimsRight[1])*pst_y_ratioRight+pst_box_ylims[1],
                    "){", YNumberSize, i, "}"),
            file=FileName, append=TRUE)
      }
      if (!missing(ylabelRight)) {
        cat(sprintf(paste0("%s %6.5f %s %6.5f %s %s %s \n"),
                    "\\uput{0.45}[0]{90}(", (ylabel_sepRight)*pst_x_ratio+pst_box_xlims[2], ",",
                    (mean(pst_yticksRight)-pst_ylimsRight[1])*pst_y_ratioRight+pst_box_ylims[1],
                    "){\\bf ", ylabelRight, "}"),
            file=FileName, append=TRUE)
      }
    } else if (YNumberSideRight == "right") {
      for (i in seq(1, NROW(pst_yticksRight), by=1)) {
        cat(sprintf(paste0("%s %6.5f %s %6.5f %s%s%1.",toString(pst_yticks_decimalRight),"f%s \n"),
                    "\\uput{0.075}[0](", pst_box_xlims[2], ",",
                    (pst_yticksRight[i]-pst_ylimsRight[1])*pst_y_ratioRight+pst_box_ylims[1],
                    "){", YNumberSize, pst_yticksRightLabels[i], "}"),
            file=FileName, append=TRUE)
      }
      if (!missing(ylabelRight)) {
        cat(sprintf(paste0("%s %6.5f %s %6.5f %s %s %s \n"),
                    "\\uput{0.45}[0]{90}(", (ylabel_sepRight)*pst_x_ratio+pst_box_xlims[2], ",",
                    (mean(pst_yticksRight)-pst_ylimsRight[1])*pst_y_ratioRight+pst_box_ylims[1],
                    "){\\bf ", ylabelRight, "}"),
            file=FileName, append=TRUE)
      }
    } else {
      for (i in seq(1, NROW(pst_yticksRight), by=1)) {
        cat(sprintf(paste0("%s %6.5f %s %6.5f %s%s%1.",toString(pst_yticks_decimalRight),"f%s \n"),
                    "\\uput{0.075}[180](", pst_box_xlims[1], ",",
                    (pst_yticksRight[i]-pst_ylimsRight[1])*pst_y_ratioRight+pst_box_ylims[1],
                    "){", YNumberSize, pst_yticksRightLabels[i], "}"),
            file=FileName, append=TRUE)
      }
      if (!missing(ylabelRight)) {
        cat(sprintf(paste0("%s %6.5f %s %6.5f %s %s %s \n"),
                    "\\uput{0.45}[180]{90}(", (-ylabel_sepRight)*pst_x_ratio+pst_box_xlims[1], ",",
                    (mean(pst_yticksRight)-pst_ylimsRight[1])*pst_y_ratioRight+pst_box_ylims[1],
                    "){\\bf ", ylabelRight, "}"),
            file=FileName, append=TRUE)
      }
    }

    # Add right y-axis grid lines
    for (i in pst_yticksRight) {
      if (!missing(pst_yticks_mainRight) && i == pst_yticks_mainRight) {
        cat(sprintf("%s %6.5f %s %6.5f %s  %6.5f %s  %6.5f %s \n",
                    "\\psline[linewidth=.5pt,linecolor=black,linestyle=dashed,dash=4pt 2pt](",
                    pst_box_xlims[1], ",", (i - pst_ylimsRight[1]) * pst_y_ratioRight + pst_box_ylims[1], ")(",
                    pst_box_xlims[2], ",", (i - pst_ylimsRight[1]) * pst_y_ratioRight + pst_box_ylims[1], ")"),
            file = FileName, append = TRUE)
      } else {
        cat(sprintf("%s%s%s %6.5f %s %6.5f %s  %6.5f %s  %6.5f %s \n",
                    "\\psline[linewidth=.25pt,linecolor=", TickLineColorRight, ",linestyle=dashed,dash=4pt 2pt](",
                    pst_box_xlims[1], ",", (i - pst_ylimsRight[1]) * pst_y_ratioRight + pst_box_ylims[1], ")(",
                    pst_box_xlims[2], ",", (i - pst_ylimsRight[1]) * pst_y_ratioRight + pst_box_ylims[1], ")"),
            file = FileName, append = TRUE)
      }
    }
  }

  # Add X-axis label if provided
  if (!missing(XLabel)) {
    cat(sprintf(paste0("%s %6.5f %s %6.5f %s %s %s \n"),
                "\\uput{0.1}[270](", (mean(pst_xlims)-pst_xlims[1])*pst_x_ratio+pst_box_xlims[1], ",",
                pst_box_ylims[1] - XLabel_sep*pst_y_ratio, "){\\bf ", XLabel, "}"),
        file=FileName, append=TRUE)
  }

  # Return PST object with all parameters
  pst <- list(
    box_xlims = pst_box_xlims,
    xlims = pst_xlims,
    x_ratio = pst_x_ratio,
    box_ylims = pst_box_ylims,
    ylims = pst_ylims,
    y_ratio = pst_y_ratio,
    ylimsRight = pst_ylimsRight,
    y_ratioRight = pst_y_ratioRight
  )

  return(pst)
}

# ========================================================================
# fPSTLine: Enhanced line plotting function with optional labels
# ========================================================================
fPSTLine <- function(pst_root,
                     pst_name,
                     pst_x,
                     pst_y,
                     pst,
                     xx_1,                    # Only required when label provided
                     yy_1,                    # Only required when label provided
                     label,                   # Optional
                     sep = 0.1,              # Default separation
                     dir = 0,                # Default direction
                     color,                  # Required
                     side = "left",          # Default to left y-axis
                     LineWidth = "1pt",      # Default line width
                     arrows = "-",           # Default no arrows
                     LineStyle = "solid",    # Default line style
                     LineDash = "5pt 3pt",   # Default dash pattern
                     ShowPoints = "false",   # Default no points
                     Scatter = "false",      # Default not scatter
                     DotStyle = "*",         # Default dot style
                     ArrowInsideNo = 0,      # Default no inside arrows
                     ArrowInside = "-",      # Default inside arrow style
                     arrowscale = 2,         # Default arrow scale
                     MakeCSV = "false") {    # Default no CSV export
  
  FileName = paste0(pst_root, "_", pst_name, '.tex')

  pst_box_xlims = pst$box_xlims
  pst_box_ylims = pst$box_ylims
  pst_xlims = as.numeric(pst$xlims)
  pst_x_ratio = pst$x_ratio
  pst_x = as.numeric(pst_x)

  # Determine which y-axis to use
  if (side == "left") {
    pst_ylims = as.numeric(pst$ylims)
    pst_y_ratio = pst$y_ratio
  } else {
    if (is.null(pst$ylimsRight)) {
      stop("Error: Right y-axis requested but no right axis was defined in fPSTAxes")
    }
    pst_ylims = as.numeric(pst$ylimsRight)
    pst_y_ratio = pst$y_ratioRight
  }

  # Clean data - remove NAs and filter to x-axis limits
  xx <- cbind(pst_x, pst_y)
  xx <- na.omit(xx)
  xx <- xx[xx[, 1] >= pst_xlims[1] & xx[, 1] <= pst_xlims[2], ]
  pst_x <- xx[, 1]
  pst_y <- xx[, 2]

  # Draw the line or scatter plot
  if (Scatter == "true") {
    cat(
      "\\psdots[linecolor=",
      color,
      ",arrows=",
      arrows,
      ",linestyle=",
      LineStyle,
      ",dash=",
      LineDash,
      ",dotstyle=",
      DotStyle,
      "]",
      "\n",
      file = FileName
    )
  } else {
    cat(
      "\\psline[linewidth=",
      LineWidth,
      ",linecolor=",
      color,
      ",arrows=",
      arrows,
      ",linestyle=",
      LineStyle,
      ",ArrowInsideNo=",
      ArrowInsideNo,
      ",ArrowInside=",
      ArrowInside,
      ",arrowscale=",
      arrowscale,
      ",dash=",
      LineDash,
      ",showpoints=",
      ShowPoints,
      "]",
      "\n",
      file = FileName
    )
  }

  # Output the data points
  cat(sprintf(
    "(%6.3f, %6.3f) \n",
    ((pst_x - pst_xlims[1]) * pst_x_ratio + pst_box_xlims[1]),
    ((pst_y - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1])
  ),
  file = FileName,
  append = TRUE)

  # Only draw label if one is provided
  if (!missing(label) && !is.null(label) && nchar(label) > 0) {
    
    # Check that required label parameters are provided
    if (missing(xx_1) || missing(yy_1)) {
      stop("Error: xx_1 and yy_1 must be provided when label is specified")
    }
    
    cat(
      sprintf(
        "%s %6.5f %s %6.5f %s %6.5f %s %6.5f %s %s%s \n",
        "\\uput{",
        sep,
        "}[",
        dir,
        "](",
        (xx_1 - pst_xlims[1]) * pst_x_ratio + pst_box_xlims[1],
        ",",
        (yy_1 - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1],
        "){\\footnotesize",
        label,
        "}"
      ),
      file = FileName,
      append = TRUE
    )
  }
  
  # Export to CSV if requested
  if (MakeCSV == "true"){
    FileNameCSV = paste0(pst_root, "_", pst_name, '.csv')
    xx = data.frame(xx)
    write_csv(xx, FileNameCSV)
  }
}

# ========================================================================
# fPSTLineNoLims: Enhanced line function without axis limit filtering
# ========================================================================
fPSTLineNoLims <- function(pst_root,
                           pst_name,
                           pst_x,
                           pst_y,
                           pst,
                           xx_1,                    # Only required when label provided
                           yy_1,                    # Only required when label provided
                           label,                   # Optional
                           sep = 0.1,              # Default separation
                           dir = 0,                # Default direction
                           color,                  # Required
                           side = "left",          # Default to left y-axis
                           LineWidth = "1pt",      # Default line width
                           arrows = "-",           # Default no arrows
                           LineStyle = "solid",    # Default line style
                           LineDash = "5pt 3pt",   # Default dash pattern
                           ShowPoints = "false",   # Default no points
                           Scatter = "false",      # Default not scatter
                           DotStyle = "*",         # Default dot style
                           ArrowInsideNo = 0,      # Default no inside arrows
                           ArrowInside = "-",      # Default inside arrow style
                           arrowscale = 2,         # Default arrow scale
                           MakeCSV = "false") {    # Default no CSV export
  
  FileName = paste0(pst_root, "_", pst_name, '.tex')

  pst_box_xlims = pst$box_xlims
  pst_box_ylims = pst$box_ylims
  pst_xlims = as.numeric(pst$xlims)
  pst_x_ratio = pst$x_ratio
  pst_x = as.numeric(pst_x)

  if (side == "left") {
    pst_ylims = as.numeric(pst$ylims)
    pst_y_ratio = pst$y_ratio
  } else {
    if (is.null(pst$ylimsRight)) {
      stop("Error: Right y-axis requested but no right axis was defined in fPSTAxes")
    }
    pst_ylims = as.numeric(pst$ylimsRight)
    pst_y_ratio = pst$y_ratioRight
  }

  # Clean data but don't filter by axis limits
  xx <- cbind(pst_x, pst_y)
  xx <- na.omit(xx)
  pst_x <- xx[, 1]
  pst_y <- xx[, 2]

  if (Scatter == "true") {
    cat(
      "\\psdots[linecolor=",
      color,
      ",arrows=",
      arrows,
      ",linestyle=",
      LineStyle,
      ",dash=",
      LineDash,
      ",dotstyle=",
      DotStyle,
      "]",
      "\n",
      file = FileName
    )
  } else {
    cat(
      "\\psline[linewidth=",
      LineWidth,
      ",linecolor=",
      color,
      ",arrows=",
      arrows,
      ",linestyle=",
      LineStyle,
      ",ArrowInsideNo=",
      ArrowInsideNo,
      ",ArrowInside=",
      ArrowInside,
      ",arrowscale=",
      arrowscale,
      ",dash=",
      LineDash,
      ",showpoints=",
      ShowPoints,
      "]",
      "\n",
      file = FileName
    )
  }

  cat(sprintf(
    "(%6.3f, %6.3f) \n",
    ((pst_x - pst_xlims[1]) * pst_x_ratio + pst_box_xlims[1]),
    ((pst_y - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1])
  ),
  file = FileName,
  append = TRUE)

  # Add label if provided
  if (!missing(label) && !is.null(label) && nchar(label) > 0) {
    if (missing(xx_1) || missing(yy_1)) {
      stop("Error: xx_1 and yy_1 must be provided when label is specified")
    }
    
    cat(
      sprintf(
        "%s %6.5f %s %6.5f %s %6.5f %s %6.5f %s %s%s \n",
        "\\uput{",
        sep,
        "}[",
        dir,
        "](",
        (xx_1 - pst_xlims[1]) * pst_x_ratio + pst_box_xlims[1],
        ",",
        (yy_1 - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1],
        "){\\footnotesize",
        label,
        "}"
      ),
      file = FileName,
      append = TRUE
    )
  }
  
  if (MakeCSV == "true"){
    FileNameCSV = paste0(pst_root, "_", pst_name, '.csv')
    xx = data.frame(xx)
    write_csv(xx, FileNameCSV)
  }
}

# ========================================================================
# fPSTText: Add text labels to charts (positioning always required)
# ========================================================================
fPSTText <- function(pst_root,
                     pst_name,
                     pst,
                     xx_1,                    # Required - x position
                     yy_1,                    # Required - y position  
                     label,                   # Required - text to display
                     sep = 0.1,              # Default separation
                     dir = 0,                # Default direction
                     side = "left") {        # Default to left y-axis
  
  FileName = paste0(pst_root, "_", pst_name, '.tex')

  pst_box_xlims = pst$box_xlims
  pst_box_ylims = pst$box_ylims
  pst_xlims = as.numeric(pst$xlims)
  pst_x_ratio = pst$x_ratio

  if (side == "left") {
    pst_ylims = as.numeric(pst$ylims)
    pst_y_ratio = pst$y_ratio
  } else {
    if (is.null(pst$ylimsRight)) {
      stop("Error: Right y-axis requested but no right axis was defined in fPSTAxes")
    }
    pst_ylims = as.numeric(pst$ylimsRight)
    pst_y_ratio = pst$y_ratioRight
  }

  cat(
    sprintf(
      "%s %6.5f %s %6.5f %s %6.5f %s %6.5f %s %s%s \n",
      "\\uput{",
      sep,
      "}[",
      dir,
      "](",
      (xx_1 - pst_xlims[1]) * pst_x_ratio + pst_box_xlims[1],
      ",",
      (yy_1 - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1],
      "){\\footnotesize",
      label,
      "}"
    ),
    file = FileName
  )
}

# ========================================================================
# fPSTArea: Enhanced filled area function with optional labels
# ========================================================================
fPSTArea <- function(pst_root,
                     pst_name,
                     pst_x,
                     pst_y,
                     pst_y2,
                     pst,
                     xx_1,                          # Only required when label provided
                     yy_1,                          # Only required when label provided
                     label,                         # Optional
                     sep = 0.1,                    # Default separation
                     dir = 0,                      # Default direction
                     color,                        # Required
                     side = "left",                # Default to left y-axis
                     LineWidth = "1pt",            # Default line width
                     arrows = "-",                 # Default no arrows
                     LineStyle = "solid",          # Default line style
                     LineDash = "5pt 3pt",         # Default dash pattern
                     LineColor = "pstcolor",       # Default line color
                     Opacity = 1,                  # Default opacity
                     MakeCSV = "false") {          # Default no CSV export
  
  FileName = paste0(pst_root, "_", pst_name, '.tex')

  pst_box_xlims = pst$box_xlims
  pst_box_ylims = pst$box_ylims
  pst_xlims = as.numeric(pst$xlims)
  pst_x_ratio = pst$x_ratio
  pst_x = as.numeric(pst_x)

  if (side == "left") {
    pst_ylims = as.numeric(pst$ylims)
    pst_y_ratio = pst$y_ratio
  } else {
    if (is.null(pst$ylimsRight)) {
      stop("Error: Right y-axis requested but no right axis was defined in fPSTAxes")
    }
    pst_ylims = as.numeric(pst$ylimsRight)
    pst_y_ratio = pst$y_ratioRight
  }

  pst_x2 <- pst_x
  xx <- cbind(pst_x, pst_y)
  xx <- na.omit(xx)
  xx <- xx[xx[, 1] >= pst_xlims[1] & xx[, 1] <= pst_xlims[2], ]
  pst_x <- xx[, 1]
  pst_y <- xx[, 2]
  
  if (MakeCSV == "true"){
    FileNameCSV = paste0(pst_root, "_", pst_name, '_1.csv')
    xx = data.frame(xx)
    write_csv(xx, FileNameCSV)
  }

  xx <- cbind(pst_x2, pst_y2)
  xx <- na.omit(xx)
  xx <- xx[xx[, 1] >= pst_xlims[1] & xx[, 1] <= pst_xlims[2], ]
  pst_x2 <- xx[nrow(xx):1, 1]
  pst_y2 <- xx[nrow(xx):1, 2]
  
  if (MakeCSV == "true"){
    FileNameCSV = paste0(pst_root, "_", pst_name, '_2.csv')
    xx = data.frame(xx)
    write_csv(xx, FileNameCSV)
  }

  cat(paste0("\\definecolor{pstcolor}{rgb}{",color,"}"), "\n", file = FileName)
  cat(paste0("\\pspolygon*[linestyle=",LineStyle,",linecolor=",LineColor,",linewidth=",LineWidth,
             ",fillstyle=solid,fillcolor=pstcolor,opacity=",Opacity,"]"),
      "\n", file = FileName, append = TRUE)
  
  cat(sprintf(
    "(%6.3f, %6.3f) \n",
    ((pst_x - pst_xlims[1]) * pst_x_ratio + pst_box_xlims[1]),
    ((pst_y - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1])
  ),
  file = FileName,
  append = TRUE)
  
  cat(sprintf(
    "(%6.3f, %6.3f) \n",
    ((pst_x2 - pst_xlims[1]) * pst_x_ratio + pst_box_xlims[1]),
    ((pst_y2 - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1])
  ),
  file = FileName,
  append = TRUE)

  # Add label if provided
  if (!missing(label) && !is.null(label) && nchar(label) > 0) {
    if (missing(xx_1) || missing(yy_1)) {
      stop("Error: xx_1 and yy_1 must be provided when label is specified")
    }
    
    cat(
      sprintf(
        "%s %6.5f %s %6.5f %s %6.5f %s %6.5f %s %s%s \n",
        "\\uput{",
        sep,
        "}[",
        dir,
        "](",
        (xx_1 - pst_xlims[1]) * pst_x_ratio + pst_box_xlims[1],
        ",",
        (yy_1 - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1],
        "){\\footnotesize",
        label,
        "}"
      ),
      file = FileName,
      append = TRUE
    )
  }
}

# ========================================================================
# fPSTAreaGradient: Enhanced gradient-filled area function
# ========================================================================
fPSTAreaGradient <- function(pst_root,
                             pst_name,
                             pst_x,
                             pst_y,
                             pst_y2,
                             pst,
                             xx_1,                          # Only required when label provided
                             yy_1,                          # Only required when label provided
                             label,                         # Optional
                             sep = 0.1,                    # Default separation
                             dir = 0,                      # Default direction
                             color,                        # Required - main color
                             gradbegincolor = color,       # Default gradient start color
                             gradendcolor = color,         # Default gradient end color
                             gradlines = 500,              # Default gradient lines
                             gradmidpoint = 0.5,           # Default gradient midpoint
                             gradangle = 0,                # Default gradient angle
                             side = "left",                # Default to left y-axis
                             LineWidth = "1pt",            # Default line width
                             LineStyle = "solid",          # Default line style
                             Opacity = 1,                  # Default opacity
                             MakeCSV = "false") {          # Default no CSV export
  
  FileName = paste0(pst_root, "_", pst_name, '.tex')

  pst_box_xlims = pst$box_xlims
  pst_box_ylims = pst$box_ylims
  pst_xlims = as.numeric(pst$xlims)
  pst_x_ratio = pst$x_ratio
  pst_x = as.numeric(pst_x)

  if (side == "left") {
    pst_ylims = as.numeric(pst$ylims)
    pst_y_ratio = pst$y_ratio
  } else {
    if (is.null(pst$ylimsRight)) {
      stop("Error: Right y-axis requested but no right axis was defined in fPSTAxes")
    }
    pst_ylims = as.numeric(pst$ylimsRight)
    pst_y_ratio = pst$y_ratioRight
  }

  pst_x2 <- pst_x
  xx <- cbind(pst_x, pst_y)
  xx <- na.omit(xx)
  xx <- xx[xx[, 1] >= pst_xlims[1] & xx[, 1] <= pst_xlims[2], ]
  pst_x <- xx[, 1]
  pst_y <- xx[, 2]

  xx <- cbind(pst_x2, pst_y2)
  xx <- na.omit(xx)
  xx <- xx[xx[, 1] >= pst_xlims[1] & xx[, 1] <= pst_xlims[2], ]
  pst_x2 <- xx[nrow(xx):1, 1]
  pst_y2 <- xx[nrow(xx):1, 2]

  cat(paste0("\\definecolor{pstcolor}{rgb}{",color,"}"), "\n", file = FileName)
  cat(paste0("\\definecolor{gradbegincolor}{rgb}{",gradbegincolor,"}"), "\n", file = FileName,append = TRUE)
  cat(paste0("\\definecolor{gradendcolor}{rgb}{",gradendcolor,"}"), "\n", file = FileName,append = TRUE)
  cat(paste0("\\pspolygon*[linestyle=",LineStyle,",linewidth=",LineWidth,
             ",fillstyle=gradient,fillcolor=pstcolor,opacity=",Opacity,
             ",gradbegin=gradbegincolor,gradend=gradendcolor,gradlines=",gradlines,
             ",gradmidpoint=",gradmidpoint,",gradangle=",gradangle,"]"),
      "\n", file = FileName, append = TRUE)
  
  cat(sprintf(
    "(%6.3f, %6.3f) \n",
    ((pst_x - pst_xlims[1]) * pst_x_ratio + pst_box_xlims[1]),
    ((pst_y - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1])),
    file = FileName, append = TRUE)
  
  cat(sprintf(
    "(%6.3f, %6.3f) \n",
    ((pst_x2 - pst_xlims[1]) * pst_x_ratio + pst_box_xlims[1]),
    ((pst_y2 - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1])
  ),
  file = FileName, append = TRUE)

  # Add label if provided
  if (!missing(label) && !is.null(label) && nchar(label) > 0) {
    if (missing(xx_1) || missing(yy_1)) {
      stop("Error: xx_1 and yy_1 must be provided when label is specified")
    }
    
    cat(
      sprintf(
        "%s %6.5f %s %6.5f %s %6.5f %s %6.5f %s %s%s \n",
        "\\uput{",
        sep,
        "}[",
        dir,
        "](",
        (xx_1 - pst_xlims[1]) * pst_x_ratio + pst_box_xlims[1],
        ",",
        (yy_1 - pst_ylims[1]) * pst_y_ratio + pst_box_ylims[1],
        "){\\footnotesize",
        label,
        "}"
      ),
      file = FileName,
      append = TRUE
    )
  }
}

# ========================================================================
# Utility Functions for LaTeX Compilation (Original Functions)
# ========================================================================

fCompile <- function(pst_root, SubFiles, WorkingDirectory, PictureSize, Baseline) {
  if (missing(WorkingDirectory)) {
    WorkingDirectory = "/shared/crt/OptimalBlue/RateCalcs/Figures/"
  }

  OutputFile <- paste0(pst_root, ".tex")
  BlankFile <- "/shared/StressTest/program/fFigures/Output/PSTBlankNew.tex"
  basepst_root <- basename(pst_root)

  con <- file(BlankFile, open = "r")
  linn <- readLines(con, 1)
  cat(linn, "\n", file = OutputFile)
  
  for (i in 1:11) {
    linn = readLines(con, 1)
    cat(linn, "\n", file = OutputFile, append = TRUE)
  }
  
  for (i in 1:NROW(SubFiles)) {
    cat(
      paste0("\\input{", basepst_root, "_", SubFiles[[i]], ".tex}", "\n"),
      file = OutputFile,
      append = TRUE
    )
  }
  
  for (i in 1:3) {
    linn = readLines(con, 1)
    cat(linn, "\n", file = OutputFile, append = TRUE)
  }
  close(con)

  setwd(WorkingDirectory)
  
  system(paste("/opt/apps/texlive/2013/bin/x86_64-linux/latex", basename(OutputFile)), timeout = 5)
  system(paste("/opt/apps/texlive/2013/bin/x86_64-linux/dvips -o ", paste0(basepst_root, ".ps"), paste0(basepst_root, ".dvi")), timeout = 5)
  system(paste("ps2pdf ", paste0(basepst_root, ".ps"), paste0(basepst_root, ".pdf ")))
  system(paste("ps2eps ", paste0(basepst_root, ".ps -f -quiet")))
  system(paste("epstopdf ", paste0(basepst_root, ".eps  -quiet")))
  system(paste("pdftoppm ", paste0(basepst_root, ".pdf"), paste0(basepst_root, " -png -r 300  ")))
  
  system(
    paste0(
      "rm ",
      basepst_root,
      ".aux ",
      basepst_root,
      ".dvi ",
      basepst_root,
      ".log ",
      basepst_root,
      ".nav ",
      basepst_root,
      ".out ",
      basepst_root,
      ".snm ",
      basepst_root,
      ".toc "
    )
  )
}

fCompileLatex <- function(pst_root, SubFiles, WorkingDirectory) {
  if (missing(WorkingDirectory)) {
    WorkingDirectory = "/shared/crt/OptimalBlue/RateCalcs/Figures/"
  }

  OutputFile <- paste0(pst_root, ".tex")
  BlankFile <- "/shared/StressTest/program/fFigures/Output/PSTBlankNew.tex"
  basepst_root <- basename(pst_root)

  con <- file(BlankFile, open = "r")
  linn <- readLines(con, 1)
  cat(linn, "\n", file = OutputFile)
  
  for (i in 1:11) {
    linn = readLines(con, 1)
    cat(linn, "\n", file = OutputFile, append = TRUE)
  }
  
  for (i in 1:NROW(SubFiles)) {
    cat(
      paste0("\\input{", basepst_root, "_", SubFiles[[i]], ".tex}", "\n"),
      file = OutputFile,
      append = TRUE
    )
  }
  
  for (i in 1:3) {
    linn = readLines(con, 1)
    cat(linn, "\n", file = OutputFile, append = TRUE)
  }
  close(con)

  setwd(WorkingDirectory)
  
  system(paste("/opt/apps/texlive/2013/bin/x86_64-linux/latex", basename(OutputFile)), timeout = 5)
  system(paste("/opt/apps/texlive/2013/bin/x86_64-linux/dvips -o ", paste0(basepst_root, ".ps"), paste0(basepst_root, ".dvi")), timeout = 5)
  system(paste("ps2pdf ", paste0(basepst_root, ".ps"), paste0(basepst_root, ".pdf ")))
  system(paste("ps2eps ", paste0(basepst_root, ".ps -f -quiet")))
  system(paste("epstopdf ", paste0(basepst_root, ".eps  -quiet")))
  system(paste("pdftoppm ", paste0(basepst_root, ".pdf"), paste0(basepst_root, " -png -r 300  ")))
  
  system(
    paste0(
      "rm ",
      basepst_root,
      ".aux ",
      basepst_root,
      ".dvi ",
      basepst_root,
      ".log ",
      basepst_root,
      ".nav ",
      basepst_root,
      ".out ",
      basepst_root,
      ".snm ",
      basepst_root,
      ".toc "
    )
  )
}

fCompileTable <- function(pst_root,
                          Width,
                          Height,
                          WorkingDirectory,
                          PictureSize,
                          create_csv) {
  if (missing(WorkingDirectory)) {
    WorkingDirectory = "/shared/crt/OptimalBlue/RateCalcs/Figures/"
  }
  if (missing(Width)) {
    Width = "\\textwidth"
  }
  if (missing(Height)) {
    Height = "!"
  }

  OutputFile <- paste0(pst_root, ".tex")
  BlankFile <- "/shared/StressTest/program/fFigures/Output/PSTBlankNew.tex"
  basepst_root <- basename(pst_root)

  con <- file(BlankFile, open = "r")
  linn <- readLines(con, 1)
  cat(linn, "\n", file = OutputFile)
  
  for (i in 1:11) {
    linn = readLines(con, 1)
    cat(linn, "\n", file = OutputFile, append = TRUE)
  }
  
  cat(paste0("\\input{", basepst_root, ".tex}", "\n"), file = OutputFile, append = TRUE)
  
  for (i in 1:3) {
    linn = readLines(con, 1)
    cat(linn, "\n", file = OutputFile, append = TRUE)
  }
  close(con)

  setwd(WorkingDirectory)
  
  system(paste("/opt/apps/texlive/2013/bin/x86_64-linux/latex", basename(OutputFile)), timeout = 5)
  system(paste("/opt/apps/texlive/2013/bin/x86_64-linux/dvips -o ", paste0(basepst_root, ".ps"), paste0(basepst_root, ".dvi")), timeout = 5)
  system(paste("ps2pdf ", paste0(basepst_root, ".ps"), paste0(basepst_root, ".pdf ")))
  system(paste("ps2eps ", paste0(basepst_root, ".ps -f -quiet")))
  system(paste("epstopdf ", paste0(basepst_root, ".eps  -quiet")))
  system(paste("pdftoppm ", paste0(basepst_root, ".pdf"), paste0(basepst_root, " -png -r 300  ")))
  
  system(
    paste0(
      "rm ",
      basepst_root,
      ".aux ",
      basepst_root,
      ".dvi ",
      basepst_root,
      ".log ",
      basepst_root,
      ".nav ",
      basepst_root,
      ".out ",
      basepst_root,
      ".snm ",
      basepst_root,
      ".toc "
    )
  )
}

# End of fGraphicsFunctionsNew.R
# ========================================================================