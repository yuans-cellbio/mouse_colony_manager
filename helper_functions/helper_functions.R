
##################################
# Overwrite functions from ggped #
##################################

ggdraw.pedigree1 <- function (dat = NULL, features = c("affected"), features.as.lables = NULL, 
                              plot.names = T, plot.kinship.label = T, column.names = list(x = "x", 
                                                                                          y = "y", Name = "Name", mate.id = "mate.id", twin.id = "twin.id", 
                                                                                          mate.centerpoint = "mate.centerpoint", family.centerpoint = "family.centerpoint", 
                                                                                          twin.centerpoint = "twin.centerpoint", sex = "sex", 
                                                                                          adopted = "adopted", pregnancy = "pregnancy"), col.feature = NA, col.palette = suppressWarnings(brewer.pal(length(unique(dat[, 
                                                                                                                                                                                                                       features.as.lables])), "Set2")), col.lables = c(`wt/wt` = "black", 
                                                                                                                                                                                                                                                                       wt = "black", `+/+` = "black", `wt/mut` = col.palette[1], 
                                                                                                                                                                                                                                                                       het = col.palette[1], `+/-` = col.palette[1], `mut/mut` = col.palette[2], 
                                                                                                                                                                                                                                                                       hom = col.palette[2], `-/-` = col.palette[2]), col.tree = "#000000", 
                              col.double = "#808080", chrtype = "autosome", packed = TRUE, 
                              align = TRUE, width = 10, shape.size = 7, text.size = 8, ...) 
{
  if (class(dat) == "pedigree") {
    dat <- dfalign.pedigree(ped, chrtype = chrtype, packed = packed, 
                            align = align, width = width)
  }
  else {
    if (class(dat) != "data.frame") {
      stop("dat has to be eihter of class data.frame or pedigree.")
    }
  }
  if (length(unique(names(column.names))) != length(names(column.names))) {
    stop("Column names provided are not unique.")
  }
  if (length(unique(features)) != length(features)) {
    stop("Feature column names must be unique.")
  }
  
  if (is.na(col.feature)){
    col.feature <- suppressWarnings(brewer.pal(length(unique(dat$features)), "Set2"))
    names(col.feature) <- features
  }
  
  # Not converting features to logical. 
  
  # for (i in features) {
  #   if (is.character(dat[, i])) {
  #     if (length(unique(dat[!(is.na(dat[, i])), i])) < 
  #       3) {
  #       dat[, i] <- dat[, i] == dat[1, i]
  #       warning(paste0("Feature ", i, " automatically converted into logical by setting ", 
  #         dat[1, i], " as TRUE. "))
  #     }
  #   }
  #   if (is.numeric(dat[, i])) {
  #     if (length(unique(dat[!(is.na(dat[, i])), i])) < 
  #       3) {
  #       dat[, i] <- dat[, i] == max(dat[, i])
  #       warning(paste0("Feature ", i, " automatically converted into logical by setting ", 
  #         max(dat[, i]), " as TRUE. "))
  #     }
  #   }
  # }
  
  tolabel <- as.vector(unlist(apply(dat[features.as.lables], 
                                    2, unique)))
  if (!all(tolabel %in% names(col.lables))) {
    whichmissing <- logical(length = length(tolabel))
    for (i in 1:length(whichmissing)) {
      whichmissing[i] <- !(tolabel[i] %in% names(col.lables))
    }
    stop("the following features selected in 'features.as.lables' have no colour assigned in 'col.labels': ", 
         paste(tolabel[whichmissing], ", "))
  }
  for (i in names(column.names)) {
    if (!(i %in% c("adopted", "pregnancy"))) {
      colnames(dat)[colnames(dat) == i] <- column.names[i]
    }
    else {
      if (i %in% colnames(dat)) {
        colnames(dat)[colnames(dat) == i] <- column.names[i]
      }
      else {
        dat[, i] <- F
      }
    }
  }
  
  # Allow adjustments of shape and text size
  
  # shape.size = 7
  # text.size = 8 
  voffset = 0
  dat$y <- (-dat$y)
  
  # features are modified to take dbl 0-1. 
  
  features_to_long <- function (df, features) {
    if (!all(sapply(df[, features], class) == "numeric")) {
      if (any(df[, features] < 0 | df[, features] > 1)) {
        stop("The 'feature.value' column must be between 0 and 1.")
      }}
    as.data.frame(pivot_longer(df, cols = all_of(features), names_to = "feature.name", 
                               values_to = "feature.value", values_drop_na = F))
  }
  if (length(features) > 1) {
    dat <- features_to_long(dat, features[lapply(dat[, features], 
                                                 class) == "numeric"])
  }
  else {
    dat <- features_to_long(dat, features[class(dat[, features]) == 
                                            "numeric"])
  }
  
  # if (length(features) > 1) {
  #   dat <- features_to_long(dat, features[lapply(dat[, features], 
  #     class) == "logical"])
  # }
  # else {
  #   dat <- features_to_long(dat, features[class(dat[, features]) == 
  #     "logical"])
  # }
  
  plt <- ggplot(dat, aes(x = x, y = y, label = "id")) + geom_line(data = dat[!is.na(dat$mate.id), 
  ], aes(group = floor(mate.id)), colour = col.tree)
  if (any(dat$kinship > 0 & !is.na(dat$mate.id))) {
    plt <- plt + geom_line(data = dat[(dat$mate.id %in% 
                                         dat$mate.id[dat$kinship > 0]) & !is.na(dat$mate.id), 
    ], aes(group = mate.id, y = y + 0.02), colour = col.tree)
    if (plot.kinship.label) {
      plt <- plt + geom_text(data = dat[(dat$mate.id %in% 
                                           dat$mate.id[dat$kinship > 0]) & !is.na(dat$mate.id), 
      ], aes(label = paste0("Kinship:\n", as.character(kinship)), 
             x = mate.centerpoint), vjust = (shape.size/2/min(dat$y)), 
      size = text.size/ggplot2:::.pt)
    }
  }
  if (length(dat$Name) != length(unique(dat$Name))) {
    plt <- plt + geom_line(data = dat, aes(group = ID, y = y - 
                                             0.02), linetype = 2, colour = col.double)
  }
  plt <- plt + geom_segment(data = dat[!is.na(dat$mate.id), 
  ], aes(group = mate.id, x = mate.centerpoint, xend = mate.centerpoint, 
         yend = y - 0.5), colour = col.tree) + geom_segment(data = dat[!is.na(dat$mate.id),
         ], aes(group = mate.id, x = mate.centerpoint, xend = family.centerpoint,
                y = y - 0.5, yend = y - 0.75), colour = col.tree)
  if (any(!is.na(dat$twin.id))) {
    dat$xfambarcoord <- unlist(apply(dat[, c("x", "twin.centerpoint", 
                                             "family.centerpoint")], 1, function(x) {
                                               if (x["x"] >= x["family.centerpoint"]) {
                                                 if (!is.na(x["twin.centerpoint"])) {
                                                   max(x["x"], x["twin.centerpoint"])
                                                 }
                                                 else {
                                                   x["x"]
                                                 }
                                               }
                                               else {
                                                 if (!is.na(x["twin.centerpoint"])) {
                                                   min(x["x"], x["twin.centerpoint"])
                                                 }
                                                 else {
                                                   x["x"]
                                                 }
                                               }
                                             }))
    plt <- plt + geom_line(data = dat[dat$family %in% dat$family[!is.na(dat$twin.id)], 
    ], aes(group = family, y = y + 0.25, x = xfambarcoord), 
    colour = col.tree) + geom_segment(data = dat[!is.na(dat$twin.id), 
    ], aes(group = family, xend = twin.centerpoint, 
           x = x, yend = y + 0.25), colour = col.tree) + geom_line(data = dat[dat$twin.type == 
                                                                                2, ], aes(group = twin.id), colour = col.tree)
  }
  plt <- plt + geom_line(data = dat[!is.na(dat$family) & is.na(dat$twin.id), 
  ], aes(group = family, y = y + 0.25), colour = col.tree) + 
    geom_segment(data = dat[is.na(dat$twin.id) & !is.na(dat$family), 
    ], aes(group = family, xend = x, yend = y + 0.25), 
    colour = col.tree)
  plt <- plt + geom_pedigreepoint1(mapping = aes(sex = as.factor(sex),
                                                 isdead = as.factor(status), feature.name = feature.name,
                                                 feature.value = feature.value, adopted = adopted, pregnancy = pregnancy),
                                   ...)
  
  # geom_text is modified
  
  if ("Name" %in% colnames(dat) && plot.names) {
    plt <- plt + geom_text(data = dat, aes(label = Name), 
                           vjust = -(shape.size * 2/min(dat$y)),
                           hjust = 0.5, 
                           size = text.size/ggplot2:::.pt)
    voffset <- -(shape.size * 1/min(dat$y)) + text.size/ggplot2:::.pt * 1.01
  }
  
  # if ("Name" %in% colnames(dat) && plot.names) {
  #   plt <- plt + geom_text(data = dat, aes(label = Name), 
  #     vjust = -(shape.size * 1.5/min(dat$y)), hjust = "outward", 
  #     size = text.size/ggplot2:::.pt)
  #   voffset <- -(shape.size * 1.5/min(dat$y)) + text.size/ggplot2:::.pt * 
  #     1.01
  # }
  for (i in features.as.lables) {
    plt <- plt + geom_text(data = dat, aes_string(label = i,
                                                  colour = i), vjust = voffset, hjust = "outward",
                           size = text.size/ggplot2:::.pt) + scale_colour_manual(values = col.lables,
                                                                                 guide = FALSE)
    voffset <- voffset + text.size/ggplot2:::.pt * 1.01
  }
  plt <- plt + 
    theme_void()
  # theme(legend.position = "bottom", 
  # legend.box = "vertical")
  plt
}

#' geom_pedigreepoint
#'
#' The pedigreepoint geom is used to create the symbols on a pedigree chart representing individuals. Points are shaped by gender, can be marked as deceased and tagged with multiple colours.
#'
#' @inheritParams ggplot2::geom_point
#' @name geom_pedigreepoint
#' @section Aesthetics:
#' \code{geom_pedigreepoint()} understands the following aesthetics (required aesthetics are in bold - data type is critical for correct plotting):
#' \itemize{
#'  \item \strong{x} Coordinate. Numeric.
#'  \item \strong{y} Coordinate. Numeric.
#'  \item \strong{sex} Gender. Factor. 1 or M is Male, 2 or F is Female
#'  \item \strong{isdead} Factor or Logical. 0 or FALSE is Alive, 1 or TRUE is Dead. If factor, 2 is Stillbirth or Miscarriage. All others are Unknown.
#'  \item feature.name Features to plot.
#'  \item feature.value Corresponding status of feature.
#'  \item adopted Logical. Depicted as brackets around symbol.
#'  \item pregnancy Logical. Depicted as P in symbol.
#'  \item colour Stroke colour. Default: "black".
#'  \item alpha Symbol alpha. Default: 1.
#'  \item stroke Stroke thickness.
#' }
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 unit
#' @importFrom grid unit
#' @importFrom grid polygonGrob
#' @importFrom grid pointsGrob
#' @seealso Scales
#' @export
NULL
GeomPedigreePoint <- ggproto(
  "GeomPedigreePoint",
  Geom,
  required_aes = c("x", "y", "sex", "isdead"),
  optional_aes = c(
    "feature.name",
    "feature.value",
    "feature.colours", 
    "adopted",
    "pregnancy",
    "colour",
    "alpha",
    "stroke"
  ),
  default_aes = aes(
    # feature.name = "#FFFFFF",
    feature.name = NA,
    feature.value = NA,
    # feature.colours = "#FFFFFF", # pass custom colors
    adopted = FALSE,
    pregnancy = FALSE,
    sex = 22,
    colour = "black",
    alpha = 1,
    stroke = 1,
    isdead = 0
  ),
  setup_data = function(data, param) {
    if (!is.factor(data$sex) && !is.numeric(data$sex) && !is.character(data$sex)) {
      stop("The 'sex' column must be coercible to pedigree shape codes.")
    }

    sex_chr <- toupper(trimws(as.character(data$sex)))
    sex_num <- suppressWarnings(as.numeric(sex_chr))
    sex_num[is.na(sex_num) & sex_chr %in% c("M", "MALE")] <- 22
    sex_num[is.na(sex_num) & sex_chr %in% c("F", "FEMALE")] <- 21
    sex_num[is.na(sex_num)] <- 0
    data$sex <- sex_num

    if (!is.factor(data$isdead) && !is.logical(data$isdead) && !is.numeric(data$isdead) && !is.character(data$isdead)) {
      stop("The 'isdead' column must be coercible to death-status codes.")
    }

    isdead_chr <- trimws(as.character(data$isdead))
    isdead_num <- suppressWarnings(as.numeric(isdead_chr))
    isdead_num[is.na(isdead_num) & toupper(isdead_chr) %in% c("FALSE", "ALIVE")] <- 0
    isdead_num[is.na(isdead_num) & toupper(isdead_chr) %in% c("TRUE", "DEAD", "ENDED")] <- 1
    isdead_num[is.na(isdead_num)] <- 0
    data$isdead <- isdead_num
    
    ####################################################
    # 'feature.value' is modified to take a value 0-1. #
    ####################################################
    
    if (!is.numeric(data$feature.value)) {
      stop("The 'feature.value' column must be a number.")
    } else if (any(data$feature.value < 0 | data$feature.value > 1, na.rm = TRUE)) {
      stop("The 'feature.value' column must be between 0 and 1.")
    }
    
    data
  },
  draw_panel = function(data,
                        panel_scales,
                        coord,
                        na.colour,
                        size) {
    ## Transformations
    coords <-
      coord$transform(data[, c("x", "y")], panel_scales)
    coords$x <- unit(coords$x, "npc")
    coords$y <- unit(coords$y, "npc")
    ## Make arc and rect template
    range <- seq(0, 2 * pi, pi / 100)
    
    circh <-
      split_by_feature(sin(-range),
                       unique(data$feature.name),
                       size)
    circv <-
      split_by_feature(cos(-range),
                       unique(data$feature.name),
                       size)
    
    recth <-
      split_by_feature(c(
        seq(0,-12.5, by = -0.5),
        rep(-12.5, 50),
        seq(-12, 12.5, by =
              0.5),
        rep(12.5, 50),
        seq(12, 0.5, by =
              -0.5)
      ) / 12.5,
      unique(data$feature.name),
      size)
    rectv <-
      split_by_feature(c(
        rep(12.5, 25),
        seq(12,-12.5, by =
              -0.5),
        rep(-12.5, 50),
        seq(-12, 12.5, by =
              0.5),
        rep(12.5, 25)
      ) / 12.5,
      unique(data$feature.name),
      size)
    
    rhombh <-
      split_by_feature(c(
        seq(0,-12, by = -0.5),
        seq(-12.5, 12.5, by =
              0.5),
        seq(12, 0.5, by =
              -0.5)
      ) / 12.5,
      unique(data$feature.name),
      size)
    rhombv <-
      split_by_feature(c(seq(-12.5, 12, by = 0.5),
                         seq(12.5,-12, by =
                               -0.5)) / 12.5,
                       unique(data$feature.name),
                       size)
    
    tokeep <-
      sort(unique(c(as.vector(
        apply(cbind(recth, rectv), 2, function(x) {
          c(which.max(x),
            which.min(x))
        })
      ),
      1,
      dim(recth)[1])))
    h = dim(circh)[1]
    pieid <-
      rep(1:length(unique(data$feature.name)), each = h)
    
    ## Calculate arcs or rects around each midpoint
    id <- NULL
    cid <- 0
    fill <- NULL
    alpha <- NULL
    stroke <- NULL
    colour <- NULL
    for (i in 1:dim(coords)[1]) {
      if (i == 1) {
        if (data$sex[i] == 22) {
          x <- coords$x[i] + unit(recth[, data$feature.name[i]], "points")
          y <-
            coords$y[i] + unit(rectv[, data$feature.name[i]], "points")
          id <- rep(i, dim(recth)[1])
        } else{
          if (data$sex[i] == 0) {
            x <- coords$x[i] + unit(rhombh[, data$feature.name[i]], "points")
            y <-
              coords$y[i] + unit(rhombv[, data$feature.name[i]], "points")
            id <- rep(i, dim(rhombh)[1])
          } else{
            x <- coords$x[i] + unit(circh[, data$feature.name[i]], "points")
            y <-
              coords$y[i] + unit(circv[, data$feature.name[i]], "points")
            id <- rep(i, dim(circh)[1])
          }
        }
      } else{
        if (data$sex[i] == 22) {
          x <-
            unit.c(x, coords$x[i] + unit(recth[, data$feature.name[i]], "points"))
          y <-
            unit.c(y, coords$y[i] + unit(rectv[, data$feature.name[i]], "points"))
          id <-
            c(id, rep(i, dim(recth)[1]))
        } else{
          if (data$sex[i] == 0) {
            x <-
              unit.c(x, coords$x[i] + unit(rhombh[, data$feature.name[i]], "points"))
            y <-
              unit.c(y, coords$y[i] + unit(rhombv[, data$feature.name[i]], "points"))
            id <-
              c(id, rep(i, dim(rhombh)[1]))
          } else{
            x <-
              unit.c(x, coords$x[i] + unit(circh[, data$feature.name[i]], "points"))
            y <-
              unit.c(y, coords$y[i] + unit(circv[, data$feature.name[i]], "points"))
            id <-
              c(id, rep(i, dim(circh)[1]))
            
          }
        }
      }
      # Feature status (Fill)
      
      # The Fill logic is modified to fill a color which intensity changes according to feature.value (0-1). 
      
      calculate_lighter_color_hex <- function(darker_color_hex, alpha) {
        # Convert the hex color to RGB
        darker_color <- col2rgb(darker_color_hex)
        
        # Assuming the background is white
        background_color <- c(255, 255, 255)
        
        # Calculate the lighter color in the normalized 0-1 range
        lighter_color <- alpha * darker_color + (1 - alpha) * background_color
        
        # Convert the RGB to hex
        lighter_color_hex <- rgb(lighter_color[1,], lighter_color[2,], lighter_color[3,], maxColorValue=255)
        
        return(tolower(lighter_color_hex))
      }
      
      if (is.na(data$feature.value[i])) {
        fill <- c(fill, na.colour)
      } else {
        fill <- c(fill, calculate_lighter_color_hex(data$feature.name[i], data$feature.value[i]))
      }
      
      # if (is.na(data$feature.value[i])) {
      #   fill <- c(fill, na.colour)
      # } else{
      #   if (data$feature.value[i]) {
      #     fill <- c(fill, data$feature.name[i])
      #   } else{
      #     fill <- c(fill, "#FFFFFF")
      #   }
      # }
      
      # Generic point aesthetics
      alpha <- c(alpha, data$alpha[i])
      stroke <- c(stroke, data$stroke[i])
      colour <- c(colour, data$colour[i])
    }
    
    # subset "non-alive"
    if (any(data$isdead != 0)) {
      status <- cbind(coords[data$isdead != 0, ],
                      data[data$isdead != 0, !(colnames(data) %in% c("x", "y"))])
      status$cex <- 2.25
    }
    # subset "adopted"
    if (any(data$adopted)) {
      adopted <- cbind(coords[data$adopted, ],
                       data[data$adopted, !(colnames(data) %in% c("x", "y"))])
      adopted$cex <- .5
    }
    # subset "pregnancy"
    if (any(data$pregnancy)) {
      pregnancy <- cbind(coords[data$pregnancy, ],
                         data[data$pregnancy, !(colnames(data) %in% c("x", "y"))])
      pregnancy$cex <- .5
    }
    
    offset <- unit(0.5, "points")
    # Draw
    obj <- polygonGrob(
      # subject
      x = x,
      y = y,
      id = id,
      default.units = "npc",
      gp = gpar(
        fill = fill,
        alpha = alpha,
        lwd = stroke,
        col = colour
      )
    )
    
    if (any(data$isdead != 0)) {
      obj <- gList(
        obj,
        pointsGrob(
          x = status$x,
          y = status$y,
          size = unit(7, "char"),
          gp = gpar(
            cex = status$cex,
            alpha = status$alpha,
            lwd = status$stroke,
            col = status$colour
          ),
          pch = status$isdead
        )
      )
    }
    
    if (any(data$pregnancy)) {
      obj <- gList(obj,
                   pointsGrob(
                     x = pregnancy$x,
                     y = pregnancy$y,
                     size = unit(7, "char"),
                     gp = gpar(
                       cex = pregnancy$cex,
                       alpha = pregnancy$alpha,
                       lwd = pregnancy$stroke,
                       col = pregnancy$colour
                     ),
                     pch = 65
                   ))
    }
    
    if (any(data$adopted)) {
      obj <- gList(obj,
                   # pointsGrob(
                   #   x = adopted$x,
                   #   y = adopted$y,
                   #   size = unit(7, "char"),
                   #   gp =gpar(cex=adopted$cex,alpha=adopted$alpha,
                   #            lwd=adopted$stroke, col=adopted$colour),
                   #   pch=80)
                   textGrob(
                     "[  ]",
                     x = adopted$x,
                     y = adopted$y,
                     vjust = 0.4,
                     gp = gpar(
                       cex = adopted$cex * 2.75,
                       alpha = adopted$alpha,
                       lwd = adopted$stroke,
                       col = adopted$colour
                     )
                   ))
    }
    obj
  },
  draw_key = function (data, params, size)  {
    if (data$isdead == 0) {
      shape <- ifelse(data$sex != 0, data$sex, 23)
    } else{
      shape <- data$isdead
    }
    pointsGrob(
      0.5,
      0.5,
      pch = shape,
      gp = grid::gpar(
        fill = data$feature.name,
        alpha = data$alpha,
        lwd = data$stroke,
        col = data$colour
      )
    )
  }
)

#' @describeIn geom_pedigreepoint
#' @param na.colour \strong{Colour to fill symbols with if feature is \code{NA}}.
#' @param size \strong{Symbol size}.
#' @importFrom ggplot2 layer
#' @examples
#' tmp<-data.frame(x=c(1,1,2),y=c(1,2,1),sex=as.factor(c(1,2,1)),status=as.factor(c(2,1,1)),feature.name=as.factor(c("test","test","test")),feature.value=c(TRUE,FALSE,TRUE))
#' ggplot()+
#'   geom_pedigreepoint(tmp,aes(x=x,y=y,sex=sex, isdead=status, feature.name=feature.name, feature.value=feature.value),
#'                        size=7,
#'                        na.colour="#808080")
#' @export
geom_pedigreepoint1 <-
  function(data = NULL,
           mapping = NULL,
           stat = "identity",
           position = "identity",
           ... ,
           na.rm = FALSE,
           na.colour = "#080808",
           size = 5,
           show.legend = NA,
           inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomPedigreePoint,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        na.colour = na.colour,
        size = size,
        ...
      )
    )
  }


#' Scale constructors
#' 
#' Discrete scale constructors for \link{geom_pedigreepoint}
#' @param ... Additional parameters passed on to \link[ggplot2]{discrete_scale}.
#' @inheritParams ggplot2::scale_shape_manual
#' @param na.value What aesthetic value should the missing values be displayed as?
#' @param set  A palette name from the lists in \link[RColorBrewer]{brewer.pal}.
#' @param main.feature.black Should the main (first) feature always be displayed as black?
#' @param name Legend title.
#' @name Scales
NULL

#' @describeIn Scales Scale constructor for the \var{feature.name} argument.
#' @importFrom ggplot2 discrete_scale
#' @importFrom RColorBrewer brewer.pal
#' @export
scale_feature.name_manual<-function(...,
                                    values = NULL,
                                    na.value = "grey50",
                                    main.feature.black=TRUE,
                                    name="Features") {
  discrete_scale(
    aesthetics = "feature.name", 
    scale_name = "feature.name_d", 
    palette = function(x){
      palette<-values
      if(main.feature.black){
        palette<-c("#000000",palette[-1])
      }else{
        palette
      }
    },
    name=name,
    drop=F,
    na.value = na.value,
    ...
  )
}

split_by_feature <- ggped:::split_by_feature

# `dfalign.pedigree` has a bug which calculates the `family.centerpoint` wrong when the same male breeds with two females. The follow code fixes it. 

dfalign.pedigree <- function (ped, chrtype = "autosome", packed = TRUE, width = 10, 
                              align = TRUE, hints = ped$hints) 
{
  if (!(class(ped) %in% c("pedigree"))) {
    stop("Input 'ped' must be of class 'pedigree'.")
  }
  if (nrow(as.data.frame(ped)) < 1) {
    stop("Input 'ped' must have at least one entry.")
  }
  if (!requireNamespace("kinship2", quietly = TRUE)) {
    stop("The 'kinship2' package is required but not installed. Please install it using: install.packages('kinship2')")
  }
  struct <- align.pedigree(ped, packed = packed, width = width, 
                           align = align, hints = hints)
  ckall <- ped$id[is.na(match(ped$id, ped$id[struct$nid[struct$nid != 
                                                          0]]))]
  if (length(ckall > 0)) 
    message("Did not include the following subject(s):", 
            paste("\n", ckall), "\n Reason: No evidence for relation to other subjects in the tree.\n")
  nvalid <- length(struct$nid[struct$nid != 0])
  out <- data.frame(ID = numeric(nvalid), Name = numeric(nvalid), 
                    family = numeric(nvalid), mate.id = numeric(nvalid), 
                    mate.helper = numeric(nvalid), mate.type = numeric(nvalid), 
                    twin.id = numeric(nvalid), twin.type = numeric(nvalid), 
                    dad.id = numeric(nvalid), mom.id = numeric(nvalid), sex = numeric(nvalid), 
                    status = numeric(nvalid), y = numeric(nvalid), x = numeric(nvalid), 
                    mate.centerpoint = numeric(nvalid), family.centerpoint = numeric(nvalid), 
                    twin.centerpoint = numeric(nvalid), kinship = numeric(nvalid), 
                    stringsAsFactors = T)
  ks <- kinship(ped, chrtype)
  n = 1
  mate.id = 1
  twin.id = 1
  make.fam.unique <- 0
  for (i in 1:length(struct$n)) {
    for (j in 1:struct$n[i]) {
      out$ID[n] <- struct$nid[i, j]
      out$Name[n] <- ped$id[out$ID[n]]
      out$sex[n] <- ped$sex[out$ID[n]]
      out$dad.id[n] <- ped$findex[out$ID[n]]
      out$mom.id[n] <- ped$mindex[out$ID[n]]
      out$status[n] <- if (length(ped$status[out$ID[n]]) > 
                           0) {
        ped$status[out$ID[n]]
      }
      else {
        0
      }
      if (!is.null(ped$affected)) {
        if (is.vector(ped$affected)) {
          out[n, "affected"] <- ped$affected[out$ID[n]]
        }
        if (is.matrix(ped$affected)) {
          out[n, colnames(ped$affected)] <- ped$affected[out$ID[n], 
          ]
        }
      }
      else {
        out[n, "affected"] <- F
      }
      out$y[n] <- i
      out$x[n] <- struct$pos[i, j]
      if (struct$fam[i, j] > 0) {
        out$family[n] <- struct$fam[i, j] + make.fam.unique
      }
      else {
        out$family[n] <- struct$fam[i, j]
      }
      out$mate.type[n] <- struct$spouse[i, j]
      if (n > 1 && (out$mate.type[n - 1] > 0)) {
        if (length(out$mate.id[n - 2]) != 0 && !is.na(out$mate.id[n - 
                                                                  2]) && !is.na(out$mate.id[n - 1])) {
          if (out$mate.id[n - 1] == out$mate.id[n - 2]) {
            out$mate.id[n - 1] <- mate.id - 1
            out$mate.id[n] <- mate.id - 0.5
            out$mate.helper[n - 1] <- out$mate.type[n - 
                                                      1]
            out$mate.helper[n] <- out$mate.type[n - 1]
          }
          else {
            out$mate.id[n - 1] <- mate.id
            out$mate.id[n] <- mate.id
            out$mate.helper[n - 1] <- out$mate.type[n - 
                                                      1]
            out$mate.helper[n] <- out$mate.type[n - 1]
          }
        }
        else {
          out$mate.id[n - 1] <- mate.id
          out$mate.id[n] <- mate.id
          out$mate.helper[n - 1] <- out$mate.type[n - 
                                                    1]
          out$mate.helper[n] <- out$mate.type[n - 1]
        }
        out$mate.centerpoint[n - 1] <- mean(c(out$x[n], 
                                              out$x[n - 1]))
        out$mate.centerpoint[n] <- NA
        out$kinship[n - 1] <- ks[as.character(out$Name[n - 
                                                         1]), as.character(out$Name[n])]
        mate.id <- mate.id + 1
      }
      else {
        out$mate.id[n] <- NA
        out$mate.helper[n] <- NA
        out$mate.centerpoint[n] <- NA
      }
      if ("twins" %in% names(struct)) {
        out$twins[n] <- struct$twins[i, j]
        if (n > 1 && (out$twins[n - 1] > 0)) {
          if (length(out$twin.id[n - 2]) != 0 && !is.na(out$twin.id[n - 
                                                                    2]) && !is.na(out$twin.id[n - 1])) {
          }
          if (length(out$twin.id[n - 2]) != 0 && !is.na(out$twin.id[n - 
                                                                    2]) && !is.na(out$twin.id[n - 1])) {
            if (out$twin.id[n - 1] == out$twin.id[n - 
                                                  2]) {
              out$twin.id[n - 1] <- twin.id - 1
              out$twin.id[n] <- twin.id - 0.5
              out$twin.type[n - 1] <- out$twins[n - 1]
              out$twin.type[n] <- out$twins[n - 1]
            }
            else {
              out$twin.id[n - 1] <- twin.id
              out$twin.id[n] <- twin.id
              out$twin.type[n - 1] <- out$twins[n - 1]
              out$twin.type[n] <- out$twins[n - 1]
            }
          }
          else {
            out$twin.id[n - 1] <- twin.id
            out$twin.id[n] <- twin.id
            out$twin.type[n - 1] <- out$twins[n - 1]
            out$twin.type[n] <- out$twins[n - 1]
          }
          out$twin.centerpoint[n - 1] <- mean(c(out$x[n], 
                                                out$x[n - 1]))
          out$twin.centerpoint[n] <- out$twin.centerpoint[n - 
                                                            1]
          out$kinship[n - 1] <- ks[as.character(out$Name[n - 
                                                           1]), as.character(out$Name[n])]
          twin.id <- twin.id + 1
        }
        else {
          out$twin.id[n] <- NA
          out$twin.type[n] <- NA
          out$twin.centerpoint[n] <- NA
        }
      }
      else {
        out$twin.id[n] <- NA
        out$twin.type[n] <- NA
        out$twin.centerpoint[n] <- NA
      }
      if (out$kinship[n - 1] == 0 && !is.na(out$mate.helper[n])) {
        out$kinship[n - 1] <- out$mate.helper[n] - 1
      }
      n = n + 1
    }
    make.fam.unique <- make.fam.unique + max(struct$fam[i, 
    ])
  }
  if (is.vector(ped$affected)) {
    out$affected <- as.logical(out$affected)
  }
  else {
    for (i in colnames(ped$affected)) {
      out[, i] <- as.logical(out[, i])
    }
  }
  out$mate.type <- as.factor(out$mate.type)
  out$mate.helper <- NULL
  out$family <- as.factor(out$family)
  out$sex <- as.factor(out$sex)
  out$status <- as.factor(out$status)
  # for (i in levels(out$family)) {
  #     out$family.centerpoint[out$ID %in% c(unique(out$mom.id[out$family == 
  #         i]), unique(out$dad.id[out$family == i]))] <- mean(out$x[out$family == 
  #         i], na.rm = T)
  # }
  
  for (i in levels(out$family)) {
    # All pups must have unique mom and dad, but not all mom and dad have a unique family id.
    # The work-around here finds the correct mom and dad and matches `mate.id`. 
    mateIds <- out$mate.id[out$ID %in% c(unique(out$mom.id[out$family ==
                                                             i]), unique(out$dad.id[out$family == i]))]
    freq_table <- table(mateIds)
    mateId <- mateIds[which.max(freq_table)]
    
    out$family.centerpoint[out$mate.id == mateId] <- mean(out$x[out$family ==
                                                                  i], na.rm = T)
  }
  out$family[out$family == 0] <- NA
  out
}

# Define ggplot theme
theme_yuan <- function(baseFontSize = 10, baseLineWidth = 0.5) {
  font <- "sans"
  
  theme_classic() %+replace%
    
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_blank(), 
      
      axis.title = element_text(
        family = font,
        face = "bold",
        size = baseFontSize
      ),
      
      axis.text = element_text(
        family = font,
        face = "bold",
        size = baseFontSize * 0.8
      ),
      
      legend.title = element_text(
        family = font,
        face = "bold",
        size = baseFontSize
      ),
      
      legend.text = element_text(
        family = font,
        face = "bold",
        size = baseFontSize * 0.8
      ),
      
      axis.line = element_line(color = "black", linewidth = baseLineWidth),
      axis.ticks = element_line(color = "black", linewidth = baseLineWidth),
      axis.ticks.length = unit(baseLineWidth * 2, "mm")
    )
}

subset_descendants <- function(df, identifier, id_col = "Physical.Tag", father_id_col = "Sire.Tag", mother_id_col = "Dam.Tag", include.breeder = TRUE) {
  # Function to recursively find all descendants of a given mouse
  get_descendants <- function(current_ids) {
    if (length(current_ids) == 0) {
      return(current_ids)
    } else {
      # Find direct children of current_ids
      children <- df[[id_col]][df[[father_id_col]] %in% current_ids | df[[mother_id_col]] %in% current_ids]
      return(c(current_ids, get_descendants(children)))
    }
  }
  
  # Start the recursive search from the given identifier
  descendants <- get_descendants(identifier)
  descendants <- unique(descendants)
  
  if (include.breeder) {
    descendants <- unique(c(df[[father_id_col]][df[[id_col]] %in% descendants], df[[mother_id_col]][df[[id_col]] %in% descendants], descendants))
  }
  
  # Subset the dataframe to include only the descendants (and breeders if include.breeder is TRUE)
  subset_df <- df[df[[id_col]] %in% descendants, ]
  return(subset_df)
}

# Function to find all ancestors of a given mouse
find_ancestors <- function(mouse_id, mouse_colony) {
  # Initialize an empty vector to store ancestors
  ancestors <- c()
  
  # Find the current mouse's row in the dataframe
  current_mouse <- mouse_colony[mouse_colony$Physical.Tag == mouse_id,]
  
  # Check if the mouse has a sire (father)
  if (!is.na(current_mouse$Sire.Tag) && current_mouse$Sire.Tag != "") {
    # Add the sire to the ancestors list
    ancestors <- c(ancestors, current_mouse$Sire.Tag)
    # Recursively find the sire's ancestors
    ancestors <- c(ancestors, find_ancestors(current_mouse$Sire.Tag, mouse_colony))
  }
  
  # Check if the mouse has a dam (mother)
  if (!is.na(current_mouse$Dam.Tag) && current_mouse$Dam.Tag != "") {
    # Add the dam to the ancestors list
    ancestors <- c(ancestors, current_mouse$Dam.Tag)
    # Recursively find the dam's ancestors
    ancestors <- c(ancestors, find_ancestors(current_mouse$Dam.Tag, mouse_colony))
  }
  
  # Return the unique ancestors to avoid duplicates
  return(unique(ancestors))
}

# Function to find all ancestors and their offspring of a given mouse
find_ancestors_and_offspring <- function(mouse_id, mouse_colony) {
  # Initialize a vector to store ancestors and offspring, including the queried ID itself
  relatives <- c(mouse_id)

  # Recursive helper function to find ancestors
  find_ancestors_recursive <- function(current_id) {
    # Initialize an empty vector to store current ancestors
    ancestors <- c()

    # Find the current mouse's row in the dataframe
    current_mouse <- mouse_colony[mouse_colony$Physical.Tag == current_id,]

    # Check if the mouse has a sire (father)
    if (!is.na(current_mouse$Sire.Tag) && current_mouse$Sire.Tag != "") {
      # Add the sire to the ancestors list
      ancestors <- c(ancestors, current_mouse$Sire.Tag)
      # Recursively find the sire's ancestors
      ancestors <- c(ancestors, find_ancestors_recursive(current_mouse$Sire.Tag))
    }

    # Check if the mouse has a dam (mother)
    if (!is.na(current_mouse$Dam.Tag) && current_mouse$Dam.Tag != "") {
      # Add the dam to the ancestors list
      ancestors <- c(ancestors, current_mouse$Dam.Tag)
      # Recursively find the dam's ancestors
      ancestors <- c(ancestors, find_ancestors_recursive(current_mouse$Dam.Tag))
    }

    # Return the ancestors
    return(ancestors)
  }

  # Add ancestors of the queried mouse to the relatives list
  relatives <- c(relatives, find_ancestors_recursive(mouse_id))

  # Function to find offspring from breeder pairs
  find_offspring <- function(parent_id, mouse_colony) {
    offspring <- unique(c(mouse_colony[mouse_colony$Sire.Tag == parent_id, "Physical.Tag"],
                          mouse_colony[mouse_colony$Dam.Tag == parent_id, "Physical.Tag"]))
    offspring <- offspring[!is.na(offspring) & offspring != ""]
    return(offspring)
  }

  # Loop through each ancestor to find their offspring
  for (ancestor_id in relatives) {
    relatives <- c(relatives, find_offspring(ancestor_id, mouse_colony))
  }

  # Return the unique relatives to avoid duplicates
  return(unique(relatives))
}

trace_descendants_plus_ancestors_and_sibs <- function(mouse_id, mouse_colony,
                                                      id_col   = "Physical.Tag",
                                                      sire_col = "Sire.Tag",
                                                      dam_col  = "Dam.Tag",
                                                      return_rows = FALSE) {
  df <- mouse_colony
  
  # Normalize missing values: "" or whitespace -> NA for key columns
  for (cc in c(id_col, sire_col, dam_col)) {
    if (!cc %in% names(df)) stop(sprintf("Column '%s' not found in mouse_colony.", cc))
    df[[cc]] <- ifelse(is.na(df[[cc]]) | trimws(df[[cc]]) == "", NA, as.character(df[[cc]]))
  }
  
  # Parents of an ID (could be multiple rows, take union)
  get_parents <- function(id) {
    rows <- df[df[[id_col]] == id, , drop = FALSE]
    if (nrow(rows) == 0) return(character(0))
    parents <- c(rows[[sire_col]], rows[[dam_col]])
    parents <- parents[!is.na(parents)]
    unique(parents)
  }
  
  # Children of a parent ID (as sire OR dam)
  get_children <- function(parent_id) {
    kids <- df[df[[sire_col]] == parent_id | df[[dam_col]] == parent_id, id_col]
    kids <- kids[!is.na(kids)]
    unique(as.character(kids))
  }
  
  # Siblings of an ID:
  # Defined as anyone sharing sire OR dam with the target (includes half-sibs if only one parent matches).
  # If both parents are known, this returns union across both parents (still includes half-sibs).
  get_siblings <- function(id) {
    parents <- get_parents(id)
    if (length(parents) == 0) return(character(0))
    sibs <- unique(unlist(lapply(parents, get_children), use.names = FALSE))
    sibs <- sibs[!is.na(sibs)]
    setdiff(unique(sibs), id)  # siblings only; remove self
  }
  
  # 1) Ancestors of mouse_id (upward recursion)
  find_ancestors <- function(start_id) {
    visited <- character(0)
    out <- character(0)
    
    rec <- function(id) {
      if (is.na(id) || id %in% visited) return()
      visited <<- c(visited, id)
      
      parents <- get_parents(id)
      new_parents <- setdiff(parents, out)
      out <<- c(out, new_parents)
      
      for (p in parents) rec(p)
    }
    
    rec(start_id)
    unique(out)
  }
  
  ancestors <- find_ancestors(mouse_id)
  
  # 2) Siblings of each ancestor (collateral upward)
  ancestor_sibs <- unique(unlist(lapply(ancestors, get_siblings), use.names = FALSE))
  
  # 3) Descendants of mouse_id only (downward recursion)
  find_descendants <- function(start_id) {
    visited <- character(0)
    out <- character(0)
    
    rec <- function(id) {
      if (is.na(id) || id %in% visited) return()
      visited <<- c(visited, id)
      
      kids <- get_children(id)
      new_kids <- setdiff(kids, out)
      out <<- c(out, new_kids)
      
      for (k in kids) rec(k)
    }
    
    rec(start_id)
    unique(out)
  }
  
  descendants <- find_descendants(mouse_id)
  
  # Combine:
  relatives <- unique(c(mouse_id, descendants, ancestors, ancestor_sibs))
  
  if (!return_rows) {
    return(relatives)
  } else {
    return(df[df[[id_col]] %in% relatives, , drop = FALSE])
  }
}



#' Get All Ancestor IDs for a Given Mouse
#'
#' Recursively traverses Sire.Tag and Dam.Tag to collect all ancestors
#' of a specified mouse from colony data.
#'
#' @param df Data frame of colony data with columns: Physical.Tag, Sire.Tag, Dam.Tag
#' @param mouse_id Character. The Physical.Tag of the mouse to query.
#' @param include_self Logical. Whether to include the query mouse in the output. Default FALSE.
#' @return A character vector of unique ancestor Physical.Tag values.

get_ancestors <- function(df, mouse_id, include_self = FALSE) {
  
  # Ensure key columns are character for consistent matching
  df$Physical.Tag <- as.character(df$Physical.Tag)
  df$Sire.Tag    <- as.character(df$Sire.Tag)
  df$Dam.Tag     <- as.character(df$Dam.Tag)
  
  visited <- character(0)
  
  traverse <- function(id) {
    # Stop if NA, empty, already visited, or not in the data
    if (is.na(id) || id == "" || id == "NA" || id %in% visited) return()
    visited <<- c(visited, id)
    
    row <- df[df$Physical.Tag == id, , drop = FALSE]
    if (nrow(row) == 0) return()  # founder / external mouse not in colony
    
    # Use first match if duplicates exist
    sire <- row$Sire.Tag[1]
    dam  <- row$Dam.Tag[1]
    
    if (!is.na(sire) && sire != "" && sire != "NA") traverse(sire)
    if (!is.na(dam)  && dam  != "" && dam  != "NA") traverse(dam)
  }
  
  traverse(mouse_id)
  
  if (!include_self) {
    visited <- setdiff(visited, mouse_id)
  }
  
  return(visited)
}

