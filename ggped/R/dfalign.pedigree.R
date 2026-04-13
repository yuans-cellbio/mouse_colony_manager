#' dfalign.pedigree
#'
#' Calculate pedigree drawing coordinates and join them with pedigree data.
#'
#' This function calculates pedigree drawing coordinates using the \code{\link[kinship2]{align.pedigree}} function and combines them with the provided pedigree data. The resulting data frame can be used to plot pedigrees with \code{\link{ggplot2}} or specific pedigree plotting functions like \code{\link{ggdraw.pedigree}}.
#'
#' @inheritParams kinship2::align.pedigree
#' @inheritParams kinship2::kinship
#' @importFrom kinship2 align.pedigree
#' @importFrom cli cli_abort cli_alert_info
#' @return A \code{data.frame} containing pedigree drawing coordinates and data with the following columns:
#' \describe{
#'   \item{ID}{The numeric ID of each subject.}
#'   \item{Name}{The name of each subject.}
#'   \item{family}{The numeric ID of each family.}
#'   \item{mate.id}{The numeric ID of each mating.}
#'   \item{mate.helper}{Numeric. Information about the mating.}
#'   \item{mate.type}{Factor. 1 = subject plotted to the immediate right is a spouse, 2 = subject plotted to the immediate right is an inbred spouse, 0 = not a spouse.}
#'   \item{twin.id}{The numeric ID of each twin pair.}
#'   \item{twin.type}{Numeric. Information about the twin pair.}
#'   \item{dad.id, mom.id}{Identification variables for father and mother. Founders' parents should be coded as NA.}
#'   \item{sex}{Gender of the individual noted in 'id'. Either character ("male","female","Male","Female","M","F") or numeric (1="male", 2="female") data is understood by downstream function \code{\link{ggdraw.pedigree}}.}
#'   \item{status}{Numeric. Life status: 0=alive/missing, 1=dead, 2=stillbirth, 3=miscarriage.}
#'   \item{x, y}{Drawing coordinates of each subject.}
#'   \item{mate.centerpoint, family.centerpoint, twin.centerpoint}{Centerpoints for mating, offspring, and twins, respectively, for drawing the tree.}
#'   \item{kinship}{Numeric. Kinship between mating individuals as calculated by the \code{\link{kinship}} function.}
#'   \item{...}{Further columns of type logical, containing affected indicators.}
#' }
#' Each row represents one subject
#' @examples
#' data(minnbreast)
#' bpeds <- with(minnbreast, pedigree(id, fatherid, motherid, sex, affected=proband, famid=famid))
#' bped.id8 <- bpeds['8']
#' df<-dfalign.pedigree(bped.id8)
#' @seealso \link[kinship2]{kinship}, \link[kinship2]{align.pedigree}, \link{ggdraw.pedigree}
#' @export
dfalign.pedigree <-
  function(ped,
           chrtype = "autosome",
           packed = TRUE,
           width = 10,
           align = TRUE,
           hints = ped$hints) {
    
    if (!requireNamespace("kinship2", quietly = TRUE)) {
      cli::cli_abort("The 'kinship2' package is required but not installed. Please install it using: install.packages('kinship2')")
    }
    
    # Validate 'ped' parameter
    if (!(class(ped) %in% c("pedigree"))) {
      cli::cli_abort("Input 'ped' must be of class 'pedigree'. Provided class: {.val {class(ped)}}")
    }
    
    # Validate 'chrtype' parameter
    if (!is.character(chrtype) || !chrtype %in% c("autosome", "x", "y", "mt")) {
      cli::cli_abort("Parameter 'chrtype' must be one of 'autosome', 'x', 'y', or 'mt'. Provided value: {.val {chrtype}}")
    }
    
    # Validate 'packed' parameter
    if (!is.logical(packed)) {
      cli::cli_abort("Parameter 'packed' must be a logical value (TRUE or FALSE). Provided type: {.val {class(packed)}}")
    }
    
    # Validate 'width' parameter
    if (!is.numeric(width) || width <= 0) {
      cli::cli_abort("Parameter 'width' must be a positive numeric value. Provided value: {.val {width}} of type {.val {class(width)}}")
    }
    
    # Validate 'align' parameter
    if (!is.logical(align)) {
      cli::cli_abort("Parameter 'align' must be a logical value (TRUE or FALSE). Provided type: {.val {class(align)}}")
    }
    
    # Validate 'hints' parameter
    if (!is.null(hints) && !is.list(hints)) {
      cli::cli_abort("Parameter 'hints' must be a list or NULL. Provided type: {.val {class(hints)}}")
    }
    
    # Check if 'ped' has at least one entry
    if (nrow(as.data.frame(ped)) < 1) {
      cli::cli_abort("Input 'ped' must have at least one entry. The provided pedigree has {.val {nrow(as.data.frame(ped))}} entries.")
    }
    
    struct <-
      align.pedigree(
        ped,
        packed = packed,
        width = width,
        align = align,
        hints = hints
      )
    
    ckall <-
      ped$id[is.na(match(ped$id, ped$id[struct$nid[struct$nid != 0]]))]
    if (length(ckall > 0)) {
      cli::cli_alert_info(
        "Did not include the following subject(s): {.val {paste(ckall, collapse = ', ')}}. Reason: No evidence for relation to other subjects in the tree."
      )
    }
    
    nvalid <-
      length(struct$nid[struct$nid != 0])
    out <- data.frame(
      ID = numeric(nvalid),
      Name = numeric(nvalid),
      family = numeric(nvalid),
      mate.id = numeric(nvalid),
      mate.helper = numeric(nvalid),
      mate.type = numeric(nvalid),
      twin.id = numeric(nvalid),
      twin.type = numeric(nvalid),
      dad.id = numeric(nvalid),
      mom.id = numeric(nvalid),
      sex = numeric(nvalid),
      status = numeric(nvalid),
      y = numeric(nvalid),
      x = numeric(nvalid),
      mate.centerpoint = numeric(nvalid),
      family.centerpoint = numeric(nvalid),
      twin.centerpoint = numeric(nvalid),
      kinship = numeric(nvalid),
      stringsAsFactors = T
    )
    
    ks <- kinship(ped, chrtype)
    n = 1 # count up subjects
    mate.id = 1 # count up matings
    twin.id = 1 # count up twin pairs
    make.fam.unique <- 0
    for (i in 1:length(struct$n)) {
      #for every row of the pedigree
      for (j in 1:struct$n[i]) {
        # do for each subject of that row
        out$ID[n] <- struct$nid[i, j]
        out$Name[n] <- ped$id[out$ID[n]]
        out$sex[n] <- ped$sex[out$ID[n]]
        out$dad.id[n] <- ped$findex[out$ID[n]]
        out$mom.id[n] <- ped$mindex[out$ID[n]]
        out$status[n] <-
          if (length(ped$status[out$ID[n]]) > 0) {
            # life status, set to life=0 if not present
            ped$status[out$ID[n]]
          } else{
            0
          }
        if (!is.null(ped$affected)) {
          # affected status, if not given make one column labeling all as unaffected
          if (is.vector(ped$affected)) {
            out[n, "affected"] <- ped$affected[out$ID[n]]
          }
          if (is.matrix(ped$affected)) {
            out[n, colnames(ped$affected)] <- ped$affected[out$ID[n],]
          }
        } else{
          out[n, "affected"] <- F
        }
        out$y[n] <- i
        out$x[n] <- struct$pos[i, j]
        if (struct$fam[i, j] > 0) {
          # unique family identifier, necessary as align.ped seems to ambiguously name families among generations
          out$family[n] <- struct$fam[i, j] + make.fam.unique
        } else{
          out$family[n] <- struct$fam[i, j]
        }
        out$mate.type[n] <- struct$spouse[i, j]
        if (n > 1 && (out$mate.type[n - 1] > 0)) {
          # mating info
          if (length(out$mate.id[n - 2]) != 0 &&
              !is.na(out$mate.id[n - 2]) &&
              !is.na(out$mate.id[n - 1])) {
            # mating w more than one neigbour can only occur after the second plotted subject
            if (out$mate.id[n - 1] == out$mate.id[n - 2]) {
              # special case: individual participates/d in more than one mating
              out$mate.id[n - 1] <- mate.id - 1
              out$mate.id[n] <- mate.id - 0.5
              out$mate.helper[n - 1] <- out$mate.type[n - 1]
              out$mate.helper[n] <- out$mate.type[n - 1]
            } else{
              out$mate.id[n - 1] <- mate.id
              out$mate.id[n] <- mate.id
              out$mate.helper[n - 1] <- out$mate.type[n - 1]
              out$mate.helper[n] <- out$mate.type[n - 1]
            }
          } else{
            out$mate.id[n - 1] <- mate.id
            out$mate.id[n] <- mate.id
            out$mate.helper[n - 1] <- out$mate.type[n - 1]
            out$mate.helper[n] <- out$mate.type[n - 1]
          }
          out$mate.centerpoint[n - 1] <-
            mean(c(out$x[n], out$x[n - 1]))
          out$mate.centerpoint[n] <- NA
          out$kinship[n - 1] <-
            ks[as.character(out$Name[n - 1]), as.character(out$Name[n])]
          mate.id <- mate.id + 1
        } else{
          out$mate.id[n] <- NA
          out$mate.helper[n] <- NA
          out$mate.centerpoint[n] <- NA
        }
        
        if ("twins" %in% names(struct)) {
          out$twins[n] <- struct$twins[i, j]
          if (n > 1 && (out$twins[n - 1] > 0)) {
            # twin info
            if (length(out$twin.id[n - 2]) != 0 &&
                !is.na(out$twin.id[n - 2]) &&
                !is.na(out$twin.id[n - 1])) {
              
            }
            
            if (length(out$twin.id[n - 2]) != 0 &&
                !is.na(out$twin.id[n - 2]) &&
                !is.na(out$twin.id[n - 1])) {
              # mating w more than one neighbor can only occur after the second plotted subject
              if (out$twin.id[n - 1] == out$twin.id[n - 2]) {
                # special case: individual participates/d in more than one mating
                out$twin.id[n - 1] <- twin.id - 1
                out$twin.id[n] <- twin.id - 0.5
                out$twin.type[n - 1] <- out$twins[n - 1]
                out$twin.type[n] <- out$twins[n - 1]
              } else{
                out$twin.id[n - 1] <- twin.id
                out$twin.id[n] <- twin.id
                out$twin.type[n - 1] <- out$twins[n - 1]
                out$twin.type[n] <- out$twins[n - 1]
              }
            } else{
              out$twin.id[n - 1] <- twin.id
              out$twin.id[n] <- twin.id
              out$twin.type[n - 1] <- out$twins[n - 1]
              out$twin.type[n] <- out$twins[n - 1]
            }
            out$twin.centerpoint[n - 1] <-
              mean(c(out$x[n], out$x[n - 1]))
            out$twin.centerpoint[n] <- out$twin.centerpoint[n - 1]
            out$kinship[n - 1] <-
              ks[as.character(out$Name[n - 1]), as.character(out$Name[n])]
            twin.id <- twin.id + 1
          } else{
            out$twin.id[n] <- NA
            out$twin.type[n] <- NA
            out$twin.centerpoint[n] <- NA
          }
        } else{
          out$twin.id[n] <- NA
          out$twin.type[n] <- NA
          out$twin.centerpoint[n] <- NA
        }
        
        if (out$kinship[n - 1] == 0 && !is.na(out$mate.helper[n])) {
          out$kinship[n - 1] <- out$mate.helper[n] - 1
        }
        n = n + 1
      }
      make.fam.unique <- make.fam.unique + max(struct$fam[i,])
    }
    
    if (is.vector(ped$affected)) {
      out$affected <- as.logical(out$affected)
    } else{
      for (i in colnames(ped$affected)) {
        out[, i] <- as.logical(out[, i])
      }
    }
    
    out$mate.type <- as.factor(out$mate.type)
    out$mate.helper <- NULL#as.factor(out$mate.helper)
    out$family <- as.factor(out$family)
    out$sex <- as.factor(out$sex)
    out$status <- as.factor(out$status)
    
    for (i in levels(out$family)) {
      # make centre points of families
      out$family.centerpoint[out$ID %in% c(unique(out$mom.id[out$family == i]), unique(out$dad.id[out$family ==
                                                                                                  i]))] <-
        mean(out$x[out$family == i], na.rm = T)
    }
    
    #print(out$ID!=0 & out$family!=0 & out$mate.id!=0)
    out$family[out$family == 0] <- NA
    out$twin.type<-as.factor(out$twin.type)
    out
    
  }
