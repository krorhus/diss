
################################################################################
############################# ALFREDO GEOMEASURE ###############################
################################################################################

## Load utilities
library(sf)

## Change the path to your own directory
load("Prj.RData")
load("L_Grid.RData")
load("Wi_Grid.RData")

L_Grid <- st_transform(st_as_sf(L_Grid),crs = Prj@projargs)
Wi_Grid <- st_transform(st_as_sf(Wi_Grid),crs = Prj@projargs)

## Funtion 01. Import spatial data with additional information
#' @title readG
#' @param dsn: Same as 'st_read'
#' @param layer: Same as 'st_read'
#' @param typol: Shape to import. c("i","s","r").
#' @return: Imports the data with additional setting its prj to the grid's prj for
#' measuring. It loads all three shapes and it records the number of sides
#' @export

######   ----  START FUNCTION 01

##Rationale: Loads all three geometrics
readG <- function(dsn,layer,typol = "i"){

  if (typol == "i"){
    ## Read the file
    pol <- suppressWarnings(st_read(dsn = dsn, layer = layer))
    pol <- st_transform(pol, crs = Prj@projargs)
    n_sides <- nrow(pol$geometry[[1]][[1]])-1
    if (n_sides > 5) warning("i.pol with more than 5 sides unadvised")
    pol$type <- "i.pol"
    pol$n_sides <- n_sides
  }

  if (typol == "s"){
    pol <- suppressWarnings(st_read(dsn = dsn, layer = layer))
    pol <- st_transform(pol, crs = Prj@projargs)
    pol$type <- "s.pol"
  }

  if (typol == "r"){
    pol <- suppressWarnings(st_read(dsn = dsn, layer = layer))
    pol <- st_transform(pol, crs = Prj@projargs)
    pol$type <- "r.pol"
    pol$n_sides <- "Complex"
  }

  return(pol)
}

######   ----  END FUNCTION 01

## Funtion 02. Centers the object on the Wi_Grid and L_Grid axes
#' @title centr
#' @param x: Object to be centered
#' @return: The centred object
#' @export

######   ----  START FUNCTION 02

centr <- function(x){

  if (is.null(x$geometry) == FALSE){
    c <- suppressWarnings(st_centroid(x))

    xtrns <- 5-c$geometry[[1]][[1]]
    ytrns <- 5-c$geometry[[1]][[2]]

    x$geometry[[1]][[1]][,1] <- x$geometry[[1]][[1]][,1] + xtrns
    x$geometry[[1]][[1]][,2] <- x$geometry[[1]][[1]][,2] + ytrns
  } else if (is.null(x$x) == FALSE){
    c <- suppressWarnings(st_centroid(x))

    xtrns <- 5-c$x[[1]][[1]]
    ytrns <- 5-c$x[[1]][[2]]

    x$x[[1]][[1]][,1] <- x$x[[1]][[1]][,1] + xtrns
    x$x[[1]][[1]][,2] <- x$x[[1]][[1]][,2] + ytrns

  } else {
    stop("geometries undefined")
  }

  return(x)
}

######   ----  END FUNCTION 02


## Funtion 03. Checks the orientation of the artifact.
#' @title is_oriented
#' @param x: Object to check
#' @param axis: Is the artifact located along a vertical ("v") or horizontal ("h") axis.
#' Default is "h"
#' @return: Returns a vector indicating the oriented side.
#' @details: This function assumes that the piece is distributed
#' along an axis. From there, and following most archaeological conventions,
#' it returns the orientation considering the smaller half (or the point) of
#' object. For example, if the object is pointing 'right' or 'up', it will return
#' 'right' or 'up' respectively and so on. If the object has no specific point,
#' the smaller half is considered the point. If reliability (Reliab) is low, the
#' function might show innapropriate behaviour.
#' The functions measures ines at the right and left sides of the polygon on the
#' centred grid. Adds the lines at each side. If the total length is longer at one side,
#' the orienation is to the other (smaller) side.
#' @export

######   ----  START FUNCTION 03

is_oriented <- function(x, axis = "h"){

  ## Centre the object
  o <- centr(x)
  if (axis == "h"){
    ## Assign object's crs to grid
    st_crs(L_Grid) <- st_crs(o)

    ## Create intersections
    inters <- suppressWarnings(st_intersection(o,L_Grid))
    inters <- as.numeric(st_length(inters[[length(inters)]]))

    ## Create differences
    differ <- rep(0,length(inters)-1)
    for (i in 2:length(inters)){
      differ[i-1] <- inters[i]-inters[i-1]
    }

    ## Establish trend
    neg <- length(which(differ<0))
    pos <- length(which(differ>0))

    ## Establish trend, where 1 is positive trend and 0 is negative trend
    if (neg < pos){
      trend <- 1
    } else {
      trend <- 0
    }

    res <- ifelse(trend == 1, "Left", "Right")

  } else if (axis == "v"){
    ## Assign object's crs to grid
    st_crs(L_Grid) <- st_crs(o)

    ## Create intersections
    inters <- suppressWarnings(st_intersection(o,Wi_Grid))
    inters <- as.numeric(st_length(inters[[length(inters)]]))

    ## Create differences
    differ <- rep(0,length(inters)-1)
    for (i in 2:length(inters)){
      differ[i-1] <- inters[i]-inters[i-1]
    }

    ## Establish trend
    neg <- length(which(differ<0))
    pos <- length(which(differ>0))

    ## Establish trend, where 1 is positive trend and 0 is negative trend
    if (neg < pos){
      trend <- 1
    } else {
      trend <- 0
    }

    res <- ifelse(trend == 1, "Down", "Up")

  }

  return(res)
}

######   ----  END FUNCTION 03


## Funtion 04. Reorients the artifact.
#' @title orient
#' @param x: Object to orient
#' @param side: How do you want to orient the artifcat
#' ("Up","Down","Left","Right","Custom")
#' @param degrees: If the orientation is custom, specify the degrees to rotate
#' @return: Returns the same artifact, reoriented on its centroid towards
#' the chosen side.
#' @details: The function first checks the orientation of the artifact. If the
#' original orientation is the same as the desired orientation (e.g. orientating
#' simultaneously large numbers of pieces) the function does nothing and produces
#' a warning.
#' @export

######   ----  START FUNCTION 04

orient <-function(x,side = c("Up","Down","Right","Left","Custom"), degrees){

  o <- is_oriented(x)

  if (side == "Custom"){
    rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
    p <- st_geometry(x)
    cent <- st_centroid(p)
    rad <- degrees*(pi/180)
    rotated <- (p-cent)*rot(rad) + cent
    rotated <- st_as_sf(rotated)
    rotated$n_sides <- x$n_sides
    rotated$x <- st_geometry(rotated)

  } else if (o == side){
    warning("x already oriented to your chosen side")
    rotated <- x

  } else {
    ## Function for rotation
    rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
    ## Rotate the piece 180B:
    p <- st_geometry(x)
    cent <- st_centroid(p)
    rotated <- (p-cent)*rot(pi) + cent
    rotated <- st_as_sf(rotated)
    rotated$n_sides <- x$n_sides
    rotated$x <- st_geometry(rotated)

  }

  return(rotated)
}

######   ----  END FUNCTION 4


## Funtion 06. Computes the reliability of the artifact.
#' @title Reliab
#' @param x: i.pol
#' @param y: s.pol
#' @return: Returns the percentage of the remaining artefact
#' @details: It computes the area of s.pol over the area of i.pol
#' @export

######   ----  START FUNCTION 046

Reliab <- function(x,y){
  ai <- as.numeric(st_area(x))
  as <- as.numeric(st_area(y))
  rel <- round(as/ai*100,2)
  if (rel > 100) error("i/s polygons are missplaced")
  names(rel) <- "Reliability"
  return(rel)
}

## END FUNCTION 6



################################################################################
############################# ALFREDO COCINA 2023 ##############################
################################################################################

label_imbalance <- function(labels) { ## Function by Courtenay 2022
  
  n <- length(labels)
  g <- length(levels(labels))
  c <- c()
  entropy_index <- c()
  for (ci in 1:g) {
    c <- c(
      c, table(labels)[ci][[1]]
    )
    entropy_index <- c(
      entropy_index, (c[ci] / n) * log(c[ci] / n)
    )
  }
  H <- -sum(entropy_index)
  balance_index <- H / log(g)
  return(balance_index)
  
}
