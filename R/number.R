#' collate a group of files
#'
#' @param a number of files to process
#'
#' @return list, with list of data
#' @export number
#'
#' @import data.table, rgdal, raster, ncdf4
#'
#' @examples
#'  S\donotrun(
#' number(a = 2)
#' )
number <- function(a){
  pr <- data.table()
  lat <- data.table()
  lon <- data.table()
  b <- raster()
  list_of_files <- vector(mode = "list", length = a)
  for (i in 1:a){
    nc <- nc_open(filename = fls[i])
    pr <- ncvar_get(nc = nc,varid = "pr")
    lon <- ncvar_get(nc = nc,varid = "lon")
    lat <- ncvar_get(nc = nc,varid = "lat")
    nc_close(nc)
    b <- brick(x = pr)
    extent(x = b) <- c(range(x = lon),range(x = lat))
    ext <- extent(x = pha_wgs) * 1.75
    aux <- as.data.table(x = t(x = extract(x = b,y = ext)))
    names(x = aux) <- as.character(x = cellsFromExtent(object = b[[1]], extent = ext))
    n <- nchar(x = fls[i])
    d <- strsplit(x = substr(x = fls[i],start = n - 27,stop = n - 3),split = "-")
    dt <- data.table(date = seq(from = as.POSIXct(x = d[[1]][1],format = "%Y%m%d%H%M"),
                                to = as.POSIXct(x = d[[1]][2],format = "%Y%m%d%H%M"),by = "hour"),aux)
    dt_m <- melt(data = dt,id.vars = "date", variable.name = "id",variable.factor = FALSE)
    dt_m[, c("lon", "lat") := as.data.table(x = xyFromCell(object = b[[1]],cell = as.numeric(x = id)))]
    dt_m[, forcing := paste(c(strsplit(fls[i], '_')[[1]][4:9]), collapse = "_")]
    list_of_files[[i]] <- dt_m
    print(i)}
}
