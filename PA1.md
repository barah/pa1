Peer Assessment 1
========================================================

read data

```r
rawdata <- read.csv("activity/activity.csv")
```


caculate daily mean :


```r
data <- ddply(rawdata, ~date, summarise, mean = mean(steps), sd = sd(steps), 
    tot = sum(steps), median = median(steps))
```

```
## Error: could not find function "ddply"
```


plot histogram of the  total number of steps taken each day and print the data:


```r
qplot(tot, data = data, geom = "histogram")
```

```
## Error: could not find function "qplot"
```

```r
dev.copy(png, "figures/plot1.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```

```r
data
```

```
## function (..., list = character(), package = NULL, lib.loc = NULL, 
##     verbose = getOption("verbose"), envir = .GlobalEnv) 
## {
##     fileExt <- function(x) {
##         db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
##         ans <- sub(".*\\.", "", x)
##         ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
##             x[db])
##         ans
##     }
##     names <- c(as.character(substitute(list(...))[-1L]), list)
##     if (!is.null(package)) {
##         if (!is.character(package)) 
##             stop("'package' must be a character string or NULL")
##         if (any(package %in% "base")) 
##             warning("datasets have been moved from package 'base' to package 'datasets'")
##         if (any(package %in% "stats")) 
##             warning("datasets have been moved from package 'stats' to package 'datasets'")
##         package[package %in% c("base", "stats")] <- "datasets"
##     }
##     paths <- find.package(package, lib.loc, verbose = verbose)
##     if (is.null(lib.loc)) 
##         paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
##             paths)
##     paths <- unique(paths[file.exists(paths)])
##     paths <- paths[file_test("-d", file.path(paths, "data"))]
##     dataExts <- tools:::.make_file_exts("data")
##     if (length(names) == 0L) {
##         db <- matrix(character(), nrow = 0L, ncol = 4L)
##         for (path in paths) {
##             entries <- NULL
##             packageName <- if (file_test("-f", file.path(path, 
##                 "DESCRIPTION"))) 
##                 basename(path)
##             else "."
##             if (file_test("-f", INDEX <- file.path(path, "Meta", 
##                 "data.rds"))) {
##                 entries <- readRDS(INDEX)
##             }
##             else {
##                 dataDir <- file.path(path, "data")
##                 entries <- tools::list_files_with_type(dataDir, 
##                   "data")
##                 if (length(entries)) {
##                   entries <- unique(tools::file_path_sans_ext(basename(entries)))
##                   entries <- cbind(entries, "")
##                 }
##             }
##             if (NROW(entries)) {
##                 if (is.matrix(entries) && ncol(entries) == 2L) 
##                   db <- rbind(db, cbind(packageName, dirname(path), 
##                     entries))
##                 else warning(gettextf("data index for package %s is invalid and will be ignored", 
##                   sQuote(packageName)), domain = NA, call. = FALSE)
##             }
##         }
##         colnames(db) <- c("Package", "LibPath", "Item", "Title")
##         footer <- if (missing(package)) 
##             paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
##                 "\n", "to list the data sets in all *available* packages.")
##         else NULL
##         y <- list(title = "Data sets", header = NULL, results = db, 
##             footer = footer)
##         class(y) <- "packageIQR"
##         return(y)
##     }
##     paths <- file.path(paths, "data")
##     for (name in names) {
##         found <- FALSE
##         for (p in paths) {
##             if (file_test("-f", file.path(p, "Rdata.rds"))) {
##                 rds <- readRDS(file.path(p, "Rdata.rds"))
##                 if (name %in% names(rds)) {
##                   found <- TRUE
##                   if (verbose) 
##                     message(sprintf("name=%s:\t found in Rdata.rds", 
##                       name), domain = NA)
##                   thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
##                   thispkg <- sub("_.*$", "", thispkg)
##                   thispkg <- paste0("package:", thispkg)
##                   objs <- rds[[name]]
##                   lazyLoad(file.path(p, "Rdata"), envir = envir, 
##                     filter = function(x) x %in% objs)
##                   break
##                 }
##                 else if (verbose) 
##                   message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
##                     name, paste(names(rds), collapse = ",")), 
##                     domain = NA)
##             }
##             if (file_test("-f", file.path(p, "Rdata.zip"))) {
##                 warning("zipped data found for package ", sQuote(basename(dirname(p))), 
##                   ".\nThat is defunct, so please re-install the package.", 
##                   domain = NA)
##                 if (file_test("-f", fp <- file.path(p, "filelist"))) 
##                   files <- file.path(p, scan(fp, what = "", quiet = TRUE))
##                 else {
##                   warning(gettextf("file 'filelist' is missing for directory %s", 
##                     sQuote(p)), domain = NA)
##                   next
##                 }
##             }
##             else {
##                 files <- list.files(p, full.names = TRUE)
##             }
##             files <- files[grep(name, files, fixed = TRUE)]
##             if (length(files) > 1L) {
##                 o <- match(fileExt(files), dataExts, nomatch = 100L)
##                 paths0 <- dirname(files)
##                 paths0 <- factor(paths0, levels = unique(paths0))
##                 files <- files[order(paths0, o)]
##             }
##             if (length(files)) {
##                 for (file in files) {
##                   if (verbose) 
##                     message("name=", name, ":\t file= ...", .Platform$file.sep, 
##                       basename(file), "::\t", appendLF = FALSE, 
##                       domain = NA)
##                   ext <- fileExt(file)
##                   if (basename(file) != paste0(name, ".", ext)) 
##                     found <- FALSE
##                   else {
##                     found <- TRUE
##                     zfile <- file
##                     zipname <- file.path(dirname(file), "Rdata.zip")
##                     if (file.exists(zipname)) {
##                       Rdatadir <- tempfile("Rdata")
##                       dir.create(Rdatadir, showWarnings = FALSE)
##                       topic <- basename(file)
##                       rc <- .External(C_unzip, zipname, topic, 
##                         Rdatadir, FALSE, TRUE, FALSE, FALSE)
##                       if (rc == 0L) 
##                         zfile <- file.path(Rdatadir, topic)
##                     }
##                     if (zfile != file) 
##                       on.exit(unlink(zfile))
##                     switch(ext, R = , r = {
##                       library("utils")
##                       sys.source(zfile, chdir = TRUE, envir = envir)
##                     }, RData = , rdata = , rda = load(zfile, 
##                       envir = envir), TXT = , txt = , tab = , 
##                       tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
##                       txt.bz2 = , txt.xz = assign(name, read.table(zfile, 
##                         header = TRUE, as.is = FALSE), envir = envir), 
##                       CSV = , csv = , csv.gz = , csv.bz2 = , 
##                       csv.xz = assign(name, read.table(zfile, 
##                         header = TRUE, sep = ";", as.is = FALSE), 
##                         envir = envir), found <- FALSE)
##                   }
##                   if (found) 
##                     break
##                 }
##                 if (verbose) 
##                   message(if (!found) 
##                     "*NOT* ", "found", domain = NA)
##             }
##             if (found) 
##                 break
##         }
##         if (!found) 
##             warning(gettextf("data set %s not found", sQuote(name)), 
##                 domain = NA)
##     }
##     invisible(names)
## }
## <bytecode: 0x00000000086981b8>
## <environment: namespace:utils>
```


average daily activity pattern plot :


```r
xyplot(interval ~ steps, data = rawdata, type = "l")
```

```
## Error: could not find function "xyplot"
```

```r
dev.copy(png, "figures/plot2.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```


calucate the number of missing values :


```r
MissingValues = NROW(rawdata) - NROW(na.omit(rawdata))
```


calcualte the mean number of steps per interval :


```r
IntervalMean <- mean(rawdata$steps, na.rm = TRUE)
```



fill the gap using interval mean  :


```r
rawdata$steps[is.na(rawdata$steps)] <- IntervalMean
```


plot histogram of the  total number of steps taken each day after the gap filling :


```r
qplot(tot, data = data, geom = "histogram")
```

```
## Error: could not find function "qplot"
```

```r
dev.copy(png, "figures/plot3.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```

prepare weekdays factor  :


```r
wd <- data$date
```

```
## Error: object of type 'closure' is not subsettable
```

```r
wd <- strptime(wd, "%Y-%m-%d")
```

```
## Error: object 'wd' not found
```

```r
wd <- weekdays(wd)
```

```
## Error: object 'wd' not found
```

```r
wd <- as.character(wd)
```

```
## Error: object 'wd' not found
```

```r

for (i in 1:length(wd)) {
    if (wd[i] == "Saturday" | wd[i] == "Sunday") {
        wd[i] <- "weekend"
    } else {
        wd[i] <- "weekday"
    }
}
```

```
## Error: object 'wd' not found
```

```r
wd <- factor(wd)
```

```
## Error: object 'wd' not found
```

plot difference btweeen weekend and weekdays   :


```r
rawdata <- transform(rawdata, wd = factor(wd))
```

```
## Error: object 'wd' not found
```

```r
xyplot(steps ~ interval | wd, data = rawdata, type = "l", layout = c(1, 2))
```

```
## Error: could not find function "xyplot"
```

```r
dev.copy(png, "figures/plot14.png")
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```

