# All Functions

## Processing of raw camera trap data ####
### Renaming camera trap images (Added 2021-09-24) - Deprecated 2021-10-05
cameraRename <- function(imagedir, exifdir){
  #imagedir <- "I:/AI/new_20210705/images"                 # Location of the image files (This should be to the folder containing the camera folders)
  #exifdir <- "c:/users/kutjy001/downlads/exiftool-12.31"  # Location of exiftool

  print(paste("This function started at ", Sys.time(), sep = ""))
  camtrapR::exiftoolPath(exifdir)
  images <- camtrapR::imageRename(imagedir, copyImages = F, hasCameraFolders = F, keepCameraSubfolders = F)
  images2 <- split(images, f = factor(images$Directory))
  print(paste("There are ", length(images2), " camera folders.", sep = ""))
  in.dir <- lapply(1:length(images2), function(x){names(images2)[x]})
  imagefiles <- lapply(1:length(images2), function(x){paste(in.dir[[x]], images2[[x]]$FileName, sep = "/")})
  out.dir <- lapply(1:length(images2), function(x){names(images2)[x]})
  names <- lapply(1:length(images2), function(x){paste(out.dir[[x]], "/", format(images2[[x]]$DateTimeOriginal, "%Y %m %d %H %M %S"), " ", formatC(seq(1:nrow(images2[[x]])), width = 5, flag = "0"), ".JPG", sep = "")})
  pbapply::pblapply(1:length(images2), function(x){file.rename(imagefiles[[x]], names[[x]])})
  print(paste("This function finished at ", Sys.time(), sep = ""))
  rm(images, images2, in.dir, imagefiles, out.dir, names)
  #rm(imagedir, exifdir)
}

### Updated image renaming function (Added 2021-10-05, Modified 2022-01-18, Modified 2022-05-05, Modified 2022-05-11)
cameraRename2 <- function(in.dir, out.dir=NULL, file.type, trigger.info=NULL, rename=FALSE, adjust = NULL, fix.names = FALSE){
  #in.dir <- "J:/test/new_20220117/images_ConLate"  # More than likely, this must be a folder containing camera folders
  #out.dir <- "J:/test/new_20220501"
  #file.type <- ".jpg"  # What type of file do you want to rename
  #trigger.info <- "Hyperfire2"  # Should the output include info about trigger method, and photo numbers? Currently only available for Hyperfire 2 cameras
  #rename <- F  # Should the original image files be renamed? Note: this affects the raw file names and cannot be easily undone.
  #adjust <- NULL
  #adjust <- c("2021-01-01 00:00:00", "2021-01-01 01:00:00")  # Do the image date-times need adjustment? Specify the original date-time and the new date-time. This is used to calculate a difftime object for adjustment purposes.
  #fix.names <- F

  print(paste("This function started at ", Sys.time(), ". Loading images...", sep = ""))
  # Define which metadata tags you want to include in your output
  if(isTRUE(is.null(trigger.info))){
    Tag <- c("DateTimeOriginal")
  }else if(trigger.info=="Hyperfire2"){
    Tag <- c("DateTimeOriginal", "TriggerMode", "Sequence", "EventNumber", "AmbientTemperature", "UserLabel")
  }else if(trigger.info=="Ultrafire_Video"){
    Tag <- c("CreateDate")
  }else{
    Tag <- c("DateTimeOriginal")
    warning("Additional info is not supported for your camera model. Only date-time information will be provided. If you want additional information, choose one of c('Hyperfire2', 'Ultrafire_Video'). To figure out which metadata tags you want, run exiftoolr::exif_read('image file path').")
  }

  # Check if exiftool is installed within R
  out <- tryCatch(exiftoolr::exif_version())
  if(is(out, "try-error")){
    warning("Exiftool was not installed on your version of R. The most up-to-date version of exiftool was installed using defaults...")
    exiftoolr::install_exiftool()
  }

  # Check if the "in" and "out" directories exist
  if(!dir.exists(in.dir)){
    stop("in.dir does not exist. Check your directory name")
  }
  ## If the out directory is not specified, it is set as the same as the "in" directory.
  if(length(out.dir)<=1){
    if(is.null(out.dir)){
      out.dir <- in.dir
    }else if(out.dir == "in.dir"){
      out.dir <- in.dir
    }else if(!dir.exists(out.dir)){
      stop("out.dir does not exist. Do you need to create it? If you want out.dir==in.dir, specify either c(NULL,'in.dir').")
    }
  }else{
    outdir.exist <- dir.exists(unique(out.dir))
    print(paste(length(outdir.exist), " unique out.dir(s) were specified", sep = ""))
    if(!all(outdir.exist)){
      stop("Some of your out.dirs do not exist. Do you need to create them?")
    }
  }

  # Check file types and ensure correct file types are used
  if(file.type=="image"){
    file.type <- ".jpg"
  }else if(file.type=="video"){
    file.type <- ".mp4"
  }else{
    file.type <- file.type
  }

  # Find files and check directory structure
  images <- list.files(path = in.dir, pattern = paste(file.type, "$", sep = ""), ignore.case = T, full.names = T, recursive = T)
  if(length(images)==0){
    stop("You have no images in your directory. Did you specify your path correctly?")
  }
  print(paste("Images loaded at ", Sys.time(), ". Checking file structure of the lowest two directories...", sep = ""))
  imagetest <- data.frame(do.call(rbind, strsplit(images, split = "/")))
  print(paste(length(unique(imagetest[,ncol(imagetest)-1])), " unique folder(s) was detected at the lowest directory. Is this correct?", sep = ""))
  print(paste(length(unique(imagetest[,ncol(imagetest)-2])), " unique folder(s) was detected at the second lowest directory. Is this correct?", sep = ""))
  if(length(out.dir)!=1){
    if(length(out.dir)!=length(images)){
      stop("out.dir must be either length 1 or the same length as the number of images")
    }
  }

  # Run exiftool and extract metadata
  print(paste("File structure checked. Cancel the function if any of the above is unexpected. Running exiftool...", sep = ""))
  exif1 <- exiftoolr::exif_read(images, tags = Tag)
  # Change the column name for video files because DateTimeOriginal does not exist
  if(isTRUE(grepl(".mp4", file.type, ignore.case = T))){
    colnames(exif1) <- sub("CreateDate", "DateTimeOriginal", colnames(exif1))
  }
  exif1b <- exif1[order(exif1$DateTimeOriginal),]
  print(paste("Exiftool completed at ", Sys.time(), ". Specifying image paths...", sep = ""))

  # Specify the proper paths for renaming images
  indir <- data.frame(do.call(rbind, strsplit(in.dir, "/")))
  if(length(unique(sapply(strsplit(exif1b$SourceFile, "/"), length)))>1){
    stop("Check your folder structure. You do not have the same number of subdirectories for each image")
  }
  folders <- data.frame(do.call(rbind, strsplit(exif1b$SourceFile, "/")))

  if(ncol(indir) == ncol(folders)-1){  # For the case of your in directory leading directly to images
    print("You have selected a folder with no sub-directories. out.dir should lead directly to pictures.")
    relativepath <- folders[,seq(1,ncol(folders)-1)]
    inpath <- indir[,seq(1,ncol(indir))]
    if(length(out.dir)==1){
      outpath <- data.frame(do.call(rbind, strsplit(rep(out.dir, times = nrow(folders)), "/")))
    }else{
      outpath <- data.frame(do.call(rbind, strsplit(out.dir, split = "/")))
    }
  }else if(ncol(indir) < (ncol(folders) - 1)){  # For any other case
    print("You have selected a folder with sub-directories. The relative paths will be preserved in out.dir")
    relativepath <- folders[,seq(ncol(indir)+1,ncol(folders)-1)]
    inpath <- folders[,seq(1,ncol(folders)-1)]
    outpath <- data.frame(do.call(rbind, strsplit(out.dir, "/")), relativepath)
  }else{
    stop("Something happened. in.dir has more sub-directories than the images.")
  }
  if(is.null(dim(relativepath))){
    relativepath2 <- relativepath
  }else{
    relativepath2 <- apply(relativepath, 1, paste, collapse = "/")
  }
  if(is.null(dim(inpath))){
    inpath2 <- inpath
  }else{
    inpath2 <- apply(inpath, 1, paste, collapse = "/")
  }
  if(is.null(dim(outpath))){
    outpath2 <- outpath
  }else{
    outpath2 <- apply(outpath, 1, paste, collapse = "/")
  }
  print(paste("Image paths specified at ", Sys.time(), ". Cleaning up and renaming...", sep = ""))

  # Do any adjustments to date-times, if necessary
  if(is.null(adjust)){
    dt.diff <- 0
  }else{
    print("Adjustments to the date-time are being made")
    if(isTRUE(lubridate::is.difftime(adjust))){
      dt.diff <- adjust
    }else{
      if(length(adjust)!=2){
        stop("Two date-times must be provided in adjust. If only one is provided, it needed to be a difftime object.")
      }
      dt.diff <- difftime(adjust[2], adjust[1], units = "secs")
    }
  }
  datetime <- lubridate::ymd_hms(exif1b$DateTimeOriginal) + dt.diff

  # Compile all the file paths and other information
  exif2 <- data.frame(SourceFile = exif1b$SourceFile,
                      inpath = inpath2,
                      indir = in.dir,
                      outpath = outpath2,
                      outdir = out.dir,
                      #relativepath = relativepath2,
                      old.name = folders[,ncol(folders)],
                      year = formatC(lubridate::year(datetime), width = 4, flag = "0"),
                      month = formatC(lubridate::month(datetime), width = 2, flag = "0"),
                      day = formatC(lubridate::day(datetime), width = 2, flag = "0"),
                      hour = formatC(lubridate::hour(datetime), width = 2, flag = "0"),
                      minute = formatC(lubridate::minute(datetime), width = 2, flag = "0"),
                      second = formatC(lubridate::second(datetime), width = 2, flag = "0"))

  # Add serial numbers based on the lowest sub-directory
  exif3 <- do.call(rbind, lapply(split(exif2, f = factor(exif2$inpath)), function(x){x$serial <- formatC(seq(1:nrow(x)), width = 5, flag = "0"); return(x)}))
  rownames(exif3) <- NULL
  exif3$new.name <- with(exif3, paste(year, " ", month, " ", day, " ", hour, " ", minute, " ", second, " ", serial, file.type, sep = ""))

  # Only add serial numbers to names that are renamed
  if(isTRUE(fix.names)){
    print("Image names were only replaced if they were not in the form YYYY MM DD HH MM SS.jpg. Serial number added to all images.")
    exif3$complete <- complete.cases(apply(do.call(rbind,strsplit(sub(".jpg", "", exif3$old.name, ignore.case = T)," ")),2,as.numeric))
    exif3$new.name[exif3$complete==T] <- paste(sub(".jpg", "", exif3$old.name[exif3$complete==T], ignore.case = T), " ", exif3$serial[exif3$complete==T], ".jpg", sep = "")
  }

  # Rename the images if desired (this cannot be undone)
  if(isTRUE(rename)){
    print(paste("Files will be renamed and replaced. If you wanted to keep originals, specify c('copy') instead. Renaming started at ", Sys.time(), sep = ""))
    file.rename(with(exif3, paste(inpath, old.name, sep = "/")), with(exif3, paste(outpath, new.name, sep = "/")))
    print(paste("File renaming completed at ", Sys.time(), sep = ""))
  }else if(rename=="replace"){
    print(paste("Files will be renamed and replaced. Renaming started at ", Sys.time(), sep = ""))
    file.rename(with(exif3, paste(inpath, old.name, sep = "/")), with(exif3, paste(outpath, new.name, sep = "/")))
    print(paste("File renaming completed at ", Sys.time(), sep = ""))
  }else if(rename=="copy"){
    print(paste("Files will be renamed and copied. Renaming started at ", Sys.time(), sep = ""))
    file.copy(with(exif3, paste(inpath, old.name, sep = "/")), with(exif3, paste(outpath, new.name, sep = "/")))
    print(paste("File renaming completed at ", Sys.time(), sep = ""))
  }else if(isFALSE(rename)){
    print("No files were renamed")
  }else if(rename=="none"){
    print("No files were renamed")
  }else{
    warning("You did not specify a correct value for rename. Choose one of c(TRUE,FALSE,'replace','copy','none'). No files were renamed")
  }

  # Clean up the data and prepare to shut down the function
  exif4 <- exif3[,c("SourceFile", "inpath", "outpath", "old.name", "new.name")]
  exif5 <- merge.data.frame(exif4, exif1b, by = "SourceFile")

  # One last data integrity check
  if(nrow(exif5) != nrow(exif4)){
    warning("Something happened. Some pictures went missing or got added between loading the exif data and renaming images.")
  }

  print(paste("This function completed at ", Sys.time(), sep = ""))
  return(exif5[,2:ncol(exif5)])

  rm(Tag, images, exif1, exif1b, indir.length, relativepath, inpath, outpath, relativepath2, inpath2, outpath2, folders, dt.diff, datetime, exif2, exif3, exif4, exif5)
  #rm(in.dir, out.dir, file.type, trigger.info, rename, adjust, fix.names)
}

### Another major update to the renaming function (Added 2022-06-01)
cameraRename3 <- function(in.dir, out.dir=NULL, file.type, trigger.info=NULL, rename="none", return.type = "list", adjust = NULL, fix.names = FALSE, pp = FALSE, cores.left = NULL){
  #in.dir <- "J:/test/new_20220117/images_ConLate"  # More than likely, this must be a folder containing camera folders
  #out.dir <- "J:/test/new_20220501"
  #file.type <- ".jpg"  # What type of file do you want to rename
  #trigger.info <- "Hyperfire2"  # Should the output include info about trigger method, and photo numbers? Currently only available for Hyperfire 2 cameras
  #rename <- "none"  # Should the original image files be renamed? Note: this affects the raw file names and cannot be easily undone.
  #return.type <- "df"
  #adjust <- NULL
  #adjust <- c("2021-01-01 00:00:00", "2021-01-01 01:00:00")  # Do the image date-times need adjustment? Specify the original date-time and the new date-time. This is used to calculate a difftime object for adjustment purposes.
  #fix.names <- F
  #pp <- T
  #cores.left <- 2

  print(paste("This function started at ", Sys.time(), ". Loading images...", sep = ""))
  # Define which metadata tags you want to include in your output
  if(isTRUE(is.null(trigger.info))){
    Tag <- c("DateTimeOriginal")
  }else if(trigger.info=="Hyperfire2"){
    Tag <- c("DateTimeOriginal", "TriggerMode", "Sequence", "EventNumber", "AmbientTemperature", "UserLabel", "SerialNumber")
  }else if(trigger.info=="Ultrafire_Video"){
    Tag <- c("CreateDate")
  }else{
    Tag <- c("DateTimeOriginal")
    warning("Additional info is not supported for your camera model. Only date-time information will be provided. If you want additional information, choose one of c('Hyperfire2', 'Ultrafire_Video'). To figure out which metadata tags you want, run exiftoolr::exif_read('image file path').")
  }

  # Check if exiftool is installed within R
  out <- tryCatch(exiftoolr::exif_version())
  if(is(out, "try-error")){
    warning("Exiftool was not installed on your version of R. The most up-to-date version of exiftool was installed using defaults...")
    exiftoolr::install_exiftool()
  }

  # Check if the "in" and "out" directories exist
  if(!dir.exists(in.dir)){
    stop("in.dir does not exist. Check your directory name")
  }
  ## If the out directory is not specified, it is set as the same as the "in" directory.
  if(length(out.dir)<=1){
    if(is.null(out.dir)){
      out.dir <- in.dir
    }else if(out.dir == "in.dir"){
      out.dir <- in.dir
    }else if(!dir.exists(out.dir)){
      stop("out.dir does not exist. Do you need to create it? If you want out.dir==in.dir, specify either c(NULL,'in.dir').")
    }
  }else{
    print(paste(length(dir.exists(unique(out.dir))), " unique out.dir(s) were specified", sep = ""))
    if(!all(dir.exists(unique(out.dir)))){
      stop("Some of your out.dirs do not exist. Do you need to create them?")
    }
  }

  # Check file types and ensure correct file types are used
  if(file.type=="image"){
    file.type <- ".jpg"
  }else if(file.type=="video"){
    file.type <- ".mp4"
  }else{
    file.type <- file.type
  }

  # Find files and check directory structure
  images <- list.files(path = in.dir, pattern = paste(file.type, "$", sep = ""), ignore.case = T, full.names = T, recursive = T)
  if(length(images)==0){
    stop("You have no images in your directory. Did you specify your path and file type correctly?")
  }
  print(paste("Images loaded at ", Sys.time(), ". Checking file structure of the lowest two directories...", sep = ""))
  imagelist <- data.frame(do.call(dplyr::bind_rows, lapply(strsplit(images, split = "/"), function(x){names(x) <- paste("X", seq(1:length(x)), sep = ""); return(x)})))
  if(!all(complete.cases(imagelist))){
    stop("Your images are not at the same directory level in each folder. This function likely won't work properly")
  }else{
    message(paste(length(unique(imagelist[,ncol(imagelist)-1])), " unique folder(s) was detected at the lowest directory. Is this correct?", sep = ""))
    message(paste(length(unique(imagelist[,ncol(imagelist)-2])), " unique folder(s) were detected at the second lowest directory. Is this correct?", sep = ""))
  }

  colnames(imagelist)[ncol(imagelist)] <- "image"
  imagelist$dir <- apply(imagelist, 1, function(x){paste(x[-length(x)], collapse = "/")})

  images_split <- split(imagelist, f = imagelist$dir)
  images_split2 <- lapply(images_split, function(x){file.path(x$dir, x$image)})

  if(length(out.dir)==1){
    out.dir.length <- "one"
    message("The length of out.dir is 1. out.dir will reference a relative path for all images")
  }else if(length(out.dir)==length(images_split2)){
    out.dir <- as.list(out.dir)
    out.dir.length <- "cams"
    message("The length of out.dir is the same as the number of unique directories. out.dir will be unique for each camera folder")
  }else{
    stop("The length of out.dir is not the same as the number of unique image directories")
  }

  # Run exiftool and extract metadata
  print(paste("File structure checked. Cancel the function if any of the above is unexpected. Running exiftool...", sep = ""))

  if(isTRUE(pp)){
    message("Parallel processing enabled")
    if(is.null(cores.left)){
      cores.left <- 2
    }else{
      cores.left <- tryCatch(as.numeric(cores.left),
                             error = function(e){message("There was an error coercing cores.left to a number. The default of 2 cores are not utilized"); return(2)},
                             warning = function(w){message("Could not coerce cores.left to a number. The default of 2 cores are not utilized"); return(2)})
    }
    cl1 <- parallel::makeCluster(parallel::detectCores()-cores.left, outfile = "out.txt")
    parallel::clusterExport(cl1, varlist = c("images_split2", "Tag"), envir = environment())
  }else{
    cl1 <- NULL
  }

  exif1 <- pbapply::pblapply(1:length(images_split2), cl = cl1, function(i){
    tryCatch(exiftoolr::exif_read(images_split2[[i]], tags = Tag),
             error = function(e){message(paste("There is likely a corrupt file in: \n", names(images_split2)[i], "\nThe original error is: ", sep = "")); message(e); return(names(images_split2)[i])})
  })

  if(isTRUE(pp)){
    parallel::stopCluster(cl1)
  }
  # Change the column name for video files because DateTimeOriginal does not exist
  if(isTRUE(grepl(".mp4", file.type, ignore.case = T))){
    exif1 <- lapply(exif1, function(x){colnames(x) <- sub("CreateDate", "DateTimeOriginal", colnames(x)); return(x)})
  }
  exif1b <- lapply(exif1, function(x){x[order(x$DateTimeOriginal),]})
  print(paste("Exiftool completed at ", Sys.time(), ". Specifying image paths...", sep = ""))

  # Specify the proper paths for renaming images
  indir <- data.frame(do.call(rbind, strsplit(in.dir, "/")))
  folders <- lapply(exif1b, function(x){data.frame(do.call(rbind, strsplit(x$SourceFile, "/")))})
  if(ncol(indir)==unique(unlist(lapply(folders, function(x){ncol(x)-1})))){  # For the case of your in directory leading directly to images
    equal.dirs <- "Y"
    message("You have selected a folder with no sub-directories. out.dir should lead directly to pictures.")
  }else if(ncol(indir) < unique(unlist(lapply(folders, function(x){ncol(x)-1})))){  # For the case where your directory has sub-directories
    equal.dirs <- "N"
    message("You have selected a folder with sub-directories. The relative paths will be preserved in out.dir")
  }else{
    stop("Something happened. in.dir has more sub-directories than the images.")
  }

  paths <- lapply(1:length(folders), function(x){
    if(equal.dirs=="Y"){
      relativepath <- folders[[x]][,seq(1,ncol(folders[[x]])-1)]
      inpath <- indir[,seq(1,ncol(indir))]
      if(out.dir.length == "one"){
        outpath <- data.frame(do.call(rbind, strsplit(rep(out.dir, times = nrow(folders[[x]])), "/")))
      }else if(out.dir.length == "cams"){
        outpath <- data.frame(do.call(rbind, strsplit(rep(out.dir[[x]], times = nrow(folders[[x]])), split = "/")))
      }
    }else if(equal.dirs=="N"){
      relativepath <- folders[[x]][,seq(ncol(indir)+1,ncol(folders[[x]])-1)]
      inpath <- folders[[x]][,seq(1,ncol(folders[[x]])-1)]
      if(out.dir.length == "one"){
        outpath <- data.frame(do.call(rbind, strsplit(out.dir, "/")), relativepath)
      }else if(out.dir.length == "cams"){
        outpath <- data.frame(do.call(rbind, strsplit(out.dir[[x]], "/")), relativepath)
      }
    }
    return(list(relativepath = relativepath, inpath = inpath, outpath = outpath))
  })

  paths2 <- lapply(paths, function(x){
    if(is.null(dim(x$relativepath))){
      return(list(relativepath2 = x$relativepath, inpath2 = x$inpath, outpath2 = x$outpath))
    }else{
      lapply(x, function(y){apply(y, 1, paste, collapse = "/")})
    }
  })
  print(paste("Image paths specified at ", Sys.time(), ". Cleaning up and renaming...", sep = ""))

  # Do any adjustments to date-times, if necessary
  if(is.null(adjust)){
    dt.diff <- 0
  }else{
    print("Adjustments to the date-time are being made")
    if(isTRUE(lubridate::is.difftime(adjust))){
      dt.diff <- adjust
    }else{
      if(length(adjust)!=2){stop("Two date-times must be provided in adjust. If only one is provided, it needed to be a difftime object.")}
      dt.diff <- difftime(adjust[2], adjust[1], units = "secs")
    }
  }

  # Compile all the file paths, information for new file names, and combine with metadata
  exif2 <- lapply(1:length(exif1b), function(i){
    datetime <- lubridate::ymd_hms(exif1b[[i]]$DateTimeOriginal) + dt.diff
    x1 <- data.frame(SourceFile = exif1b[[i]]$SourceFile,
                     inpath = paths2[[i]]$inpath,
                     #indir = in.dir,
                     outpath = paths2[[i]]$outpath,
                     #outdir = ifelse(length(out.dir == 1), out.dir, out.dir[[i]]),
                     old.name = folders[[i]][,ncol(folders[[i]])],
                     year = formatC(lubridate::year(datetime), width = 4, flag = "0"),
                     month = formatC(lubridate::month(datetime), width = 2, flag = "0"),
                     day = formatC(lubridate::day(datetime), width = 2, flag = "0"),
                     hour = formatC(lubridate::hour(datetime), width = 2, flag = "0"),
                     minute = formatC(lubridate::minute(datetime), width = 2, flag = "0"),
                     second = formatC(lubridate::second(datetime), width = 2, flag = "0")
    )
    # Add serial numbers based on the lowest sub-directory
    x1$serial <- formatC(seq(1:nrow(x1)), width = 5, flag = "0")
    x1$new.name <- with(x1, paste(year, " ", month, " ", day, " ", hour, " ", minute, " ", second, " ", serial, file.type, sep = ""))

    # Only add serial numbers to names that are renamed
    if(isTRUE(fix.names)){
      message("Image names were only replaced if they were not in the form YYYY MM DD HH MM SS.jpg. Serial number added to all images.")
      x1$complete <- complete.cases(apply(do.call(rbind, strsplit(sub(file.type, "", x1$old.name, ignore.case = T), " ")), 2, as.numeric))
      x1$new.name[isTRUE(x1$complete)] <- paste(sub(file.type, "", x1$old.name[isTRUE(x1$complete)], ignore.case = T), " ", x1$serial[isTRUE(x1$complete)], file.type, sep = "")
    }

    # Cleanup the output
    x2 <- x1[,c("SourceFile", "inpath", "outpath", "old.name", "new.name")]
    x3 <- merge.data.frame(x2, exif1b[[i]], by = "SourceFile")
    if(nrow(x3) != nrow(x2)){  # One last data integrity check
      warning("Something happened. Some pictures went missing or got added between loading the exif data and renaming images.")
    }
    return(x3[,2:ncol(x3)])
  })

  # Rename the images if desired (this cannot be undone)
  if(isTRUE(pp)){
    cl1 <- parallel::makeCluster(parallel::detectCores()-cores.left, outfile = "out.txt")
    parallel::clusterExport(cl1, varlist = c("exif2"), envir = environment())
  }

  if(rename=="replace"){
    print(paste("Files will be renamed and replaced. Renaming started at ", Sys.time(), sep = ""))
    pbapply::pblapply(exif2[which(!is.na(exif2))], cl = cl1, function(x){file.rename(from = with(x, file.path(inpath, old.name)), to = with(x, file.path(outpath, new.name)))})
    print(paste("File renaming completed at ", Sys.time(), sep = ""))
  }else if(rename=="copy"){
    print(paste("Files will be renamed and copied. Renaming started at ", Sys.time(), sep = ""))
    pbapply::pblapply(exif2[which(!is.na(exif2))], cl = cl1, function(x){file.copy(from = with(x, file.path(inpath, old.name)), to = with(x, file.path(outpath, new.name)))})
    print(paste("File renaming completed at ", Sys.time(), sep = ""))
  }else if(rename=="none"){
    message("No files were renamed")
  }else{
    message("No files were renamed because you did not specify a correct value for rename. Choose one of c('replace', 'copy', 'none').")
  }

  if(isTRUE(pp)){
    parallel::stopCluster(cl1)
  }

  # Clean up the data and prepare to shut down the function
  print(paste("This function completed at ", Sys.time(), sep = ""))
  if(return.type == "list"){
    exif3 <- exif2
  }else if(return.type == "df"){
    exif3 <- do.call(rbind, exif2)
  }else{
    message("You chose an invalid return type. A list is returned. Choose one of c('list', 'df').")
    exif3 <- exif2
  }
  return(exif3)

  rm(Tag, out, images, imagelist, images_split, images_split2, out.dir.length, cl1, exif1, exif1b, indir, folders, equal.dirs, paths, paths2, dt.diff, exif2, exif3)
  #rm(in.dir, out.dir, file.type, trigger.info, rename, return.type, adjust, fix.names, pp, cores.left)
}

### A function to find corrupt files (Added 2022-06-01)
findCorruptImages <- function(in.dir, file.type, pp, cores.left = NULL){
  #in.dir <- "K:/new_HY_20220524/RH6-1/20220524"
  #file.type <- ".jpg"

  files <- list.files(in.dir, pattern = paste(file.type, "$", sep = ""), ignore.case = T, full.names = T, recursive = T)

  if(isTRUE(pp)){
    message("Parallel processing enabled")
    if(is.null(cores.left)){
      cores.left <- 2
    }else{
      cores.left <- tryCatch(as.numeric(cores.left),
                             error = function(e){message("There was an error coercing cores.left to a number. The default of 2 cores are not utilized"); return(2)},
                             warning = function(w){message("Could not coerce cores.left to a number. The default of 2 cores are not utilized"); return(2)})
    }
    cl1 <- parallel::makeCluster(parallel::detectCores()-cores.left, outfile = "out.txt")
    parallel::clusterExport(cl1, varlist = c("images_split2", "Tag"), envir = environment())
  }else{
    cl1 <- NULL
  }

  all <- pbapply::pblapply(files, cl = cl1, function(x){tryCatch(exiftoolr::exif_read(x, tags = c("Directory")), error = function(e){return(NA)})})

  if(isTRUE(pp)){
    parallel::stopCluster(cl1)
  }

  corrupt <- which(is.na(all))

  all.corrupt <- all[c(corrupt-1,corrupt, corrupt+1)]

  return(all.corrupt)

  rm(files, all, corrupt, all.corrupt)
  #rm(in.dir, file.type)
}


## Working with Microsoft Megadetector AI outputs ####
### Move ghosts identified by the Microsoft Megadetector AI to a sorted ghosts folder (Added 2021-11-19, Modified 2022-05-05)
removeGhosts <- function(jsonfile, in.dir, out.dir, create.dirs, conf.threshold, move = T){
  #jsonfile <- "G:/AI_detector/detections_filtered_20220117.json"
  #conf.threshold <- 0.8
  #in.dir <- "G:/test/new_20220117"
  #out.dir <- "G:/test/sorted_20220117"
  #create.dirs <- T

  print(paste("This function started at ", Sys.time(), sep = ""))

  json <- jsonlite::fromJSON(jsonfile)

  images <- json$images
  ghosts <- images[images$max_detection_conf<conf.threshold,]

  ghosts.temp <- do.call(rbind, strsplit(ghosts$file, "\\\\"))
  ghosts.out <- ghosts.temp[,ncol(ghosts.temp)]

  nrow(ghosts)
  print(paste("There were ", nrow(ghosts), " ghosts out of ", nrow(images), " total images. The proportion is ", round(nrow(ghosts)/nrow(images), digits = 4)*100, "%.", sep = ""))

  out.files <- data.frame(do.call(rbind, strsplit(gsub("\\\\", "/", ghosts$file), split = "/")))
  len <- apply(out.files, 2, function(x){length(unique(x))})
  out.files$path <- paste(out.files[,which(len>1)[1]], "ghost/00", out.files[,ncol(out.files)], sep = "/")

  rename <- list(in.files = paste(in.dir, ghosts$file, sep = "/"),
                 out.files = paste(out.dir, out.files$path, sep = "/"))
  print(paste("The absolute path for the 'in' files is: ", rename[["in.files"]][1], sep = ""))
  print(paste("The absolute path for the 'out' files is: ", rename[["out.files"]][1], sep = ""))

  if(isTRUE(create.dirs)){
    print("Creating directories")
    dirs <- with(out.files, list(unique(paste(out.dir, out.files[,which(len>1)[1]], sep = "/")),
                                 unique(paste(out.dir, out.files[,which(len>1)[1]], "ghost", sep = "/")),
                                 unique(paste(out.dir, out.files[,which(len>1)[1]], "ghost", "01", sep = "/"))))
    dirsTemp <- lapply(dirs, function(x){
      lapply(x, function(y){
        ifelse(!dir.exists(y), dir.create(y), print("Folder exists"))
      })
    })
  }

  if(isTRUE(move)){
    print(paste("File renaming in progress", sep = ""))
    file.rename(from = rename[["in.files"]], to = rename[["out.files"]])
  }

  print(paste("This function finished at ", Sys.time(), sep = ""))
  return(rename)
}


## Working with camera data sorted using Sanderson and Harris' method ####
### Create a dataorganize like output from sorted images (Added 2021-10-06)
dataorganize <- function(inputdir, diagnostics = T){
  #inputdir <- "J:/AI_Test_Microsoft/Test/AI_test"
  #diagnostics <- TRUE

  filelist <- list.files(inputdir, pattern = "jpg", ignore.case = T, full.names = T, recursive = T)
  files1 <- do.call(rbind, strsplit(filelist, "/"))
  files2 <- files1[,(ncol(files1)-3):ncol(files1)]
  images1 <- sub(".jpg", "", ignore.case = T, files2[,4])
  images2 <- do.call(rbind, strsplit(images1, " "))

  out <- data.frame(files2[,1:3], images2[,1:6])
  colnames(out) <- c("site", "species", "individuals", "year", "month", "day", "hour", "minute", "second")
  if(diagnostics == T){
    diagn <- list("Sites and Species" = data.frame(dplyr::summarise(dplyr::group_by(out, site), species = length(unique(species)), pictures = n())),
                  "Unique Species" = sort(unique(out$species)),
                  "Unique Number of Individuals" = sort(unique(out$individuals)))
    print(diagn)
    rm(diagn)
  }
  return(out)

  rm(filelist, files1, files2, images1, images2, out)
  #rm(inputdir, diagnostics)
}

### From a dataorganize file, create a usable output (Added 2021-08-19, Modified 2022-01-14)
APFun_env <- function(x,y,sort.col="Camera",exclude=c("ghost"),start_date,end_date=Sys.Date(),interval=NULL,all.pics=FALSE){
  # This function takes the allpictures.txt file and changes it into a form that is usable for analyses in R
  # Inputs:
  ## x = allpictures file produced by Dataorganize
  ## y = environmental variables file. This file must have header called "Camera" containing the list of cameras
  ## sort.col = Column which you want to sort your pictures by. It should most likely either be site or camera. Defaults to camera
  ## exclude = The species you want to subset out so that you don't get information on them. Defaults to ghost
  ## start_date = start date for the AllPictures file
  ## end_date = end date for pictures. Defaults to current date
  ## interval = time in seconds between pictures for an independent event. Defaults to none
  ## all.pics = Logical on whether you want all the pictures or just the independent events
  # Outputs:
  ## A data frame containing independent events in date-time format + camera/site specific variables

  # Defining an interval if interval left as default
  if(is.null(interval)==TRUE){
    interval=1
  }

  # Formatting the columns in the AllPictures file
  colnames(x)=c("Camera","Species","Year","Month","Day","Hour","Minute","Second","N.Individuals")
  if(isTRUE(is.na(exclude))){
    x1 <- x
  }else{
    x1=subset(x, !(Species %in% exclude))
  }
  x1$Month_Day <- paste(formatC(x1[,"Year"], width = 4, format = "d", flag = "0"),
                        formatC(x1[,"Month"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Day"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Hour"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Minute"], width = 2, format = "d", flag = "0"),
                        formatC(x1[,"Second"], width = 2, format = "d", flag = "0"), sep = "")
  x1$DateTimeOriginal <- as.POSIXct(strptime(x1$Month_Day,'%Y%m%d%H%M%S'))
  x1$Date <- as.POSIXct(strptime(x1$Month_Day,'%Y%m%d'))
  x1$hms <- format(x1$DateTimeOriginal,format="%H:%M:%S")
  x1$delta.time.secs[1] <- 0

  # Adding in environmental data and time of day data
  x2 <- merge.data.frame(x1,y,by = "Camera")
  x2$time_numeric <- (as.numeric(x2$Hour)*3600 + as.numeric(x2$Minute)*60 + as.numeric(x2$Second))/(86400)
  x2$time_radians <-2*pi*x2$time_numeric

  # Subsetting the data by date
  x2a <- subset(x2,Date >= start_date)
  x2b <- subset(x2a,Date <= as.character(end_date))
  if(nrow(x2b)==0){
    stop("The specified date range is outside the range of the data")
  }
  x3 <- x2b

  # Calculating the time difference between pictures
  x3["Sort"] <- x3[sort.col]
  x4=dplyr::arrange(x3, Sort, Species, Month_Day)
  x4$delta.time.secs <- pbapply::pbsapply(1:(nrow(x4)), FUN = function(i){
    ifelse(i-1==0, interval,
           ifelse(x4$Species[i]==x4$Species[i-1],
                  difftime(x4$DateTimeOriginal[i], x4$DateTimeOriginal[i-1], units = "secs"), interval))
    #rm(i)
  })
  x4$Ident <- seq(1,nrow(x4))

  # Identifying and subsetting out independent events
  x5 <- x4[!(x4$delta.time.secs < interval),]
  x5$Ident2 <- seq(1, nrow(x5))
  x5a <- merge.data.frame(x5, x4, by="Ident", all.y=TRUE, sort.y=TRUE)
  x5b <- data.frame(x5a[,c(1,(ncol(x5)+1):ncol(x5a))], Ident2 = zoo::na.locf(x5a$Ident2, fromLast=FALSE))
  x5c <- dplyr::summarise(dplyr::group_by(x5b, Ident2), Individuals = max(`N.Individuals.y`))

  # Sorting the columns and removing unnecessary columns
  if(all.pics==TRUE){
    x5b2 <- x5b[,1:ncol(x5)]
    x5b3 <- x5b2[,c(3:ncol(x5b2)-1,1,ncol(x5b2))]
    colnames(x5b3) <- colnames(x5)
    x6 <- merge.data.frame(x5b3, x5c, by="Ident2", all.x=TRUE, sort.x=TRUE)
    x7 <- x6[,c(1,2,seq(2,length(y))+14,3,length(y)+length(x1)+5,11,12,13,14,length(y)+length(x1)+1,length(y)+length(x1)+2)]
    rm(x5b2, xb53)
  }else{
    x6 <- merge.data.frame(x5, x5c, by="Ident2", all.x=TRUE, sort.x=TRUE)
    x7 <- x6[,c(2,seq(2,length(y))+14,3,length(y)+length(x1)+5,11,12,13,14,length(y)+length(x1)+1,length(y)+length(x1)+2)]
  }
  return(x7)
  rm(x1, x2, x2a, x2b, x3, x4, x5, x5a, x5b, x5c, x6, x7)
  #rm(x, y, sort.col, exclude, start_date, end_date, interval, all.pics)
}


## Working with Timelapse Outputs of sorted camera data ####
### Convert a Timelapse file to a dataorganize output (Added 2022-01-14)
APFun_Timelapse <- function(x){
  #x <- AP1_t

  images1 <- x[,c("File", "RelativePath", "Species1", "Species1_Ind")]
  colnames(images1) <- c("File", "Path", "Species", "Individuals")

  if(!all(is.na(x$Species2))){
    images2 <- x[x$Species2!="",c("File", "RelativePath", "Species2", "Species2_Ind")]
    colnames(images2) <- c("File", "Path", "Species", "Individuals")
  }else{
    images2 <- NULL
  }
  if(!all(is.na(x$Species3))){
    images3 <- x[x$Species3!="",c("File", "RelativePath", "Species3", "Species3_Ind")]
    colnames(images3) <- c("File", "Path", "Species", "Individuals")
  }else{
    images3 <- NULL
  }
  if(!all(is.na(x$SpeciesOther))){
    images4 <- x[x$SpeciesOther!="",c("File", "RelativePath", "SpeciesOther", "Other_Ind")]
    colnames(images4) <- c("File", "Path", "Species", "Individuals")
  }else{
    images4 <- NULL
  }
  x1 <- rbind(images1,images2,images3,images4)
  x2 <- data.frame(do.call(rbind, strsplit(x1$Path, split = "\\\\")), do.call(rbind, strsplit(sub(".JPG", "", x1$File, ignore.case = T), split = " ")), x1[,3:4])
  x3 <- x2[,c(2,11,4:9,12)]
  colnames(x3) <- paste("V", seq(1:ncol(x3)), sep = "")
  x4 <- data.frame(x3[,1:2], apply(x3[,3:ncol(x3)], 2, as.integer))

  return(x4)
  rm(images1, images2, images3, images4, x1, x2, x3, x4)
  #rm(x)
}

### Move all pictures into sorted folders (Added 2022-05-05, Modified 2022-07-14)
movePictures <- function(x, in.dir, out.dir, create.dirs, type = "none"){
  #x <- read.csv("K:/Completed/new_20210927/timelapse_out_20210927.csv")
  #in.dir <- "K:/Completed/new_20210927"
  #out.dir <- "K:/Completed/new_20210927/sorted"
  #create.dirs <- T
  #type <- "none"

  print(paste("This function started at ", Sys.time(), sep = ""))

  if(!dir.exists(in.dir)){
    stop("Your in.dir does not exist. Did you specify the correct path")
  }
  if(grepl("images", in.dir, ignore.case = T)){
    message("Your in.dir path includes the images folder. File transfer may not work properly if timelapse also references this folder")
  }
  if(!dir.exists(out.dir)){
    stop("Your out.dir does not exist. Did you forget to create it?")
  }

  x$RelativePath <- gsub("\\\\", "/", x$RelativePath)

  images1 <- x[,c(1,2,11,12)]
  colnames(images1) <- c("File", "Path", "Species", "Individuals")

  if(is.character(x$Species2)){
    images2 <- x[x$Species2!="",c(1,2,13,14)]
    colnames(images2) <- c("File", "Path", "Species", "Individuals")
  }else{
    images2 <- NULL
  }
  if(is.character(x$Species3)){
    images3 <- x[x$Species3!="",c(1,2,15,16)]
    colnames(images3) <- c("File", "Path", "Species", "Individuals")
  }else{
    images3 <- NULL
  }
  if(is.character(x$SpeciesOther)){
    images4 <- x[x$SpeciesOther!="",c(1,2,17,18)]
    colnames(images4) <- c("File", "Path", "Species", "Individuals")
  }else{
    images4 <- NULL
  }
  x1 <- rbind(images1,images2,images3,images4)
  x2 <- data.frame(do.call(rbind, strsplit(x1$Path, split = "/")), x1[,c("File", "Species", "Individuals")])
  colnames(x2)[-c(ncol(x2)-2,ncol(x2)-1,ncol(x2))] <- c("Folder", "Camera", "Date")
  x2$Individuals <- formatC(x2$Individuals, flag = "0", width = 2)
  x3in <- with(x2, file.path(Folder, Camera, Date, File))
  x3out <- with(x2, file.path(Camera, Species, Individuals, File))

  if(length(x3in) != length(x3out)){
    stop("You have different numbers of files in the 'in' and 'out' directories. You broke my function...")
  }

  rename <- list(in.files = file.path(in.dir, x3in),
                 out.files = file.path(out.dir, x3out))
  print(paste("The in files will look like: ", rename[["in.files"]][[1]], sep = ""))
  print(paste("The out files will look like: ", rename[["out.files"]][[1]], sep = ""))

  if(isTRUE(create.dirs)){
    print("Creating Directories")
    dirs <- with(x2, list(unique(paste(out.dir, Camera, sep = "/")),
                          unique(paste(out.dir, Camera, Species, sep = "/")),
                          unique(paste(out.dir, Camera, Species, Individuals, sep = "/"))))
    dirsTemp <- lapply(dirs, function(x){
      lapply(x, function(y){
        ifelse(!dir.exists(y), dir.create(y), print("Folder exists"))
      })
    })
  }

  if(length(unique(x3in)) != length(unique(x3out))){
    message("You have a different number of in and out files, likely because more than one species was detected in a single picture. \nSuggest using 'copy' instead of 'move' for images.")
  }

  if(type=="move"){
    print("File transfer in progress. Images are moved from in.dir to out.dir")
    test <- file.rename(from = rename[["in.files"]], to = rename[["out.files"]])
  }else if(type=="copy"){
    print("File transfer in progress. Images are copied from in.dir to out.dir")
    test <- file.copy(from = rename[["in.files"]], to = rename[["out.files"]])
  }else if(type=="none"){
    print("No file transfer specified")
  }else{
    message("You chose an invalid type. No file transfer will occur. Choose one of c('move', 'copy', 'none') to avoid this warning")
  }

  print(paste("This function completed at ", Sys.time(), sep = ""))
  return(rename)
  #rm(timelapse, images1, images2, images3, images4, x1, x2, x3in, x3out, dirs, dirsTemp, rename)
  #rm(timelapsefile, in.dir, out.dir, create.dirs, type)
}


## Post processing for Wildlife crossing interactions ####
### Wildlife Interactions
#### From a Dataorganize file (Added 2021-12-10)
interactionsDataOrganize <- function(x,y,exclude,start_date,end_date=Sys.Date()){
  # Formatting the columns in the AllPictures file
  colnames(x)=c("Camera","Species","Year","Month","Day","Hour","Minute","Second","Individuals")
  x1=subset(x,!(Species %in% exclude))
  x1[,4]=formatC(x1[,4], width = 2, format = "d", flag = "0")
  x1[,5]=formatC(x1[,5], width = 2, format = "d", flag = "0")
  x1[,6]=formatC(x1[,6], width = 2, format = "d", flag = "0")
  x1[,7]=formatC(x1[,7], width = 2, format = "d", flag = "0")
  x1[,8]=formatC(x1[,8], width = 2, format = "d", flag = "0")

  x1$Date_Time <- with(x1, paste(Year, Month, Day, Hour, Minute, Second))
  x1$DateTimeOriginal=as.POSIXct(strptime(x1$Date_Time,'%Y %m %d %H %M %S'))
  x1$Date=as.POSIXct(strptime(x1$Date_Time,'%Y %m %d'))

  x2 <- merge.data.frame(x1, y, by = "Camera")
  x2a=subset(x2,Date >= start_date)
  x2b=subset(x2a,Date <= as.character(end_date))
  x3 <- x2b

  x4 <- x3[,c("Type", "Site", "Camera", "Species", "Date_Time", "Individuals")]
  x4$Class <- NA
  x4$Direction <- NA

  return(x4)
  rm(x1,x2,x2a,x2b,x3,x4)
  #rm(x,y,exclude, start_date, end_date)
}

#### From a timelapse file (Added 2021-12-10)
interactionsTimelapse <- function(images, envdata, exclude, create.dirs=T, copy.files=T){
  #images <- read.csv("timelapse_out_20210830.csv")
  #envdata <- openxlsx::read.xlsx(file.choose())
  #exclude <- c("ghost", "human", "bird", "rodent", "unk_lizard", "spiny_lizard", "whiptail_lizard", "leopard_frog", "unk_amphibian")
  #create.dirs <- T
  #copy.files <- T

  # Check that the correct input file has been used
  if(colnames(images)[11]!=c("Species1")){
    warning("You may have choses an incorrect input file. This function may not work properly")
  }

  # Step 1: Combine the Species1, Species2, Species3, and SpeciesOther columns
  images1 <- images[,c(1,2,11,12)]
  colnames(images1) <- c("File", "Path", "Species", "Individuals")

  if(is.character(images$Species2)){
    images2 <- images[images$Species2!="",c(1,2,13,14)]
    colnames(images2) <- c("File", "Path", "Species", "Individuals")
  }else{
    images2 <- NULL
  }
  if(is.character(images$Species3)){
    images3 <- images[images$Species3!="",c(1,2,15,16)]
    colnames(images3) <- c("File", "Path", "Species", "Individuals")
  }else{
    images3 <- NULL
  }
  if(is.character(images$SpeciesOther)){
    images4 <- images[images$SpeciesOther!="",c(1,2,17,18)]
    colnames(images4) <- c("File", "Path", "Species", "Individuals")
  }else{
    images4 <- NULL
  }
  imagesout1 <- rbind(images1,images2,images3,images4)

  print(paste("There were ", nrow(imagesout1), " rows of data.", sep = ""))

  # Step 2: Exclude all species that we do not want to do interactions on
  imagesout2 <- subset(imagesout1, !(Species %in% exclude))

  print(paste(nrow(imagesout2), " rows contained an animal of interest.", sep = ""))

  # Step 3: Create new file paths for the interactions data
  imagesout3 <- data.frame(imagesout2, do.call(rbind,strsplit(imagesout2$Path, split = "\\\\")))
  colnames(imagesout3) <- c("File", "Path", "Species", "Individuals", "Folder", "Camera", "Date")
  imagesout4 <- merge.data.frame(imagesout3, envdata, by = "Camera", all.x = T)

  imagesout4$oldpath <- with(imagesout4, paste(Folder, Camera, Date, sep = "/"))
  imagesout4$newpath <- with(imagesout4, paste("Interactions_", Date, "/", Site, "/", Side, "/", Species, sep = ""))

  # Step 4: Create the interactions file
  intfile1 <- data.frame(imagesout4[,c("Type", "Site", "Camera", "Species")], Date_time = sub(".jpg", "", imagesout4$File, ignore.case = T), Individuals = imagesout4$Individuals, Class = NA, Direction = NA)

  # Step 5: Create new directories and subdirectories and copy pictures into them (optional)
  if(isTRUE(create.dirs)){
    print("Creating directories")
    dirs <- with(imagesout4, list(unique(paste("Interactions_", Date, sep = "")),
                                  unique(paste("Interactions_", Date, "/", Site, sep = "")),
                                  unique(paste("Interactions_", Date, "/", Site, "/", Side, sep = "")),
                                  unique(paste("Interactions_", Date, "/", Site, "/", Side, "/", Species, sep = ""))))
    dirsTemp <- lapply(dirs, function(x){
      lapply(x, function(y){
        ifelse(!dir.exists(y), dir.create(y), print("Folder exists"))
      })
    })
  }

  if(isTRUE(copy.files)){
    print("Copying images")
    with(imagesout4, file.copy(from = paste(oldpath, File, sep = "/"), to = paste(newpath, File, sep = "/")))
  }

  return(list(Interactions = intfile1, Files = imagesout4))
  rm(images1, images2, images3, images4, imagesout1, imagesout2, imagesout3, imagesout4, intfile1, dirs, dirsTemp)
  #rm(images, envdata, exclude, create.dirs, copy.files)
}


## Quality control functions ####
### Converting date-time information in a CT Table to character format (Added 2021-09-06)
ctdates_fun <- function(cttable, start.col=6){
  #cttable = CT_FM1847
  #start.col = 6

  for(i in start.col:ncol(cttable)){
    cttable[,i] <- as.character(cttable[,i])
  }
  return(cttable)
  rm(cttable, start.col)
}

### Quality control for timelapse-sorted images (Added 2022-03-24)
timelapseQC <- function(ds, exclude=NULL){
  # ds <- read.csv("timelapse_out_20220117_BL.csv)

  # Check for video files
  if(isTRUE(any(grepl(".MP4", ds$File, ignore.case = T)))){
    warning("Video files detected. Number of individuals only analyzed for images")
    video <- ds[grep(".MP4", ds$File, ignore.case = T),]
    images <- ds[-grep(".MP4", ds$File, ignore.case = T),]
  }else{
    print("No video files detected.")
    images <- ds
  }

  # Check species names
  unique_spec <- list(
    unique1 = unique(ds$Species1),
    unique2 = if(!any(is.na(unique(ds$Species2)))){unique(ds$Species2)[unique(ds$Species2)!=""]},
    unique3 = if(!any(is.na(unique(ds$Species3)))){unique(ds$Species3)[unique(ds$Species3)!=""]},
    unique4 = if(!any(is.na(unique(ds$SpeciesOther)))){unique(ds$SpeciesOther)[unique(ds$SpeciesOther)!=""]}
  )
  unique_species <- unique(do.call(c, unique_spec))

  # Check missing species data in the species 1 column
  missing_spec <- ds[ds$Species1=="",]

  # Check missing individuals data in species data
  if(is.null(exclude)){
    exclude <- c("ghost", "human", "", "rodent", "bird")
  }else{
    exclude <- exclude
  }
  no_ind <- list(
    no_ind1 = images[!(images$Species1 %in% exclude),][images[!(images$Species1 %in% exclude),]$Species1_Ind==0,],
    no_ind2 = if(!any(is.na(unique(images$Species2)))){images[!(images$Species2 %in% exclude),][images[!(images$Species2 %in% exclude),]$Species2_Ind==0,]},
    no_ind3 = if(!any(is.na(unique(images$Species3)))){images[!(images$Species3 %in% exclude),][images[!(images$Species3 %in% exclude),]$Species3_Ind==0,]},
    no_ind4 = if(!any(is.na(unique(images$SpeciesOther)))){images[!(images$SpeciesOther %in% exclude),][images[!(images$SpeciesOther %in% exclude),]$Other_Ind==0,]}
  )
  missing_ind <- do.call(rbind,no_ind)

  # Output the results
  print(paste("Unique species: ", paste(unique_species, collapse = ", "), sep = ""))
  if(nrow(missing_spec)==0){
    print("All files were labelled by species")
  }else{
    warning(paste(nrow(missing_spec), " file(s) were not labelled by species. These were: \n", paste(missing_spec$File, collapse = "\n"), sep = ""))
  }
  if(nrow(missing_ind)==0){
    print("All files were labelled by individual")
  }else{
    warning(paste(nrow(missing_ind), " file(s) were not labelled by individual. These were: \n", paste(missing_ind$RelativePath, missing_ind$File, collapse = "\n", sep = "\\"), sep = ""))
  }

  out <- list("Unique Species" = unique_species, "Missing Species" = missing_spec, "Missing Ind" = missing_ind)
  return(out)
  rm(unique_spec, unique_species, missing_spec, exclude, no_inds, missing_ind, out)
  #rm(ds)
}


## Camera diagnostics functions ####
### Camera Trap Nights (Added 2021-08-19)
trapeffort_fun <- function(cttable, group, sessions=F, sessioncol){
  #cttable <- CTtable_WCS
  #group <- "Site"
  #group <- "Station"
  #sessions <- T
  #sessioncol <- "timeperiod"

  #require(camtrapR)

  if(sessions==T){
    CamOp <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T,
                               cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE,
                               sessionCol = "timeperiod"))
    CamOp2 <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = FALSE,
                                cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE,
                                sessionCol = "timeperiod"))
  }else{
    CamOp <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T,
                               cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE))
    CamOp2 <- t(camtrapR::cameraOperation(cttable, stationCol = group, setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = FALSE,
                                cameraCol = "Camera", byCamera = FALSE, allCamsOn = FALSE, camerasIndependent = FALSE))
  }

  # Active Camera Trap Nights
  activenights <- apply(CamOp,2,function(x){sum(x,na.rm=T)})

  # Total Camera Trapping Nights
  totalnights <- apply(CamOp2,2,function(x){sum(x,na.rm=T)})

  # Cleaning and merging the camera trap nights
  if(sessions==T){
    name <- unique(cttable[,c(group,sessioncol)])

  }else{
    name <- unique(cttable[,c(group)])
  }

  trapnights <- data.frame(name, activenights, totalnights)
  rownames(trapnights) <- NULL

  return(trapnights)
  rm(CamOp, CamOp2, activenights, totalnights,name, trapnights)
  #rm(cttable, group)
}

### Number of Photos (Added 2021-08-19, Modified 2022-07-14)
imageeffort_fun <- function(x, type){
  #x <- list('20220117' = read.csv("K:/Completed/new_20220117/timelapse_out_20220117.csv"), '20220214' = read.csv("K:/Completed/new_20220214/timelapse_out_20220214.csv"))
  #type <- "timelapse"

  if(is.data.frame(x)){
    x <- list(x)
  }
  if(type=="timelapse"){
    AP <- lapply(x, YamashitaFuns2::APFun_Timelapse)
  }else if(type=="dataorganize"){
    AP <- x
  }else{
    stop("Your data must be a 'timelapse' or 'dataorganize' file.")
  }

  if(is.null(names(AP))){
    name <- F
    message("Your items do not have names. They will be outputted in the order they were inputted")
  }else{
    name <- T
  }

  ghosts <- lapply(AP, function(y){y[y[,2]=="ghost",]})
  human <- lapply(AP, function(y){y[y[,2]=="human",]})

  out <- do.call(rbind, lapply(1:length(x), function(i){
    data.frame(total = nrow(x[[i]]), animal = nrow(x[[i]]) - nrow(ghosts[[i]]) - nrow(human[[i]]), human = nrow(human[[i]]), ghost = nrow(ghosts[[i]]), success = NA)
  }))
  rownames(out) <- names(AP)

  total <- apply(out, 2, sum)
  out2 <- rbind(out,total = apply(out, 2, sum))
  if(isFALSE(name)){
    rownames(out2) <- c(1:nrow(out), "total")
  }else{
    rownames(out2) <- c(names(AP), "total")
  }
  out2[,"success"] <- with(out2, animal/total)

  return(out2)
  rm(AP, name, ghosts, human, out, total, out2)
  #rm(x, type)
}


## Camera data analysis functions ####
### Animal Diel Activity (Added 2021-08-19, Modified 2021-09-10)
actfun <- function(x, split=F, splitcol=NULL, species, bw = NULL, rep = 999){
  #x <- AP2_1min$FM1847
  #split <- F
  #splitcol <- "timeperiod"
  #species <- c("bobcat", "coyote")
  #bw = NULL
  #rep = 99

  if(split==T){
    AP <- split(x, as.factor(x[,splitcol]))
  }else{
    AP <- list(all = x)
  }

  #require(activity)

  specs_all <- lapply(AP, function(x){subset(x, x$Species %in% species)})
  specs <- lapply(specs_all, function(x){split(x, as.factor(x[,"Species"]))})

  act <- lapply(specs, function(X){lapply(1:length(X), function(s){
    y <- X[[s]]
    if(is.null(splitcol)==T){
      print(paste("Started ", names(X)[s], " at ", Sys.time(), sep = ""))
    }else{
      print(paste("Started ", names(X)[s], " for group ", unique(y[,splitcol]), " at ", Sys.time(), sep = ""))
    }
    if(is.null(bw)==T){
      if(nrow(y)>=100){
        activity::fitact(y$time_radians, sample = "model", reps = rep, bw = NULL, adj = 1.5)
      }else{
        activity::fitact(y$time_radians, sample = "data", reps = rep, bw = NULL, adj = 1.5)
      }
    }else{
      if(nrow(y)>=100){
        activity::fitact(y$time_radians, sample = "model", reps = rep, bw = bw, adj = 1.5)
      }else{
        activity::fitact(y$time_radians, sample = "data", reps = rep, bw = bw, adj = 1.5)
      }
    }
  })})
  act2 <- lapply(1:length(act), function(y){names(act[[y]]) <- names(specs[[y]]); return(act[[y]])})
  names(act2) <- names(specs)

  return(list(data = specs, activity = act2))

  rm(AP, specs_all, specs, act, act2)
  #rm(x, split, splitcol, species, bw, rep)
}

### Setting up for occupancy modelling from an APFun_env output (Added 2022-01-14, Modified 2022-07-07)
occFun <- function(x, ct, unit, subset, stationCol, sessionCol=NULL, ct_probs=T, count=F){
  #x <- AP2_swift
  #ct <- CT
  #unit <- "1 days"
  #subset <- c("rio_grande_turkey")
  #count <- F
  #ct_probs <- T
  #stationCol <- "Site"
  #sessionCol <- NULL

  #require(camtrapR)
  #require(lubridate)

  if(isFALSE(grepl("day", unit))){
    stop("Use days")
  }

  unit1 <- ceiling(as.numeric(sub("days", "", unit, ignore.case = T))/2)

  # Dealing with active and inactive camera trap nights
  if(is.logical(ct_probs)){
    if(isTRUE(ct_probs)){
      if(is.null(sessionCol)==T){
        camop <- camtrapR::cameraOperation(ct, stationCol = stationCol, cameraCol = "Camera",
                                           byCamera = F, allCamsOn = F, camerasIndependent = F,
                                           setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T)
      }else{
        warning("This function is untested when specifying sessionCol other than NULL. The sortCol may not get called properly.")
        camop <- camtrapR::cameraOperation(ct, stationCol = stationCol, cameraCol = "Camera", sessionCol = sessionCol,
                                           byCamera = F, allCamsOn = F, camerasIndependent = F,
                                           setupCol = "Setup_date", retrievalCol = "Retrieval_date", hasProblems = T)
      }
    }else if(isFALSE(ct_probs)){
      warning("When there are no problems in the ct table, the ct table is only used to gather the site list and range of dates.")
      if(is.null(sessionCol)==T){
        camop <- camtrapR::cameraOperation(ct, stationCol = stationCol, cameraCol = "Camera",
                                           byCamera = F, allCamsOn = F, camerasIndependent = F,
                                           setupCol = "Setup_date", retrievalCol = "Retrieval_date")
      }else{
        warning("This function is untested when specifying sessionCol other than NULL. The sortCol may not get called properly.")
        camop <- camtrapR::cameraOperation(ct, stationCol = stationCol, cameraCol = "Camera", sessionCol = sessionCol,
                                           byCamera = F, allCamsOn = F, camerasIndependent = F,
                                           setupCol = "Setup_date", retrievalCol = "Retrieval_date")
      }
    }
  }else{
    stop("ct_probs should be logical (T,F)")
  }

  camop[camop==0] <- -5
  camop[camop==1] <- 0

  camopt <- t(camop)

  dates <- data.frame(Date = as.Date(colnames(camop)))
  dates$Date2 <- rep(seq.Date(min(dates$Date), max(dates$Date), by = unit), each = as.numeric(sub("days", "", unit, ignore.case = T)))[1:nrow(dates)]

  camdate <- data.frame(sortCol = rep(rownames(camop), each = ncol(camop)), Date = rep(dates[,1], times = nrow(camop)), Date2 = rep(dates[,2], times = nrow(camop)))
  camdate$camdate <- with(camdate, paste(sortCol, Date, sep = "_"))

  if(ncol(camop) * nrow(camop) != nrow(camdate)){
    stop("# Cameras * # dates does not equal total rows. Check the function. This error shouldn't be possible.")
  }

  active <- aggregate(camopt, by = list(Date2 = dates$Date2), sum)
  rownames(active) <- active[,1]
  activet <- t(active[,2:ncol(active)])
  activet[activet<=(-5*unit1)] <- NA
  activet[activet>(-5*unit1)] <- 0

  print(paste("There should be ", dim(activet)[1], " rows and ", dim(activet)[2], " columns in each output.", sep = ""))

  # Now for the camera data
  x$sortCol <- x[,stationCol]
  x$Date <- as.Date(x$Date)
  x$camdate <- with(x, paste(sortCol, Date, sep = "_"))
  x1 <- split(x, factor(x$Species))
  x2 <- x1[subset]

  x3 <- lapply(x2, function(a){
    a1 <- aggregate(Individuals~camdate, data = a, sum)
    a2 <- merge.data.frame(camdate, a1, by = "camdate", all.x = T)
    a2$Individuals[is.na(a2$Individuals)] <- 0

    a3 <- aggregate(Individuals~sortCol+Date2, data = a2, sum)

    a4 <- reshape(a3, direction = "wide", timevar = "Date2", idvar = "sortCol")
    row.names(a4) <- a4[,1]
    a5 <- as.matrix(a4[,-1])
    colnames(a5) <- as.character(unique(a3$Date2))

    if(dim(a5)[1]!=dim(activet)[1] | dim(a5)[2]!=dim(activet)[2]){
      warning("Your camera data and ct table have different numbers of sites and dates")
    }

    if(is.logical(count)==T){
      if(isFALSE(count)){
        a5 <- ifelse(a5>0,1,0)
      }
    }else{
      stop("The count variable must be a logical (T,F)")
    }

    length(a5[a5>0])

    if(ct_probs==T){
      comb <- a5 + activet
      print(paste(length(comb[comb>0 & !is.na(comb)]), " camera trap nights had a detection of a ", unique(a$Species), sep = ""))
      if(length(comb[comb>0 & !is.na(comb)]) < length(a5[a5>0])){
        warning(paste("For ", unique(a$Species), ", inactive camera trap nights removed some detections. This is what this function is supposed to do.", sep = ""))
      }else if(length(comb[comb>0 & !is.na(comb)]) > length(a5[a5>0])){
        warning(paste("For ", unique(a$Species), ", Something strange happened. Detections were added when accounting for inactive camera trap nights."))
      }
      return(comb)
    }else{
      print(paste(length(a5[a5>0]), " camera trap nights had a detection of a ", unique(a$Species), sep = ""))
      return(a5)
    }
    rm(a1,a2,a3,a4,a5,comb)
    rm(a)
  })

  if(is.logical(count)==T){
    if(isFALSE(count)){
      print("Presence/absence data is outputted")
    }else{
      print("Abundance data is outputted")
    }
  }else{
    stop("The count variable must be a logical (T,F)")
  }

  return(x3)

  rm(unit1, camop, camopt, dates, camdate, active, activet, x1, x2, x3)
  #rm(x, ct, unit, subset, stationCol, sessionCol, ct_probs, count)
}


## Working with GPS collar data ####
### Converting time zones to local time (Added 2022-03-24)
timeConvert <- function(ds, date.col, time.col, LocalTZ){
  #ds <- bob
  #date.col <- "RTC.date"
  #time.col <- "RTC.time"
  #LocalTZ <- "America/Chicago"

  date1 <- lubridate::ymd(ds[,date.col])
  time1 <- lubridate::hms(ds[,time.col])

  datetime1 <- paste(lubridate::year(date1), lubridate::month(date1), lubridate::day(date1), lubridate::hour(time1), lubridate::minute(time1), lubridate::second(time1), sep = " ")
  datetime2 <- lubridate::ymd_hms(datetime1)

  tzconv <- lubridate::with_tz(datetime2, tzone = LocalTZ)

  date2 <- as.Date(tzconv)
  time2 <- paste(formatC(lubridate::hour(tzconv), width = 2, flag = "0"),
                 formatC(lubridate::minute(tzconv), width = 2, flag = "0"),
                 formatC(lubridate::second(tzconv), width = 2, flag = "0"), sep = ":")

  ds$LocalDate <- date2
  ds$LocalTime <- time2

  return(ds)
  rm(date1, date2, time1, time2, datetime1, datetime2, tzconv)
  #rm(ds, date.col, time.col, LocalTZ)
}
