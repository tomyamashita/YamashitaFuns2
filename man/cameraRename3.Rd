\name{cameraRename3}
\alias{cameraRename3}
\title{Rename Camera Trap Images
}
\description{This function is used to extract metadata information and rename camera trap images using date-times and an assigned serial number to each image. This function has all the same capabilities as the cameraRename2 function but should handle large datasets and corrupt files better. Unlike the cameraRename or cameraRename2 functions, this version keeps everything within each camera directory, running the same process on each camera. This allows it to take advantage of parallel processing and other functions to improve speed for very large datasets.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
cameraRename3(in.dir, out.dir=NULL, file.type, trigger.info=NULL, rename="none", return.type = "list", adjust = NULL, fix.names = FALSE, pp = FALSE, cores.left = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{in.dir}{String. The directory where the pictures are located. I typically specify a folder containing the camera folders although any folder containing images somewhere in the directory tree could work. The function can look for either ".jpg" or ".mp4" files. See file.type.}
  \item{out.dir}{String or Vector. The directory where the pictures will be renamed to. The default is NULL which will use the in.dir as the location for images. Another way to specify the output location to be the input location is to set out.dir = "in.dir". You can use this to change the output location for each camera directory. You can specify this with a single string, in which case it will use the relative paths to the images to locate the directory location or you can specify a vector of directories. The length of this vector must equal the number of camera directories in the in.dir. If rename="copy", you should specify a different out.dir than in.dir although the function does not check for this.}
  \item{file.type}{String. The type of file you want to rename. For images, this is likely to be ".jpg". For videos, this is likely to be ".MP4". This can also take c("image", "video"). For "image", this will default the file type to ".jpg", for "video", it will default to ".mp4". If a different file type is needed, manually specify the type. You must include the "." in the file type. This function can theoretically take any image format, although I do not know how exiftool reads file types other than ".jpg" and ".mp4". The function will ignore case when looking for file types.}
  \item{trigger.info}{String or NULL. Should additional information besides the date-time be included in the output? This defaults to NULL where no additional information is included. Because camera-specific information is variable between camera models, you must specify the camera model if you want additional information. Currently, only c("Hyperfire2", "Ultrafire_Video") are supported. Note that Ultrafire pictures use the same metadata tags as the Hyperfire2. Additional camera models could be added. See details for more information on this.}
  \item{rename}{String. The default is none. Other options include c("replace", "copy"). This specifies how images should be renamed. "none" tells R to not rename any pictures, "replace" replaces the name then moves the image with its new name to the out.dir, and "copy" creates a new copy of the image with its new name in the out.dir. Copy has not been tested with an out.dir the same as the in.dir but it is likely that it will keep both images and you will end up with duplicates. I suggest choosing a different out.dir if you are going to copy. Copy also takes significantly longer than replace.}
  \item{return.type}{String. How should the data be outputted. Options are c("list", "df"). The default is as a list where each camera directory is in its own item in the list. If you output a "df", the list of each camera is converted into a single data.frame object.}
  \item{adjust}{String or NULL. Do the image date-times need adjustment? This defaults to NULL, indicating no adjustment needed. You only need to specify this if your image date-times need to be adjusted. This could arise due to daylight savings time or misentered date-time information on the camera. This can be thought of as an R version of SpecialRenamer. You can either specify a difftime object or a character vector of the original and new date-times. If you specify a vector or original and new date-times, this must have length==2.}
  \item{fix.names}{Logical. Should a serial number be appended to date-time names? The default is FALSE. This was added to allow for updating image names to include a unique serial number that have already been run through a program like Renamer or SpecialRenamer where it may be uncertain if the DateTimeOriginal in the Exif data is accurate or not. Only set this to TRUE if you know what you are doing. It is highly recommended that you set rename="none" for this then rename manually after running the function.}
  \item{pp}{Logical. Should this function take advantage of parallel processing. The default is FALSE. Because the function separates tasks by camera directory, it can use parallel processing to run multiple cameras at the same time. This is currently set up to run in Windows OS so I do not know if this will work on a Mac or Linux system. If you want this functionality on a Unix device and know how to set it up, let me know and I can incorporate it.}
  \item{cores.left}{Numeric. How many cores should be left available when using parallel processing? The default is NULL. This is only necessary when pp=TRUE. If left at the default, the function will default to 2 cores remaining which is generally enough to continue using a PC while the function runs. I would set this to be greater than 0, otherwise the function will use the entire processing power of your computer. To see how many cores you have available for parallel processing, use: parallel::detectCores().}
}
\details{Important: Exiftool must be available on the machine in its default location for the function to work. This function will check for exiftool and install it into the default directory if it is not available before running. Previous versions of this function required you to manually check this but this requirement has been removed.

If you are interested in additional metadata information that is not provided, please run exiftoolr::exif_read(["at least one image"]) and choose the columns you are interested in. Send me the exact column names, and your camera model and I can add an option to trigger.info. Please keep in mind that the more metadata tags you choose, the longer the function takes to run.

If you need to fix names (or add a serial number to existing names) rather than replace names, then you should specify fix.names to TRUE. By specifying this, it is telling the tool to check if the images are in the form YYYY MM DD HH MM SS.jpg (or, more likely some consistent form with spaces but I don't know for sure). When it does this, if an image does not fit this form, it replaces the name with the new date-time information derived from the metadata. The reason for this is that sometimes camera date-times are wrong so we needed to make an adjustment using SpecialRename, similar to what is done with the adjust call in this function. Because I did not want to go back through the images and find those that were orignally fixed and fix them again, this should leave those photos alone and only adjust improperly named images.

%%  ~~ If necessary, more details than the description above ~~
}
\value{This function outputs either a list or a data.frame, depending on whether return.type = c("list", "df")
  \item{list}{If a list, each camera directory will be kept in a separate item in the list. This allows for easy checking if there is a problem which would have thrown an error when running exiftool, namely from a corrupt file. After outputting a list, you can combine the items into a single data.frame using do.call(rbind, out) where out is the name of the list object outputted by this function.}
  \item{df}{If a data.frame, all camera directories will be combined into a single output file.}
}
\references{
exiftool: https://exiftool.org/

exiftoolr package: https://github.com/JoshOBrien/exiftoolr

Renamer and SpecialRenamer: https://smallcats.org/resources/
}
\author{Thomas J. Yamashita}

\note{Differences between this function and cameraRename2: There were several major changes in this function that differentiate it from the cameraRename2 function enough that it warranted its own function rather than an update. The main change and this affected everything down the line was that the function now acts on each camera directory separately. By doing it this way, it allows for speed improvements on large numbers of folders and images using parallel processing, allows for the addition of progress bars to help track where it is, and allows for a way to check for corrupt image files. One of the main issues with the cameraRename2 function is that if one image is bad, the entire function will fail, something that I discovered when running it on 1,031,000 pictures in 47 folders. Therefore we needed a way to ensure that if there is a bad image, the function will skip it and keep going. In this case, it skips the entire folder and keeps going, allowing it to not be caught and stopped by a bad file.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{cameraRename2}}
  \code{\link{findCorruptImages}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ files }
\keyword{ manip }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{ Camera Trapping }
\concept{ Image Rename }
% Use only one concept per line.
