\name{cameraRename2}
\alias{cameraRename2}
\title{(Deprecated) Rename Camera Trap Images
}
\description{The cameraRename2 version seeks to avoid using the camtrapR package to rename images and does so by directly reading the exif data using the exiftoolr package, then renaming based on the datetimeoriginal column of the exif data. This version has the advantage of being able to spit out any exif data the user wants (including information on the trigger number, whether images were external or internal trigger, camera name, etc.). This function is also more generalizable than the original cameraRename function and allows for performing specialRenamer like adjustment on images.

Before running this function for the first time on a new machine see the details below.
}
\usage{
cameraRename2(in.dir, out.dir, file.type, trigger.info=NULL,
              rename=FALSE, adjust = NULL, fix.names = FALSE)
}
\arguments{
  \item{in.dir}{String. The directory where the pictures are located. I typically specify a folder containing the camera folders although theoretically any folder could work. The function can look for either ".jpg" or ".mp4" files.}
  \item{out.dir}{multiple. The directory where the pictures will be renamed to. If left unspecified, this will default to the in.dir. Use this if you want to change the location of the images. You can specify this as NULL or "in.dir" to keep out.dir the same as in.dir or you can specify a vector of length 1 or length [number of pictures] to output your images to a new location.}
  \item{file.type}{String. The type of file you want to rename. For images, this is likely to be ".jpg". For videos, this is likely to be ".MP4". This can also take c("image", "video"). For images, this will default the file type to ".jpg", for videos, it will default to ".mp4". If a different file type is needed, manually specify the type. You must include the "." in the file type. This function can theoretically take any image format, although I do not know how exiftool reads file types other than ".jpg" and ".mp4". The function will ignore case when looking for file types. }
  \item{trigger.info}{String or NULL. Should additional information besides the date-time be included in the output? This defaults to NULL where no additional information is included. Because camera-specific information is variable between camera models, you must specify the camera model if you want additional information. Currently, only c("Hyperfire2", "Ultrafire_Video") are supported. Note that Ultrafire pictures use the same metadata tags as the Hyperfire2. Additional camera models could be added. See details for more information on this.}
  \item{rename}{Logical or String. Should the images be renamed? The options are c(TRUE, FALSE, "replace", "copy", "none"). TRUE and "replace" will replace/move images with their new file names in the location specified by out.dir. "copy" will create a copy of the images in the out.dir. This has not been tested with an out.dir that is the same as in.dir. FALSE or "none" does not rename images. The default is FALSE. Typically, you should specify an option that moves/copies images but if you wanted to test the output before performing the rename, you can set to FALSE. See examples for how to rename pictures if set to FALSE or "none".}
  \item{adjust}{String or NULL. Do the image date-times need adjustment? This defaults to NULL, indicating no adjustment needed. You only need to specify this if your image date-times need to be adjusted. This could arise due to daylight savings time or misentered date-time on the camera. This can be thought of as an R version of SpecialRenamer. You can either specify a difftime object or a character vector of the original and new date-times.}
  \item{fix.names}{Logical. The default is FALSE. Do you need to rename all file names or just update those that don't have serial numbers and replace those that were not originally renamed? Only specify this to TRUE if you are using this function to update/fix images that were originally renamed by Renamer/SpecialRenamer and are in the old format. Only use this option if you know what you are doing. See details below.}
}
\details{Important: You need to load the exiftoolr library and run the function, exiftoolr::install_exiftool(), before running this function. This is because the package does not have a default version of exiftool available before you run the function. See the help documentation in exiftoolr for more details about this and the reasoning behind it. This function will check if your system has exiftool installed and if it doesn't, it will warn you and install exiftool using default settings.

If you are interested in additional metadata information that is not provided, please run exiftoolr::exif_read(["a few images"]) and choose the columns you are interested in. Send me the exact column names, and your camera model and I can add an option to trigger.info. Please keep in mind that the more metadata tags you choose, the longer the function takes to run.

If you need to fix names (or add a serial number to existing names) rather than replace names, then you should specify fix.names to TRUE. By specifying this, it is telling the tool to check if the images are in the form YYYY MM DD HH MM SS.jpg (or, more likely some consistent form with spaces but I don't know for sure). When it does this, if an image does not fit this form, it replaces the name with the new date-time information derived from the metadata. The reason for this is that sometimes camera date-times are wrong so we needed to make an adjustment, similar to what is done with the adjust call except using SpecialRenamer. Because I did not want to go back through the images and find those that were orignally fixed and fix them again, this should leave those photos alone and only adjust inproperly named images.
}
\value{A data frame containing the input directory, output directory, the old image name, new image name, and any additional metadata information asked for using trigger.info.
}
\references{
exiftool: https://exiftool.org/

exiftoolr package: https://github.com/JoshOBrien/exiftoolr

Renamer and SpecialRenamer: https://smallcats.org/resources/
}
\author{Thomas J. Yamashita}

\note{This function has the potential for and was built to be expanded on. If you want to add something related to what information you want from the function or want to be able to turn on/off certain aspects of the function, let me know and I can hopefully modify it. Thanks, Duston, for being the guinea pig for some of these adjustments.
}

\section{Warning}{
In previous versions of this function, there was an issue with file.rename not working properly for large numbers of files. This was due to issues with the paste function not properly concatenating columns. This is theoretically resolved in the current version of the function.
}

\seealso{
  \code{\link{cameraRename}}, \code{\link{cameraRename3}}, \code{\link[exiftoolr]{install_exiftool}}
}
\examples{
## Not run

## The function is currently defined as
function (x)
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{manip}
\keyword{ files }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
\concept{Camera Trapping}
\concept{Image Rename}
% Use only one concept per line.
