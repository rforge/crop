\name{dev_off_crop}
\alias{dev.off.crop}
\title{Cropping Device}
\description{
  \code{dev.off.crop()} calls \code{\link{dev.off}()} to close the
  plotting device and then crops the produced graphic. This works on
  Unix-like systems if the right underlying Unix tools are installed and
  found (see below).
}
\usage{
dev.off.crop(file=NULL, warn=TRUE, ...)
}
\arguments{
  \item{file}{An output file name including extension.}
  \item{warn}{A \code{\link{logical}} indicating whether warnings shall
    be given in case the wrong operating system is used or the
    underlying Unix tools are not available.}
  \item{\dots}{Additional arguments passed to \code{\link{dev.off}()}.}
}
\value{
  \code{\link{invisible}()}.
}
\details{
  Supported file extensions are \code{.ps} (\code{epstool}), \code{.eps}
  (\code{epstool}), \code{.pdf} (\code{pdfcrop}),
  \code{.png} (\code{mogrify}) if the underlying
  tools (given in parentheses) are installed and found. If they are not
  installed or found or if the wrong operating system is used, simply
  \code{dev.off()} is called (and thus no cropping is done).
}
\author{Marius Hofert}
\seealso{
  \code{\link{dev.off}()} for closing a device.
}
\examples{
## Example data
x <- 1:10
y <- rev(x)

## PS
postscript(file=(file <- "crop.ps"))
plot(x, y, xlab="x", ylab="y")
dev.off.crop(file=file)

## EPS
setEPS() # or use onefile=FALSE, horizontal=FALSE, paper="special" in postscript()
postscript(file=(file <- "crop.eps"))
plot(x, y, xlab="x", ylab="y")
dev.off.crop(file=file)

## PDF
pdf(file=(file <- "crop.pdf"))
plot(x, y, xlab="x", ylab="y")
dev.off.crop(file=file)

## PNG with white background
png(filename=(file <- "crop.png"))
plot(x, y, xlab="x", ylab="y")
dev.off.crop(file=file)

## PNG with transparent background
## Not doable by the underlying 'mogrify' (only cropped vertically, not horizontally)
if(FALSE) {
    png(filename=(file <- "crop.png"), bg="transparent")
    plot(x, y, xlab="x", ylab="y")
    dev.off.crop(file=file)
}
}
\keyword{utilities}
