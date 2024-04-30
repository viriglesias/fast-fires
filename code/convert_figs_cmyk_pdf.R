#TLM - Earth Lab

# Function to convert a file into CMYK format using ImageMagick.
# Note that ImageMagick must be installed and accessible via command line.
# PARAMETERS
# file :: a file to convert to cmyk format
# outDir :: a directory to output the converted file. If left empty, will output in same directory as the original file.
# Example usage:
# pngs <- list.files(here::here('figs'), pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE)
# pngs |> purrr::walk(~ convert.to.cmyk(file = .x))
convert.to.cmyk <- function(file, outDir = NA) {

  # Check if the file exists
  if (!file.exists(file)) {
    warning(paste("File does not exist:", file))
    next
  }
  
  # Set outdir
  if(is.na(outDir)) {
    directory <- dirname(file)
  } else {
    directory <- outDir
  }
  
  # Construct the new filename by appending '_cmyk' before the file extension
  filePath <- normalizePath(file, mustWork = FALSE)
  fileNm <- tools::file_path_sans_ext(basename(filePath))
  fileExt <- tools::file_ext(filePath)
  newFileNm <- paste0(fileNm, "_cmyk.", fileExt)
  newPath <- file.path(directory, newFileNm)
  
  # newFileName <- file.path(directory, paste0(tools::file_path_sans_ext(basename(file)), "_cmyk", tools::file_ext(file)))
  # 
  # # Construct the new filename by appending '_cmyk' before the file extension
  # file_path <- normalizePath(file, mustWork = FALSE)
  # file_info <- tools::file_path_sans_ext(file_path)
  # file_ext <- tools::file_ext(file_path)
  # new_file_name <- paste0(file_info, "_cmyk.", file_ext)
  
  # Form the command to convert the file to CMYK
  command <- sprintf('magick "%s" -colorspace CMYK "%s"', filePath, newPath)
  
  # Execute the command using system()
  system(command, intern = FALSE)
  
  # Print the status message
  cat("Converted:", filePath, "to CMYK and saved as:", newFileNm, "\n")
}

# Function to convert a pdf into a png
# PARAMETERS
# pdfFile :: a file to convert to png
# dpi :: the required dpi of the converted png
# outDir :: a directory to output the converted file. If left empty, will output in same directory as the original file.
# Example usage:
# pdfs <- list.files(here::here('figs'), pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
# pdfs |> purrr::walk(~ convert.pdf.to.png(pdfFile = .x, dpi = 300))
convert.pdf.to.png <- function(pdfFile, dpi, outDir = NA) {
  
  # Set outdir
  if(is.na(outDir)) {
    directory <- dirname(pdfFile)
  } else {
    directory <- outDir
    if(!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
  }
  
  #Create new path
  fileNm <- tools::file_path_sans_ext(basename(pdfFile))
  newFileNm <- paste0(fileNm, ".png")
  newPath <- here::here(directory, newFileNm)
  
  #image_write does not have an overwrite option! Do this manually
  if(file.exists(newPath)) {
    print(paste0("Overwriting file: ", newPath))
    file.remove(newPath)
  }
  
  #Convert & write out
  pngImage <- pdftools::pdf_render_page(pdfFile, page = 1, dpi = dpi)
  writeImage <- magick::image_read(pngImage)
  magick::image_write(image = writeImage, path = newPath, format = "png")
}

library(here)
library(pdftools)
library(magick)
library(purrr)



#Convert pdfs from Virginia to 300 dpi pngs
pdfs <- list.files(here::here('figs', 'pdfs'), pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)
pdfs |> purrr::walk(~ convert.pdf.to.png(pdfFile = .x, dpi = 300, outDir = here::here('figs', 'pngs')))

#Convert all PNGs to CMYK versions per Science journal requirements
pngs <- list.files(here::here('figs', 'pngs'),pattern = "\\.png$", full.names = TRUE, ignore.case = TRUE)
pngs |> purrr::walk(~ convert.to.cmyk(file = .x, outDir = here::here('figs', 'cmyk_converted')))




