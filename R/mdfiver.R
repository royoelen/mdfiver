#' Create md5 checksum files
#'
#' Create md5 checksum files from supplied file
#' @param file_loc the file to create the md5 checksum file for
#' @param md5_file_loc the location of the md5 checksum file to create, defaults to the input file with .md5 appended to the end 
#' @return The location of the md5 checksum file
#' @examples 
#' md5_from_file_loc <- create_md5_for_file('~/file_contents.tsv.gz');
#' md5_from_file_loc <- create_md5_for_file('~/file_contents.tsv.gz', '~/file_contents.tsv.gz.md5');;
#' @export
create_md5_for_file <- function(file_loc, md5_file_loc=NULL) {
  # setup the output
  output_loc_md5 <- md5_file_loc
  # overwrite if empty
  if (is.null(md5_file_loc)) {
    output_loc_md5 <- paste(file_loc, '.md5', sep = '')
  }
  # check if file exists
  if (file.exists(file_loc)) {
    # create the md5 checksum
    md5_checksum <- tools::md5sum(file_loc)
    # check if the md5 file already exists
    if (file.exists(output_loc_md5)) {
      warning(paste('md5 file at', output_loc_md5, 'already exists and will be overwritten', sep = ' '))
    }
    # write result
    write.table(
      data.frame(
        x = md5_checksum[[1]]
      ),
      output_loc_md5,
      quote = F,
      row.names = F,
      col.names = F
    )
  }
  # stop if file does not exist
  else {
    stop(paste('file, ', file_loc, ' does not exist', sep = ''))
  }
  return(output_loc_md5)
}

