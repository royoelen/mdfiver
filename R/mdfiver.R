#' Create md5 checksum files
#'
#' Create md5 checksum files from supplied file
#' @param file_loc the file to create the md5 checksum file for
#' @param md5_file_loc the location of the md5 checksum file to create, defaults to the input file with .md5 appended to the end (or .sha256 if that option is used)
#' @param use_sha256 use the more secure SHA256 checksum instead of md5 
#' @return The location of the md5 checksum file
#' @examples 
#' md5_from_file_loc <- create_md5_for_file('~/file_contents.tsv.gz');
#' md5_from_file_loc <- create_md5_for_file('~/file_contents.tsv.gz', '~/file_contents.tsv.gz.md5');
#' @export
create_md5_for_file <- function(file_loc, md5_file_loc=NULL, use_sha256=F) {
  # setup the output
  output_loc_md5 <- md5_file_loc
  # overwrite if empty
  if (is.null(md5_file_loc)) {
    if (use_sha256) {
      output_loc_md5 <- paste(file_loc, '.sha256', sep = '')
    }
    else {
      output_loc_md5 <- paste(file_loc, '.md5', sep = '')
    }
  }
  # check if file exists
  if (file.exists(file_loc)) {
    # check if the file is empty
    if (file.size(file_loc) > 0) {
      # create the md5 checksum
      if (use_sha256) {
        md5_checksum <- as.character(openssl::sha256(file((file_loc))))
      }
      else {
        md5_checksum <- tools::md5sum(file_loc)[[1]]
      }
      # check if the md5 file already exists
      if (file.exists(output_loc_md5)) {
        warning(paste('md5 file at', output_loc_md5, 'already exists and will be overwritten', sep = ' '))
      }
      # write result
      write.table(
        data.frame(
          x = c(md5_checksum),
          y = c(file_loc)
        ),
        output_loc_md5,
        quote = F,
        row.names = F,
        col.names = F, 
        sep ='\t'
      )
    }
    else {
      stop(paste('file, ', file_loc, ' has size of zero', sep = ''))
    }
  }
  # stop if file does not exist
  else {
    stop(paste('file, ', file_loc, ' does not exist', sep = ''))
  }
  return(output_loc_md5)
}

#' Check md5 checksum files
#'
#' Check md5 checksum against supplied file
#' @param file_loc the file to create the md5 checksum file for
#' @param md5_file_loc the location of the md5 checksum file to check against, defaults to the input file with .md5 (or .sha256 if option is used) appended to the end if neither this or the md5_hash parameters are supplied 
#' @param md5_hash the md5 hash to check the file against, takes precedense over md5_file_loc parameter
#' @param use_sha256 use the more secure SHA256 checksum instead of md5 
#' @return True if the hashes match, False if the hashes do not match
#' @examples 
#' md5_matches <- check_md5_file('~/file_contents.tsv.gz');
#' md5_matches <- check_md5_file('~/file_contents.tsv.gz', md5_file_loc = '~/file_contents.tsv.gz.md5');
#' md5_matches <- check_md5_file('~/file_contents.tsv.gz', md5_hash = '7aedffa4687d37d4007bbd8e7fcf000d');
#' @export
check_md5_file <- function(file_loc, md5_file_loc=NULL, md5_hash=NULL, use_sha256=F) {
  # init variable
  md5_checksum_file <- NULL
  # check if file exists
  if (file.exists(file_loc)) {
    # check if the file is empty
    if (file.size(file_loc) > 0) {
      # calculate the md5 of the file
      if (use_sha256) {
        md5_checksum_file <- as.character(openssl::sha256(file((file_loc))))
      }
      else{
        
        md5_checksum_file <- tools::md5sum(file_loc)[[1]]
      }
    }
    else {
      stop(paste('file, ', file_loc, ' has size of zero', sep = ''))
    }
  }
  # stop if file does not exist
  else {
    stop(paste('file, ', file_loc, ' does not exist', sep = ''))
  }
  # set the md5 hash
  md5_hash_to_check <- md5_hash
  # if the supplied md5 is NULL, instead try to get the md5 from a file
  if (is.null(md5_hash)) {
    # set the md5 file hash
    md5_file_loc_to_check <- md5_file_loc
    # if this one is also empty, we'll get the file based on the input file
    if (is.null(md5_file_loc)) {
      # paste together the file loc
      if (use_sha256) {
        md5_file_loc_to_check <- paste(file_loc, '.sha256', sep = '')
      }
      else {
        md5_file_loc_to_check <- paste(file_loc, '.md5', sep = '')
      }
    }
    # check if the file exists
    if (file.exists(md5_file_loc_to_check)) {
      # now read the md5
      md5_table <- read.table(md5_file_loc_to_check, header = F, stringsAsFactors = F)
      md5_hash_to_check <- md5_table$V1
      # check if there is a second column
      if (ncol(md5_table) > 1 & md5_table$V2 != (file_loc)) {
        warning(paste('md5 file to check against, and file listed in md5 file do not match', (file_loc), md5_table$V2))
      }
    }
    else {
      stop("no md5 supplied and md5 file location supplied or inferred (file_loc+'.md5') file location don't exist")
    }
  }
  print(md5_checksum_file)
  print(md5_hash_to_check)
  # check if they are the same
  if (md5_checksum_file == md5_hash_to_check) {
    return(T)
  }
  else {
    return(F)
  }
}

#' Create sha256 checksum files
#'
#' Create sha256 checksum files from supplied file
#' @param file_loc the file to create the sha256 checksum file for
#' @param sha256_file_loc the location of the md5 checksum file to create, defaults to the input file with .md5 appended to the end (or .sha256 if that option is used)
#' @return The location of the sha256 checksum file
#' @examples 
#' sha256_from_file_loc <- create_md5_for_file('~/file_contents.tsv.gz');
#' sha256_from_file_loc <- create_md5_for_file('~/file_contents.tsv.gz', '~/file_contents.tsv.gz.sha256');
#' @export
create_sha256_for_file <- function(file_loc, sha256_file_loc=NULL) {
  create_md5_for_file(file_loc, md5_file_loc = sha256_file_loc, use_sha256=T)
}

#' Check sha256 checksum files
#'
#' Check sha256 checksum against supplied file
#' @param file_loc the file to create the sha256 checksum file for
#' @param sha256_file_loc the location of the sha256 checksum file to check against, defaults to the input file with .md5 (or .sha256 if option is used) appended to the end if neither this or the md5_hash parameters are supplied 
#' @param sha256_hash the sha256 hash to check the file against, takes precedense over sha256_file_loc parameter
#' @return True if the hashes match, False if the hashes do not match
#' @examples 
#' sha256_matches <- check_sha256_file('~/file_contents.tsv.gz');
#' sha256_matches <- check_sha256_file('~/file_contents.tsv.gz', sha256_file_loc = '~/file_contents.tsv.gz.md5');
#' sha256_matches <- check_sha256_file('~/file_contents.tsv.gz', sha256_hash = '7aedffa4687d37d4007bbd8e7fcf000d');
#' @export
check_sha256_file <- function(file_loc, sha256_file_loc=NULL, sha256_hash=NULL) {
  check_md5_file(file_loc, md5_file_loc = sha256_file_loc, md5_hash = sha256_hash, use_sha256=T)
}