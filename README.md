# mdfiver

simple R package to create md5 checksum files of a given file

## installation

You can install via github, no cran or bioconductor unfortunately

```r
library(devtools)
install_github("https://github.com/royoelen/mdfiver");
```

## usage

You can create an md5 checksum file using on of two function this library has
```r
md5_from_file_loc <- mdfiver::create_md5_for_file('~/big_binary.rds');
```

By default it will create an md5 checksum file with the same name as the input file, just with .md5 added to the end.

You can also manually set the location of the checksum file to be generated
```r
md5_from_file_loc <- mdfiver::create_md5_for_file('~/big_binary.rds', '~/big_binary.rds.md5');
```


To check an md5 against a file you can use the other method
```r
md5_matches <- check_md5_file('~/big_binary.rds', md5_hash = '7aedffa4687d37d4007bbd8e7fcf000d');
```


You can also supply the file that has the md5
```r
md5_matches <- check_md5_file('~/big_binary.rds', md5_file_loc = '~/big_binary.rds.md5');
```


If you omit both the md5_hash and md5_file_loc, the md5 file is inferred to be the source file with '.md5' appended
```r
md5_matches <- check_md5_file('~/big_binary.rds');
```
