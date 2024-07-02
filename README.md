# mdfiver

simple R package to create md5 checksum files of a given file

## installation

You can install via github, no cran or bioconductor unfortunately

```r
library(devtools)
install_github("https://github.com/royoelen/mdfiver");
```

## usage

You can create and md5 checksum file using the only function this library has
```r
md5_from_file_loc <- mdfiver::create_md5_for_file('~/file_contents.tsv.gz');
```

By default it will create an md5 checksum file with the same name as the input file, just with .md5 added to the end.
You can also manually set the location of the checksum file to be generated

You can create and md5 checksum file using the only function this library has
```r
md5_from_file_loc <- mdfiver::create_md5_for_file('~/file_contents.tsv.gz', '~/file_contents.tsv.gz.md5');
```
