## Test environments
* local OS X install, R 3.4, R devel
* ubuntu 12.04 (on travis-ci), R 3.4, R devel
* win-builder (R 3.4, devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 


There was 2 NOTES:

checking installed package size ... NOTE
  installed size is 13.8Mb
  sub-directories of 1Mb or more:
    htmlwidgets  11.1Mb

checking R code for possible problems ... NOTE
read.freesurf.gii: no visible global function definition for
  ‘installed.packages’
threejs_scene.default: no visible global function definition for
  ‘col2rgb’
Undefined global functions or variables:
  col2rgb installed.packages
Consider adding
  importFrom("grDevices", "col2rgb")
  importFrom("utils", "installed.packages")
to your NAMESPACE file.
