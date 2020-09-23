read_sysfile <- function (f0, pkg) {

  f <- system.file(f0, package = pkg)
  if (f == "") {
    f <- system.file(paste0("inst/", f0), package = pkg)
  }
  print(f)

  f
}
