.libPaths(unique(c(
  "R-libs/",
  "utils/build/local_R_library/R-libs/",
  .libPaths()
)))

# The call to dyn.load below is needed on ecgate@ECMWF, otherwise loading the V8 R-lib will fail.
tryCatch(
  if(Sys.getenv("ECPLATFORM") == "ecgb") {
    dyn.load("/autofs/perm/us/uscs/R/v8/gcc-7/v8/out/x64.release/lib.target/libv8.so")
  },
  # Leaving eventual error msgs for when the code attempts to load the V8 R-lib
  error=function(e) NULL,
  warning=function(w) NULL
)
