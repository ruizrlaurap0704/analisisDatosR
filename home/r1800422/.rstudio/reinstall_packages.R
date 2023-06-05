show <- function(msg) {
    write(msg, stdout())
}
log_progress <- function(msg, log_only=FALSE) {
  if (missing(log_only) || !log_only) {
    stringified_msg <- paste(unlist(msg), collapse='\n')
    show(stringified_msg)
  }
  logFile <- "/mnt/share/cloud/os_change.log"
  tm <- as.POSIXlt(Sys.time(), "UTC")
  now <- strftime(tm, "%Y-%m-%dT%H:%M:%SZ")
  log_msg <- gsub("\"", "\\\\\\\"", paste(unlist(msg), collapse=' \\n '))
  cat(paste0('{"time":"', now, '","logger":"os_change","message":"', log_msg, '"}', '\n'), file = logFile, append = TRUE)
}

home_dir <- base::path.expand("~")
lib_tmp <- paste0(home_dir, "/R/tmp")
lib_dir <- paste0(home_dir, "/R/x86_64-pc-linux-gnu-library")
saved_lockfile <- paste0(home_dir, "/.rstudio/renv.lock")
interrupted <- paste0(home_dir, "/.rstudio/.reinstall_interrupted")
success <- FALSE
options(renv.verbose = TRUE)

tryCatch({
  sink("/mnt/share/cloud/package_reinstall.out", split = TRUE, type = "output")
  sink(stdout(), type = "message") # stderr goes to the same output as stdout

  show("We have detected that your installed R packages need to be re-installed to work with the new OS version.")
  show("Please wait while we re-install your R packages...")

  show("-- Start Package Re-install --")

  # do not remove - this message is used to track metrics
  log_progress("Starting")

  # install renv in a temp location so it won't be part of the snapshot/restore itself
  log_progress("Installing renv...")
  renvlibpath <- tempfile("renv-library-")
  dir.create(renvlibpath)
  install.packages("renv", lib = renvlibpath)
  library(renv, lib.loc = renvlibpath)

  # generate a new snapshot file unless there is one from a previous interrupted re-install
  lockfile <- "/mnt/share/cloud/renv.lock"
  if (file.exists(saved_lockfile)) {
    log_progress("Using saved lock file...")
    lockfile <- saved_lockfile

    # saved lockfile could be using a non-current OS version in its CRAN repo URL
    # so we determine the current OS version and use it when restoring
    distcodename <- grep("DISTRIB_CODENAME", readLines("/etc/lsb-release"), value = TRUE)
    osversion <- strsplit(distcodename, "=")[[1]][2]
    cranrepo <- paste0("http://rspm/default/__linux__/", osversion, "/latest")

    log_progress("Restoring packages...")
    renv::restore(rebuild = TRUE, prompt = FALSE, lockfile = lockfile, repos = c(CRAN = cranrepo, RSPM = 'http://cran.rstudio.org'))
  }
  else {
    log_progress("Generating snapshot...")
    renv::snapshot(type = "all", prompt = FALSE, lockfile = lockfile)
    file.copy(lockfile, saved_lockfile, overwrite = TRUE)

    log_progress("Moving existing packages...")
    file.rename(lib_dir, lib_tmp)

    log_progress("Restoring packages...")
    renv::restore(rebuild = TRUE, prompt = FALSE, lockfile = lockfile)
  }

  show("-- Complete Package Re-install --")

  # do not remove - this message is used to track metrics
  log_progress("Success")
  success <- TRUE
},
error = function(error_condition) {
  tryCatch({
    show('There was an error while performing package re-install:')
    log_progress(error_condition)
  },
  error = function(error_condition2) {
    show('There was an error during error handling:')
    log_progress(error_condition2)
  },
  finally = {
    # do not remove - this message is used to track metrics
    log_progress("Failed")
    stop(paste("Package update failed:", error_condition))
  })
},
finally = {
  tryCatch({
    log_progress("Cleaning up...")

    tryCatch({
      saved_pack_dirs <- list.dirs(lib_tmp, recursive = FALSE, full.names = FALSE)
      for (saved_r_dir in saved_pack_dirs) {
        if (!file.exists(file.path(lib_dir, saved_r_dir))) {
          log_progress(paste("Reinstating", file.path(lib_dir, saved_r_dir), "\n"))
          dir.create(file.path(lib_dir, saved_r_dir), recursive = TRUE)
        }
        saved_packs <- list.dirs(file.path(lib_tmp, saved_r_dir), recursive = FALSE, full.names = FALSE)
        for (p in saved_packs) {
          if (!file.exists(file.path(lib_dir, saved_r_dir, p))) {
            log_progress(paste("Reinstating", file.path(lib_dir, saved_r_dir, p)))
            file.rename(file.path(lib_tmp, saved_r_dir, p), file.path(lib_dir, saved_r_dir, p))
          }
        }
      }
    },
    error = function(err) {
       log_progress(error_cond, TRUE)
    })

    log_progress(paste0("There are ", nrow(installed.packages()), " installed packages(s)"), TRUE)
  },
  error = function(error_cond) {
    log_progress(error_cond, TRUE)
  },
  warning = function(warn_cond) {
    log_progress(warn_cond, TRUE)
  },
  finally = {
    unlink(interrupted)
    unlink(lib_tmp, recursive = TRUE)
    unlink(saved_lockfile)
  })
  closeAllConnections()
  if (!success) {
    message("It looks like there was a problem during the upgrade.")
    message("Any packages missed during the upgrade have been restored to their original version.")
    message("In most cases, your project will continue to work and no further action is required.")
    message("If you get an error trying to use a package, please manually re-install it.")
  }
  log_progress("Done")
})
