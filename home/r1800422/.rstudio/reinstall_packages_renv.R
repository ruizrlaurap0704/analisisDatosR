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
show <- function(msg) {
    write(msg, stdout())
}

home_dir <- base::path.expand("~")
lib_dir <- "/cloud/project/renv/library"
saved_lockfile <- paste0(home_dir, "/.rstudio/renv.lock")
interrupted <- paste0(home_dir, "/.rstudio/.reinstall_interrupted")
success <- FALSE
options(renv.verbose = TRUE)

tryCatch({
  sink("/mnt/share/cloud/package_reinstall.out", split = TRUE, type = "output")
  sink(stdout(), type = "message") # stderr goes to the same output as stdout

  show("We have detected that your installed R packages need to be re-installed to work with the new OS version.")
  show("Please wait while we re-install your R packages...")

  show("-- Start Package Re-install with renv --")

  # do not remove - this message is used to track metrics
  log_progress("Starting with renv")

  # generate a new snapshot file unless there is one from a previous interrupted re-install
  lockfile <- "/cloud/project/renv.lock"
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
    rlib_dirs <- list.dirs(lib_dir, recursive = FALSE, full.names = FALSE)
    for (rlib_dir in rlib_dirs) {
      dir.create(file.path(lib_dir, rlib_dir, 'tmp'), showWarnings = FALSE)
      pack_dirs <- list.dirs(paste(lib_dir, rlib_dir, 'x86_64-pc-linux-gnu', sep = '/'), recursive = FALSE, full.names = FALSE)
      for (pack_dir in pack_dirs) {
        full_dir <- file.path(lib_dir, rlib_dir, 'x86_64-pc-linux-gnu', pack_dir)
        if (pack_dir != 'renv') {
          tmp_dir <- file.path(lib_dir, rlib_dir, 'tmp', pack_dir)
          file.rename(full_dir, tmp_dir)
        }
      }
    }

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
      rlib_dirs <- list.dirs(lib_dir, recursive = FALSE, full.names = FALSE)
      for (rlib_dir in rlib_dirs) {
        if (file.exists(file.path(lib_dir, rlib_dir, 'tmp'))) {
          saved_pack_dirs <- list.dirs(file.path(lib_dir, rlib_dir, 'tmp'), recursive = FALSE, full.names = FALSE)
          for (saved_pack_dir in saved_pack_dirs) {
            target_path <- file.path(lib_dir, rlib_dir, 'x86_64-pc-linux-gnu', saved_pack_dir)
            if (!file.exists(target_path)) {
              log_progress(paste("Reinstating", target_path))
              file.rename(file.path(lib_dir, rlib_dir, 'tmp', saved_pack_dir), target_path)
            }
          }
        }
      }
    },
    error = function(err) {
       log_progress(error_cond, TRUE)
    })

    log_progress(paste0("There are ", nrow(installed.packages()), " installed packages(s)"), TRUE)
    rlib_dirs <- list.dirs(lib_dir, recursive = FALSE, full.names = FALSE)
    for (rlib_dir in rlib_dirs) {
      unlink(file.path('/cloud/project/renv/library/', rlib_dir, 'tmp'), recursive = TRUE)
    }
    rm(rlib_dirs)
    rm(rlib_dir)
    rm(pack_dirs)
    rm(pack_dir)
    rm(full_dir)
    rm(tmp_dir)
  },
  error = function(error_cond) {
    log_progress(error_cond, TRUE)
  },
  warning = function(warn_cond) {
    log_progress(warn_cond, TRUE)
  },
  finally = {
    unlink(interrupted)
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
