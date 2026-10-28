################################################################################
### Terrestrial Ecosystem Model in R (TEMIR)
### Library setup script for loading and installing required R packages
################################################################################

# This script checks the required R packages for TEMIR, optionally prepends a
# user-specified R library location, and installs missing packages before
# loading them for model execution.

# Optional dedicated R library for TEMIR:
# If "r_library_loc" is NA, packages are installed to / loaded from
# the default R library path. Otherwise, TEMIR uses the user-specified library.
if (!is.na(r_library_loc) &&
    is.character(r_library_loc) &&
    length(r_library_loc) == 1 &&
    nzchar(r_library_loc)) {
    if (!dir.exists(r_library_loc)) {
        dir.create(r_library_loc, recursive = TRUE)
    }
    r_library_loc = normalizePath(r_library_loc, winslash = '/', mustWork = FALSE)
    .libPaths(c(r_library_loc, .libPaths()))
    install_lib = r_library_loc
    print(paste0('Using R package library path: ', r_library_loc), quote = FALSE)
} else {
    install_lib = NULL
    print(paste0('Using default R package library path: ', .libPaths()[1]), quote = FALSE)
}

cran_repo = 'https://cloud.r-project.org'
# Batch jobs must use an environment prepared before submission. Installing
# packages here can race other jobs and modify the running R library.
if (!exists('r_install_packages', inherits = FALSE)) {
    r_install_packages = !isTRUE(cluster_flag)
}
if (!is.logical(r_install_packages) || length(r_install_packages) != 1L ||
    is.na(r_install_packages)) {
    stop('r_install_packages must be TRUE or FALSE')
}

# Packages bundled with / recommended by R.
# TEMIR loads them but does not try to install a standalone CRAN copy.
base_package_vec = c('parallel')

# External packages required by TEMIR.
# Tested with R version 4.5.1
package_df = data.frame(
    package = c('dotCall64', 'stringr', 'abind', 'ncdf4', 'filesstrings',
                'spam', 'maps', 'fields', 'lubridate', 'dplyr', 'readxl'),
    version = c('1.2', '1.5.1', '1.4.8', '1.24', '3.4.0', '2.11.4', '3.4.3', '17.3', '1.9.4', '1.1.4', '1.4.5'),
    stringsAsFactors = FALSE
)

# Load base / recommended packages first.
for (pkg in base_package_vec) {
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

# Install "remotes" only if exact version pinning is requested.
need_remotes = FALSE
if (r_install_packages) {
    for (i in 1:nrow(package_df)) {
        pkg = package_df$package[i]
        target_ver = package_df$version[i]
        pkg_installed = requireNamespace(pkg, quietly = TRUE)
        if (!is.na(target_ver)) {
            if (!pkg_installed || as.character(packageVersion(pkg)) != target_ver) {
                need_remotes = TRUE
                break
            }
        }
    }
}

if (need_remotes && !requireNamespace('remotes', quietly = TRUE)) {
    install.packages('remotes', repos = cran_repo, lib = install_lib)
}

# Check, install if needed, and then load each external package.
for (i in 1:nrow(package_df)) {
    pkg = package_df$package[i]
    target_ver = package_df$version[i]
    if (!r_install_packages) {
        tryCatch(
            suppressPackageStartupMessages(library(pkg, character.only = TRUE)),
            error = function(e) stop(
                'Cannot load required package ', pkg, ' from ',
                paste(.libPaths(), collapse = ', '), ': ', conditionMessage(e),
                '. Prepare the R environment before submitting the job.',
                call. = FALSE
            )
        )
        if (!is.na(target_ver) && as.character(packageVersion(pkg)) != target_ver) {
            message('Using ', pkg, ' ', as.character(packageVersion(pkg)),
                    ' (TEMIR was tested with ', target_ver, ')')
        }
        next
    }
    pkg_installed = requireNamespace(pkg, quietly = TRUE)

    if (!pkg_installed) {
        if (is.na(target_ver)) {
            install.packages(pkg, repos = cran_repo, lib = install_lib,
                             dependencies = c('Depends', 'Imports', 'LinkingTo'))
        } else {
            remotes::install_version(package = pkg, version = target_ver,
                                     repos = cran_repo, lib = install_lib,
                                     dependencies = c('Depends', 'Imports', 'LinkingTo'),
                                     upgrade = 'never')
        }
    } else if (!is.na(target_ver) && as.character(packageVersion(pkg)) != target_ver) {
        remotes::install_version(package = pkg, version = target_ver,
                                 repos = cran_repo, lib = install_lib,
                                 dependencies = c('Depends', 'Imports', 'LinkingTo'),
                                 upgrade = 'never')
    }

    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

rm(base_package_vec, package_df, cran_repo, need_remotes, i, install_lib, pkg,
   target_ver)
if (exists('pkg_installed', inherits = FALSE)) rm(pkg_installed)

################################################################################
### End of package setup
################################################################################
