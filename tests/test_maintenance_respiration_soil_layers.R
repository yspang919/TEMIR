source('extension_crop/maintenance_respiration.R')

respiration <- function(root_fractions, soil_kelvin) {
    f_maintenance_respiration_fluxes_CLM45(
        root_frac_array = root_fractions,
        T_soil_array = soil_kelvin,
        T_2M = 293.15,
        leaf_N = 0, livestem_N = 0, livecoarseroot_N = 0,
        fineroot_N = 1, grain_N = 0
    )$mr_fineroot
}

# The deepest ERA5 layer must contribute when it contains the fine roots.
expected <- 2.525e-6 * 1.5 ^ ((30 - 20) / 10)
actual <- respiration(c(0, 0, 0, 1), c(283.15, 283.15, 283.15, 303.15))
stopifnot(isTRUE(all.equal(actual, expected)))
actual_merra <- respiration(c(0, 0, 0, 0, 1),
                            c(283.15, 283.15, 283.15, 283.15, 303.15))
stopifnot(isTRUE(all.equal(actual_merra, expected)))

# Preserve the existing one-layer behavior and reject mismatched layer counts.
stopifnot(isTRUE(all.equal(respiration(1, 293.15), 2.525e-6)))
stopifnot(inherits(try(respiration(c(0.5, 0.5), 293.15), silent = TRUE),
                   'try-error'))
cat('Maintenance respiration soil-layer weighting OK\n')
