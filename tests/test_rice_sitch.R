source('code_v2.0/geophys_const.R')
source('code_v2.0/Farquhar_Ball_Berry.R')

# Choose a concentration that yields the requested stomatal flux analytically.
# This checks the user's mean rice parameters and the existing generic branch.
damage = function(flux, sensitivity) {
    ppb = flux * (1.67 + 1 + 1) * 298.15 * R_uni / 101325
    f_ozone_impact(O3_conc = ppb, g_s = 1, g_b = 1, g_ah = 1,
        P_atm = 101325, theta_atm = 298.15, met_cond = TRUE,
        scheme = 'Sitch', sensitivity = sensitivity, plant_group = 'C3_grass')$O3_coef_An
}
stopifnot(abs(damage(7, 'custom-rice') - 1) < 1e-10,
          abs(damage(8, 'custom-rice') - (1 - .95 * .5)) < 1e-10,
          damage(10, 'custom-rice') == 0,
          abs(damage(6, 'low') - .75) < 1e-10)
cat('Rice Sitch mean coefficients and generic C3-grass behavior OK\n')
