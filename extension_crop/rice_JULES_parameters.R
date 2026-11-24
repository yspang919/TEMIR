# Rice JULES parameter profile (temperatures in degC; thermal time in degC day).
# Primary: Xu et al. (2025), https://doi.org/10.5194/gmd-18-7257-2025
# Fallback when absent: RICE values in Osborne et al. (2015),
# https://doi.org/10.5194/gmd-8-1139-2015 (not wheat values).
# Values below are reported by Xu; no additional numerical fallback is needed.
# This is a site-calibrated profile, not a universal rice cultivar calibration.
f_rice_JULES_parameters = function() {
    list(
        # Xu Sect. 3.1.2 explicitly retains Osborne's cardinal temperatures.
        # GDDmat/repr/emer use the imported CLM traits under the existing
        # 'CLM4.5' threshold option, or prescribed site/map thresholds.
        # This JULES profile controls only the daily temperature response.
        development = list(T_b = 8, T_o = 30, T_m = 42),
        # Xu Table 3. TEMIR uses exp(a + b*DVI), with positive intercepts
        # and negative slopes, matching Osborne (2015), Eq. (6), and TEMIR.
        # Xu's displayed minus-alpha differs from that original formulation;
        # map its Table 3 values using Osborne's positive-alpha convention.
        allocation = list(a_root = 17.4, b_root = -20,
            a_stem = 17.4, b_stem = -16.7, a_leaf = 17.9, b_leaf = -18.5,
            stem_reserve_fraction = 0.12),
        # Xu Table 3 and Sect. 3.1.3: daily loss fraction mu*(DVI-DVI_sen)^nu.
        # This depends on DVI, not on days since senescence as in TEMIR maize.
        senescence = list(DVI_onset = 1.25, mu = 2, nu = 6),
        # Xu Sect. 3.1.3: senesced leaf carbon enters the harvest pool;
        # stem reserves mobilise at 10%/day when stem partition falls below .01.
        # TEMIR calls the harvest pool grainC; economic yield is distinct.
        retranslocation = list(leaf_DVI_onset = 1.25, leaf_transfer_fraction = 1,
            stem_allocation_threshold = 0.01, stem_fraction_per_day = 0.1),
        # Xu Table 3, for TEMIR's optional custom SLA and height schemes.
        structure = list(SLA_gamma = 24.5, SLA_delta = -0.145,
            leaf_carbon_fraction = 0.399, height_kappa = 1.27,
            height_lambda = 0.24, stem_carbon_fraction = 0.404)
    )
}
