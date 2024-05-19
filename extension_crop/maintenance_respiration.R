################################################################################
### Module for calculating maintenance respirations
################################################################################

# TODO

# Currently, the length of root_frac_array is longer than T_soil_array by 1 if multilayer soil temperature input is used. Values in root_frac_array is the PFT-specific root fraction between two soil layers (total 6 layers). Values in T_soil_array is the soil temperature at the boundary of two soil layers (total 5 boundaries in MERRA2). Maintenance respirations might be underestimated during the day time and overestimated during night time... (soil temperature is measured at the bottom of a soil layer).

# Future solution 1: estimated the soil temperature at the depth where cummulative root fracation = 50 % for each PFT, then use this temperature to calculate MR. (essentially become a bulk soil layer, easier to implement but may lose some accuracy). The depth where cummulative root fraction = 50% is between 10 - 30cm (between TSOIL1 and TSOIL2, see input_TEMIR_crop_extension.R).

# Future solution 2: get the surface skin temperature from another MERRA2 input file, then estimate the soil temperature at the middle of each layer, follow by the MR calculation for each soil layer.  (more accurate? but at the same time we need to download another set of MERRA2 file just for the skin surface temperature, skin surface temperature is not included in the same file as T_soil)

################################################################################
### Functions:
################################################################################

f_maintenance_respiration_fluxes_CLM45 = function(root_frac_array, T_soil_array, T_2M, leaf_N, livestem_N, livecoarseroot_N, fineroot_N, grain_N) {
    
    # This function calculates the hourly maintenance respiration rate of PFTs
    # Plant nitrogen is not simulated in this version of TEMIR. It is inferred from plant carbon pools and prescribed PFT-specific plant C:N ratio.

    # Maintenance respiration rate per unit N at 20 Deg.C (gC gN^-1 s^-1)
    mr_perN_20C = 2.525*10^-6

    # Sensitivity coefficient Q10
    q10_mr = 1.5
    
    # Temperature in Degree Celsius is needed for the calculation
    T_2m_degC = T_2M - 273.15
    T_soil_array_degC = T_soil_array - 273.15
        
    mr_leaf = leaf_N * mr_perN_20C * q10_mr ^ ((T_2m_degC - 20) / 10)
    mr_livestem = livestem_N * mr_perN_20C * q10_mr ^ ((T_2m_degC - 20) / 10)
    mr_grain = grain_N * mr_perN_20C * q10_mr ^ ((T_2m_degC - 20) / 10)
    mr_coarseroot = livecoarseroot_N * mr_perN_20C * q10_mr ^ ((T_2m_degC - 20) / 10)
    
    # This check does not work at the time being.
    # if (length(T_soil_array) != length(root_frac_array)) warnings('The number of soil layer in the soil temperature input (length of "T_soil_array" in simulate_ij.R) does not match the number of soil layer in the calculation of root fraction (length of "root_frac_Tsoil_pft" in simulate_ij.R).')
    
    # Sum of the fine root maintenance respiration at different soil layers weighted by root fraction.
    # TODO:
    # Temporary separating the calculation for single layer and mutliple layer T_soil input 
    # -1 in "for (layer in ...)" is the temporary solution for the addition layer in root_frac_array 
    if (length(root_frac_array) == 1 && length(T_soil_array) == 1) {
        mr_fineroot_total = fineroot_N * mr_perN_20C * q10_mr^ ((T_soil_array_degC - 20) / 10)
    } else {
        mr_fineroot_total = 0
        for (layer in 1:(length(root_frac_array)-1)) {
            mr_fineroot_layer = (fineroot_N * mr_perN_20C * q10_mr ^ ((T_soil_array_degC[layer] - 20) / 10)) * root_frac_array[layer]
            mr_fineroot_total = mr_fineroot_total + mr_fineroot_layer
        }
    }
    
    output = list(mr_leaf = mr_leaf, mr_livestem = mr_livestem, mr_grain = mr_grain, mr_coarseroot = mr_coarseroot, mr_fineroot = mr_fineroot_total,
                  mr_total = (mr_leaf + mr_livestem + mr_grain + mr_coarseroot + mr_fineroot))
    
    return(output)
}

f_maintenance_respiration_fluxes_custom = function() {
    # implement your function here
}



##### Obsolete 
# f_get_root_fraction_CLM45 = function(soil_depth_array, a_rootfrac, b_rootfrac){
#     
#     # This function calculates the fraction of root mass to the total root mass for each soil layer using the method in CLM4.5.
#     # Method in CLM4.5: cummulative root fraction Y(d) = 1 - ((exp(-a*d)) + exp(-b*d)) / 2; where d is the depth of the top of a soil layer. 
#     # See Zeng (2001): Global Vegetation Root Distribution for Land Modeling
#     
#     # Required input
#     # soil_depth_array: an array that contains the depth of the top of soil layers
#     # a_rootfrac, b_rootfrac: PFTs-specific parameters used for the roof fraction calculation in CLM4.5
# 
#     cummulative_root_fraction_array = array(data = NA, dim = length(soil_depth_array))
#     root_fraction_array = array(data = NA, dim = length(soil_depth_array))
#        
#     if (length(soil_depth_array == 1)) {
#         # only one layer of soil/root, it includes all the root mass
#         cummulative_root_fraction_array[1] = 1; root_fraction_array[1] = 1
#     } else {
#         # more than one layer of soil/root
#         for (layer in 1:length(soil_depth_array)) {
#             # formula Y(d) = 1 - ((exp(-a*d)) + exp(-b*d)) / 2
#             cummulative_root_fraction_array[layer] = 1 - (exp(-a_rootfrac * soil_depth_array[layer]) + exp(-b_rootfrac * soil_depth_array[layer])) / 2
#             if (layer >= 2) {
#                 root_fraction_array[layer] = cummulative_root_fraction_array[layer] - cummulative_root_fraction_array[layer-1]
#             } else {
#                 # first layer of soil
#                 root_fraction_array[layer] = cummulative_root_fraction_array[layer]
#             }
#          }
#     }
#     output = list(root_fraction_array = root_fraction_array, cummulative_root_fraction_array = cummulative_root_fraction_array)
# }
# 
# f_get_root_fraction_custom = function(argument = NULL) {
#     # implement your function
# }
