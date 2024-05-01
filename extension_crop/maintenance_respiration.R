################################################################################
### Module for calculating maintenance respirations
################################################################################

################################################################################
### Functions:
################################################################################

f_maintenance_respiration_fluxes_CLM45 = function(woody.flag, soil_depth_array, T_soil_array, T_2M,
                                            leaf_N, livestem_N, livecoarseroot_N, fineroot_N, grain_N,
                                            root_frac) {
    
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
    
    if (length(T_soil_array) != length(root_frac)) warnings('The number of soil layer in the soil temperature input (length of "T_soil_array" in simulate_ij.R) does not match the number of soil layer in the calculation of root fraction (length of "root_frac_Tsoil_pft" in simulate_ij.R).')
    
    # Sum of the fine root maintenance respiration at different soil layers weighted by root fraction.  
    mr_fineroot = 0
    for (layer in 1:length(root_frac)){
        mr_fineroot = mr_fineroot + (fineroot_N * mr_perN_20C * q10_mr ^ ((T_soil_array_degC[layer] - 20) / 10)) * root_frac[layer]
    }
        
    output = list(mr_leaf = mr_leaf, mr_livestem = mr_livestem, mr_grain = mr_grain, mr_coarseroot = mr_coarseroot, mr_fineroot = mr_fineroot,
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