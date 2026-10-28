# Planting-to-emergence review for the 2015 draft

Source: `G:/My Drive/Angus_FYP/data/publications/中国农业物候.pdf`.
The figures below subtract the published mean sowing DOY from the published
mean emergence DOY. They are station-group averages, not 2015 grid-cell
observations. Wheat, maize, and rice summarize 1981–2010; soybean summarizes
1992–2011.

| Crop group | Stations | Mean sowing DOY | Mean emergence DOY | Difference (days) | PDF table |
| --- | ---: | ---: | ---: | ---: | --- |
| Spring wheat, spring-wheat region | — | 98.7 | 117.1 | 18.4 | 2.6 |
| Spring wheat, mixed winter/spring region | — | 94.0 | 115.4 | 21.4 | 2.6 |
| Spring maize | 76 | 116 | 131 | 15 | 3.3 |
| Summer maize | 13 | 164 | 170 | 6 | 3.3 |
| Relay maize | 25 | 147 | 161 | 14 | 3.3 |
| Early rice | 16 | 72 | 78 | 6 | 4.2 |
| Late rice | 15 | 178 | 181 | 3 | 4.2 |
| Middle/single rice | 22 | 101 | 108 | 7 | 4.2 |
| Northern spring soybean | 33 | 130 | 146 | 16 | 5.3 |
| Summer soybean | 14 | 167 | 174 | 7 | 5.3 |
| Southern spring soybean | 4 | 88 | 96 | 8 | 5.3 |

Current 2015 backtracking uses fixed GDDemer values of 80 °C days for maize,
35 for spring wheat, and 50 for rice, with ERA5 surface-soil temperature.
Among accepted planting dates, the current median duration is 7 days for
maize and 4 days for spring wheat. There are 54 maize cells and 8 spring-wheat
cells above 30 days. Three spring-wheat cells exceed 50 days: 61, 112, and
120 days. Thus the mismatch includes many intervals that are too short as
well as a few that are implausibly long.

The rice records used by the 2015 map identify **transplanting**, not
biological emergence, as DOE. Table 4.2 reports mean transplanting DOY 105,
206, and 145 for early, late, and middle rice, respectively. The mean
sowing-to-transplanting intervals are therefore 33, 28, and 44 days. The
current rice map's DOE-minus-inferred-planting interval must not be compared
with the table's sowing-to-emergence interval of 6, 3, or 7 days.

Recommended next draft for maize and spring wheat:

1. Match each grid cell to a crop-season/region group; do not infer spring,
   summer, and relay maize from longitude alone. Use the published interval
   as a prior or plausibility range for the planting date.
2. If a planting date is changed, recompute the cell's GDDemer from ERA5
   surface-soil temperature between planting and observed emergence.
   Recompute GDDrepr and GDDmat from T2M between planting and the recorded
   reproductive and maturity dates. Keeping the original fixed GDDemer with
   a substantially changed planting date would move modeled emergence away
   from the observed DOE.
3. Treat rice separately: either model nursery sowing and emergence before
   transplanting, or define transplanting as the model's planting event and
   calibrate a post-transplant establishment threshold. TEMIR currently has
   no functional rice PFT, so this can wait for the rice module.

For future soybean inputs, Table 5.3 gives mean flowering/maturity DOY
194/268 (northern spring), 212/270 (summer), and 139/184 (southern spring).
The southern spring group has only four stations. These are regional priors;
2015 ERA5 temperatures and a defensible spatial assignment are still needed
to produce grid-cell GDD maps.
