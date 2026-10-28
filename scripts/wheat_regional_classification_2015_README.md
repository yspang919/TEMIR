# Draft 2015 wheat calendar for spring-wheat simulation

The map and `wheat_2015_regional_calendar.csv` classify the 1,598 wheat cells
with complete 2015 stage dates. These are provisional simulation labels, not
observed cultivar identities. `GR&EM` can mean spring emergence or winter
green-up, so its date alone does not identify the cultivar.

## Rules

- `GR&EM`/DOE ≤ 75: winter wheat; DOE ≥ 105: spring wheat.
- For DOE 76–104, label Xinjiang, Xizang, Qinghai, western Sichuan
  (longitude ≤ 104.5°E and latitude ≥ 31°N), and western Gansu/Hexi
  (longitude ≤ 103.5°E) as spring wheat.
- Assign other DOE 76–104 cells using the majority of definite-date wheat
  cells within 1.01°, then 2.01°, then the weighted eight nearest cells.
- The CSV records the assigned label, province, reason, and nearby vote counts
  for each cell. The Natural Earth administrative boundary is copied into
  `natural_earth_50m_admin1` for repeat runs.

## Results

Of the 134 formerly uncertain cells, 123 are spring and 11 winter. The final
calendar has 751 spring and 847 winter cells. The separate GDD output in
`../../gdd_requirements_2015_wheat_regional_draft/` includes 726 usable spring
cells, 5 spring cells with zero thermal time in a phase, 64 cells with invalid
stage order, and 803 excluded winter cells. Compared with the original GDD
table, 94 more cells have usable spring-wheat GDD and no previously usable
cell is lost. Shared usable cells have identical GDD values. The original GDD
table and existing maps remain the baseline.

Run the scripts with Python from this folder's parent:

```powershell
python .\classify_wheat_calendar_regional_2015.py
python ..\prepare_crop_gdd_requirements_2015.py --build --crops spring_wheat --wheat-classification-csv .\wheat_regional_classification_draft\wheat_2015_regional_calendar.csv --output-dir ..\gdd_requirements_2015_wheat_regional_draft
```

Use `--overwrite` only to rebuild the existing draft GDD output. The
`--wheat-classification-csv` option leaves the builder's default filtering
unchanged when omitted.
