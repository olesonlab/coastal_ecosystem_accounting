

## Project Structure

``` markdown
. 📂 coastal_ecosystem_accounting
├── 📄 LICENSE
└── 📂 R/
│  └── 📂 helper_functions/
│    └── 📂 01_importing/
│      ├── 📄 extract_single_xlsx_sheet.R
│      ├── 📄 import_xlsx_sheets.R
│    └── 📂 02_processing/
│      ├── 📄 pivot_prefix_cols_longer.R
│      ├── 📄 tidy_comm_ev.R
│      ├── 📄 tidy_noncomm_ev.R
│    └── 📂 03_exporting/
│      ├── 📄 export_to_csv.R
│    └── 📂 utils/
│      ├── 📄 create_file_path.R
│      ├── 📄 get_todays_date.R
│  └── 📂 targets/
│    ├── 📄 process_fisheries_valuation_data.R
├── 📄 README.md
├── 📄 README.qmd
└── 📂 _targets/
├── 📄 _targets.R
├── 📄 coastal_ecosystem_accounting.Rproj
└── 📂 data/
└── 📂 documentation/
└── 📂 renv/
└── 📄 renv.lock
```
