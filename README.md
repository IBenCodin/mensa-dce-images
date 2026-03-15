# What makes meat-eaters reconsider?
## Discrete Choice Experiment (DCE) with meal images and nutritional labels

This repository contains the materials for an exploratory discrete choice experiment on cafeteria meal choices. The project examines under which circumstances meat-eaters choose vegetarian or vegan meals in a university cafeteria setting.

The study combines a blocked DCE design, image-based meal alternatives, survey implementation in Qualtrics, and econometric analysis in R. The focus lies on the role of price, NutriScore labels, a High Protein label, and protein type in shaping meal choice.

## Project overview

The project was developed as an applied discrete choice experiment with two types of task structures:

- **Within-dish tasks**, in which respondents choose between meat, vegetarian, and vegan versions of the same dish family
- **Cross-dish tasks**, in which respondents choose between different dish families

This distinction was used to balance internal validity and realism. The survey was implemented in Qualtrics and analyzed in R using multinomial logit models.

Because the realized sample size remained limited, the empirical results are exploratory. The main purpose of the repository is therefore to document the full workflow from design generation to preprocessing and estimation in a transparent and reproducible way.

## Repository contents

### `code/`
Contains the main R scripts used in the project.

- `01_generate_design_and_qualtrics_tasks.R`  
  Generates the experimental design, applies constraints, assigns tasks to blocks, and exports the files needed for estimation and Qualtrics implementation.

- `02_preprocess_qualtrics_to_estimation_long.R`  
  Reads the raw Qualtrics export, identifies the DCE block completed by each respondent, merges the responses with the design file, and creates the estimation-ready long-format dataset.

- `03_estimate_mnl_realdata.R`  
  Estimates multinomial logit models using the processed long-format dataset and exports coefficient and model fit tables.

- `04_descriptive_summary_nondce.R`  
  Extracts and summarizes the non-DCE survey responses, including attitudes, demographics, and cafeteria-related variables.

### `data/`
Contains raw and processed data files.

Typical files in this folder include:

- raw Qualtrics survey export (`.csv`)
- generated design files
- processed estimation dataset
- descriptive summary files
- exported coefficient and model fit tables

### `survey/`
Contains the Qualtrics survey materials.

Typical files may include:

- Qualtrics `.qsf` survey file
- printable or exported questionnaire version
- screenshots of survey flow or question implementation

### `images/`
Contains the image assets used in the experiment.

- `food/`  
  Meal images used in the DCE alternatives

- `labels/`  
  NutriScore icons, High Protein label, and blank placeholder images

### `report/`
Contains the written report and related files.

Typical files may include:

- report source (`.tex`, `.bib`, figures)
- compiled report PDF

## Experimental design summary

The final DCE design contains:

- **24 total tasks**
- **4 blocks**
- **6 tasks per respondent**
- **3 alternatives per task**

Each block contains:

- **3 within-dish tasks**
- **3 cross-dish tasks**

The design uses the following dish families:

- Gyros
- Pasta
- Curry

The experimentally varied information cues are:

- **Price**: €4.0, €4.5, €5.0
- **NutriScore**: A, B, C
- **High Protein label**: present or absent

The design was generated under several constraints, including:

- meat can never be the cheapest option in a task
- no task may contain three completely identical alternatives
- within-dish tasks always compare one meat, one vegetarian, and one vegan alternative
- NutriScore A appears only rarely
- overly dominant or “too easy” tasks are filtered out

## Survey structure

The survey consists of four parts:

1. Study explanation, eligibility, and consent  
2. Demographic questions, revealed choices, and cafeteria environment  
3. The discrete choice experiment  
4. Attitudes, socio-demographics, and survey feedback

Respondents who do not consume meat are screened out, as the study focuses specifically on the behavior of meat-eaters when confronted with vegetarian and vegan alternatives.

## Main outputs

The main files generated in the workflow include:

- `design_long_for_estimation.csv`
- `qualtrics_design_wide.csv`
- `qualtrics_loopmerge_minimal.csv`
- `qualtrics_tasks_clean.csv`
- `qualtrics_estimation_long.csv`
- `realdata_mnl_coefficients.csv`
- `realdata_mnl_coefficients_detailed.csv`
- `realdata_mnl_fit.csv`
- `descriptives_categorical.csv`
- `descriptives_numeric.csv`

## Notes on interpretation

The empirical analysis in this repository is based on a small realized sample. The estimated coefficients should therefore be interpreted as exploratory rather than population-representative. The repository is intended to document the survey design, preprocessing pipeline, and estimation strategy, and to provide a transparent basis for future extensions with larger samples.

## Transparency and reproducibility

All core materials used in the project are included in this repository:

- survey design files
- DCE image assets
- raw and processed data files
- R scripts for preprocessing and estimation
- report materials

This is intended to make the project transparent and reproducible and to allow the workflow to be inspected, replicated, or extended.

## Author

Benedikt Igl
