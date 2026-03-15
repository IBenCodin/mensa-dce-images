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

### `images/`
Contains the image assets used in the experiment.

- `food/`  
  Meal images used in the DCE alternatives

- `labels/`  
  NutriScore icons, High Protein label, and blank placeholder images


