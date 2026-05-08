# Partisan differences in childhood measles vaccination and general refusals: a retrospective cohort study of electronic health records in the United States, 1988-2024

This repository contains code and publicly available data for our paper "Partisan differences in childhood measles vaccination and general refusals: a retrospective cohort study of electronic health records in the United States, 1988-2024."

## Structure

The repository is organized into “scripts” related to data preparation, “analysis” to create figures and tables from prepared data, and “data” for publicly available datasets prepared in the project. 

### Scripts and Analysis 

All scripts and analysis are organized numerically. Numbers start from “00” and are in all code file names. All data preparation is done in Python, while figures and tables are created in R.

This code cannot be run without the AFC (available from Stanford University Center for Population Health Sciences) and L2 (available from L2) datasets. We make the code available for transparency, but fully reproducing the results of the study will require the AFC and L2 data.

### Data

refusal_source_values.csv: The ICD-9 and ICD-10 codes used to locate vaccine refusal events.

MMR_codes_IDs.csv: The codes and IDs, across several schemes, used to locate MMR vaccination events

state_vaccine_exemptions.csv: The dates that all 50 states and Washington, D.C. had medical, religious, and/or personal MMR vaccine exemptions. These dates are determined from state policy accessed through Westlaw.

## Acknowledgments

We are grateful to members of the RegLab for their feedback on this project.

## Contact

deho [AT] stanford [DOT] edu

## License

MIT
