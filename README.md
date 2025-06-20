# The Evolution of Partisan Differences in Vaccinations: Evidence from Electronic Health Records, 1988-2024

This repository contains code and publicly available data for our paper "The Evolution of Partisan Differences in Vaccinations: Evidence from Electronic Health Records, 1988-2024." 

## Quick Summary
In this observational study of 46,619 patients, we find that Republican parents became increasingly more likely than Democratic parents to refuse vaccinations for their children and less likely to vaccinate their children against MMR from 1988-2024. The partisan gap in vaccination refusal was wider in states allowing non-medical vaccine exemptions.

## Abstract

Importance: Vaccination remains one of the most effective public health tools, yet recent years have seen increasing political polarization around vaccination in the United States, as well as measles outbreaks among unvaccinated children. No study has examined the national evolution of parental political polarization for childhood vaccination.

Objective: To examine the association between parental political party affiliation and vaccine behavior over time in the United States.

Design: Observational study on patients born from 1988 to 2024.

Setting: A sample of primary care provider data drawn from all 50 U.S. states, and voter registration data for all 50 U.S. states.

Participants: Children born from 1988-2024 (n = 46,619) recorded in a nationwide primary care registry who were matched via voter records to at least one parent registered as a Democrat or a Republican.

Main Outcomes and Measures: Whether or not a patient had a recorded vaccination refusal for any childhood vaccination and whether a patient received their first measles, mumps, and rubella (MMR) vaccination dose on time, with a delay, or not at all.

Results: Partisan polarization in childhood vaccine refusals spiked during the COVID-19 pandemic, but has been increasing since approximately 2000, with Republican parents more likely to refuse vaccinations than Democratic parents. Refusals are significantly more polarized in states allowing non-medical vaccine exemptions. Political polarization in MMR vaccinations has also increased over time, with children of Republican parents becoming approximately 0.4 percentage points less likely (P<.001) to receive a timely or delayed first MMR vaccine than children of Democratic parents each year.

Conclusions and Relevance: This study provides the first nationwide, individual-level evidence linking parental political affiliation to both vaccine refusal rates and MMR vaccination rates. The emergence and widening of partisan gaps over time suggest that political identity has become an increasingly important factor in health decision-making. 

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

We are grateful to memebrs of the RegLab for their feedback on this project.

## Contact

deho [AT] stanford [DOT] edu

## License

MIT
