Defining success in the first cohort of CIHR Health System Impact Fellows: An eDelphi Study
=========

The repository contains data analysis for the second (quantitative) stage of the eDelphi technqique

## Objectives
The study will describe 
- 1) the __contributions__ HSI fellows made to their respective host organizations and academic institutions, and 
- 2) the optimal __criteria__ for evaluating the success of the HSI Fellowship

from the perspectives of 
- A) the inaugural cohort of CIHR Health System Impact __Fellows__, 
- B) their __health system__ supervisors, and 
- C) their __academic__ supervisors on the key 


## Results

- data preparation script `./manipulation/0-greeter.R` inputs the survey data and prepares it for analysis
- analysis script `./analysis/report-1/report-1.R` generates the graphs with descriptive statistics and the table of results
- view more graphs in [`./analysis/report-1/prints/README.md`][prints]
- download the table of results in long form from [`./analysis/report-1/products/ds_descriptives.csv`][data-download]

![contribution-all][contribution-all]

![criterion-all][criterion-all]

[prints]:analysis/report-1/prints/README.md

[data-download]:analysis/report-1/products/ds_descriptives.csv

## Methods

An electronic Delphi (hereafter, eDelphi) (1, 2) study was conducted to elicit the perspectives of fellows, host organization supervisors, and academic supervisors regarding the key contributions fellows have made in their environment and the criteria that each group uses to define a successful fellowship. The eDelphi method was chosen due to the distribution of fellows and their host organizations and academic institutions across Canada.
The entire 2017 cohort of the Health System Impact Fellows (n=46), their health system organization supervisors (n=?)and academic supervisors (n=?) were invited to complete the survey (http://www.cihr-irsc.gc.ca/e/50660.html). As the first cohort of this innovative new program, participants were considered the experts of their experience. Data were collected through online evaluation surveys administered by CIHR, consisting of 2 rounds of questions sent 4 weeks apart. Up to 2 weekly reminders were sent to non-responders. 

In Round 1, two open-ended questions were administered to each group (see APPENDIX A). For each of the 3 group of respondents, 3 researchers independently analyzed qualitative data generated in Round 1 using conventional content analysis to identify themes (3). Afterwards, the 3 researchers reached an agreement on the list of items that were included in the subsequent round. Thus, the list of items between the 3 groups were kept separate. 

In Round 2, participants ranked their agreement with the items generated in Round 1 on a 5-point Likert-type scale (0 to 4) (see APPENDIX B). Quantitative analyses was conducted using Kendall’s W to assess the level of agreement with ranking and a principal components analysis was conducted to determine if there was consensus among each group of stakeholders (4). All quantitative analyses were performed using SPSS for Mac (version 24.0, IBM corporation, Armonk, NY, USA). Ethical clearance for the study was approved by the University of Waterloo Ethics Review Board (#?).


[contribution-all]:analysis/report-1/prints/Contribution.jpg
[contribution-common]:analysis/report-1/prints/Contribution-Common.jpg
[contribution-unique]:analysis/report-1/prints/Contribution-Unique.jpg


[criterion-all]:analysis/report-1/prints/Criterion.jpg
[criterion-common]:analysis/report-1/prints/Criterion-Common.jpg
[criterion-unique]:analysis/report-1/prints/Criterion-Unique.jpg
