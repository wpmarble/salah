1- Run cleaning.R
	- Imports csv from qualtrics (under legacy qualtrics downloads)
	- Cleans the data, creates treatments, and creates weights, creates principal components, and control variables/variables for heterogeneous treatments
	- Outputs survey_clean.rds

2- Run main_figure.R
	- Inputs survey_clean.rds
	- Outputs the main figure in the survey experiment section of the paper

2- Run analysis_ate.R
	- Inputs survey_clean.rds
	- Runs the treatment check and the ATE regressions reported in the appendix
	- Outputs regression tables and figures for ATE that are shown in the appendix

3- Run analysis_amce_success.R
	- Inputs survey_clean.rds
	- Conducts regressions and outputs tables and figures for AMCE for the success/failure treatments that are shown in the appendix

4- Run analysis_amce_rlgn.R
	- Inputs survey_clean.rds
	- Conducts regressions and outputs tables and figures for AMCE for the religion/character treatments that are shown in the appendix

5- Run averages.R
	- Inputs survey_clean.rds
	- Produces bar charts for the outcomes by treatment groups 

6- Run games.R
	- Tests how survey outcomes changed around Liverpool victories and Salah goals

7- balance_table.R 
	- Produces the balance table in the appendix