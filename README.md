# grattan-truck-model
## A Grattan model of the Australian truck fleet

This project includes the analysis in the Grattan truck plan. 

A detailed breakdown of the assumptions included in the modelling can be found in the technical appendix, a pdf file included in this project. The code underpinning the model itself is also included, as described below. 



## Fleet model + vehicle activity


Input data to the model which relate to vehicle sales, attrition, use, fuel consumption/electricity use, urban vs rural driving, and EV costs are included in `R/model-inputs`. Each script in this folder produces a dataset (all cleaned datasets are saved under `data/`, raw data from external sources is under `data/raw/`). 

The fleet model, which simulates vehicle turnover and use, is included in `R/model`. There are four scripts in this folder: `08-fleet-turnover`, `09-policy-scenarios`, `10-checks-policy-scenarios` and `11-policy-outcomes`. They are numbered to show the order they must be run in to create the needed output. 

In series, these scripts produce a dataset, `policy-outcomes.rds` which is used as the basis for most of the results used in the grattan truck plan (described in more detail below). 

The first script, `fleet-turnover`, uses vehicle sales, VKTs, attrition rates, fuel consumption, electric uptake the energy intensity of the grid, and pollutant emissions factors to generate an output, `all-fleets` (saved under `data/`). This contains preliminary data of each year's vehicle fleet the model is run over, by vehicle type/sales year etc. The subsequent script, `policy-scenarios` takes this input data `all-fleets` and assesses the effect of different policy scenarios on the outcomes we're interested - i.e. co2 pollution and health pollution. It adds scenarios relating to zero-emission vehicle uptake targets,  euro standards and technology standards (tyre + engine regs). The output is a file called `policy-scenarios` (saved under `data/`) which is the basis for further analysis. 

The following script `checks-policy-scenarios` performs some simple QC checks to ensure that the data all makes sense and trends are as expected. The final script in this part of the modelling, `policy-outcomes` builds on the `policy-scenarios` script to generate outcomes under each scenario we have tested, including adding detail of secondary/non-exhaust pollutants and other important details. Because these details have not been added until this point, the output of these changes, the `policy-outcomes.rds` dataset saved under `data/` is the dataset that should be used for all analysis and is the basis of all the charts in the analysis. 

It should be noted that aside from health costs, none of these scripts/datasets include details of costs (as is needed for the CBA). All details relating to costs are included in scriptsunder the `R/costs-modelling` folder and are explained in more detail later. 


### The `policy-outcomes.rds` dataset

This dataset is the output of the model, and is what is used throughout the report to estimate CO2/pollution/health costs under the various policy scenarios. There are many columns in the data, but broadly it contains data, breaking the fleet down by:
* fleet year (`fleet_year`): the calendar year in question
* scenario (`scenario`) the policy scenario in question and (`vkt_scenario`) the assumed freight growth trajectory
* vkts: km driven per year (`vkt`) and where they are driven (`region`)
* Number of vehicles in each class/row: (`total`)
* vehicle type and characteristics: type of vehicle (`fuel_class`), year of vehicle manufacture (`sales_year`), and age (`age`)
* details for diesel vehicles: diesel consumption (L/100km) (`diesel_rate_100`). Note that the diesel proportion of vehicles in that row is (1-`electric_share`)
* details for electric vehicles: proportion of all vehicles that are electric in that row (`electric_share`), emissions intensity of grid (gCO2/Wh) (`ei_g_wh`), electricity consumption per km (kWh/km) (`ev_consumption`). Note the conversions that must be made between kWh and Wh. 
* tech regulation impacts: engine efficiency factor (`engine_efficiency`), tyre improvement factor (`tyre_improvement`). These factors have *already been applied* to the fuel_consumption estimates for each vehicle, and are already represented in the summary data as a result. They do not need to be used, and are purely included for reference purposes. 
* pollutant details: euro type/year (`pollutant_year`), pollutant and value (`pollutant` and `pollutant_rate`). `pollutant_rate` is specified in grams, either as per L of fuel, or per km travelled, depending on the pollutant listed (this is included as the suffix of `pollutant`, either as _l or _km). Figures per litre only apply to diesel vehicles (assume a value of 0 for the ev share), and per km values are assumed to apply equally to EV and diesel vehicles. These factors have already been accounted for in the calculation of summary data (below). 
* summary data: These summary columns are likely to be the most useful sources of data in the dataset, and are used to calculate most of the charts. Summary data is calculated for each vehicle class in terms of fuel use, CO2, and specific to each pollutant (as indicated by the `pollutant` variable) for pollutant/health costs. Total fuel consumption (`fuel_consumption`), CO2 in *grams* from diesel (`co2_ice`), CO2 from EVs (`co2_ev`), total co2 (ev and ice combined) (`co2`), total pollution (listed in *tonnes*) for specific pollutant listed (`pollutant_total`), higher level pollutant categories for grouping (`pollutant_cat`, `pollutant_cat2`), damage cost values per tonne of pollutant (`damage_cost_t`), and total health costs (`health_cost_total`). 


*Important notes about the `policy-outcomes` dataset* 

Because the dataset is too large for it to be in an ideal long format (with every row an individual vehicle), when summing characteristics care needs to be taken. In particular, it's important to note that the `pollutant` variable is in a long format, meaning that for each combination of all other variables, there are multiple rows (one for each type of pollutant). So each vehicle class (type/age/etc.) makes up multiple rows in the data - one row for each pollutant considered, where all the other variables are duplicated. This means that simple addition of all the rows in the dataset will not work for other variables, unless a single row for each class is selected (this can be done by selecting a single pollutant variable, for example, or by removing the columns with the pollutant information, and then using `unique()`. Otherwise, details such as the number of vehicles or total fuel consumption will be counted 10 x or similar. 

The split between metro and rural categories is included in the `vkt` variable category. For each vehicle class, a proportion of vkt's are assigned to the metro rows, and others to the rural rows. These rows must be summed if total/average vkt's are being considered. 

Although vehicle data is included up to 2060, sales are only included until 2040. Data beyond 2040 should only be used to calculate residual values. 


## Cost modelling

Further modelling which relates to costs, such as the CBA undertaken regarding zero-emission heavy vehicle sales targets are included under the `R/cost-modelling` folder. 

`CBA.R` is the script used to underpin the cost-benefit analysis undertaken on zero-emission sales targets for heavy vehicles. This analysis builds off the data inlcuded in the `policy-outcomes.rds` dataset, adding cost details for zero-emissions and diesel vehicles. 

A script titled `estimate_tco.R` is also included with the costs scripts. This script contains a function used to estimate the point of total cost of ownership parity between diesel and electric trucks. It can be used under a variety of conditions (such as with different fuel/electricity prices etc.) to sensitivity test estimates. This script is also used, in combination with other data from the `policy-outcomes.rds` dataset to estimate the value of vouchers that could be used to subsidise the upfront purchase of zero emissions heavy vehicles. These calculations are included in `R/costs-modelling/ev-voucher-value-calculations`. 

Some preliminary estimates of the payback period for different technology regulations (such as tyre and engine improvements) is also included in the `R/costs-modelling/payback-periods-tech-standards.R` script. 



### Charts 

Scripts for all charts included in the report and technical appendix are included in `R/charts`, and all charts are included under `atlas/`. A single script, `R/charts/all-report-charts-export.R` can be used to call all of the relevant scripts producing charts in the `R/charts/report-charts/` folder, generating a single pdf and single .ppt with all of the charts produced (saved under `atlas/`). 





