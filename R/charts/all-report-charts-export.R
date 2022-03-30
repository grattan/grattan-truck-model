

#' Collating all chapter 3 (and some chapter 2) charts for the report 
#' All these charts are from code within the report-charts folder, and all are saved
#' into a single pdf file under atlas/report-charts/report-charts-joined in the dropbox

source("R/00-setup.R")
#' Reading all relevant scripts (this might take a bit of time to run) ---------
path <- c("R/charts/report-charts/")    
source_files <- list.files(path, "*.R$")  
map(paste0(path, source_files), source) 


# Names of plots (to check that all have generated properly)
#report_charts <- list(c2_euro_vi_pollution, c2_euro_vi_cost, c2_old_trucks_share, c2_old_trucks_vkt_pmc25,
#                      c2_health_costs_per_truck, c3_emissions_forecast, c3_offset_costs, c3_emission_policy_scenarios,
#                      c3_tco_estimate, c3_carbon_ev_diesel, c3_cba_chart)# c3_illustrative_uptake_curves)
                      #Not worrying about the illustrate chart because that's mostly in ppt and doesn't use real data anyhow


# Saving the pdf files in one location--------------------------------
# Function to save the plots in one spot 
save_plots <- function(types, data, names) {
  i <- 1
  while (i <= length(names)) {
    grattan_save(filename = paste0("atlas/report-charts/", names[i], ".pdf"),
                 object = data[[i]],
                 type = types[i],
                 save_ppt = FALSE,
                 force_labs = TRUE,
                ## device = cairo_pdf,
                 ignore_long_title = TRUE)
    i <- i + 1
  } 
}


# Preparing the data for the function
report_charts <- list(c2_euro_vi_pollution, c2_euro_vi_cost, c2_old_trucks_share, c2_old_trucks_vkt_pmc25,
                      c2_health_costs_per_truck, c3_emissions_forecast, c3_offset_costs, c3_emission_policy_scenarios,
                      c3_technology_costs, c3_tco_estimate, c3_carbon_ev_diesel, c3_cba_chart)
report_chart_names <- c("c2_euro_vi_pollution", "c2_euro_vi_cost", "c2_old_trucks_share", "c2_old_trucks_vkt_pmc25",
                        "c2_health_costs_per_truck", "c3_emissions_forecast", "c3_offset_costs", "c3_emission_policy_scenarios",
                        "c3_technology_costs", " c3_tco_estimate", "c3_carbon_ev_diesel", "c3_cba_chart")
report_chart_types_pdf <- c("wholecolumn", "wholecolumn", "wholecolumn", "wholecolumn",
                            "wholecolumn", "normal", "wholecolumn", "normal", 
                            "normal", "normal", "normal", "normal")

# Saving plots using our function
save_plots(data = report_charts, 
           names = report_chart_names,
           types = report_chart_types_pdf)


# Saving to a single pdf file ---------------------------------------

library(pdftools)

paths <- paste0("atlas/report-charts/", report_chart_names, "/", report_chart_names, "_", report_chart_types_pdf, ".pdf")

# Using the paths to merge all the pdfs 
pdf_combine(as_vector(paths), output = "atlas/report-charts/report-charts-joined.pdf")




# Saving all charts to a .pptx file ----------------------------------


grattan_save_pptx(report_charts, "atlas/all-report-charts.pptx")  
















