library(officer)
library(flextable)
library(dplyr)

stations = read.csv(file.path("fig_output","station_table.csv")) %>%
  dplyr::rename(`Temporal resolution`=Temporal.resolution)

annual_coefficients = read.csv(file.path("fig_output","annual coefficients.csv"))
monthly_coefficients = read.csv(file.path("fig_output","monthly coefficients.csv")) %>%
  dplyr::mutate(Model=dplyr::case_when(
    Model == "Individual zooplankton groups" ~ "Zooplankton",
    Model == "Lower trophic level aggregates" ~ "Lower trophic",
    Model == "Upper trophic level aggregates" ~ "Upper trophic"))

station_tbl <- flextable::flextable(stations) %>%
  font(fontname="Calibri", part="all") %>%
  fontsize(size=10, part="all") %>%
  width(j="Survey", width=1.15) %>%
  width(j="Temporal resolution", width=0.89) %>%
  width(j="Stations", width=3.94) %>%
  padding(padding=0, part="all") %>%
  set_caption(caption="Table S1. Stations used to calculate input data for annual and monthly models.")

annual_coefficients_tbl <- flextable::flextable(annual_coefficients) %>%
  font(fontname="Calibri", part="all") %>%
  fontsize(size=10, part="all") %>%
  width(j="Model", width=0.89) %>%
  width(j="Region", width=1.13) %>%
  width(j="Response", width=0.92) %>%
  width(j="op", width=0.43) %>%
  width(j="Predictor", width=0.9) %>%
  padding(padding=0, part="all") %>%
  set_caption(caption="Table S2. Path coefficients for annual and annual-regional SEMS.")

monthly_coefficients_tbl <- flextable::flextable(monthly_coefficients) %>%
  font(fontname="Calibri", part="all") %>%
  fontsize(size=10, part="all") %>%
  width(j="Model", width=0.95) %>%
  width(j="Region", width=1.09) %>%
  width(j="Response", width=0.97) %>%
  width(j="op", width=0.43) %>%
  width(j="Predictor", width=1.17) %>%
  padding(padding=0, part="all") %>%
  set_caption(caption="Table S3. Path coefficients for monthly-regional SEMS.")

supp_doc <- read_docx(path=file.path("fig_output","SI_template.docx")) %>%
  body_add_flextable(station_tbl) %>%
  body_add_par("") %>%
  body_add_par("") %>%
  body_add_flextable(annual_coefficients_tbl) %>%
  body_add_par("") %>%
  body_add_par("") %>%
  body_add_flextable(monthly_coefficients_tbl) %>%
  body_end_block_section(block_section(
    prop_section(page_margins=page_mar(bottom=0.5, top=0.5, right=0.5, left=0.5,
                                       header=0, footer=0, gutter=0)
  )))

print(supp_doc, target=file.path("fig_output","Supplemental_Information.docx"))
