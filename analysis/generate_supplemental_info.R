library(officer)
library(flextable)
library(dplyr)

variables = read.csv(file.path("fig_output", "full definitions table.csv"))%>%
  mutate(across(c(Months_missing, Years_missing), ~ifelse(is.na(.x), "", .x)))%>%
  mutate(Months_missing=sub("â€’","\U2012",Months_missing),
         Years_missing=sub("â€’","\U2012",Years_missing)) %>%
  rename(`Monthly years (missing months)`=Months_missing,
         `Annual years (missing years)`=Years_missing)

stations = read.csv(file.path("fig_output","station_table.csv")) %>%
  dplyr::rename(`Temporal resolution`=Temporal.resolution)

annual_coefficients = read.csv(file.path("fig_output","annual coefficients.csv"))
monthly_coefficients = read.csv(file.path("fig_output","monthly coefficients.csv")) %>%
  dplyr::mutate(Model=dplyr::case_when(
    Model == "Individual zooplankton groups" ~ "Zooplankton",
    Model == "Lower trophic level aggregates" ~ "Lower trophic",
    Model == "Upper trophic level aggregates" ~ "Upper trophic"),
    across(c(Response, Predictor),
           ~recode(.x, amphi_m="amphi", amphi_m_1="amphi_1", rotif_m="rotif", rotif_m_1="rotif_1"))) #Renaming variables for consistency

variable_tbl <- flextable::flextable(variables) %>%
  font(fontname="Calibri", part="all") %>%
  fontsize(size=10, part="all") %>%
  width(j="Variable", width=1.15) %>%
  width(j="Monthly years (missing months)", width=1.09) %>%
  width(j="Annual years (missing years)", width=1.09) %>%
  width(j="Definition", width=3.63) %>%
  valign(valign="top", part="body")%>%
  padding(padding=0, part="all") %>%
  border_inner_h(part="body", border=fp_border(color="gray70")) %>%
  set_caption(caption="Table S1. Variable definitions and temporal extent for the monthly and annual datasets.")

station_tbl <- flextable::flextable(stations) %>%
  font(fontname="Calibri", part="all") %>%
  fontsize(size=10, part="all") %>%
  width(j="Survey", width=1.15) %>%
  width(j="Temporal resolution", width=0.89) %>%
  width(j="Stations", width=3.94) %>%
  padding(padding=0, part="all") %>%
  border_inner_h(part="body", border=fp_border(color="gray70")) %>%
  set_caption(caption="Table S2. Stations used to calculate input data for annual and monthly models.")

annual_coefficients_tbl <- flextable::flextable(annual_coefficients) %>%
  font(fontname="Calibri", part="all") %>%
  fontsize(size=10, part="all") %>%
  width(j="Model", width=0.89) %>%
  width(j="Region", width=1.13) %>%
  width(j="Response", width=0.92) %>%
  width(j="op", width=0.43) %>%
  width(j="Predictor", width=0.9) %>%
  padding(padding=0, part="all") %>%
  set_caption(caption="Table S3. Path coefficients for annual and annual-regional SEMS.")

monthly_coefficients_tbl <- flextable::flextable(monthly_coefficients) %>%
  font(fontname="Calibri", part="all") %>%
  fontsize(size=10, part="all") %>%
  width(j="Model", width=0.95) %>%
  width(j="Region", width=1.09) %>%
  width(j="Response", width=0.97) %>%
  width(j="op", width=0.43) %>%
  width(j="Predictor", width=1.17) %>%
  padding(padding=0, part="all") %>%
  set_caption(caption="Table S4. Path coefficients for monthly-regional SEMS.")

supp_doc <- read_docx(path=file.path("fig_output","SI_template.docx"))%>%
  body_add_par(value = "Evaluating top-down, bottom-up, and environmental drivers of pelagic food web dynamics along an estuarine gradient", style = "heading 1") %>%
  body_add_par("Tanya L. Rogers, Samuel M. Bashevkin, Christina E. Burdi, Denise D. Colombano, Peter N. Dudley, Brian Mahardja, Lara Mitchell, Sarah Perry, Parsa Saffarinia") %>%
  body_add_par("") %>%
  body_add_par("") %>%
  body_add_flextable(variable_tbl) %>%
  body_add_par("") %>%
  body_add_break() %>%
  body_add_flextable(station_tbl) %>%
  body_add_par("") %>%
  body_add_break() %>%
  body_add_flextable(annual_coefficients_tbl) %>%
  body_add_par("") %>%
  body_add_break() %>%
  body_add_flextable(monthly_coefficients_tbl) %>%
  body_end_block_section(block_section(
    prop_section(page_margins=page_mar(bottom=0.5, top=0.5, right=0.5, left=0.5,
                                       header=0, footer=0, gutter=0)
  )))

print(supp_doc, target=file.path("fig_output","Supplemental_Information.docx"))

