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

supp_doc <- read_docx(path=file.path("fig_output","SI_template.docx"))%>%
  body_add_par(value = "Evaluating top-down, bottom-up, and environmental drivers of pelagic food web dynamics along an estuarine gradient", style = "heading 1") %>%
  body_add_par("Tanya L. Rogers, Samuel M. Bashevkin, Christina E. Burdi, Denise D. Colombano, Peter N. Dudley, Brian Mahardja, Lara Mitchell, Sarah Perry, Parsa Saffarinia") %>%
  body_add_par(value = "Supplemental data and model tables for Appendix S1", style = "heading 2") %>%
  body_add_par("") %>%
  body_add_par("") %>%
  body_add_flextable(variable_tbl) %>%
  body_add_par("") %>%
  body_add_break() %>%
  body_add_flextable(station_tbl) %>%
  body_end_block_section(block_section(
    prop_section(page_margins=page_mar(bottom=0.5, top=0.5, right=0.5, left=0.5,
                                       header=0, footer=0, gutter=0)
  )))

print(supp_doc, target=file.path("fig_output","Supplemental_Information.docx"))
