## Create a nice map showing final survey stations that can be used in a manuscript.
## Adapted from Survey_extents.Rmd.
## Also output station tables.

library(sf)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(deltamapr)
library(USAboundaries)

########################################################################################

## Water:
water_estuary <- deltamapr::WW_Delta
water_ocean <- sf::st_read("data/data_in/ocean_shapefile/ocean_shapefile.shp")


## Regions:
regions_WGS84 <- sf::st_read("data/data_in/regions_shapefile/regions.shp")
regions_NAD83 <- sf::st_transform(regions_WGS84, crs=sf::st_crs(water_estuary))
##regions_NAD83_with_buffer <- sf::st_buffer(regions_NAD83, dist=-10)

## Stations from annual data:
annual_station_file <- file.path("data/stations/station_list_for_annual_analysis.csv")
station_annual_points_filtered_NAD83 <- read.csv(annual_station_file,
																								stringsAsFactors=FALSE) %>%
	sf::st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, agr="constant") %>%
	sf::st_transform(crs=sf::st_crs(water_estuary)) %>%
  dplyr::mutate(Survey_f=factor(Survey,
                                levels=c("EMP Benthic","EMP Zoop","EMP Nutrients",
                                         "DJFMP","STN","Bay Study","FMWT"),
                                ordered=TRUE))


## Stations from monthly data:
monthly_station_file <- file.path("data/stations/station_list_for_monthly_analysis.csv")
station_month_points_filtered_NAD83 <- read.csv(monthly_station_file,
																								stringsAsFactors=FALSE) %>%
	sf::st_as_sf(coords=c("Longitude", "Latitude"), crs=4326, agr="constant") %>%
	sf::st_transform(crs=st_crs(water_estuary)) %>%
  dplyr::mutate(Survey_f=factor(Survey,
                                levels=c("EMP Benthic","EMP Zoop","EMP Nutrients",
                                         "DJFMP","Bay Study"),
                                ordered=TRUE))


## California:
california <- USAboundaries::us_states(states="California")


## Base map:
xmin <- -122.6
xmax <- -121.45
ymin <- 37.749250
ymax <- 38.29925

blankTheme <- function() {
	theme_bw() +
	theme(panel.grid.major=element_blank(),
				panel.grid.minor=element_blank(),
				axis.title=element_blank(),
				axis.text=element_blank(),
				axis.ticks=element_blank(),
				panel.background=element_rect(fill="white"),
				plot.background = element_rect(fill="transparent", color=NA))
}

minTheme <- function() {
	theme_bw() +
		theme(panel.grid.major=element_blank(),
					panel.grid.minor=element_blank(),
					axis.title=element_blank()) +
		theme(plot.margin=unit(c(t=0, r=0, b=0, l=0.1), "cm"))
}

baseMap <- ggplot() +
	minTheme() +
	geom_sf(data=water_estuary, color="slategray1", fill="slategray1") +
  geom_sf(data=water_ocean, color="slategray1", fill="slategray1") +
	xlim(xmin, xmax) +
	ylim(ymin, ymax)


## Feature labels:
feature_lab_df <- as.data.frame(do.call("rbind",
	list(c(-122.59, 37.812614, "Pacific\nOcean"),
	     c(-122.448605, 37.775, "San\nFrancisco"),
			 c(-122.381544, 37.869070, "San\nFrancisco\nBay"),
			 c(-122.381544, 38.069, "San Pablo\nBay"),
			 #c(-122.053572, 37.955, "Suisun\nBay"),
			 c(-122.018701, 37.975, "Suisun\nBay"),
			 c(-121.62, 37.80, "Pumping\nfacilities")
			)))
names(feature_lab_df) <- c("lon","lat","Name")
feature_lab <- sf::st_as_sf(feature_lab_df, coords=c("lon","lat"), crs=sf::st_crs(4326)) %>%
	sf::st_transform(crs=sf::st_crs(regions_NAD83))

feature_arrow_df <- as.data.frame(do.call("rbind",
  list(c(-122.018701, 38, -122.018701, 38.05, "Suisun Bay")
  )))
names(feature_arrow_df) <- c("x1","y1","x2","y2","loc")
feature_arrow_df$x1 <- as.numeric(feature_arrow_df$x1)
feature_arrow_df$y1 <- as.numeric(feature_arrow_df$y1)
feature_arrow_df$x2 <- as.numeric(feature_arrow_df$x2)
feature_arrow_df$y2 <- as.numeric(feature_arrow_df$y2)


region_lab_df <- as.data.frame(do.call("rbind",
	list(c(-122.55, 38.14, "San\nPablo"),
			 c(-121.9834, 38.265, "Suisun"),
			 c(-121.73, 38.265, "Sacramento"),
			 #c(-121.65, 37.96, "San\nJoaquin")
			 c(-121.544, 37.87, "San Joaquin")
			)))
names(region_lab_df) <- c("lon","lat","Name")
region_lab <- sf::st_as_sf(region_lab_df, coords=c("lon","lat"), crs=sf::st_crs(4326)) %>%
	sf::st_transform(crs=sf::st_crs(regions_NAD83))


## California inset:
caInset <- ggplot() +
	blankTheme() +
	geom_sf(data=california, fill=NA, lwd=0.3, color="black") +
	geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), fill=NA, col="black",
						size=0.8) +
	theme(plot.margin=unit(c(0,0,0,0), "cm"))

arrowPlot <- ggplot() +
	blankTheme() +
	theme(panel.background=element_rect(fill=NA),
				panel.border=element_blank(),
				plot.background=element_rect(fill=NA, colour=NA)) +
	geom_segment(aes(x=x1, y=y1, xend=x2, yend=y2),
							 # data=data.frame(x1=0.095, x2=0.8, y1=0.79, y2=0.79), size=0.6,
							 data=data.frame(x1=0.22, x2=1, y1=0.5, y2=0.5), size=0.5,
							 arrow=arrow(angle=30, length=unit(0.13,"inches"), type="closed"),
							 arrow.fill="black") +
	xlim(0,1) +
	ylim(0,1)


## Annual regional map:
annualRegionalMap <- baseMap +
	minTheme() +
  geom_segment(aes(x=x1, y=y1, xend=x2, yend=y2),
               data=feature_arrow_df, size=0.5,
               arrow=arrow(angle=30, length=unit(0.07,"inches"), type="closed"),
               arrow.fill="gray50", col="gray50") +
	geom_sf(data=subset(regions_NAD83, Region == "West"), fill=NA, color="green4") +
	geom_sf(data=subset(regions_NAD83, Region == "North"), fill=NA, color="violetred1") +
	geom_sf(data=subset(regions_NAD83, Region == "South"), fill=NA, color="purple") +
	geom_sf_text(data=feature_lab, aes(label=Name), size=3, col="gray50", lineheight=0.9) +
	geom_sf_text(data=subset(region_lab, Name == "Suisun"), aes(label=Name),
								color="green4") +
	geom_sf_text(data=subset(region_lab, Name == "Sacramento"), aes(label=Name),
								color="violetred1") +
	geom_sf_text(data=subset(region_lab, Name == "San Joaquin"), aes(label=Name),
								color="purple") +
	ggspatial::annotation_scale(
    location="bl", text_cex=0.5, bar_cols=c("grey60", "white"), pad_x=unit(1.7, "in")) +
  ggspatial::annotation_north_arrow(
    location="bl", which_north="true",
		height=unit(0.7, "cm"), width=unit(0.7, "cm"), pad_x=unit(3.3, "in"),
		style=ggspatial::north_arrow_orienteering(text_size=9)) +
  geom_sf(data=station_annual_points_filtered_NAD83,
          aes(color=Survey_f, shape=Survey_f),
          size=2) +
  # geom_sf(data=regions_NAD83, aes(fill=Region), alpha=0.25) +
  scale_color_manual(name="Survey",
                     values=c("Bay Study"="darkorange","EMP Zoop"="#6A00A8FF",
                              "EMP Benthic"="#B12A90FF","EMP Nutrients"="#0D0887FF",
                              "DJFMP"="green3","FMWT"="red","STN"="black")) +
  scale_shape_manual(name="Survey",
                     values=c("Bay Study"=20,"EMP Zoop"=17,"EMP Benthic"=15,
                              "EMP Nutrients"=7,"DJFMP"=8,"FMWT"=3,"STN"=5)) +
	annotate(geom="text", x=-122.6, y=ymin + 0.01, label="a)")

annual_map <- cowplot::ggdraw(annualRegionalMap) +
  cowplot::draw_plot(caInset, x=0.00, y=0.65, width=0.33, height=0.33) +
  cowplot::draw_plot(arrowPlot, x=0.07, y=0.72, width=0.25, height=0.25)
  # cowplot::draw_plot(caInset, x=0.01, y=0.65, width=0.33, height=0.33) +
  # cowplot::draw_plot(arrowPlot, x=0.08, y=0.72, width=0.25, height=0.25)
  # cowplot::draw_plot(caInset, x=0.03, y=0.62, width=0.3, height=0.3) +
  # cowplot::draw_plot(arrowPlot, x=0.08, y=0.67, width=0.25, height=0.25)


## Monthly regional map:
monthlyRegionalMap <- baseMap +
	minTheme() +
  #geom_segment(aes(x=x1, y=y1, xend=x2, yend=y2),
  #             data=feature_arrow_df, size=0.5,
  #             arrow=arrow(angle=30, length=unit(0.07,"inches"), type="closed"),
  #             arrow.fill="gray50", col="gray50") +
	geom_sf(data=subset(regions_NAD83, Region == "Far West"), fill=NA, color="blue") +
	geom_sf(data=subset(regions_NAD83, Region == "West"), fill=NA, color="green4") +
	geom_sf(data=subset(regions_NAD83, Region == "North"), fill=NA, color="violetred1") +
	geom_sf(data=subset(regions_NAD83, Region == "South"), fill=NA, color="purple") +
	#geom_sf_text(data=feature_lab, aes(label=Name), size=3, col="gray50", lineheight=0.9) +
	geom_sf_text(data=subset(region_lab, Name == "San\nPablo"), aes(label=Name),
								color="blue") +
	geom_sf_text(data=subset(region_lab, Name == "Suisun"), aes(label=Name),
								color="green4") +
	geom_sf_text(data=subset(region_lab, Name == "Sacramento"), aes(label=Name),
								color="violetred1") +
	geom_sf_text(data=subset(region_lab, Name == "San Joaquin"), aes(label=Name),
								color="purple") +
  geom_sf(data=station_month_points_filtered_NAD83,
          aes(color=Survey_f, shape=Survey_f),
          size=2) +
  scale_color_manual(name="Survey",
                     values=c("Bay Study"="darkorange","EMP Zoop"="#6A00A8FF",
                              "EMP Benthic"="#B12A90FF",
                              "EMP Nutrients"="#0D0887FF","DJFMP"="green3")) +
  scale_shape_manual(name="Survey",
                     values=c("Bay Study"=20,"EMP Zoop"=17,"EMP Benthic"=15,
                              "EMP Nutrients"=7,"DJFMP"=8)) +
	annotate(geom="text", x=-122.6, y=ymin + 0.01, label="b)")

monthly_map <- cowplot::ggdraw(monthlyRegionalMap)
  # cowplot::draw_plot(caInset, x=0.03, y=0.62, width=0.3, height=0.3) +
  # cowplot::draw_plot(arrowPlot, x=0.08, y=0.67, width=0.25, height=0.25)


both_maps <- cowplot::plot_grid(annual_map, monthly_map, nrow=2, labels=c("(a)", "(b)"))
both_maps


ggsave(
  filename=file.path("fig_output","annual_and_monthly_maps.png"),
  plot=both_maps,
	device="png",
  width=7,
  height=7.5,
  units=c("in"),
  dpi = 300,
  bg = "white"
)



# ggsave(
  # filename=file.path("fig_output","annualRegionalMap.png"),
  # plot=annual_map,
	# device="png",
  # width=7,
  # height=5,
  # units=c("in"),
  # dpi = 300
# )


# ggsave(
  # filename=file.path("fig_output","monthlyRegionalMap.png"),
  # plot=monthly_map,
	# device="png",
  # width=7,
  # height=5,
  # units=c("in"),
  # dpi = 300
# )


## Station tables:
station_annual_points_filtered_NAD83 <- station_annual_points_filtered_NAD83 %>%
  dplyr::mutate(`Temporal resolution`="Annual")

station_month_points_filtered_NAD83 <- station_month_points_filtered_NAD83 %>%
  dplyr::mutate(`Temporal resolution`="Monthly")

station_table <- rbind(station_annual_points_filtered_NAD83,
                              station_month_points_filtered_NAD83) %>%
  sf::st_drop_geometry() %>%
  dplyr::group_by(Survey, `Temporal resolution`) %>%
  dplyr::summarize(Stations=paste(Station, collapse=", "), .groups="drop") %>%
  dplyr::arrange(`Temporal resolution`, Survey)

write.csv(station_table, file=file.path("fig_output","station_table.csv"),
          row.names=FALSE)

