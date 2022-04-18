
######################################################################################
## Coordinates:

getAnnualCoordinates <- function() {
  x1 <- 1
  x2 <- 3.5

  coordinate_map <- list(
    c("estfish_bsmt",x1 - 1.5,5),
    c("estfish",x1,5),
    c("estfish_stn",x1 + 1.5,5),

    c("fish",x1,4),
    c("pzoop",x1,3),
    c("hzoop",x1,2),
    c("chla",x1,1),

    c("flow",x2,4),
    c("temp",x2,3),
    c("turbid",x2,2),
    c("potam",x2,1),
    c("corbic",x2,1)
  )

  df <- as.data.frame(do.call("rbind", coordinate_map))
  names(df) <- c("Shortname","x","y")
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)

  ## R2 coordinates outside of the node:
  df$x_R2 <- df$x + 0.4
  df$y_R2 <- df$y - 0.45
  df$y_R2[df$y == max(df$y)] <- df$y[df$y == max(df$y)] + 0.43
  df$x_R2[df$y == max(df$y)] <- df$x[df$y == max(df$y)]

  return(df)
}

getUpperTrophicCoordinates <- function(df) {
  x1 <- 1
  x2 <- 3.5
  x3 <- 6

  coordinate_map <- list(
    c("estfish_bsmt_1",x1,5),
    c("potam_1",x1,4),
    c("pzoop_1",x1,3),
    c("hzoop_1",x1,2),
    c("chla_1",x1,1),

    c("estfish_bsmt",x2,5),
    c("pzoop",x2,3),
    c("hzoop",x2,2),

    c("estfish_bsmt_gr",x2,5),
    c("pzoop_gr",x2,3),
    c("hzoop_gr",x2,2),

    c("marfish_bsmt_1",x3,4),
    c("flow",x3,3),
    c("temp",x3,2),
    c("turbid",x3,1),

    c("corbic_1",x1,4),

    c("sside_1",x3,6),
    c("cent_1",x3,4),
    c("sbass1_bsmt_1",x3,5)
  )

  df <- as.data.frame(do.call("rbind", coordinate_map))
  names(df) <- c("Shortname","x","y")
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)

  ## R2 coordinates outside of the node:
  df$x_R2 <- df$x - 0.4
  df$y_R2 <- df$y - 0.4

  return(df)
}

getLowerTrophicCoordinates <- function(df) {
  x1 <- 1
  x2 <- 3.5
  x3 <- 6

  coordinate_map <- list(
    c("potam_1",x1,5),
    c("corbic_1",x1,5),
    c("pzoop_1",x1,4),
    c("hzoop_1",x1,3),
    c("chla_1",x1,2),
    c("din_1",x1,1),

    c("potam",x2,5),
    c("corbic",x2,5),
    c("chla",x2,2),
    c("din",x2,1),

    c("potam_gr",x2,5),
    c("corbic_gr",x2,5),
    c("chla_gr",x2,2),
    c("din_gr",x2,1),

    c("flow",x3,4),
    c("temp",x3,3),
    c("turbid",x3,2)
  )

  df <- as.data.frame(do.call("rbind", coordinate_map))
  names(df) <- c("Shortname","x","y")
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)

  ## R2 coordinates outside of the node:
  df$x_R2 <- df$x - 0.4
  df$y_R2 <- df$y - 0.4

  return(df)
}


getZoopCoordinates <- function(region) {
  x1 <- 1
  x2 <- 3.5
  x3 <- 6

  if(region == "Far West") {
    coordinate_map <- list(
      c("estfish_bsmt_1",x1,6),
      c("pcope_1",x1,5),
      c("hcope_1",x1,4),
      c("amphi_m_1",x1,3),
      c("rotif_m_1",x1,2),
      c("chla_1",x1,1),

      c("estfish_bsmt",x2,6),
      c("pcope",x2,5),
      c("hcope",x2,4),
      c("amphi_m",x2,3),
      c("rotif_m",x2,2),
      c("chla",x2,1),

      c("estfish_bsmt_gr",x2,6),
      c("pcope_gr",x2,5),
      c("hcope_gr",x2,4),
      c("amphi_m_gr",x2,3),
      c("rotif_m_gr",x2,2),
      c("chla_gr",x2,1),

      c("potam_1",x3,5),
      c("flow",x3,4),
      c("temp",x3,3),
      c("turbid",x3,2))

  } else if(region == "West") {
    coordinate_map <- list(
      c("estfish_bsmt_1",x1,7),
      c("mysid_1",x1,6),
      c("pcope_1",x1,5),
      c("hcope_1",x1,4),
      c("amphi_m_1",x1,3),
      c("rotif_m_1",x1,2),
      c("chla_1",x1,1),

      c("estfish_bsmt",x2,7),
      c("mysid",x2,6),
      c("pcope",x2,5),
      c("hcope",x2,4),
      c("amphi_m",x2,3),
      c("rotif_m",x2,2),
      c("chla",x2,1),

      c("estfish_bsmt_gr",x2,7),
      c("mysid_gr",x2,6),
      c("pcope_gr",x2,5),
      c("hcope_gr",x2,4),
      c("amphi_m_gr",x2,3),
      c("rotif_m_gr",x2,2),
      c("chla_gr",x2,1),

      c("potam_1",x3,5),
      c("flow",x3,4),
      c("temp",x3,3),
      c("turbid",x3,2))

  } else if(region == "North") {
    coordinate_map <- list(
      c("estfish_bsmt_1",x1,8),
      c("mysid_1",x1,7),
      c("pcope_1",x1,6),
      c("hcope_1",x1,5),
      c("clad_1",x1,4),
      c("amphi_m_1",x1,3),
      c("rotif_m_1",x1,2),
      c("chla_1",x1,1),

      c("estfish_bsmt",x2,8),
      c("mysid",x2,7),
      c("pcope",x2,6),
      c("hcope",x2,5),
      c("clad",x2,4),
      c("amphi_m",x2,3),
      c("rotif_m",x2,2),
      c("chla",x2,1),

      c("estfish_bsmt_gr",x2,8),
      c("mysid_gr",x2,7),
      c("pcope_gr",x2,6),
      c("hcope_gr",x2,5),
      c("clad_gr",x2,4),
      c("amphi_m_gr",x2,3),
      c("rotif_m_gr",x2,2),
      c("chla_gr",x2,1),

      c("corbic_1",x3,5),
      c("flow",x3,4),
      c("temp",x3,3),
      c("turbid",x3,2))

  } else if(region == "South") {
    coordinate_map <- list(
      c("estfish_bsmt_1",x1,7),
      c("pcope_1",x1,6),
      c("hcope_1",x1,5),
      c("clad_1",x1,4),
      c("amphi_m_1",x1,3),
      c("rotif_m_1",x1,2),
      c("chla_1",x1,1),

      c("estfish_bsmt",x2,7),
      c("pcope",x2,6),
      c("hcope",x2,5),
      c("clad",x2,4),
      c("amphi_m",x2,3),
      c("rotif_m",x2,2),
      c("chla",x2,1),

      c("estfish_bsmt_gr",x2,7),
      c("pcope_gr",x2,6),
      c("hcope_gr",x2,5),
      c("clad_gr",x2,4),
      c("amphi_m_gr",x2,3),
      c("rotif_m_gr",x2,2),
      c("chla_gr",x2,1),

      c("corbic_1",x3,5),
      c("flow",x3,4),
      c("temp",x3,3),
      c("turbid",x3,2))
  }

  df <- as.data.frame(do.call("rbind", coordinate_map))
  names(df) <- c("Shortname","x","y")
  df$x <- as.numeric(df$x)
  df$y <- as.numeric(df$y)

  ## R2 coordinates outside of the node:
  df$x_R2 <- df$x - 0.4
  df$y_R2 <- df$y - 0.4

  return(df)
}

######################################################################################
## Edge options:

getAnnualPortOptions <- function() {
  tmp1 <- expand.grid(from_name=c("flow","temp","turbid","potam","corbic"),
                      to_name=c("fish","pzoop","hzoop","chla"),
                      headport="e",
                      tailport="w")

  tmp2 <- data.frame(from_name=c("chla","hzoop","pzoop"),
                     to_name=c("hzoop","pzoop","fish"),
                     headport="s",
                     tailport="n")

  # tmp3 <- data.frame(from_name=c("chla","hzoop","chla"),
  #                    to_name=c("pzoop","fish","fish"),
  #                    headport="w",
  #                    tailport="w")

  tmp4<- data.frame(from_name=c("fish"),
                    to_name=c("estfish","estfish_stn","estfish_bsmt"),
                    headport="s",
                    tailport="n")

  ret <- do.call("rbind", list(tmp1, tmp2, tmp4))
  for(colname in names(ret)) {
    if(is.factor(ret[ ,colname])) {
      ret[ ,colname] <- as.character(ret[ ,colname])
    }
  }
  stopifnot(sum(duplicated(ret[ ,1:2])) == 0)

  return(ret)
}

getUpperTrophicPortOptions <- function() {
  tmp1 <- expand.grid(from_name=c("estfish_bsmt_1","potam_1","pzoop_1","hzoop_1",
                                  "chla_1","corbic_1"),
                      to_name=c("estfish_bsmt","pzoop","hzoop","estfish_bsmt_gr","pzoop_gr","hzoop_gr"),
                      headport="w",
                      tailport="e")

  tmp2 <- expand.grid(from_name=c("marfish_bsmt_1","flow","temp","turbid",
                                  "sside_1","cent_1"),
                      to_name=c("estfish_bsmt","pzoop","hzoop","estfish_bsmt_gr","pzoop_gr","hzoop_gr"),
                      headport="e",
                      tailport="w")

  ret <- do.call("rbind", list(tmp1, tmp2))
  for(colname in names(ret)) {
    if(is.factor(ret[ ,colname])) {
      ret[ ,colname] <- as.character(ret[ ,colname])
    }
  }
  stopifnot(sum(duplicated(ret[ ,1:2])) == 0)

  return(ret)
}

getLowerTrophicPortOptions <- function() {
  tmp1 <- expand.grid(from_name=c("potam_1","pzoop_1","hzoop_1","chla_1","din_1",
                                  "corbic_1"),
                      to_name=c("potam","chla","din","corbic","potam_gr","chla_gr","din_gr","corbic_gr"),
                      headport="w",
                      tailport="e")

  tmp2 <- expand.grid(from_name=c("flow","temp","turbid"),
                      to_name=c("potam","chla","din","corbic","potam_gr","chla_gr","din_gr","corbic_gr"),
                      headport="e",
                      tailport="w")

  ret <- do.call("rbind", list(tmp1, tmp2))
  for(colname in names(ret)) {
    if(is.factor(ret[ ,colname])) {
      ret[ ,colname] <- as.character(ret[ ,colname])
    }
  }
  stopifnot(sum(duplicated(ret[ ,1:2])) == 0)

  return(ret)
}

getZoopPortOptions <- function() {
  tmp1 <- expand.grid(from_name=c("pcope_1","hcope_1","amphi_m_1","chla_1","rotif_m_1",
                                  "mysid_1","estfish_bsmt_1","clad_1"),
                      to_name=c("pcope","hcope","amphi_m","chla","mysid","clad","rotif_m","estfish_bsmt",
                                "pcope_gr","hcope_gr","amphi_m_gr","chla_gr","mysid_gr","clad_gr","rotif_m_gr","estfish_bsmt_gr"),
                      headport="w",
                      tailport="e")

  tmp2 <- expand.grid(from_name=c("flow","temp","turbid","corbic_1","potam_1"),
                      to_name=c("pcope","hcope","amphi_m","chla","mysid","clad","rotif_m","estfish_bsmt",
                                "pcope_gr","hcope_gr","amphi_m_gr","chla_gr","mysid_gr","clad_gr","rotif_m_gr","estfish_bsmt_gr"),
                      headport="e",
                      tailport="w")

  ret <- do.call("rbind", list(tmp1, tmp2))
  for(colname in names(ret)) {
    if(is.factor(ret[ ,colname])) {
      ret[ ,colname] <- as.character(ret[ ,colname])
    }
  }
  stopifnot(sum(duplicated(ret[ ,1:2])) == 0)

  return(ret)
}


######################################################################################
## Nodes and edges:

## For coloring lines according to significance and pos/neg:
colorFcn <- function(pval, coef, sig, col_pos, col_neg, col_ns) {
  stopifnot(length(pval) == length(coef))
  ret <- rep("black", length(pval))

  ## Significant:
  ret[(pval < sig) & (coef > 0)] <- col_pos
  ret[(pval < sig) & (coef < 0)] <- col_neg

  ## Not significant:
  ret[(pval >= sig)] <- col_ns

  return(ret)
}

## For setting line width according to coefficients:
widthFcn <- function(coef, digits) {
  5*(round(abs(coef), digits) + 1/15)
}

getNodes <- function(fit) {
  ## Adapted from the lavaanPlot package:

  regress <- fit@ParTable$op == "~"
  latent <- fit@ParTable$op == "=~"

  observed_nodes <- c()
  latent_nodes <- c()

  if(any(regress)){
    observed_nodes <- c(observed_nodes, unique(fit@ParTable$rhs[regress]))
    observed_nodes <- c(observed_nodes, unique(fit@ParTable$lhs[regress]))
  }
  if(any(latent)) {
    observed_nodes <- c(observed_nodes, unique(fit@ParTable$rhs[latent]))
    latent_nodes <- c(latent_nodes, unique(fit@ParTable$lhs[latent]))
  }
  # make sure latent variables don't show up in both
  observed_nodes <- setdiff(observed_nodes, latent_nodes)

  ## Add R2 values:
  R2_df <- data.frame("R2"=round(lavaan::lavInspect(fit, what="rsquare"),3))
  R2_df$Shortname <- row.names(R2_df)
  ## Remove class "lavaan.vector" from the rsquare values after getting names:
  R2_df$R2 <- as.numeric(R2_df$R2)

  ret <- data.frame(Shortname=c(observed_nodes, latent_nodes),
                    var_type=c(rep("observed",length(observed_nodes)),
                               rep("latent",length(latent_nodes)))
  ) %>%
    dplyr::left_join(R2_df, by="Shortname")

  return(ret)
}

getEdges <- function(fit, node_df, sig, digits, col_pos, col_neg, col_ns) {
  ## For mapping from short variable name to node id for creating edges:
  map_node_name_to_id <- node_df$id
  names(map_node_name_to_id) <- node_df$Shortname

  ## Create input for edges data frame:
  ret <- as.data.frame(fit@ParTable) %>%
    dplyr::mutate(lhs_id=map_node_name_to_id[lhs],
                  rhs_id=map_node_name_to_id[rhs],
                  zval=est/se,
                  pval=(1 - stats::pnorm(abs(zval))) * 2,
                  var_type=dplyr::case_when(op == "~" ~ "regress",
                                            op == "=~" ~ "latent",
                                            op == "~~" & lhs != rhs ~ "cov"),
                  from_name=dplyr::case_when(var_type == "regress" ~ rhs,
                                             var_type == "latent" ~ lhs,
                                             var_type == "cov" ~ lhs),
                  from_id=dplyr::case_when(var_type == "regress" ~ rhs_id,
                                           var_type == "latent" ~ lhs_id,
                                           var_type == "cov" ~ lhs_id),
                  to_name=dplyr::case_when(var_type == "regress" ~ lhs,
                                           var_type == "latent" ~ rhs,
                                           var_type == "cov" ~ rhs),
                  to_id=dplyr::case_when(var_type == "regress" ~ lhs_id,
                                         var_type == "latent" ~ rhs_id,
                                         var_type == "cov" ~ rhs_id),
                  dir=dplyr::case_when(var_type == "cov" ~ "both"),
                  penwidth=widthFcn(est, digits=digits),
                  color=colorFcn(pval=pval, coef=est, sig=sig, col_pos, col_neg,
                                 col_ns)) %>%
    dplyr::filter(var_type %in% c("regress","latent","cov")) %>%

  return(ret)
}


######################################################################################
## Graph:

createGraph <- function(fit, reference_df, model_type, region=NULL,
                        title="", cov=FALSE, manual_port_settings=FALSE,
                        addR2Outside=TRUE, addR2Inside=FALSE,
                        sig=0.05, digits=2,
                        line_col_positive="#00B0F0",
                        line_col_negative="red",
                        line_col_notsig="gray60") {
  ## model_type must be one of the following:
  ##  "annual","monthly_upper_trophic","monthly_lower_trophic","monthly_zoop"

  ## Names come from reference_df.
  ## Coordinates come from coord_input.
  ## Port options come from port_opt_df.

  ## Check reference_df:
  stopifnot(all(c("Shortname","Diagramname") %in% names(reference_df)))
  if(any(is.na(reference_df$Shortname))) {
    stop("At least one Shortname missing in reference_df.")
  }
  reference_df$Diagramname <- ifelse(is.na(reference_df$Diagramname),
                                     reference_df$Shortname,
                                     reference_df$Diagramname)

  ## Get node coordinates and edge preferences:
  if(model_type == "annual") {
    coord_input <- getAnnualCoordinates()
    port_opt_df <- getAnnualPortOptions()
  } else if(model_type == "monthly_upper_trophic") {
    coord_input <- getUpperTrophicCoordinates()
    port_opt_df <- getUpperTrophicPortOptions()
  } else if(model_type == "monthly_lower_trophic") {
    coord_input <- getLowerTrophicCoordinates()
    port_opt_df <- getLowerTrophicPortOptions()
  } else if(model_type == "monthly_zoop") {
    if(is.null(region)) {
      stop("region must be defined for monthly_zoop model")
    }
    coord_input <- getZoopCoordinates(region)
    port_opt_df <- getZoopPortOptions()
  }

  ## Create nodes. Needs to stay in order according to the id column that gets created.
  ## Edge matching apparently occurs by ordering despite the id column.
  node_input_df <- getNodes(fit) %>%
    dplyr::left_join(reference_df, by="Shortname") %>%
    dplyr::left_join(coord_input, by="Shortname")

  stopifnot(all(node_input_df$node %in% reference_df$Shortname))
  if(any(is.na(node_input_df[ ,c("x","y")]))) {
    print(node_input_df)
    stop("Missing value(s) of x and/or y.")
  }
  if(any(duplicated(node_input_df[ ,c("x","y")]))) {
    print(node_input_df)
    stop("Duplicated coordinates.")
  }

  if(addR2Inside) {
    node_input_df <- node_input_df %>%
      dplyr::mutate(label=ifelse(is.na(R2), Diagramname,
                                  sprintf("%s\n(%s)",Diagramname,R2)))
  } else {
    node_input_df <- node_input_df %>%
      dplyr::mutate(label=Diagramname)
  }

  node_df <- DiagrammeR::create_node_df(
    n=nrow(node_input_df),
    label=node_input_df$label,
    Shortname=node_input_df$Shortname,
    x=node_input_df$x,
    y=node_input_df$y,
    fontcolor="white",
    color=node_input_df$Color,
    fillcolor=node_input_df$Color,
    shape=dplyr::case_when(node_input_df$var_type == "observed" ~ "polygon",
                           node_input_df$var_type == "latent" ~ "ellipse"),
    width=1,
    fixedsize=FALSE)

  ## For R2 outside the nodes:
  R2_input_df <- node_input_df %>%
    dplyr::filter(!is.na(R2))
  R2_node_df <- DiagrammeR::create_node_df(
    n=nrow(R2_input_df),
    label=R2_input_df$R2,
    Shortname=R2_input_df$Shortname,
    x=R2_input_df$x_R2,
    y=R2_input_df$y_R2,
    fontcolor="black",
    color="#FFFFFF00",  # use transparency to prevent a shadow
    fillcolor="#FFFFFF00",  # use transparency to prevent a shadow
    style="filled",
    shape="ellipse",
    penwidth=0,
    width=0.4,
    height=0.2,
    fixedsize=TRUE)
  R2_node_df$id <- R2_node_df$id + nrow(node_df)

  ## Create edges:
  edge_input_df <- getEdges(fit, node_df, sig=sig, digits=digits,
                            col_pos=line_col_positive, col_neg=line_col_negative,
                            col_n=line_col_notsig)
  if(!cov) {
    edge_input_df <- subset(edge_input_df, var_type != "cov")
  }

  if(manual_port_settings) {
    edge_input_df <- edge_input_df %>%
      dplyr::left_join(port_opt_df, by=c("from_name","to_name"))
  } else {
    edge_input_df <- edge_input_df %>%
      dplyr::mutate(headport=NA, tailport=NA)
  }
  stopifnot(all(edge_input_df$lhs_ok) && all(edge_input_df$rhs_ok))

  edges_df <- DiagrammeR::create_edge_df(
    from=edge_input_df$from_id,
    to=edge_input_df$to_id,
    from_name=edge_input_df$from_name,
    to_name=edge_input_df$to_name,
    penwidth=edge_input_df$penwidth,
    color=edge_input_df$color,
    dir=edge_input_df$dir,
    headport=edge_input_df$headport,
    tailport=edge_input_df$tailport)

  ## Create graph:
  graph <- DiagrammeR::create_graph() %>%
    DiagrammeR::add_node_df(node_df=node_df) %>%
    DiagrammeR::add_edge_df(edge_df=edges_df) %>%
    DiagrammeR::add_global_graph_attrs(attr="splines",
                                       value="spline",
                                       attr_type="graph") %>%
    DiagrammeR::add_global_graph_attrs(attr="bgcolor",
                                       value="transparent",
                                       attr_type="graph")
  if(addR2Outside) {
    graph <- graph %>%
      DiagrammeR::add_node_df(R2_node_df)
  }

  graph <- graph %>%
    DiagrammeR::render_graph(title=title)

  return(graph)
}


convert_html_to_grob = function(html_input, resolution){

  temp_name = "temp.png"

  html_input %>%
    export_svg %>%
    charToRaw %>%
    rsvg_png("temp.png", height = resolution)

  out_grob = rasterGrob(readPNG("temp.png", native = FALSE))

  file.remove("temp.png")
  return(out_grob)
}

