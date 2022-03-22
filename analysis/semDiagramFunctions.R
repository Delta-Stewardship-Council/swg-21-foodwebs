

generateInputDf <- function(fit, reference_df) {

  ParTable <- fit@ParTable
  ind <- ParTable$op != ":="
  focus_vars <- unique(c(ParTable$lhs[ind], ParTable$rhs[ind]))

  stopifnot(all(c("Shortname","Diagramname") %in% names(reference_df)))
  stopifnot(all(focus_vars %in% reference_df$Shortname))

  ret <- subset(reference_df, Shortname %in% focus_vars)

  if(any(is.na(ret$Diagramname))) {
    print(ret)
    stop("At least one Diagramname missing.")
  }

  return(ret)
}


getUpperTrophicCoord <- function(df) {
  x1 <- 1
  x2 <- 3.5
  x3 <- 6

  df[df$Shortname == "estfish_bsmt_1",c("x","y")] <- list(x1,5)
  df[df$Shortname == "potam_1",c("x","y")] <- list(x1,4)
  df[df$Shortname == "pzoop_1",c("x","y")] <- list(x1,3)
  df[df$Shortname == "hzoop_1",c("x","y")] <- list(x1,2)
  df[df$Shortname == "chla_1",c("x","y")] <- list(x1,1)

  df[df$Shortname == "estfish_bsmt",c("x","y")] <- list(x2,5)
  df[df$Shortname == "pzoop",c("x","y")] <- list(x2,3)
  df[df$Shortname == "hzoop",c("x","y")] <- list(x2,2)

  df[df$Shortname == "marfish_bsmt_1",c("x","y")] <- list(x3,5)
  df[df$Shortname == "flow",c("x","y")] <- list(x3,3)
  df[df$Shortname == "temp",c("x","y")] <- list(x3,2)
  df[df$Shortname == "turbid",c("x","y")] <- list(x3,1)


  df[df$Shortname == "corbic_1",c("x","y")] <- list(x1,4)


  df[df$Shortname == "sside_1",c("x","y")] <- list(x3,5)
  df[df$Shortname == "cent_1",c("x","y")] <- list(x3,4)

  if(any(duplicated(df[ ,c("x","y")]))) {
    print(df)
    stop("Duplicated coordinates.")
  }

  return(df)
}


getLowerTrophicCoord <- function(df) {
  x1 <- 1
  x2 <- 3.5
  x3 <- 6

  df[df$Shortname == "potam_1",c("x","y")] <- list(x1,5)
  df[df$Shortname == "pzoop_1",c("x","y")] <- list(x1,4)
  df[df$Shortname == "hzoop_1",c("x","y")] <- list(x1,3)
  df[df$Shortname == "chla_1",c("x","y")] <- list(x1,2)
  df[df$Shortname == "din_1",c("x","y")] <- list(x1,1)

  df[df$Shortname == "potam",c("x","y")] <- list(x2,5)
  df[df$Shortname == "chla",c("x","y")] <- list(x2,2)
  df[df$Shortname == "din",c("x","y")] <- list(x2,1)

  df[df$Shortname == "flow",c("x","y")] <- list(x3,4)
  df[df$Shortname == "temp",c("x","y")] <- list(x3,3)
  df[df$Shortname == "turbid",c("x","y")] <- list(x3,2)


  df[df$Shortname == "corbic_1",c("x","y")] <- list(x1,5)

  df[df$Shortname == "corbic",c("x","y")] <- list(x2,5)

  if(any(duplicated(df[ ,c("x","y")]))) {
    print(df)
    stop("Duplicated coordinates.")
  }

  return(df)
}


getZoopCoord <- function(df, region) {
  x1 <- 1
  x2 <- 3.5
  x3 <- 6

  if(region == "Far West") {
    df[df$Shortname == "potam_1",c("x","y")] <- list(x1,5)
    df[df$Shortname == "pcope_1",c("x","y")] <- list(x1,4)
    df[df$Shortname == "hcope_1",c("x","y")] <- list(x1,3)
    df[df$Shortname == "amphi_1",c("x","y")] <- list(x1,2)
    df[df$Shortname == "chla_1",c("x","y")] <- list(x1,1)

    df[df$Shortname == "pcope",c("x","y")] <- list(x2,4)
    df[df$Shortname == "hcope",c("x","y")] <- list(x2,3)
    df[df$Shortname == "amphi",c("x","y")] <- list(x2,2)
    df[df$Shortname == "chla",c("x","y")] <- list(x2,1)

  } else if(region == "West") {
    df[df$Shortname == "potam_1",c("x","y")] <- list(x1,6)
    df[df$Shortname == "mysid_1",c("x","y")] <- list(x1,5)
    df[df$Shortname == "pcope_1",c("x","y")] <- list(x1,4)
    df[df$Shortname == "hcope_1",c("x","y")] <- list(x1,3)
    df[df$Shortname == "amphi_1",c("x","y")] <- list(x1,2)
    df[df$Shortname == "chla_1",c("x","y")] <- list(x1,1)

    df[df$Shortname == "mysid",c("x","y")] <- list(x2,5)
    df[df$Shortname == "pcope",c("x","y")] <- list(x2,4)
    df[df$Shortname == "hcope",c("x","y")] <- list(x2,3)
    df[df$Shortname == "amphi",c("x","y")] <- list(x2,2)
    df[df$Shortname == "chla",c("x","y")] <- list(x2,1)

  } else if(region == "North") {
    df[df$Shortname == "corbic_1",c("x","y")] <- list(x1,6)
    df[df$Shortname == "mysid_1",c("x","y")] <- list(x1,5)
    df[df$Shortname == "pcope_1",c("x","y")] <- list(x1,4)
    df[df$Shortname == "hcope_1",c("x","y")] <- list(x1,3)
    df[df$Shortname == "amphi_1",c("x","y")] <- list(x1,2)
    df[df$Shortname == "chla_1",c("x","y")] <- list(x1,1)

    df[df$Shortname == "mysid",c("x","y")] <- list(x2,5)
    df[df$Shortname == "pcope",c("x","y")] <- list(x2,4)
    df[df$Shortname == "hcope",c("x","y")] <- list(x2,3)
    df[df$Shortname == "amphi",c("x","y")] <- list(x2,2)
    df[df$Shortname == "chla",c("x","y")] <- list(x2,1)

  } else if(region == "South") {
    df[df$Shortname == "corbic_1",c("x","y")] <- list(x1,6)
    df[df$Shortname == "pcope_1",c("x","y")] <- list(x1,5)
    df[df$Shortname == "hcope_1",c("x","y")] <- list(x1,4)
    df[df$Shortname == "clad_1",c("x","y")] <- list(x1,3)
    df[df$Shortname == "amphi_1",c("x","y")] <- list(x1,2)
    df[df$Shortname == "chla_1",c("x","y")] <- list(x1,1)

    df[df$Shortname == "pcope",c("x","y")] <- list(x2,5)
    df[df$Shortname == "hcope",c("x","y")] <- list(x2,4)
    df[df$Shortname == "clad",c("x","y")] <- list(x2,3)
    df[df$Shortname == "amphi",c("x","y")] <- list(x2,2)
    df[df$Shortname == "chla",c("x","y")] <- list(x2,1)
  }

  df[df$Shortname == "flow",c("x","y")] <- list(x3,5)
  df[df$Shortname == "temp",c("x","y")] <- list(x3,4)
  df[df$Shortname == "turbid",c("x","y")] <- list(x3,3)
  df[df$Shortname == "estfish_bsmt_1",c("x","y")] <- list(x3,2)

  if(any(duplicated(df[ ,c("x","y")]))) {
    print(df)
    stop("Duplicated coordinates.")
  }

  return(df)
}


createGraph <- function(input_df, fit, sig=0.05, digits=2, line_col_positive="#00B0F0",
                        line_col_negative="red", line_col_notsig="gray50") {

  required_fields <- c("Shortname","Diagramname","x","y")
  stopifnot(all(required_fields %in% names(input_df)))

  if(any(is.na(input_df$x)) || any(is.na(input_df$y))) {
    stop("Missing value(s) of x and/or y.")
  }

  if("Color" %in% names(input_df)) {
    input_df$Color <- ifelse(is.na(input_df$Color), "black", input_df$Color)
  }

  ## Needs to stay in order according to the id column that gets created.
  ## Edge matching apparently occurs by ordering despite the id column.
  node_df <- DiagrammeR::create_node_df(n=nrow(input_df),
                                        label=input_df$Diagramname,
                                        Shortname=input_df$Shortname,
                                        x=input_df$x,
                                        y=input_df$y,
                                        color=input_df$Color,
                                        fillcolor=input_df$Color,
                                        shape="polygon",
                                        width=1,
                                        fixedsize=FALSE)

  # Create graph:
  graph <- DiagrammeR::create_graph() %>%
    DiagrammeR::add_node_df(node_df=node_df)

  ## For mapping from short variable name to node id for creating edges:
  map_node_name_to_id <- node_df$id
  names(map_node_name_to_id) <- node_df$Shortname

  ## And for determining headport:
  map_node_name_to_x <- node_df$x
  names(map_node_name_to_x) <- node_df$Shortname

  colorFcn <- function(pval, coef, sig) {
    stopifnot(length(pval) == length(coef))
    ret <- rep("black", length(pval))

    ## Significant:
    ret[(pval < sig) & (coef > 0)] <- line_col_positive
    ret[(pval < sig) & (coef < 0)] <- line_col_negative

    ## Not significant:
    ret[(pval >= sig)] <- line_col_notsig

    return(ret)
  }

  # Set line width according to coefficients:
  widthFcn <- function(coef, digits) {
    5*(round(abs(coef), digits) + 1/15)
  }

  ## Create input for edges data frame:
  fit_df <- as.data.frame(fit@ParTable) %>%
    dplyr::filter(op %in% c("~","=~")) %>%
    dplyr::mutate(lhs_id=map_node_name_to_id[lhs],
                  rhs_id=map_node_name_to_id[rhs],
                  zval=est/se,
                  pval=(1 - stats::pnorm(abs(zval))) * 2,
                  type=dplyr::case_when(op == "~" ~ "regress",
                                        op == "=~" ~ "latent"),
                  from_name=dplyr::case_when(type == "regress" ~ rhs,
                                             type == "latent" ~ lhs),
                  from_id=dplyr::case_when(type == "regress" ~ rhs_id,
                                           type == "latent" ~ lhs_id),
                  to_name=dplyr::case_when(type == "regress" ~ lhs,
                                           type == "latent" ~ rhs),
                  to_id=dplyr::case_when(type == "regress" ~ lhs_id,
                                         type == "latent" ~ rhs_id),
                  penwidth=widthFcn(est, digits=digits),
                  color=colorFcn(pval=pval, coef=est, sig=sig),
                  from_node_x=map_node_name_to_x[from_name],
                  lhs_ok=(lhs %in% input_df$Shortname),
                  rhs_ok=(rhs %in% input_df$Shortname))

  ## Stop if any nodes are missing from input_df:
  stopifnot(all(fit_df$lhs_ok) && all(fit_df$rhs_ok))

  # Create edges:
  if(any(fit_df$type == "regress")) {
    regress_df <- subset(fit_df, type == "regress")

    edges_regress <- DiagrammeR::create_edge_df(
      from=regress_df$from_id,
      to=regress_df$to_id,
      from_name=regress_df$from_name,
      to_name=regress_df$to_name,
      penwidth=regress_df$penwidth,
      color=regress_df$color,
      from_node_x=regress_df$from_node_x) %>%
      dplyr::mutate(headport=dplyr::case_when(
                      from_node_x == min(map_node_name_to_x) ~ "w",
                      from_node_x == max(map_node_name_to_x) ~ "e"),
                    tailport=dplyr::case_when(
                      from_node_x == min(map_node_name_to_x) ~ "e",
                      from_node_x == max(map_node_name_to_x) ~ "w"))

    graph <- graph %>%
      DiagrammeR::add_edge_df(edge_df=edges_regress)
  }

  if(any(fit_df$type == "latent")) {
    latent_df <- subset(fit_df, type == "latent")

    edges_latent <- DiagrammeR::create_edge_df(
      from=latent_df$from_id,
      to=latent_df$to_id,
      from_name=latent_df$from_name,
      to_name=latent_df$to_name,
      penwidth=latent_df$penwidth,
      color=latent_df$color,
      from_node_x=latent_df$from_node_x) %>%
      dplyr::mutate(headport=dplyr::case_when(
                      from_node_x == min(map_node_name_to_x) ~ "w",
                      from_node_x == max(map_node_name_to_x) ~ "e"),
                    tailport=dplyr::case_when(
                      from_node_x == min(map_node_name_to_x) ~ "e",
                      from_node_x == max(map_node_name_to_x) ~ "w"))

    graph <- graph %>%
      DiagrammeR::add_edge_df(edge_df=edges_latent)
  }

  return(graph)
}




