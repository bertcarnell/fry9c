.num_pattern <- "^[1-9][0-9]*[.]$|^[1-9][0-9]*[.][a-z][.]$|^[1-9][0-9]*[.][a-z][.][(][1-9][)]$|^[1-9][0-9]*[.][a-z][.][(][1-9][)][(][a-z][)]$"

#' Parse a FR Y-9c XML schema
#'
#' @param xml_filename The full filename of the FR Y-9c schema
#' @param xsd_filename (Default NA) An optional parameter defining the xml schema document
#'
#' @return an FRY9c object
#' @export
#'
#' @details Need to add ... to pass arguments to read_xml
#'
#' @examples
#' xml <- '
#' <FRY9C date="20160101" omb_number="1" title="Test FR Y-9C">
#' <schedule desig="A" title="B">
#' <component num="1." name="Name1" key="NA">
#' <component num="1.a." name="Name1a" key="NA">
#' </component>
#' </component>
#' <component num="2." name="Name2" key="NA">
#' </component>
#' </schedule>
#' </FRY9C>
#' '
#'
#' tmpfilename <- tempfile(fileext = ".xml")
#' tmpfile <- file(tmpfilename, encoding = "UTF-8")
#' cat(xml, file = tmpfile)
#' close(tmpfile)
#' fry_9c <- parse_fry9c(tmpfilename)
#' unlink(tmpfilename)
parse_fry9c <- function(xml_filename, xsd_filename = NA)
{
  #xml_filename <- "C:\\developer\\repositories\\ProvableBanking\\FR_Y-9C20180331.xml"
  #xsd_filename <- "C:\\developer\\repositories\\ProvableBanking\\FR_Y-9c.xsd"
  assertthat::assert_that(file.exists(xml_filename))
  X <- xml2::read_xml(xml_filename)
  if (length(xsd_filename) == 1 && !is.na(xsd_filename))
  {
    assertthat::assert_that(file.exists(xsd_filename))
    assertthat::assert_that(xml2::xml_validate(X, schema = xsd_filename))
  }
  fry9c_attrs <- xml_attrs(X)
  fry9c <- Fry9c(fry9c_attrs["date"], fry9c_attrs["omb_number"], fry9c_attrs["title"])
  fry9c_scheds <- xml_children(X)
  for (i in seq_along(fry9c_scheds))
  {
    fry9c$add(parse_schedule(fry9c_scheds[[i]]))
  }
  return(fry9c)
}


#' Parse the schedule portion of the FR Y-9c schema
#'
#' @param nodeset an xml2 nodeset like that returned by xml_children
#'
#' @return an xml2 nodeset
parse_schedule <- function(nodeset)
{
  #nodeset <- fry9c_scheds[[1]]
  sched_attrs <- xml_attrs(nodeset)
  sched <- Schedule(sched_attrs["desig"], sched_attrs["title"])
  sched_components <- xml_children(nodeset)
  for (i in seq_along(sched_components))
  {
    sched$add(parse_component(sched_components[[i]]))
  }
  return(sched)
}

#' Parse the component portion of the FR Y-9c schema
#'
#' @param nodeset an xml2 nodeset like that returned by xml_children
#'
#' @return an xml2 nodeset
parse_component <- function(nodeset)
{
  #nodeset <- sched_components[[1]]
  comp_attrs <- xml_attrs(nodeset)
  key <- ifelse(comp_attrs["key"] == "NA", NA, comp_attrs["key"])
  assertthat::assert_that(grepl(.num_pattern, comp_attrs["num"]),
                          msg = paste0(comp_attrs["num"], " does not match the pattern"))
  comp <- Component(num = comp_attrs["num"], name = comp_attrs["name"], key = key)
  if (xml_length(nodeset) == 0)
  {
    return(comp)
  }
  comp_childs <- xml_children(nodeset)
  for (i in seq_along(comp_childs))
  {
    comp$add(parse_component(comp_childs[[i]]))
  }
  return(comp)
}
