link_drzave <- read_html("https://eurovision.tv/countries")
drzave <- link_drzave %>%
  html_nodes(xpath = "//div[@class='flex flex-wrap']//h4[@class='font-bold text-xl leading-tight group-hover:text-blue-600']") %>%
  html_text()
#drzave %>% View
vektor_drzave <- gsub("\n", "", drzave)
