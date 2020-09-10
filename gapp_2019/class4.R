### installs if necessary and loads tidyverse and sf, another package which we will be using today
list.of.packages <- c("tidyverse", "rvest", "countrycode", "maps")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

invisible(lapply(list.of.packages, library, character.only = TRUE))


### useful function to get link behind text on webpage
get_link_text <- function(text, html_page){
  tryCatch({
    html_page %>%
      html_nodes(xpath=paste0("//a[text()='", text, "']")) %>% 
      .[[1]] %>% 
      html_attr("href")
  }, error = function(e) {
    print(paste("Error for", text))
  })
}

# get webpage
html_heads <- read_html("https://en.wikipedia.org/wiki/List_of_current_heads_of_state_and_government")

# extract table
df <- html_heads %>%
  html_nodes(xpath = '/html/body/div[3]/div[3]/div[5]/div[1]/table[2]') %>%
  html_table(fill=TRUE) %>%
  .[[1]] %>%
  mutate(leader = gsub(".*– ", "", `Head of government`)) %>%
  mutate(leader = gsub(".*\u2013 ", "", leader)) %>% # this line is for the windows computers where the previous line does not work
  mutate(leader = gsub("^Sheikh |^Cardinal |^Prince ", "", leader)) %>%
  mutate(leader = gsub(" \\(.*$|\\[.*$", "", leader)) %>%
  filter(State!="Switzerland" | (State=="Switzerland" & leader=="Ueli Maurer")) %>%
  mutate(leader = ifelse(leader=="Hasina", "Sheikh Hasina", leader)) %>%
  select(State, leader) %>%
  filter(!duplicated(State)) %>%
  mutate(link_leader = lapply(leader, get_link_text, html_page=html_heads))

# get links to each party
## first do example
get_link_party_example <- "https://en.wikipedia.org/wiki/Jean_Castex" %>%
  read_html() %>%
  html_nodes(xpath='//table[contains(@class, "infobox")]/tbody/tr/th[text()="Political party"]/../td/a') %>%
  html_attr("href") %>%
  tail(., n=1)

## then write function 
get_link_party <- function(leader_link){
  paste0("https://en.wikipedia.org", leader_link) %>%
    read_html() %>%
    html_nodes(xpath='//table[contains(@class, "infobox")]/tbody/tr/th[text()="Political party"]/../td/a') %>%
    html_attr("href") %>%
    tail(., n=1)
}

## then write loop
df$link_party <- ""
for (i in 1:nrow(df)) {
  tryCatch({
    df$link_party[i] <- get_link_party(df$link_leader[i])
    print(paste("Got link for party of", df$leader[i]))
  }, error = function(e) {
    print(paste("Error for", df$leader[i]))
  })
}

## first write example
get_political_position_example <- "https://en.wikipedia.org/wiki/La_R%C3%A9publique_En_Marche!" %>%
  read_html() %>%
  html_nodes(xpath='//table[contains(@class, "infobox")]') %>%
  html_table(fill=TRUE) %>%
  .[[1]] %>%
  set_tidy_names() %>%
  filter(grepl('*olitical.*osition', .[,1])) %>%
  .[1,2]

## then write function 
get_political_position <- function(party_link){
  paste0("https://en.wikipedia.org", party_link) %>%
    read_html() %>%
    html_nodes(xpath='//table[contains(@class, "infobox")]') %>%
    html_table(fill=TRUE) %>%
    .[[1]] %>%
    set_tidy_names() %>%
    filter(grepl('*olitical.*osition', .[,1])) %>%
    .[1,2]
}

## then write loop 
df$political_position <- ""
for (i in 1:nrow(df)) {
  tryCatch({
    df$political_position[i] <- get_political_position(df$link_party[i])
    print(paste("Got political position of", df$leader[i]))
  }, error = function(e) {
    print(paste("Error for", df$leader[i]))
  })
}

## Cleaning
political_classification = c("far left","left wing","centre left to left wing", "centre left", 
                             "centre to centre left", "centre/big tent", "centre to centre right", "centre right",
                             "centre right to right wing", "right wing", "unavailable")

df_clean <- df %>%
  select(State, political_position) %>%
  mutate(political_position_clean = political_position) %>%
  mutate(political_position_clean=gsub("^[^:]+:", " ", political_position_clean)) %>%
  mutate(political_position_clean=gsub(":.*$", " ", political_position_clean)) %>%
  mutate(political_position_clean=gsub("[0-9]{4}.*$", " ", political_position_clean)) %>%
  mutate(political_position_clean=gsub("\\([^()]*\\)", "", political_position_clean)) %>%
  mutate(political_position_clean=gsub("\\[[^[]]*\\]", "", political_position_clean)) %>%
  mutate(political_position_clean=tolower(political_position_clean)) %>%
  mutate(political_position_clean=gsub("[^a-z ]", " ", political_position_clean)) %>%
  mutate(political_position_clean=str_trim(political_position_clean)) %>%
  mutate(political_position_clean=ifelse(political_position_clean=="", NA, political_position_clean)) %>%
  mutate(political_position_clean=gsub("center", "centre", political_position_clean)) %>%
  mutate(political_position_clean=gsub("tocentre", "to centre", political_position_clean)) %>%
  mutate(political_position_clean=gsub("toright", "to right", political_position_clean)) %>%
  mutate(political_position_clean=gsub(" formerly", "", political_position_clean)) %>%
  mutate(political_position_clean=gsub("historical", "", political_position_clean)) %>%
  mutate(political_position_clean=gsub(" correa era", "", political_position_clean)) %>%
  mutate(political_position_clean=gsub(" citation needed", "", political_position_clean)) %>%
  mutate(political_position_clean=gsub("economic", "", political_position_clean)) %>%
  mutate(political_position_clean=gsub("after", "", political_position_clean)) %>%
  mutate(political_position_clean=str_trim(political_position_clean)) %>%
  mutate(political_position_clean=gsub("centre left to centre right", "centre", political_position_clean)) %>%
  mutate(political_position_clean=gsub("big tent of the left", "big tent", political_position_clean)) %>%
  mutate(political_position_clean=gsub("big tent", "centre/big tent", political_position_clean)) %>%
  mutate(political_position_clean=gsub("^centre$", "centre/big tent", political_position_clean)) %>%
  mutate(political_position_clean = ifelse(political_position_clean %in% political_classification, political_position_clean, NA)) %>%
  mutate(iso3 = countrycode(State, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(State=="Micronesia", "FSM", iso3))

map_world <- map_data("world") %>%
  mutate(iso3 = countrycode(region, "country.name", "iso3c")) %>%
  mutate(iso3 = ifelse(region=="Micronesia", "FSM", iso3)) %>%
  left_join(df_clean, by="iso3") %>%
  mutate(political_position_clean=ifelse(is.na(political_position_clean), "unavailable", political_position_clean)) %>%
  mutate(political_position_clean=factor(political_position_clean,
                                         levels = political_classification,
                                         labels = political_classification))

plot <- ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group, fill = political_position_clean)) +
  scale_fill_manual(values = c("#FF0000","#E2001C","#C60038", "#AA0055", "#8D0071", 
                               "#71008D", "#5500AA", "#3800C6","#1C00E2", "#0000FF", "#bebebe")) + coord_fixed()

plot
ggsave("Output/worldmappolitics.jpg", width = 20, height=12, device = "jpg")





