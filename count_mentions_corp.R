library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.tidy)
setwd("C:/Users/dapon/Dropbox/Harvard/dissertation")
quanteda_options(threads = 6)


raw <- read_csv("data/ParlSpeech/HouseOfCommons.csv") 
# read in mps data 
mps2019 <- read_csv("data/uk_geography/pcon_data/mps_by_constituency_2019.csv") %>%
  select(mp, constit = constituency) %>% mutate(parliament = 2019) %>% 
  mutate(mp = str_remove(mp, "Sir "))

mps2017 <- read_csv("data/uk_geography/pcon_data/mps_by_constituency_2017.csv") %>%
  select(mp, constit = constituency) %>% mutate(parliament = 2017) %>% 
  mutate(mp = str_remove(mp, "Sir "))

mps2015 <- read_csv("data/uk_geography/pcon_data/mps_by_constituency_2015.csv") %>% 
  select(mp, constit = constituency) %>% mutate(parliament = 2015) %>% 
  mutate(mp = str_trim(str_split(mp, "\\(", simplify = TRUE)[,1])) %>% 
  mutate(mp = str_remove(mp, "Sir "))

mps2010 <- read_csv("data/uk_geography/pcon_data/mps_by_constituency_2010.csv") %>% 
  select(mp, constit = constituency) %>% mutate(parliament = 2010) %>% 
  mutate(mp = str_trim(str_split(mp, "\\(", simplify = TRUE)[,1])) %>% 
  mutate(mp = str_remove(mp, "Sir ")) 

mps2005 <- read_csv("data/uk_geography/pcon_data/mps_by_constituency_2005.csv") %>% 
  select(mp, constit = constituency) %>% mutate(parliament = 2005) %>% 
  mutate(mp = str_remove(mp, "Sir "),
         mp = str_remove(mp, "Dr "),
         mp = str_remove(mp, " Bt"),
         mp = case_when(
           str_detect(mp, "Lembit") ~ "Lembit",
           str_detect(mp, " Simon") ~ "Sion Simon",
           TRUE ~ mp
         ))

mps2001 <- read_csv("data/uk_geography/pcon_data/mps_by_constituency_2001.csv") %>% 
  select(mp, constit = constituency) %>% mutate(parliament = 2001) %>% 
  mutate(mp = str_remove(mp, "Sir "),
         mp = str_remove(mp, "Dr "),
         mp = str_remove(mp, " Bt"),
         mp = case_when(
           str_detect(mp, "Lembit") ~ "Lembit",
           str_detect(mp, " Simon") ~ "Sion Simon",
           TRUE ~ mp
         ))



mps <- bind_rows(mps2019, mps2017, mps2015, mps2010, mps2005, mps2001)

election2019 <- as.Date("2019-12-12")
election2017 <- as.Date("2017-06-08")
election2015 <- as.Date("2015-05-07")
election2010 <- as.Date("2010-05-06")
election2005 <- as.Date("2005-05-05")
election2001 <- as.Date("2001-06-07")

df <- raw %>% 
  dplyr::select(date, speaker, party, terms, text,
                agenda, party.facts.id, chair) %>% 
  mutate(docid = 1:n()) %>% 
  filter(chair == FALSE) %>% 
  select(-chair) %>% 
  # there are some duplicates - deal with those
  # distinct(date, speaker, party, terms, text, agenda, party.facts.id) %>% 
  filter(date > election2001) %>% 
  mutate(parliament = case_when(
    date <= election2005 ~ 2001, 
    date > election2005 & date <= election2010 ~ 2005, 
    date <= election2015 & date > election2010 ~ 2010,
    date > election2015 & date < election2017 ~ 2015, 
    date >= election2017 & date < election2019 ~ 2017, 
    date >= election2019 ~ 2019
  )) %>% 
  # clean up names for merging 
  mutate(speaker = case_when(
    speaker == "Andrew Love" ~ "Andy Love",
    speaker == "Andrew Slaughter" ~ "Andy Slaughter",
    speaker == "Andy Burnham" ~ "Andy Burnham",
    speaker == "Annette L Brooke" ~ "Annette Brooke",
    speaker == "Brian H Donohoe" ~ "Brian Donohoe",
    speaker == "Chi Onwurah" ~ "Chinyelu Onwurah",
    speaker == "Christopher Hunhe" ~ "Chris Hunhe",
    speaker == "Daniel Poulter" ~ "Dan Poulter",
    speaker == "David Anderson" ~ "Dave Anderson",
    speaker == "Diana R Johnson" ~ "Diana Johnson",
    speaker == "Diana R. Johnson" ~ "Diana Johnson",
    speaker == "Edward Balls" ~ "Ed Balls",
    speaker == "Edward Davey" ~ "Ed Davey",
    speaker == "Gareth R Thomas" ~ "Gareth Thomas",
    speaker == "George Young" ~ "George Young, Bt",
    speaker == "Grahame M Morris" ~ "Grahame Morris",
    speaker == "Gregory Barker" ~ "Greg Barker",
    speaker == "Ian Paisley" ~ "Ian Paisley, Jr.",
    speaker == "James Paice" ~ "Jim Paice",
    speaker == "Jeffrey M Donaldson" | speaker == "Jeffrey M. Donaldson" ~ "Jeffrey Donaldson",
    speaker == "Jennifer Willott" ~ "Jenny Willott",
    speaker == "Jim Sheridan" ~ "James Sheridan",
    speaker == "John Martin McDonnell" ~ "John McDonnell",
    speaker == "Matthew Hancock" ~ "Matt Hancock",
    speaker == "Michael Weir" ~ "Mike Weir",
    speaker == "Nicholas Dakin" ~ "Nic Dakin",
    speaker == "Nick De Bois" ~ "Nick de Bois",
    speaker == "Robert Smith" ~ "Robert Smith, Bt",
    speaker == "Robert Walter" ~ "Bob Walter",
    speaker == "Sian C James" ~ "Sian James",
    speaker == "Stephen Barclay" ~ "Steve Barclay",
    speaker == "Stephen Gilbert" ~ "Steve Gilbert",
    speaker == "Steve Pound" ~ "Stephen Pound",
    speaker == "Vincent Cable" ~ "Vince Cable",
    speaker == "William Bain" ~ "Willie Bain",
    TRUE ~ speaker
  ))

df2001 <- df %>% 
  filter(parliament == 2001) %>% 
  mutate(speaker = case_when(
    speaker == "Christopher Huhne" ~ "Chris Huhne",
    speaker == "Andrew MacKinlay" ~ "Andrew Mackinlay",
    speaker == "Bob Walter" ~ "Robert Walter",
    speaker == "George Young, Bt" ~ "George Young",
    speaker == "Rob Flello" ~ "Robert Flello",
    speaker == "Jim Paice" ~ "James Paice",
    speaker == "Vince Cable" ~ "Vincent Cable",
    speaker == "Steve McCabe" ~ "Stephen McCabe",
    speaker == "Pete Wishart" ~ "Peter Wishart",
    str_detect(speaker, " James") == TRUE ~ "Sian James",
    speaker == "Chris McCafferty" ~ "Christine McCafferty",
    speaker == "Greg Barker" ~ "Gregory Barker",
    speaker == "Dave Anderson" ~ "David Anderson",
    speaker == "Jonathan R Shaw" ~ "Jonathan Shaw",
    speaker == "Mike Weir" ~ "Michael Weir",
    speaker == "Michael Jabez Foster" ~ "Michael Foster",
    speaker == "Ed Davey" ~ "Edward Davey",
    speaker == "Hermon" ~ "Lady Sylvia Hermon",
    speaker == "Doug Naysmith" ~ "Douglas Naysmith",
    speaker == "Desmond Turner" ~ "Des Turner",
    speaker == "Anthony D Wright" ~ "Tony Wright",
    speaker == "Piara S Khabra" ~ "Piara Khabra",
    speaker == "Robrt Smith, Bt" ~ "Robert Smith,",
    speaker == "Robert Wareing" ~ "Bob Wareing",
    speaker == "Brady" ~ "Graham Brady",
    speaker == "Heath" ~ "David Heath",
    speaker == "Garnier" ~ "Edward Garnier",
    speaker == "Angela E Smith" ~ "Angela Smith",
    speaker == "Howard" ~ "Michael Howard",
    speaker == "Bailey" ~ "Adrian Bailey",
    speaker == "Benyon" ~ "Richard Benyon",
    speaker == "Butler" ~ "Dawn Butler",
    speaker == "McCartney" ~ "Ian McCartney",
    speaker == "Wallace" ~ "Ben Wallace",
    speaker == "Vara" ~ "Shailesh Vara",
    speaker == "Carmichael" ~ "Alistair Carmichael",
    speaker == "Dave Watts" ~ "David Watts",
    speaker == "Roy" ~ "Frank Roy ",
    speaker == "Cryer" ~ "Ann Cryer",
    speaker == "McGovern" ~ "Jim McGovern",
    speaker == "Henderson" ~ "Doug Henderson",
    speaker == "Evans" ~ "Nigel Evans",
    speaker == "Cox" ~ "Geoffrey Cox",
    speaker == "Walker" ~ "Charles Walker",
    speaker == "Marsden" ~ "Gordon Marsden",
    speaker == "Gummer" ~ "John Gummer",
    speaker == "Hancock" ~ "Mike Hancock",
    speaker == "Dodds" ~ "Nigel Dodds",
    speaker == "Hunt" ~ "Jeremy Hunt", 
    TRUE ~ speaker)) %>% 
  mutate(speaker = case_when(
    speaker == "George Young" ~ "George Young, 6th Baronet",
    speaker == "Ian Paisley, Jr." ~ "The Rev. Ian Paisley",
    speaker == "Peter Mandelson" ~ "Peter Mandelson (resigned)",
    speaker == "Peter L Pike" ~ "Peter Pike",
    speaker == "Andrew Mackinlay" ~ "Andrew MacKinlay",
    speaker == "Andrew F Bennett" ~ "Andrew Bennett",
    speaker == "Martin Smyth" ~ "The Rev. Martin Smyth",
    speaker == "Robert Smith, Bt" ~ "Robert Smith",
    speaker == "Sylvia Eileen Paisley" ~ "Sylvia Hermon",
    speaker == "Bill Cash" ~ "William Cash",
    speaker == "Alan Beith" ~ "Alan James Beith",
    speaker == "Stephen McCabe" ~ "Steve McCabe",
    speaker == "Douglas Naysmith" ~ "Doug Naysmith",
    speaker == "Wiggin" ~ "Bill Wiggin",
    speaker == "Hopkins" ~ "Kelvin Hopkins",
    speaker == "Marshall" ~ "Jim Marshall (deceased)",
    speaker == "Davis" ~ "Terry Davis (resigned)",
    speaker == "Lady Sylvia Hermon" ~ "Sylvia Hermon",
    speaker == "Paul Daisley" ~ "Paul Daisley (deceased)",
    speaker == "Pollard" ~ "Kerry Pollard",
    speaker == "Ross" ~ "Ernie Ross",
    speaker == "Reed" ~ "Andy Reed",
    speaker == "Calum MacDonald" ~ "Calum Macdonald",
    speaker == "Nigel Doddis" ~ "Nigel Dodds",
    speaker == "Banks" ~ "Tony Banks",
    speaker == "Lloyd" ~ "Tony Lloyd",
    speaker == "Brian Donohue" ~ "Brian Donohoe",
    str_detect(speaker, "n Simon") == TRUE ~ "Sion Simon",
    str_detect(speaker, "Lembit") == TRUE ~ "Lembit",
    str_detect(speaker, "Pete Wish") == TRUE ~ "Peter Wishart",
    TRUE ~ speaker 
  )) %>% 
  left_join(mps, by = c('speaker' = "mp", "parliament"))



# df2001 %>% filter(!is.na(speaker) & is.na(constit)) %>% pull(speaker) %>% as_tibble() %>% distinct(value) %>% view()

df2010 <- df %>% 
  filter(parliament == 2010) %>% 
  left_join(mps, by = c("speaker" = "mp",'parliament'))
df2010_namesreformat <- unique(df2010 %>% filter(is.na(constit)) %>% pull(speaker) %>% as_tibble())

df2015 <- df %>% 
  filter(parliament == 2015) %>% 
  mutate(speaker = case_when(
    speaker == "Rebecca Long Bailey" ~ "Rebecca Long-Bailey",
    speaker == "Margaret Hodge" ~ "Margaret, Lady Hodge",
    speaker == "Baker" | speaker == "Steven Baker" ~ "Steve Baker",
    speaker == "Martin Docherty-Hughes" ~ "Martin Docherty",
    speaker == "Liz Saville Roberts" ~ "Liz Saville-Roberts",
    speaker == "Dave Anderson" ~ "David Anderson",
    speaker == "Stuart C McDonald" ~ "Stuart McDonald",
    speaker == "Nusrat Ghani" ~ "Nus Ghani",
    speaker == "Chris Davies" ~ "Christopher Davies",
    speaker == "Angela Watkinson" ~ "Dame Angela Watkinson",
    speaker == "Christian Matheson" ~ "Chris Matheson",
    speaker == "Harriett Baldwin" ~ "Harriet Baldwin",
    speaker == "Dan Poulter" ~ "Daniel Poulter",
    speaker == "Philip Boswell" ~ "Phil Boswell",
    speaker == "Sylvia Eileen Paisley" ~ "Sylvia, Lady Hermon",
    speaker == "Miller" ~ "Maria Miller",
    speaker == "Stuart Blair Donaldson" ~ "Stuart Donaldson",
    speaker == "Matt Hancock" ~ "Matthew Hancock",
    speaker == "Victoria Borwick" ~ "Victoria, Lady Borwick",
    speaker == "Poulter" ~ "Daniel Poulter",
    speaker == "Margaret Beckett" ~ "Dame Margaret Beckett",
    TRUE ~ speaker
  )) %>% left_join(mps, by = c("speaker" = "mp",'parliament'))
df2017 <- df %>% 
  filter(parliament == 2017)%>% 
  mutate(speaker = case_when(
    speaker == "Rebecca Long Bailey" ~ "Rebecca Long-Bailey",
    speaker == "Baker" | speaker == "Steven Baker" ~ "Steve Baker",
    speaker == "Martin Docherty-Hughes" ~ "Martin Docherty",
    speaker == "Liz Saville Roberts" ~ "Liz Saville-Roberts",
    speaker == "Dave Anderson" ~ "David Anderson",
    speaker == "Stuart C McDonald" ~ "Stuart McDonald",
    speaker == "Nusrat Ghani" ~ "Nus Ghani",
    speaker == "Chris Davies" ~ "Christopher Davies",
    speaker == "Angela Watkinson" ~ "Dame Angela Watkinson",
    speaker == "Dan Poulter" ~ "Daniel Poulter",
    speaker == "Philip Boswell" ~ "Phil Boswell",
    speaker == "Sylvia Eileen Paisley" ~ "Lady Sylvia Hermon",
    speaker == "Miller" ~ "Maria Miller",
    speaker == "Stuart Blair Donaldson" ~ "Stuart Donaldson",
    speaker == "Matt Hancock" ~ "Matthew Hancock",
    speaker == "Victoria Borwick" ~ "Victoria, Lady Borwick",
    speaker == "Poulter" ~ "Daniel Poulter",
    speaker == "Ian Paisley, Jr." ~ "Ian Paisley Jr",
    speaker == "Marsha De Cordova" | speaker == "MarshaDeCordova" ~ "Marsha de Cordova",
    speaker == "Angela Smith" ~ "Angela Christine Smith",
    speaker == "Suella Fernandes" ~ "Suella Braverman",
    speaker == "Sylvia, Lady Hermon" ~ "Lady Sylvia Hermon",
    speaker == "Julia Dockerill" ~ "Julia Lopez",
    speaker == "Tanmanjeet Singh Dhesi" ~ "Tan Dhesi",
    speaker == "Preet Kaur Gill" ~ "Preet Gill",
    speaker == "Graham P Jones" ~ "Graham Jones",
    speaker == "Howarth" ~ "George Howarth",
    speaker == "Leslie" ~ "Chris Leslie",
    speaker == "Hunt" ~ "Jeremy Hunt",
    speaker == "Carmichael" ~ "Alistair Carmichael",
    speaker == "Robertson" ~ "Laurence Robertson",
    TRUE ~ speaker
  )) %>% 
  left_join(mps, by = c("speaker" = "mp",'parliament'))
df2019 <- df %>% 
  filter(parliament == 2019)%>% 
  left_join(mps, by = c("speaker" = "mp",'parliament'))


df2005 <- df %>% 
  filter(parliament == 2005) %>% 
  mutate(speaker = case_when(
    speaker == "Ian Paisley, Jr." ~ "The Rev. Ian Paisley",

    speaker == "Christopher Huhne" ~ "Chris Huhne",
    speaker == "Andrew MacKinlay" ~ "Andrew Mackinlay",
    speaker == "Bob Walter" ~ "Robert Walter",
    speaker == "George Young, Bt" ~ "George Young",
    speaker == "Rob Flello" ~ "Robert Flello",
    speaker == "Jim Paice" ~ "James Paice",
    speaker == "Vince Cable" ~ "Vincent Cable",
    speaker == "Steve McCabe" ~ "Stephen McCabe",
    speaker == "Pete Wishart" ~ "Peter Wishart",
    str_detect(speaker, " James") == TRUE ~ "Sian James",
    speaker == "Chris McCafferty" ~ "Christine McCafferty",
    speaker == "Greg Barker" ~ "Gregory Barker",
    speaker == "Dave Anderson" ~ "David Anderson",
    speaker == "Jonathan R Shaw" ~ "Jonathan Shaw",
    speaker == "Mike Weir" ~ "Michael Weir",
    speaker == "Michael Jabez Foster" ~ "Michael Foster",
    speaker == "Ed Davey" ~ "Edward Davey",
    speaker == "Hermon" ~ "Lady Sylvia Hermon",
    speaker == "Doug Naysmith" ~ "Douglas Naysmith",
    speaker == "Desmond Turner" ~ "Des Turner",
    speaker == "Anthony D Wright" ~ "Tony Wright",
    speaker == "Piara S Khabra" ~ "Piara Khabra",
    speaker == "Robrt Smith, Bt" ~ "Robert Smith,",
    speaker == "Robert Wareing" ~ "Bob Wareing a",
    speaker == "Brady" ~ "Graham Brady",
    speaker == "Heath" ~ "David Heath",
    speaker == "Garnier" ~ "Edward Garnier",
    speaker == "Angela E Smith" ~ "Angela Smith",
    speaker == "Howard" ~ "Michael Howard",
    speaker == "Bailey" ~ "Adrian Bailey",
    speaker == "Benyon" ~ "Richard Benyon",
    speaker == "Butler" ~ "Dawn Butler",
    speaker == "McCartney" ~ "Ian McCartney",
    speaker == "Wallace" ~ "Ben Wallace",
    speaker == "Vara" ~ "Shailesh Vara",
    speaker == "Carmichael" ~ "Alistair Carmichael",
    speaker == "Dave Watts" ~ "David Watts ",
    speaker == "Roy" ~ "Frank Roy ",
    speaker == "Cryer" ~ "Ann Cryer",
    speaker == "McGovern" ~ "Jim McGovern",
    speaker == "Henderson" ~ "Doug Henderson",
    speaker == "Evans" ~ "Nigel Evans",
    speaker == "Cox" ~ "Geoffrey Cox",
    speaker == "Walker" ~ "Charles Walker",
    speaker == "Marsden" ~ "Gordon Marsden",
    speaker == "Gummer" ~ "John Gummer",
    speaker == "Hancock" ~ "Mike Hancock",
    speaker == "Dodds" ~ "Nigel Dodds",
    speaker == "Hunt" ~ "Jeremy Hunt",
    TRUE ~ speaker
  )) %>% 
  left_join(mps, by = c("speaker" = "mp","parliament"))

# create corpus function 
create_corpus <- function(data, text_field = "text", 
                          constit_field = "constit"){
  corpus <- corpus(data, text_field = text_field)
  docvars(corpus, "constit") <- data[[constit_field]]
  return(corpus)
}

# function to count place-name mentions
count_mentions <- function(corpus, placenames, df_to_merge,
                                    min_docfreq = 200, min_termfreq = 200){
  
  #text_field = character, name of text column in df
  #corpus = corpus (created above) 
  #placenames = vector of placenames to c ount
  places <- as.list(placenames)
  names(places) <- placenames
  places_dict <- dictionary(places)
  
  #make into a tokens object
  tokens <- tokens(corpus, remove_punct = TRUE,
                   remove_numbers = FALSE,
                   remove_url = TRUE, 
                   remove_symbols = FALSE)
  
  #remove stopwords via tokens_select
  toks_no_stop <- tokens_select(tokens,
                                pattern = stopwords(language="en",source="marimo"),
                                selection = 'remove')
  
  #get number of mentions of places per document in the corpus
  dfm_dict_toks <- dfm(tokens_lookup(toks_no_stop,
                                     places_dict))
  #turn dfm into tibble, then do rowsums
  dict_tibble <- dfm_dict_toks %>%
    as_tibble() %>% 
    dplyr::select(-doc_id)
  
  #place_mentions <- rowSums(dict_tibble)
  
  #docvars(corpus, "place_mentions") <- place_mentions
  
  #get number of place mentions as proportion of all words
  #place_mentions_prop <- place_mentions/total_words
  #docvars(corpus, "place_mentions_prop") <- place_mentions_prop
  
  out <- bind_cols(
    df_to_merge, 
    dict_tibble
  )
  return(out)
  
}



# read in places data
read_places_function <- function(year){
  out <- read_csv(paste("data/uk_geography/places_constits_matched_",
                              as.character(year), ".csv", sep = "")) %>% 
    mutate(name = str_remove(name, " Boro Const"),
           name = str_remove(name, " Burgh Const")) %>% 
    select(location, locality, name)
  return(out)
}

# function to clean place-names data and put in the right format
clean_names_function <- function(places_df, constituency){
  
  constits <- unique(places_df$name)
  
  # get places + coounty name 
  places_not_constit <- places_df %>% 
    filter(name == constituency) %>% 
    # add county name
    add_row(location = places_df %>% 
              filter(name == constituency) %>% 
              pull(locality) %>% unique()) 
  
  # get distinct words in constituency name 
  constit_names <- as_tibble(t(str_split(places_not_constit$name[1], 
                                         " ", simplify = TRUE))) %>% 
    filter(!V1 %in% c("and","And","The","the",
                      "East","North","South",'West')) %>% 
    rename(value = V1)
  
  
  out <- bind_rows(
    places_not_constit %>% select(value = location), 
    constit_names
  ) %>% 
    mutate(constit = constituency) %>% 
    distinct(value, constit)
  return(out)
  
  
}

# run places-cleaning function
run_clean_places <- function(places_df){
  constits <- unique(places_df$name)
  
  places_out <- clean_names_function(places_df, constituency = constits[1])
  for(i in 2:length(constits)){
    print(i)
    out <- clean_names_function(places_df, constits[i])
    places_out <- bind_rows(places_out, out)
  }
  return(places_out)
}


# function to run the constit places mention counting 
run_count_places_in_constit <- function(corpus, constituency, places_data, df_to_merge){
  # subset corpus
  corpus_constit <- corpus %>% corpus_subset(constit == constituency)
  #subset places to those in constituency - this counts constituency & county as well
  places_constit <- places_data$value[places_data$constit == constituency]
  # convert to deal with scotland 
  places_constit <- iconv(as.character(places_constit), to = "ASCII//TRANSLIT")
  if(sum(is.na(places_constit)) == length(places_constit)){
    # skip teh constituency if the places are NA - not sure what's going on here
    i <- i + 1
  } else {
    # subset MP info dataframe to constitunjecy
    df_out <- df_to_merge %>% filter(constit == constituency)
    # run the count mentions function 
    out <- count_mentions(corpus = corpus_constit,
                          df_to_merge = df_out,
                          placenames = places_constit) 
    # summarize count mentions function - get total number of mentions 
    out <- out %>% 
      mutate(place_mentions_in_constit = 
               out %>% 
               select(-(date:constit)) %>% 
               rowSums()) %>% 
      select(date:constit, place_mentions_in_constit)
    
  }
 
  return(out)

}

# #####################################################
## BIG FUNCTION TO RUN ALL OF THIS 
#############################################

run_everything <- function(data, year){
  # make corpus using corpus function 
  corpus <- create_corpus(data = data)
  
  # read places data using read_places function
  places <- read_places_function(year)
  
  # clean places data using clean-placs_data function
  places_clean <- run_clean_places(places) 
  
  # get constituencies from place data 
  constits <- unique(places_clean$constit)
  constits <- constits[!is.na(constits)]
  
  # run function to count placs in constit 
  out <- run_count_places_in_constit(
    corpus = corpus, 
    places_data = places_clean,
    constituency = constits[1],
    df_to_merge = data
  )
  
  for(i in 2:length(constits)){
    print(i)
    out_in <- run_count_places_in_constit(
      corpus = corpus,
      places_data = places_clean,
      df_to_merge = data,
      constit = constits[i]
    )
    out <- bind_rows(out, out_in)
  }
  
  # select relevant columns, get rid of text for size purposes
  out <- out %>% 
    mutate(year = as.numeric(str_split(as.character(out$date), "-",
                                       simplify = TRUE)[,1]),
           month = as.numeric(str_split(as.character(out$date), "-",
                                        simplify = TRUE)[,2]), 
           my = paste(year, month, "01", sep = "-")) %>% 
    group_by(speaker, date, terms) %>% 
    mutate(docn = 1:n()) %>% 
    filter(docn == 1) %>% 
    select(-text, -docn)
  # write to csv 
  write.csv(out, paste("data/uk_geography/places_output/out", year, ".csv", sep = ""))
  print("Done!")

}


##########################################
### RUN FUNCTIONS ON EACH YEAR
######################################

out2001 <- run_everything(data = df2001, year = 2001)
out2005 <- run_everything(data = df2005, year = 2005)

out2010 <- run_everything(data = df2010, year = 2010)
out2015 <- run_everything(data = df2015, year = 2015)
out2017 <- run_everything(data = df2017, year = 2017)



##### 
# NEXT - CLEAN 2017 AND 2019 NAMES , THEN RUN ON THOSE DFS AS WELL
# THEN - GET 2005 CONSTITUENCIES 


