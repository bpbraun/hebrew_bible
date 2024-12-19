library(xml2)
library(tidyverse)
library(janitor)

# Function to parse a single book XML file
parse_book_xml <- function(file_path) {
  book_name <- str_remove(basename(file_path), "\\.xml$")
  doc <- read_xml(file_path)
  
  xml_find_all(doc, "//c") %>%
    map_df(function(chapter) {
      chapter_number <- xml_attr(chapter, "n") %>% as.integer()
      xml_find_all(chapter, ".//v") %>%
        map_df(function(verse) {
          verse_number <- xml_attr(verse, "n") %>% as.integer()
          text <- xml_find_all(verse, ".//w") %>%
            xml_text() %>%
            str_c(collapse = " ")
          tibble(Book = book_name, Chapter = chapter_number, Verse = verse_number, Text = text)
        })
    })
}

# Function to parse DH XML for a single Torah book
parse_dh_xml <- function(dh_file) {
  dh_doc <- read_xml(dh_file)
  
  xml_find_all(dh_doc, "//c") %>%
    map_df(function(chapter) {
      chapter_number <- xml_attr(chapter, "n") %>% as.integer()
      xml_find_all(chapter, ".//v") %>%
        map_df(function(verse) {
          verse_number <- xml_attr(verse, "n") %>% as.integer()
          dh_source <- xml_attr(verse, "s")
          tibble(Chapter = chapter_number, Verse = verse_number, DH_Source = dh_source)
        })
    })
}

# Function to order the dataset by canonical order
order_by_canonical <- function(df, canonical_order) {
  df %>%
    mutate(Book = factor(Book, levels = canonical_order, ordered = TRUE)) %>%
    arrange(Book)
}

# General contractor function
build_tanach_with_dh <- function(books_folder, dh_folder, canonical_order, torah_books) {
  # Parse all books
  book_files <- list.files(books_folder, pattern = "\\.xml$", full.names = TRUE)
  tanach_df <- map_df(book_files, parse_book_xml)
  
  # Parse all DH files
  dh_files <- list.files(dh_folder, pattern = "\\.xml$", full.names = TRUE)
  all_dh_data <- map_df(dh_files, function(dh_file) {
    book_name <- str_remove(basename(dh_file), "\\.DH\\.xml$")
    parse_dh_xml(dh_file) %>%
      mutate(Book = book_name)
  })
  
  # Order tanach by canonical order
  tanach_df <- order_by_canonical(tanach_df, canonical_order)
  
  # Attach DH data to the Torah books
  tanach_with_dh <- tanach_df %>%
    left_join(all_dh_data, by = c("Book", "Chapter", "Verse"))
  
  return(tanach_with_dh)
}

add_section_label <- function(tanach_df){
  tanach_df %>% 
    mutate(section = case_when(
      Book %in% torah_books ~ "Torah",
      Book %in% prophets_books ~ "Trophets",
      Book %in% writings_books ~ "Writings"
      ))
}

remove_markings <- function(tanach_df) {
  tanach_df %>% 
    mutate(text_without_marks = str_remove_all(Text, "[\u0591-\u05C7\u05BE\u05F3\u05F4]"))
}

# Canonical order of books
canonical_order <- c(
  "Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy",
  "Joshua", "Judges", "Samuel_1", "Samuel_2",
  "Kings_1", "Kings_2", "Isaiah", "Jeremiah", "Ezekiel",
  "Hosea", "Joel", "Amos", "Obadiah", "Jonah", "Micah",
  "Nahum", "Habakkuk", "Zephaniah", "Haggai", "Zechariah", "Malachi",
  "Psalms", "Proverbs", "Job", "Song_of_Songs", "Ruth",
  "Lamentations", "Ecclesiastes", "Esther", "Daniel",
  "Ezra", "Nehemiah", "Chronicles_1", "Chronicles_2"
)

# Define Torah books
torah_books <- c("Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy")
prophets_books <- c("Joshua", "Judges", "Samuel_1", "Samuel_2",
                    "Kings_1", "Kings_2", "Isaiah", "Jeremiah", "Ezekiel",
                    "Hosea", "Joel", "Amos", "Obadiah", "Jonah", "Micah",
                    "Nahum", "Habakkuk", "Zephaniah", "Haggai", "Zechariah", "Malachi")
writings_books <- c("Psalms", "Proverbs", "Job", "Song_of_Songs", "Ruth",
                    "Lamentations", "Ecclesiastes", "Esther", "Daniel",
                    "Ezra", "Nehemiah", "Chronicles_1", "Chronicles_2")

# Build the full dataset
books_folder <- "books"
dh_folder <- "dh_files"
tanach_with_dh <- build_tanach_with_dh(books_folder, dh_folder, canonical_order, torah_books)

tanach_df <- tanach_with_dh %>%
  add_section_label() %>% 
  remove_markings() %>%
  clean_names() %>%
  select(section, book, chapter, verse, text, text_without_marks, dh_source)

# Inspect the final dataset
write_rds(tanach_df, "processed_data/tidy_tanach.rds")
