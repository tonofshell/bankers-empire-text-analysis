*Bankers and Empire* Text Analysis
================

Loading and Cleaning
--------------------

``` r
#load the book from text file
book_text = here("bankers_and_empire.txt") %>% 
  read_file() %>%
  tibble(txt = .) %>%
  unnest_tokens(line, txt, token = "lines")

#remove page numbers
temp = tibble(line = character())
for (line_index in 1:length(book_text[[1]])) {
  if (!str_detect(book_text[line_index,], "/") && !(str_length(book_text[line_index,]) < 50)) {
    temp = bind_rows(temp, book_text[line_index,])
  }
}
book_text = temp %>% unnest_tokens(word, line, token = "words")
rm(temp)

#remove possessive "'s" from words
for (line_index in 1:length(book_text[[1]])) {
  if (str_detect(book_text[line_index,], "’s") || str_detect(book_text[line_index,], "'s")) {
    book_text[line_index,] = str_sub(book_text[line_index,], 
                                     start = 1, end = (str_length(book_text[line_index,]) - 2))
  }
}
```

Wordcloud
---------

``` r
book_text %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 150, colors = brewer.pal(8, "Set2")))
```

![](README_files/figure-markdown_github/wordcloud-1.png)

Prevalence of Architecture
--------------------------

Flesch-Kincaid Grade Level
--------------------------

``` r
#flesch.kincaid(here("bankers_and_empire.txt"), force.lang = "en")
```

N-Gram
------

Sentiment Analysis
------------------

### In General

### Across the Book
