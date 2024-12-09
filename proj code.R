setwd("~/Slavic 4530 - Hacking Lit/corpus_files_eng")
text_v <- scan("anna karenina_eng_garnett.txt", what = "character", sep = "\n") #insert txt file here
length(text_v)
#start_v <- which(text_v == "Mon Portrait") #make sure to update first line. copy and paste to avoid typos.
novel_lines_v <- text_v[1:length(text_v)] 
novel_v <- paste(novel_lines_v, collapse = " ") 
novel_lower_v <- tolower(novel_v)
one_words_l <- strsplit(novel_lower_v, "\\W")
one_word_v <- unlist(one_words_l)
not_blanks_v <- which(one_word_v != "")
one_word_v <- one_word_v[not_blanks_v]
one_freqs_t <- table(one_word_v)
sorted_one_freqs_t <- sort(one_freqs_t, decreasing=TRUE)
sorted_one_rel_freqs_t <- 100 * ( sorted_one_freqs_t/sum(sorted_one_freqs_t) )
plot(sorted_one_rel_freqs_t[1:10],
     main = "Text One",
     type = "b",
     xlab = "Top Ten Words",
     ylab = "Percentage",
     xaxt = "n")
axis(1,1:10, labels = names(sorted_one_rel_freqs_t[1:10]))
text_v <- scan("war and peace_eng_garnett.txt", what = "character", sep = "\n") #insert correct txt file here
#start_v <- which(text_v == "CRIME AND PUNISHMENT") #make sure to update first line. copy and paste to avoid typos.
length(text_v)
novel_lines_v <- text_v[1:length(text_v)] 
novel_v <- paste(novel_lines_v, collapse=" ")
novel_lower_v <- tolower(novel_v)
two_words_l <- strsplit(novel_lower_v, "\\W")
two_word_v <- unlist(two_words_l)
not_blanks_v <- which(two_word_v != "")
two_word_v <- two_word_v[not_blanks_v]
two_freqs_t <- table(two_word_v)
sorted_two_freqs_t <- sort(two_freqs_t , decreasing=TRUE) 
sorted_two_rel_freqs_t <- 100 * (
  sorted_two_freqs_t/sum(sorted_two_freqs_t)
)
plot(sorted_two_rel_freqs_t[1:10],
     main = "Text Two",
     type = "b",
     xlab = "Top Ten Words",
     ylab = "Percentage",
     xaxt = "n")
axis(1,1:10, labels = names(sorted_two_rel_freqs_t[1:10]))
names(sorted_two_rel_freqs_t[
  which(names(sorted_two_rel_freqs_t[1:200])
        %in% names(sorted_one_rel_freqs_t[1:200]))])

library(stylo)
setwd("~/Slavic 4530 - Hacking Lit")
raw.corpus <- load.corpus(files = "all", corpus.dir = "corpus_files_ru", encoding = "UTF-8")
tokenized.corpus <- txt.to.words.ext(raw.corpus, preserve.case = FALSE, corpus.lang = "Russian.all")
summary(tokenized.corpus)
russian_pronouns <- c(
  # Personal pronouns (first, second, third person)
  "я", "меня", "мне", "меня", "мной", "мною", "обо мне",
  "мы", "нас", "нам", "нас", "нами", "о нас",
  "ты", "тебя", "тебе", "тебя", "тобой", "тобою", "о тебе",
  "вы", "вас", "вам", "вас", "вами", "о вас",
  "он", "его", "ему", "его", "им", "о нём",
  "она", "её", "ей", "её", "ею", "о ней",
  "оно", "его", "ему", "его", "им", "о нём",
  "они", "их", "им", "их", "ими", "о них",
  
  # Possessive pronouns (first and second person, including plural forms)
  "мой", "моя", "моё", "мои",  # Nominative
  "моего", "моей", "моего", "моих",  # Genitive
  "моему", "моей", "моему", "моим",  # Dative
  "моего", "мою", "моё", "мои",  # Accusative
  "моим", "моей", "моим", "моими",  # Instrumental
  "о моём", "о моей", "о моём", "о моих",  # Prepositional
  
  "наш", "наша", "наше", "наши",  # Nominative
  "нашего", "нашей", "нашего", "наших",  # Genitive
  "нашему", "нашей", "нашему", "нашим",  # Dative
  "нашего", "нашу", "наше", "наши",  # Accusative
  "нашим", "нашей", "нашим", "нашими",  # Instrumental
  "о нашем", "о нашей", "о нашем", "о наших",  # Prepositional
  
  "твой", "твоя", "твоё", "твои",  # Nominative
  "твоего", "твоей", "твоего", "твоих",  # Genitive
  "твоему", "твоей", "твоему", "твоим",  # Dative
  "твоего", "твою", "твоё", "твои",  # Accusative
  "твоим", "твоей", "твоим", "твоими",  # Instrumental
  "о твоём", "о твоей", "о твоём", "о твоих",  # Prepositional
  
  "ваш", "ваша", "ваше", "ваши",  # Nominative
  "вашего", "вашей", "вашего", "ваших",  # Genitive
  "вашему", "вашей", "вашему", "вашим",  # Dative
  "вашего", "вашу", "ваше", "ваши",  # Accusative
  "вашим", "вашей", "вашим", "вашими",  # Instrumental
  "о вашем", "о вашей", "о вашем", "о ваших",  # Prepositional
  
  # Third person possessive (invariable)
  "его", "её", "их",
  
  # Reflexive possessive pronouns (свой, своя, своё, свои)
  "свой", "своя", "своё", "свои",  # Nominative
  "своего", "своей", "своего", "своих",  # Genitive
  "своему", "своей", "своему", "своим",  # Dative
  "своего", "свою", "своё", "свои",  # Accusative
  "своим", "своей", "своим", "своими",  # Instrumental
  "о своём", "о своей", "о своём", "о своих",  # Prepositional
  
  # Reflexive pronouns
  "себя", "себе", "собой", "о себе"
)
corpus.no.pronouns <- delete.stop.words(tokenized.corpus, stop.words = russian_pronouns)
sliced.corpus <- make.samples(corpus.no.pronouns, sampling = "normal.sampling")
frequent.features <- make.frequency.list(sliced.corpus, head = 3000)
freqs <- make.table.of.frequencies(sliced.corpus, features = frequent.features)
head(freqs) 
culled.freqs <- perform.culling(freqs, culling.level = 80)

#First data visualization: dendrogram
pdf("output_dendrogram.pdf", width = 12, height = 16)  # Adjust width and height as necessary
stylo(frequencies = culled.freqs, gui = FALSE)
dev.off()  # Close the PDF device to save the file

# Second data visualization: correlation matrix
pdf("output_plot.pdf", width = 10, height = 10)  # Adjust width and height as needed
stylo(frequencies = culled.freqs, analysis.type = "PCR",
      custom.graph.title = "Russian Novels", 
      gui = FALSE)
dev.off()

library(stylo)
setwd("~/Slavic 4530 - Hacking Lit")
raw.corpus <- load.corpus(files = "all", corpus.dir = "corpus_files_eng", encoding = "UTF-8")
tokenized.corpus <- txt.to.words.ext(raw.corpus, preserve.case = FALSE, corpus.lang = "English.all")
summary(tokenized.corpus)
stop_words <- stylo.pronouns(corpus.lang = "English")
corpus.no.pronouns <- delete.stop.words(tokenized.corpus, stop.words = stop_words)
sliced.corpus <- make.samples(corpus.no.pronouns, sampling = "normal.sampling")
frequent.features <- make.frequency.list(sliced.corpus, head = 3000)
freqs <- make.table.of.frequencies(sliced.corpus, features = frequent.features)
head(freqs) 
culled.freqs <- perform.culling(freqs, culling.level = 80)

#First data visualization: dendrogram
pdf("output_dendrogram.pdf", width = 12, height = 16)# Adjust width and height as necessary
stylo(frequencies = culled.freqs, gui = FALSE)
dev.off()  # Close the PDF device to save the file

# Second data visualization: correlation matrix
pdf("output_plot.pdf", width = 10, height = 10)  # Adjust width and height as needed
stylo(frequencies = culled.freqs, analysis.type = "PCR",
      custom.graph.title = "Russian Novels", 
      gui = FALSE)
dev.off()

setwd("~/Slavic 4530 - Hacking Lit/corpus_files_ru")
frank_v <- scan("anna karenina_ru_tolstoy.txt", 
                what = "character",
                sep = "\n"
)
frank_v
frank_lower_v <- tolower(frank_v)
frank_word_l <- strsplit(frank_lower_v, "\\W")
frank_word_v <- unlist(frank_word_l)
not_blanks_v <- which(frank_word_v != "")
frank_word_v <- frank_word_v[not_blanks_v]
frank_length_v <- length(frank_word_v)
frank_freqs_t <- table(frank_word_v)
sorted_frank_freqs_t <- sort(frank_freqs_t, decreasing = TRUE)
sorted_frank_freqs_t[1:100]
sorted_frank_rel_freqs_t <- 100*(sorted_frank_freqs_t/frank_length_v)
plot(
  sorted_frank_rel_freqs_t[1:100], type = "b",
  xlab = "Top 50 Words in Anna Karenina Russian",
  ylab = "Percentage of Full Text",
  xaxt = "n"
)
axis(
  1, 1:100,
  labels = names(sorted_frank_rel_freqs_t [1:100])
)
n_time_v <- seq(from = 1, to = length(frank_word_v))
life_v <- which(frank_word_v == "eyes")
l_count_v <- rep(NA, times = length(n_time_v))
l_count_v[life_v] <- 1
plot(
  l_count_v,
  main = "Dispersion Plot of 'eyes' in Anna Karenina English",
  xlab = "Novel Time",
  ylab = "eyes",
  type = "h",
  ylim = c(0, 1), yaxt = 'n'
)
n_time_v <- seq(from = 1, to = length(frank_word_v))
life_v <- which(frank_word_v == "глаза")
l_count_v <- rep(NA, times = length(n_time_v))
l_count_v[life_v] <- 1
plot(
  l_count_v,
  main = "Dispersion Plot of 'глаза' in Anna Karenina Russian",
  xlab = "Novel Time",
  ylab = "глаза",
  type = "h",
  ylim = c(0, 1), yaxt = 'n'
)
