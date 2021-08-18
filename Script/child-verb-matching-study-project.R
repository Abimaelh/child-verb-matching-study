# Child verb matching study code

#load the library
library(childesr)
library(ggplot2)
library(tidyverse)
library(tidyboot)
library(magrittr)
library(tidytext)
library(plyr)
library(dplyr)
library(ddply)
library(scales)

#setwd("C:/Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children")
getwd()

#importing the symmetricals table
#sym_list <- read.csv(file = "~\\GitHub\\child-verb-matching-study\\Data\\sym_list3.csv", header = TRUE)
sym_list <- read.csv(file = "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Data/sym_list3.csv", header = TRUE)

sym_list$unique_stems <- NULL

#how many unique sym stems do we have?
length(unique(sym_list$stem)) #25 unique stems. including nouns, etc.

#childesdb search of symmetricals for 3 year olds.
three_year_olds_tokens_sym_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = sym_list$form
)
#checking how many children came up in the search.
length(unique(three_year_olds_tokens_sym_df$target_child_id))#55

#trim the database by selecting the columns we are interested in
three_sym_df_trimmed <- select(three_year_olds_tokens_sym_df, 'target_child_id', 'corpus_name', 'target_child_age',
                               'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')

#renaming columns
names(three_sym_df_trimmed)[names(three_sym_df_trimmed) == "part_of_speech"] <- "pos"
names(three_sym_df_trimmed)[names(three_sym_df_trimmed) == "gloss"] <- "form"
three_sym_df_trimmed

#filter the df by part (past participle??) and v (verb)
three_sym_filtered_pos_df <- three_sym_df_trimmed %>% filter(pos == 'v' | pos == 'part')

unique(three_sym_filtered_pos_df$stem)
length(unique(three_sym_filtered_pos_df$stem))

length(three_sym_filtered_pos_df$target_child_id) #362
#now we want to know what words don't appear in the db
three_sym_not_in_db <- sym_list %>% filter(!form %in% three_sym_filtered_pos_df$form)

write.csv(three_sym_not_in_db, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/three_sym_not_in_db.csv")

length(three_sym_filtered_pos_df$target_child_id) #362
length(unique(three_sym_filtered_pos_df$target_child_id)) #33

#making df of unique ids in three year olds
ids_for_threes_sym <- as.data.frame(unique(three_sym_filtered_pos_df$target_child_id))
names(ids_for_threes_sym)[names(ids_for_threes_sym) == "unique(three_sym_filtered_pos_df$target_child_id)"] <- "target_child_id"
length(unique(ids_for_threes_sym$target_child_id)) #33 unique ids

length(sym_list$stem)
# generate 90 (because our sym_list has 90 rows) instances of an ID (n # of participants x f # of forms )
# we are trying to create a dataframe that has 2,970 rows (33 * 90)
three_many_ids_sym <- ids_for_threes_sym %>% slice(rep(1:n(), each = 90))
length(three_many_ids_sym$target_child_id) #2970
length(unique(sym_list$stem))#25
sym_list

#generate 33 instances of each symmetrical - because we want the ids and sym words to have the same # of rows before
#we merge them together. #33 is the number of participants we have.
n = 33
threes_many_syms <- do.call("rbind", replicate(n, sym_list, simplify = FALSE))
length(threes_many_syms$form) #2970

# Merge many IDS with many_syms
three_sym_and_id <- cbind(threes_many_syms, target_child_id = three_many_ids_sym$target_child_id) #2970
length(three_sym_and_id$target_child_id) #2970

#the number of participants differ from the original code "child-production-of-symmetricals"
#because we searched for nouns and adjectives in that code. Here we focus on verbs. 
#So the total number of children searched between 3 and 4 should be the same.

#replacing age with 3
three_sym_filtered_pos_df$target_child_age <-  replace(three_sym_filtered_pos_df$target_child_age,
                                                       three_sym_filtered_pos_df$target_child_age >= 36.00 &
                                                         three_sym_filtered_pos_df$target_child_age <= 47.99, 3)
length(three_sym_filtered_pos_df$target_child_id)#362
#you can use three_sym_filtered_pos_df to extract sentence frames for three year olds Using utterance_id
#saving

# We are doing this to get a count for the words that are not~ recorded. So that when we merge with
# the words from sym_list all the words not produced, receive an NA, which we later change to 0.
detach(package:plyr)
three_counts_sym <- three_sym_filtered_pos_df %>% group_by(form, target_child_id, target_child_age, target_child_sex,corpus_name,utterance_id) %>%
  summarize(count = sum(unique(length(form)))) #349 rows
#checking we still have the same amount of children.
length(unique(three_counts_sym$target_child_id)) #33

#this could be useful later on.
three_counts_pasted_targetid_uttid_stem_sym <- as.data.frame(paste(three_sym_filtered_pos_df$target_child_id, three_sym_filtered_pos_df$stem, three_sym_filtered_pos_df$utterance_id))
names(three_counts_pasted_targetid_uttid_stem_sym)[names(three_counts_pasted_targetid_uttid_stem_sym) == "paste(three_sym_filtered_pos_df$target_child_id, three_sym_filtered_pos_df$stem, three_sym_filtered_pos_df$utterance_id)"] <- "wordstem"
three_counts_pasted_targetid_uttid_stem_separate_sym <- three_counts_pasted_targetid_uttid_stem_sym %>% separate(wordstem, c("target_child_id", "stem", "utterance_id"))
length(unique(three_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id)) #33
unique(three_counts_pasted_targetid_uttid_stem_separate_sym$stem) #ADDED ALL THE SYMS NOW
three_counts_pasted_targetid_uttid_stem_separate_sym <- three_counts_pasted_targetid_uttid_stem_separate_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate')
length(unique(three_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id)) #31
length(three_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id) #310 #this number may change depending on if there are any errors in the db or filtering process.
#we dont really use this so we're not going to save it for now.
#write.csv(three_counts_pasted_targetid_uttid_stem_separate, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\three_counts_pasted_targetid_uttid_stem_separate.csv")
#arranging by target_id helps when merging columns.
three_counts_sym <- three_counts_sym %>% arrange(target_child_id)
length(unique(three_counts_sym$target_child_id)) #33

length(three_counts_sym$form) #349, because this only includes counts for words that were produced by the child.Can we use this to plot data for people that have more than one count?
length(three_sym_and_id$form) #2970

three_full_sym <- merge(three_counts_sym, three_sym_and_id, all = TRUE)
three_full_sym <- three_full_sym %>% arrange(target_child_id)
#we dont really need sex, corpus, and utt id here.
three_full_sym$target_child_sex <-NULL
three_full_sym$corpus_name <- NULL
three_full_sym$utterance_id <- NULL

length(unique(three_full_sym$form)) #90
length(unique(three_full_sym$target_child_id)) #33
length(three_full_sym$form) #3172

#now go in and change NAs for age to 3 and NAs for count to 0
three_full_sym$count[is.na(three_full_sym$count)] <- 0
three_full_sym$target_child_age[is.na(three_full_sym$target_child_age)] <- 3
sum(three_full_sym$count) #362 matches with three_sym_filtered_pos_df - this df doesn't count tokens, so we count the rows. 1 row = one token.

#we select our ALL symmetricals here. 
filter_by_stem_sym <- three_full_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate')
length(filter_by_stem_sym$target_child_id) #1760 rows
length(unique(filter_by_stem_sym$target_child_id)) #33

#collapsed by stem + the number of counts for each stem
three_child_sum_sym <- aggregate(filter_by_stem_sym$count, by=list(filter_by_stem_sym$stem), sum)

#every stem and their count for each child.
three_child_all_stems_per_child_sym <- filter_by_stem_sym %>% group_by(target_child_id,stem) %>%
  summarize(tokens = sum(count))

#test2 <- filter_by_stem %>% group_by(stem) %>%
#summarize(num_chi = )


#only_attach <- test %>% filter(stem == 'attach')
#checking <- as.data.frame(only_attach$target_child_id[only_attach$tokens > 0])
#table(checking) #this works but how to scale it up?

three_child_all_stems_per_child_no_zeros_sym <- three_child_all_stems_per_child_sym

three_child_all_stems_per_child_no_zeros_sym <- three_child_all_stems_per_child_no_zeros_sym %>% filter(tokens != 0)
length(unique(three_child_all_stems_per_child_no_zeros_sym$target_child_id)) #31
#eliminating the zeros worked!
verb_pairs_for_three_child_sheet_sym <- three_child_all_stems_per_child_no_zeros_sym %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))
write.csv(verb_pairs_for_three_child_sheet_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/verb_pairs_for_three_child_sheet_sym.csv")

length(unique(three_child_all_stems_per_child_no_zeros_sym$target_child_id)) #31

#Extracting corpus information so we can exclude atypical children
exclusion_three_info_sym <- three_sym_df_trimmed %>% filter(utterance_id %in% three_counts_pasted_targetid_uttid_stem_separate_sym$utterance_id) 
length(unique(exclusion_three_info_sym$stem)) #14 out of how many that we search for?
unique(exclusion_three_info_sym$stem) #nouns and empty stems
exclusion_three_info_sym_final <- exclusion_three_info_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate')
length(unique(exclusion_three_info_sym_final$stem)) #12
#good this is the one we need to pull frames!
length(exclusion_three_info_sym_final$target_child_id) #314
length(unique(exclusion_three_info_sym_final$target_child_id)) #31

exclusion_three_info_sym_final$target_child_id %in% three_child_all_stems_per_child_no_zeros_sym$target_child_id
exclusion_three_info_sym_final <- exclusion_three_info_sym_final %>% arrange(target_child_id)
exclusion_three_info_sym_final <- exclusion_three_info_sym_final %>% filter(pos == 'v' | pos == 'part')
length(exclusion_three_info_sym_final$target_child_id) #310 should be the same when we add the sum of counts for no zeros three.
sum(three_child_all_stems_per_child_no_zeros_sym$tokens) #310 Yup they match.
length(unique(exclusion_three_info_sym_final$target_child_id))#31

write.csv(exclusion_three_info_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/exclusion_three_info_sym_final.csv")
#check the corpra in this df
unique(exclusion_three_info_sym_final$corpus_name)

#tokens per corpus
corpra_tokens_three_sym <- exclusion_three_info_sym_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = length(corpus_name))
write.csv(corpra_tokens_three_sym,"/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/tokens-from-each-corpus-three-sym.csv")
#do the verb pair tokens add up to the number of tokens in the corpora?
  #verb pair = 310 and the corpora tokens = 310
sum(verb_pairs_for_three_child_sheet_sym$tokens)
sum(corpra_tokens_three_sym$tokens)

#number of children in each corpus
num_of_children_in_each_corpus_three_sym <- exclusion_three_info_sym_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = length(unique(target_child_id)))
write.csv(num_of_children_in_each_corpus_three_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/num-of-children-in-each-corpus-three-sym.csv")

#token frequency per child to check for outliers
token_freq_per_child_three_sym <- exclusion_three_info_sym_final %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = (length(stem)), corpus_name = (corpus_name))
token_freq_per_child_three_sym_final <- unique(token_freq_per_child_three_sym)
write.csv(token_freq_per_child_three_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/token-freq-per-child-three-sym-final.csv")

#checking one participant, all good.
get_participants(
  collection = "Eng-NA",
  corpus = "Demetras1",
  age = 39
)

check_prov <- get_participants(
  collection = "Eng-NA",
  corpus = "Providence",
)

## ** ** ** ** Child production of Symmetricals 4 year olds ** ** ** ** ##

#now to get a clean token df for 4 year olds only.
four_year_olds_tokens_sym_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(48, 60),
  token = sym_list$form
)

length(unique(four_year_olds_tokens_sym_df$target_child_id)) #63 (including some from the 3 year old db)

four_sym_df_trimmed <- select(four_year_olds_tokens_sym_df, 'target_child_id', 'corpus_name', 'target_child_age',
                              'gloss', 'part_of_speech','target_child_sex','stem','utterance_id')

#renaming columns
names(four_sym_df_trimmed)[names(four_sym_df_trimmed) == "part_of_speech"] <- "pos"
names(four_sym_df_trimmed)[names(four_sym_df_trimmed) == "gloss"] <- "form"

#filter the df by part (past participle) and v (verb)
four_sym_filtered_pos_df <- four_sym_df_trimmed %>% filter(pos == 'v' | pos == 'part')
length(four_sym_filtered_pos_df$target_child_id) #504
#now we want to know what words don't appear in the db
four_sym_not_in_db <- sym_list %>% filter(!form %in% four_sym_filtered_pos_df$form)
write.csv(four_sym_not_in_db, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/four_sym_not_in_db.csv")

length(four_sym_filtered_pos_df$target_child_id) #504
length(unique(four_sym_filtered_pos_df$target_child_id)) #55
# brb #

#making df of unique ids in four year olds
ids_for_fours_sym <- as.data.frame(unique(four_sym_filtered_pos_df$target_child_id))
names(ids_for_fours_sym)[names(ids_for_fours_sym) == "unique(four_sym_filtered_pos_df$target_child_id)"] <- "target_child_id"
length(unique(ids_for_fours_sym$target_child_id)) #55 unique ids with duplicate children from the 3 year old db

#storing the 3 year olds in the four year old db in repeats.
repeats_sym <- ids_for_fours_sym %>% filter(target_child_id %in% ids_for_threes_sym$target_child_id)
length(unique(repeats_sym$target_child_id)) #14

#removing the 3 year olds from the 4 year old db.
#ids_for_fours_no_rep_sym <- ids_for_fours_sym %>% filter(!target_child_id %in% ids_for_threes_sym$target_child_id)
#length(unique(ids_for_fours_no_rep_sym$target_child_id)) #41

#checking if they are gone!
#repeats_sym$target_child_id %in% ids_for_fours_no_rep_sym$target_child_id

#generate 90 instances of an ID
length(sym_list$form)
four_many_ids_sym <- ids_for_fours_sym %>% slice(rep(1:n(), each = 90))
#four_many_ids_sym <- ids_for_fours_no_rep_sym %>% slice(rep(1:n(), each = 90))
length(four_many_ids_sym$target_child_id) #4950 (55 children * 90)

#generate 55 (for each unique id) instances of sym_words #should be 40 but i think this (the extra rows) gets filtered out later. 
length(unique(four_many_ids_sym$target_child_id))
n = 55
four_many_syms <- do.call("rbind", replicate(n, sym_list, simplify = FALSE))
length(four_many_syms$form) #4950

# Merge many IDS with many syms
four_sym_and_id <- cbind(four_many_syms, target_child_id = four_many_ids_sym$target_child_id)
length(four_sym_and_id$target_child_id) #4950

#replacing age with 4

four_sym_filtered_pos_df$target_child_age <-  replace(four_sym_filtered_pos_df$target_child_age,
                                                      four_sym_filtered_pos_df$target_child_age >= 48.00 &
                                                        four_sym_filtered_pos_df$target_child_age < 60.00, 4)

length(four_sym_filtered_pos_df$target_child_id) #504
#you can use four_sym_filtered_pos_df to extract sentence frames for three year olds Using utterance_id
#No! Remove the repeats first!

#saving
#save.image("~/GitHub/childesr-corpus-analysis/symmetricals/children/child-production-of-symmetricals-shortened-environment.RData")

#Keep them! This is why we got the same numbers when we reran the code. 
#four_sym_filtered_pos_df <- four_sym_filtered_pos_df %>% filter(!target_child_id %in% ids_for_threes_sym$target_child_id)
length(unique(four_sym_filtered_pos_df$target_child_id)) #55
#now you can use it to extract frames.

#filtering four_sym_filtered_pos_df by our stems of interest. We could do this earlier.
four_sym_filtered_pos_df <- four_sym_filtered_pos_df %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate')

detach(package:plyr)
four_counts_sym <- four_sym_filtered_pos_df %>% group_by(form, target_child_id, target_child_age, target_child_sex,corpus_name,utterance_id) %>%
  summarize(count = sum(unique(length(form)))) #349 rows
#checking we still have the same amount of children.
length(unique(four_counts_sym$target_child_id)) #54

#this could be useful later on.
four_counts_pasted_targetid_uttid_stem_sym <- as.data.frame(paste(four_sym_filtered_pos_df$target_child_id, four_sym_filtered_pos_df$stem, four_sym_filtered_pos_df$utterance_id))
names(four_counts_pasted_targetid_uttid_stem_sym)[names(four_counts_pasted_targetid_uttid_stem_sym) == "paste(four_sym_filtered_pos_df$target_child_id, four_sym_filtered_pos_df$stem, four_sym_filtered_pos_df$utterance_id)"] <- "wordstem"
four_counts_pasted_targetid_uttid_stem_separate_sym <- four_counts_pasted_targetid_uttid_stem_sym %>% separate(wordstem, c("target_child_id", "stem", "utterance_id"))
#four_counts_pasted_targetid_uttid_stem_separate_sym <- four_counts_pasted_targetid_uttid_stem_separate_sym %>% filter(utterance_id != 704599) #eliminates think
#four_counts_pasted_targetid_uttid_stem_separate <- four_counts_pasted_targetid_uttid_stem_separate %>% filter(target_child_id != 2591) #eliminate bump noun?
four_counts_pasted_targetid_uttid_stem_separate_sym <- four_counts_pasted_targetid_uttid_stem_separate_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate')
#USE THIS TO GET THE FRAMES! AND FIND DUPLICATES!
#length(unique(four_counts_pasted_targetid_uttid_stem_separate2$target_child_id))
length(four_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id) #455 ignoring this comment ->  (THIS SHOULD ADD UP TO THE LENGTH OF EXCLUSION FOR FINAL AND THE SUM OF NO ZEROS DF) it doesn't because this includes words
#not in the top 12. OKAY FIXED
#length(unique(four_counts_pasted_targetid_uttid_stem_separate2$target_child_id))
length(unique(four_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id)) #54

#arranging by target_id helps when merging columns.
four_counts_sym <- four_counts_sym %>% arrange(target_child_id)
length(unique(four_counts_sym$target_child_id)) #55

#checking which child is missing from four_counts_pasted_targetid_uttid_stem_separate_sym
#num_of_ids <- four_counts_sym %>% filter(!target_child_id %in% four_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id)

length(four_counts_sym$form) #442
length(four_sym_and_id$form)#4950

#its okay for the rows/columns here to not be equal
four_full_sym <- merge(four_counts_sym, four_sym_and_id, all = TRUE) #Original 
length(four_full_sym$target_child_id) #5156

length(unique(four_full_sym$form)) #90
#deals with the random "thought" that was inserted.
#four_full<- four_full %>% filter(form %in% sym_list$form)
length(unique(four_full_sym$form)) #90
length(unique(four_full_sym$target_child_id)) #55

four_full_sym$target_child_sex <-NULL
four_full_sym$corpus_name <- NULL
four_full_sym$utterance_id <- NULL

length(unique(four_full_sym$form)) #90
length(unique(four_full_sym$target_child_id)) #55
length(four_full_sym$form) #5156

#now go in and change NAs for age to 4 and NAs for count to 0

four_full_sym$count[is.na(four_full_sym$count)] <- 0
four_full_sym$target_child_age[is.na(four_full_sym$target_child_age)] <- 4
sum(four_full_sym$count) #455. Matches the length of four_sym_filtered_pos_df?
#taking out think here
#four_sym_filtered_pos_df <- four_sym_filtered_pos_df %>% filter(stem != 'think')
unique(four_sym_filtered_pos_df$stem)
#we select our top 12 symmetricals here - select all symmetricals
filter_by_stem_four_sym <- four_full_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate')
length(filter_by_stem_four_sym$target_child_id) #2846
length(unique(filter_by_stem_four_sym$target_child_id)) #55
# need to add another constraint. 
# the count must be greater than 0 to be in filter_by_stem_four

#collapsed by stem + the number of counts for each stem
four_child_sum_sym <- aggregate(filter_by_stem_four_sym$count, by=list(filter_by_stem_four_sym$stem), sum)

#every stem and their count for each child.
four_child_all_stems_per_child_sym <- filter_by_stem_four_sym %>% group_by(target_child_id,stem) %>%
  summarize(tokens = sum(count))

four_child_all_stems_per_child_no_zeros_sym <- four_child_all_stems_per_child_sym
#this is key!
four_child_all_stems_per_child_no_zeros_sym <- four_child_all_stems_per_child_no_zeros_sym %>% filter(tokens != 0)
four_child_all_stems_per_child_no_zeros_sym <- four_child_all_stems_per_child_no_zeros_sym %>% arrange(target_child_id)
#write.csv(four_child_all_stems_per_child_no_zeros_sym, "~\\GitHub\\child-verb-matching-study\\Output\\symmetricals\\child\\four\\four_child_all_stems_per_child_no_zeros_sym.csv")
length(unique(four_child_all_stems_per_child_no_zeros_sym$target_child_id)) #54 total children represented. 
sum(four_child_all_stems_per_child_no_zeros_sym$tokens) #455
#eliminating the zeros worked!
verb_pairs_for_four_child_sheet_sym <- four_child_all_stems_per_child_no_zeros_sym %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))
write.csv(verb_pairs_for_four_child_sheet_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/verb_pairs_for_four_child_sheet_sym.csv")
length(unique(four_child_all_stems_per_child_no_zeros_sym$target_child_id)) #54
sum(four_child_all_stems_per_child_no_zeros_sym$tokens) #455
#length(exclusion_four_info_final_sym$target_child_id) #288

#Extracting corpus information so we can exclude atypical children
exclusion_four_info_sym <- four_sym_df_trimmed %>% filter(utterance_id %in% four_counts_pasted_targetid_uttid_stem_separate_sym$utterance_id) 
length(unique(exclusion_four_info_sym$stem)) #17
unique(exclusion_four_info_sym$stem) #nouns and empty stems
exclusion_four_info_sym_final <- exclusion_four_info_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate')
length(unique(exclusion_four_info_sym_final$stem)) #12
#good this is the one we need to pull frames!
length(exclusion_four_info_sym_final$target_child_id) #457

exclusion_four_info_sym_final$target_child_id %in% four_child_all_stems_per_child_no_zeros_sym$target_child_id
exclusion_four_info_sym_final <- exclusion_four_info_sym_final %>% arrange(target_child_id)
exclusion_four_info_sym_final <- exclusion_four_info_sym_final %>% filter(pos == 'v' | pos == 'part')
length(exclusion_four_info_sym_final$target_child_id) #455
write.csv(exclusion_four_info_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/exclusion_four_info_sym_final.csv")
#check the corpora in this df
unique(exclusion_four_info_sym_final$corpus_name)

length(unique(exclusion_four_info_sym_final$target_child_id)) #54
#need to exclude 'bumped' here perhaps? Maybe exclude a bit earlier in the code.
length(unique(filter_by_stem_four_sym$target_child_id)) #55
missing_child_for_four_sym <- filter_by_stem_four_sym %>% filter(!target_child_id %in% exclusion_four_info_sym_final$target_child_id)
length(unique(missing_child_for_four_sym$target_child_id))

length(exclusion_four_info_sym_final$target_child_id) #455

#tokens per corpus
corpora_tokens_four_sym <- exclusion_four_info_sym_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = length(corpus_name))
write.csv(corpora_tokens_four_sym,"/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/tokens-from-each-corpus-four-sym.csv")

#number of children in each corpus
num_of_children_in_each_corpus_four_sym <- exclusion_four_info_sym_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = length(unique(target_child_id)))
write.csv(num_of_children_in_each_corpus_four_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/num-of-children-in-each-corpus-four-sym.csv")

#token frequency per child to check for outliers
token_freq_per_child_four_sym <- exclusion_four_info_sym_final %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = (length(stem)), corpus_name = (corpus_name))
token_freq_per_child_four_sym_final <- unique(token_freq_per_child_four_sym)
write.csv(token_freq_per_child_four_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/token-freq-per-child-four-sym-final.csv")

#verb_pair numbers using exclusion info

#*** Combined child child production of symmetricals ***


#combined verb-pairs
combined_three_four_verb_pair_tokens_sym <- rbind(verb_pairs_for_four_child_sheet_sym,verb_pairs_for_three_child_sheet_sym)
sum(combined_three_four_verb_pair_tokens_sym$tokens) #765

combined_three_four_verb_pair_tokens_summed_sym <- combined_three_four_verb_pair_tokens_sym %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = sum(num_chi))
write.csv(combined_three_four_verb_pair_tokens_summed_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/combined/updated-count/combined-three-four-verb-pair-tokens-summed-sym.csv")
sum(combined_three_four_verb_pair_tokens_summed_sym$tokens) #765

sum(four_child_all_stems_per_child_no_zeros_sym$tokens) #455
length(exclusion_four_info_sym_final$target_child_id) #455

#combined tokens per copra
combined_corpra_tokens_sym <- rbind(corpra_tokens_three_sym, corpora_tokens_four_sym)
combined_corpra_tokens_sym_final <- combined_corpra_tokens_sym %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = sum(tokens))
write.csv(combined_corpra_tokens_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/combined/updated-count/combined-corpra-tokens-sym-final.csv")

#combined number of children in each corpus
combined_num_of_children_in_each_corpus_sym <- rbind(num_of_children_in_each_corpus_three_sym,num_of_children_in_each_corpus_four_sym)
combined_num_of_children_in_each_corpus_sym_final <- combined_num_of_children_in_each_corpus_sym %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = sum(num_chi))
write.csv(combined_num_of_children_in_each_corpus_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/combined/updated-count/combined-num-of-children-in-each-corpus-sym-final.csv")
sum(combined_num_of_children_in_each_corpus_sym_final$num_chi) #85

#combined sym exclusion info
combined_exclusion_syms <- rbind(exclusion_three_info_sym_final, exclusion_four_info_sym_final)
write.csv(combined_num_of_children_in_each_corpus_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/combined/updated-count/combined_num_of_children_in_each_corpus_sym_final.csv")
length(combined_exclusion_syms$target_child_id) #765

## ** ** ** Child production of foils 3 year olds ** ** ** ##

#load the library
library(childesr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyboot)
library(magrittr)
library(tidytext)
library(plyr)
library(ddply)
library(scales)

#setwd("C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\foils\\children")
getwd()

#importing the foil table
foil_list <- read.csv(file = "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Data/foil_list.csv", header = TRUE)
foil_list$unique_stems <- NULL
#how many unique foil stems do we have in total?
length(unique(foil_list$stem)) #27

#childesdb search of foils
three_year_olds_tokens_foil_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = foil_list$form
)

#checking how many children came up in the search.
length(unique(three_year_olds_tokens_foil_df$target_child_id)) #79

#trim the database by selecting the columns we are interested in
three_foils_df_trimmed <- select(three_year_olds_tokens_foil_df, 'target_child_id', 'corpus_name', 'target_child_age',
                                 'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')

#rename columns
names(three_foils_df_trimmed)[names(three_foils_df_trimmed) == "part_of_speech"] <- "pos"
names(three_foils_df_trimmed)[names(three_foils_df_trimmed) == "gloss"] <- "form"
three_foils_df_trimmed

#filter the df by part (past participle) and v (verb)
three_foils_filtered_pos_df <- three_foils_df_trimmed %>% filter(pos == 'v' | pos == 'part')
length(three_foils_filtered_pos_df$target_child_id) #1715
#now we want to know what words don't appear in the db
three_foils_not_in_db <- foil_list %>% filter(!form %in% three_foils_filtered_pos_df$form)
#saving
write.csv(three_foils_not_in_db, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/three_foils_not_in_db.csv")

length(three_foils_filtered_pos_df$target_child_id) #1715
length(unique(three_foils_filtered_pos_df$target_child_id)) #73

#making df of unique ids in three year olds
ids_for_threes_foils <- as.data.frame(unique(three_foils_filtered_pos_df$target_child_id))
names(ids_for_threes_foils)[names(ids_for_threes_foils) == "unique(three_foils_filtered_pos_df$target_child_id)"] <- "target_child_id"
length(unique(ids_for_threes_foils$target_child_id)) #73 unique ids

# generate 105 (because our sym_list has 105 rows) instances of an ID (n # of participants x f # of forms )
# we are trying to create a dataframe that has 2,970 rows (73 * 105)
length(foil_list$stem) #105
three_many_ids_foils <- ids_for_threes_foils %>% slice(rep(1:n(), each = 105))
length(three_many_ids_foils$target_child_id) #7665

#generate 73 instances of each symmetrical - because we want the ids and sym words to have the same # of rows before
#we merge them together.
length(unique(three_foils_filtered_pos_df$target_child_id)) #73
n = 73
threes_many_foils <- do.call("rbind", replicate(n, foil_list, simplify = FALSE))
length(threes_many_foils$form) #7665

# Merge many IDS with many syms
three_foils_and_id <- cbind(threes_many_foils, target_child_id = three_many_ids_foils$target_child_id)
length(three_foils_and_id$target_child_id) #7665

#the number of participants differ from the original code "child-production-of-foils"
#because we searched for nouns and adjectives in that code. Here we focus on verbs. 
#So the total number of children searched between 3 and 4 should be the same.

#replacing age with 3
three_foils_filtered_pos_df$target_child_age <-  replace(three_foils_filtered_pos_df$target_child_age,
                                                         three_foils_filtered_pos_df$target_child_age >= 36.00 &
                                                           three_foils_filtered_pos_df$target_child_age <= 47.99, 3)
length(three_foils_filtered_pos_df$target_child_id) #1715
#you can use three_foils_filtered_pos_df to extract sentence frames for three year olds Using utterance_id
#saving
#save.image("~/GitHub/childesr-corpus-analysis/symmetricals/children/child-production-of-symmetricals-shortened-environment.RData")

# We are doing this to get a count for the words that are not~ recorded. So that when we merge with
# the words from sym_list all the words not produced, receive an NA, which we later change to 0.
detach(package:plyr)
three_counts_foils <- three_foils_filtered_pos_df %>% group_by(form, target_child_id, target_child_age, target_child_sex,corpus_name,utterance_id) %>%
  summarize(count = sum(unique(length(form)))) #349 rows
#checking we still have the same amount of children.
length(unique(three_counts_foils$target_child_id)) #73

#this could be useful later on.
three_counts_pasted_targetid_uttid_stem_foils <- as.data.frame(paste(three_foils_filtered_pos_df$target_child_id, three_foils_filtered_pos_df$stem, three_foils_filtered_pos_df$utterance_id))
names(three_counts_pasted_targetid_uttid_stem_foils)[names(three_counts_pasted_targetid_uttid_stem_foils) == "paste(three_foils_filtered_pos_df$target_child_id, three_foils_filtered_pos_df$stem, three_foils_filtered_pos_df$utterance_id)"] <- "wordstem"
three_counts_pasted_targetid_uttid_stem_separate_foils <- three_counts_pasted_targetid_uttid_stem_foils %>% separate(wordstem, c("target_child_id", "stem", "utterance_id"))
length(unique(three_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id)) #73
three_counts_pasted_targetid_uttid_stem_separate_foils <- three_counts_pasted_targetid_uttid_stem_separate_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle'
                                                                                                                            | stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                                                                                              stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                                                                                              stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy')
#change the stems to foil stems!
length(unique(three_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id)) #73
length(three_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #1715 this number might change depending on whether or not there are errors in the filter process!

#arranging by target_id helps when merging columns.
three_counts_foils <- three_counts_foils %>% arrange(target_child_id)
length(unique(three_counts_foils$target_child_id)) #73

length(three_counts_foils$form) #1690, because this only includes counts for words that were produced by the child.Can we use this to plot data for people that have more than one count?
length(three_foils_and_id$form) #7665

three_full_foils <- merge(three_counts_foils, three_foils_and_id, all = TRUE)
three_full_foils <- three_full_foils %>% arrange(target_child_id)
#we dont really need sex, corpus, and utt id here.
three_full_foils$target_child_sex <-NULL
three_full_foils$corpus_name <- NULL
three_full_foils$utterance_id <- NULL

length(unique(three_full_foils$form)) #105
length(unique(three_full_foils$target_child_id)) #73
length(three_full_foils$form) #8825

#now go in and change NAs for age to 3 and NAs for count to 0
three_full_foils$count[is.na(three_full_foils$count)] <- 0
three_full_foils$target_child_age[is.na(three_full_foils$target_child_age)] <- 3
sum(three_full_foils$count) #1715 matches with three_foils_filtered_pos_df? - this df doesn't count tokens, so we count the rows which are equal to one token.

#we would select our top 12 foils here, but im going to include all of them.
filter_by_stem_foils <- three_full_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle' |
                                                      stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                      stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                      stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy')
length(filter_by_stem_foils$target_child_id) #8825 rows
length(unique(filter_by_stem_foils$target_child_id)) #73

#collapsed by stem + the number of counts for each stem
three_child_sum_foils <- aggregate(filter_by_stem_foils$count, by=list(filter_by_stem_foils$stem), sum)

#every stem and their count for each child.
three_child_all_stems_per_child_foils <- filter_by_stem_foils %>% group_by(target_child_id,stem) %>%
  summarize(tokens = sum(count))

#test2 <- filter_by_stem %>% group_by(stem) %>%
#summarize(num_chi = )


#only_attach <- test %>% filter(stem == 'attach')
#checking <- as.data.frame(only_attach$target_child_id[only_attach$tokens > 0])
#table(checking) #this works but how to scale it up?

three_child_all_stems_per_child_no_zeros_foils <- three_child_all_stems_per_child_foils

three_child_all_stems_per_child_no_zeros_foils <- three_child_all_stems_per_child_no_zeros_foils %>% filter(tokens != 0)
length(unique(three_child_all_stems_per_child_no_zeros_foils$target_child_id))#73
length(three_child_all_stems_per_child_no_zeros_foils$target_child_id) #360
#eliminating the zeros worked!
verb_pairs_for_three_child_sheet_foils <- three_child_all_stems_per_child_no_zeros_foils %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))
length(unique(three_child_all_stems_per_child_no_zeros_foils$target_child_id)) #73


#Extracting corpus information so we can exclude atypical children
exclusion_three_info_foils <- three_foils_df_trimmed %>% filter(utterance_id %in% three_counts_pasted_targetid_uttid_stem_separate_foils$utterance_id) 
length(unique(exclusion_three_info_foils$stem)) #25
unique(exclusion_three_info_foils$stem) #nouns and empty stems
exclusion_three_info_foils_final <- exclusion_three_info_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle' |
                                                                            stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                                            stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                                            stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy')
length(unique(exclusion_three_info_foils_final$stem)) #23 - because we deleted all the foils with zeros.
unique(exclusion_three_info_foils_final$stem)
#good this is the one we need to pull frames!
length(exclusion_three_info_foils_final$target_child_id) #1760
length(unique(exclusion_three_info_foils_final$target_child_id)) #73

exclusion_three_info_foils_final$target_child_id %in% three_child_all_stems_per_child_no_zeros_foils$target_child_id
exclusion_three_info_foils_final <- exclusion_three_info_foils_final %>% arrange(target_child_id)
exclusion_three_info_foils_final <- exclusion_three_info_foils_final %>% filter(pos == 'v' | pos == 'part')
length(exclusion_three_info_foils_final$target_child_id) #1715 should be the same when we add the sum of counts for no zeros three.
sum(three_child_all_stems_per_child_no_zeros_foils$tokens) #1715. Yup they match.
length(unique(exclusion_three_info_foils_final$target_child_id))#73
length(unique(exclusion_three_info_foils_final$stem)) #22

write.csv(exclusion_three_info_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/exclusion_three_info_foils_final.csv")
#check the corpra in this df
unique(exclusion_three_info_foils_final$corpus_name)

#tokens per corpus
corpra_tokens_three_foils <- exclusion_three_info_foils_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = length(corpus_name))
write.csv(corpra_tokens_three_foils,"/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/tokens-from-each-corpus-three-foils.csv")

#number of children in each corpus
num_of_children_in_each_corpus_three_foils <- exclusion_three_info_foils_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = length(unique(target_child_id)))
write.csv(num_of_children_in_each_corpus_three_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/num-of-children-in-each-corpus-three-foils.csv")

#token frequency per child to check for outliers
token_freq_per_child_three_foils <- exclusion_three_info_foils_final %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = (length(stem)), corpus_name = (corpus_name))
token_freq_per_child_three_foils_final <- unique(token_freq_per_child_three_foils)
write.csv(token_freq_per_child_three_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/token-freq-per-child-three-foils-final.csv")
length(unique(token_freq_per_child_three_foils_final$target_child_id)) #73

#checking one participant
get_participants(
  collection = "Eng-NA",
  corpus = "Demetras1",
  age = 39
)

## ** ** Child production of foils - 4 year olds ** ** ##

four_year_olds_tokens_foils_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(48, 60),
  token = foil_list$form
)

length(unique(four_year_olds_tokens_foils_df$target_child_id)) #70

four_foils_df_trimmed <- select(four_year_olds_tokens_foils_df, 'target_child_id', 'corpus_name', 'target_child_age',
                                'gloss', 'part_of_speech','target_child_sex','stem','utterance_id')

#renaming columns
names(four_foils_df_trimmed)[names(four_foils_df_trimmed) == "part_of_speech"] <- "pos"
names(four_foils_df_trimmed)[names(four_foils_df_trimmed) == "gloss"] <- "form"
four_foils_df_trimmed

#filter the df by part (past participle) and v (verb)
four_foils_filtered_pos_df <- four_foils_df_trimmed %>% filter(pos == 'v' | pos == 'part')
length(four_foils_filtered_pos_df$target_child_id) #1915
#now we want to know what words don't appear in the db
four_foils_not_in_db <- foil_list %>% filter(!form %in% four_foils_filtered_pos_df$form)
write.csv(four_foils_not_in_db, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/four_foils_not_in_db.csv")

length(four_foils_filtered_pos_df$target_child_id) #1915  
length(unique(four_foils_filtered_pos_df$target_child_id)) #67

#making df of unique ids in four year olds
ids_for_fours_foils <- as.data.frame(unique(four_foils_filtered_pos_df$target_child_id))
names(ids_for_fours_foils)[names(ids_for_fours_foils) == "unique(four_foils_filtered_pos_df$target_child_id)"] <- "target_child_id"
length(unique(ids_for_fours_foils$target_child_id)) #67 unique ids with duplicate children from the 3 year old foil db

#storing the 3 year olds in the four year old db in repeats.
#repeats_foils <- ids_for_fours_foils %>% filter(target_child_id %in% ids_for_threes_foils$target_child_id)
#length(unique(repeats_foils$target_child_id)) #14

#removing the 3 year olds from the 4 year old db.
#ids_for_fours_no_rep_foils <- ids_for_fours_foils %>% filter(!target_child_id %in% ids_for_threes_foils$target_child_id)
#length(unique(ids_for_fours_no_rep_foils$target_child_id)) #53

#checking if they are gone!
#repeats_foils$target_child_id %in% ids_for_fours_no_rep_foils$target_child_id

#generate 105 instances of an ID
length(foil_list$form)
four_many_ids_foils <- ids_for_fours_foils %>% slice(rep(1:n(), each = 105))
length(four_many_ids_foils$target_child_id) #7035 (67 * 105)

#generate 53 (for each unique id) instances of sym_words # i think this (the extra rows) gets filtered out later. 
length(unique(ids_for_fours_foils$target_child_id)) #67 unique ids with duplicate children from the 3 year old foil db
n = 67
four_many_foils <- do.call("rbind", replicate(n, foil_list, simplify = FALSE))
length(four_many_foils$form) #7035

# Merge many IDS with many syms
four_foils_and_id <- cbind(four_many_foils, target_child_id = four_many_ids_foils$target_child_id)
length(four_foils_and_id$target_child_id) #7035

#replacing age with 4

four_foils_filtered_pos_df$target_child_age <-  replace(four_foils_filtered_pos_df$target_child_age,
                                                        four_foils_filtered_pos_df$target_child_age >= 48.00 &
                                                          four_foils_filtered_pos_df$target_child_age < 60.00, 4)

length(four_foils_filtered_pos_df$target_child_id) #1915
#you can use four_foils_filtered_pos_df to extract sentence frames for three year olds Using utterance_id
#No! Remove the repeats first!

#saving
#save.image("~/GitHub/childesr-corpus-analysis/symmetricals/children/child-production-of-symmetricals-shortened-environment.RData")

#remove comments below if you want to exclude the three year olds that eventually age into the 4-year-old db
#four_foils_filtered_pos_df <- four_foils_filtered_pos_df %>% filter(!target_child_id %in% ids_for_threes_foils$target_child_id)
length(unique(four_foils_filtered_pos_df$target_child_id)) #67
#now you can use it to extract frames.

detach(package:plyr)
four_counts_foils <- four_foils_filtered_pos_df %>% group_by(form, target_child_id, target_child_age, target_child_sex,corpus_name,utterance_id) %>%
  summarize(count = sum(unique(length(form)))) #1100 rows
#checking we still have the same amount of children.
length(unique(four_counts_foils$target_child_id)) #67

#this could be useful later on.
four_counts_pasted_targetid_uttid_stem_foils <- as.data.frame(paste(four_foils_filtered_pos_df$target_child_id, four_foils_filtered_pos_df$stem, four_foils_filtered_pos_df$utterance_id))
names(four_counts_pasted_targetid_uttid_stem_foils)[names(four_counts_pasted_targetid_uttid_stem_foils) == "paste(four_foils_filtered_pos_df$target_child_id, four_foils_filtered_pos_df$stem, four_foils_filtered_pos_df$utterance_id)"] <- "wordstem"
four_counts_pasted_targetid_uttid_stem_separate_foils <- four_counts_pasted_targetid_uttid_stem_foils %>% separate(wordstem, c("target_child_id", "stem", "utterance_id"))
length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id)
#four_counts_pasted_targetid_uttid_stem_separate <- four_counts_pasted_targetid_uttid_stem_separate %>% filter(utterance_id != 704599)
#four_counts_pasted_targetid_uttid_stem_separate <- four_counts_pasted_targetid_uttid_stem_separate %>% filter(target_child_id != 2591)
four_counts_pasted_targetid_uttid_stem_separate_foils <- four_counts_pasted_targetid_uttid_stem_separate_foils %>% filter(stem=='cover'|stem=='insert'|stem=='tie'|stem=='drop'|stem=='knot'|stem=='punch'|stem=='kick'|stem=='attack'|stem=='tickle'|stem=='pet'|stem=='stroke'|stem=='pull'|stem=='lick'|stem=='bite'|stem=='invite'|stem=='celebrate'|stem=='adopt'|stem=='choose'|stem=='check'|stem=='teach'|stem=='greet'|stem=='push'|stem=='tap'|stem=='hold'|stem=='bump'|stem=='sell'|stem=='buy'|stem=='bit')

length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id)
four_counts_pasted_targetid_uttid_stem_separate_foils[four_counts_pasted_targetid_uttid_stem_separate_foils$stem=='bit', "stem"] <- "bite"
#fixing the 'bit' stem issue
length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #1915

#USE THIS TO GET THE FRAMES! AND FIND DUPLICATES!
#length(unique(four_counts_pasted_targetid_uttid_stem_separate2$target_child_id))
length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #1915 (THIS SHOULD ADD UP TO THE LENGTH OF EXCLUSION FOR FINAL AND THE SUM OF NO ZEROS DF) it doesn't because this includes words
#not in the top 12. Does it match?

#length(unique(four_counts_pasted_targetid_uttid_stem_separate2$target_child_id))
length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #1915

#arranging by target_id helps when merging columns.
four_counts_foils <- four_counts_foils %>% arrange(target_child_id)
length(unique(four_counts_foils$target_child_id)) #67

length(four_counts_foils$form) #1871
length(four_foils_and_id$form)#7035

#its okay for the rows/columns here to not be equal
four_full_foils <- merge(four_counts_foils, four_foils_and_id, all = TRUE) #Original 
length(four_full_foils$target_child_id) #8168

length(unique(four_full_foils$form)) #105
#deals with the random "thought" that was inserted.
four_full_foils <- four_full_foils %>% filter(form %in% foil_list$form)
length(unique(four_full_foils$form)) #105
length(unique(four_full_foils$stem)) #27
length(unique(four_full_foils$target_child_id)) #67

four_full_foils$target_child_sex <-NULL
four_full_foils$corpus_name <- NULL
four_full_foils$utterance_id <- NULL

length(unique(four_full_foils$form)) #105
length(unique(four_full_foils$target_child_id)) #67
length(four_full_foils$form) #8168

#now go in and change nas for age to 4 and nas for count to 0

four_full_foils$count[is.na(four_full_foils$count)] <- 0
four_full_foils$target_child_age[is.na(four_full_foils$target_child_age)] <- 4
sum(four_full_foils$count) #1915. Matches four_year_olds_tokendf?, ** 
#taking out think here #1129
#four_foils_filtered_pos_df <- four_foils_filtered_pos_df %>% filter(stem != 'think')

#we select our top 12 foils here - going to select all of the foils for now.
filter_by_stem_four_foils <- four_full_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle' |
                                                          stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                          stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                          stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy' | stem == 'bit')
length(filter_by_stem_four_foils$target_child_id) #8168
length(unique(filter_by_stem_four_foils$target_child_id)) #67
# need to add another constraint. 
# the count must be greater than 0 to be in filter_by_stem_four

#collapsed by stem + the number of counts for each stem
four_child_sum_foils <- aggregate(filter_by_stem_four_foils$count, by=list(filter_by_stem_four_foils$stem), sum)

#every stem and their count for each child.
detach(package:plyr)
four_child_all_stems_per_child_foils <- filter_by_stem_four_foils %>% group_by(target_child_id,stem) %>%
  summarize(tokens = sum(count))
#this could be useful, once we fix the one extra token for 2291 bite
sum(four_child_all_stems_per_child_foils$tokens) #1915
#test2 <- filter_by_stem %>% group_by(stem) %>%
#summarize(num_chi = )
write.csv(four_child_all_stems_per_child_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/four_child_all_stems_per_child_foils.csv")

#only_attach <- test %>% filter(stem == 'attach')
#checking <- as.data.frame(only_attach$target_child_id[only_attach$tokens > 0])
#table(checking) #this works but how to scale it up?

four_child_all_stems_per_child_no_zeros_foils <- four_child_all_stems_per_child_foils
length(four_child_all_stems_per_child_foils$stem) #1809
#this is key!
four_child_all_stems_per_child_no_zeros_foils <- four_child_all_stems_per_child_no_zeros_foils %>% filter(tokens != 0)
four_child_all_stems_per_child_no_zeros_foils <- four_child_all_stems_per_child_no_zeros_foils %>% arrange(target_child_id)
#write.csv(four_child_all_stems_per_child_no_zeros_foils, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\foils\\children\\four_child_all_stems_per_child_no_zeros_foils.csv")
length(unique(four_child_all_stems_per_child_no_zeros_foils$target_child_id)) #67
sum(four_child_all_stems_per_child_no_zeros_foils$tokens) #1915
#eliminating the zeros worked!
verb_pairs_for_four_child_sheet_foils <- four_child_all_stems_per_child_no_zeros_foils %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))
length(unique(four_child_all_stems_per_child_no_zeros_foils$target_child_id)) #67
sum(four_child_all_stems_per_child_no_zeros_foils$tokens) #1915
write.csv(verb_pairs_for_four_child_sheet_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/verb_pairs_for_four_child_sheet_foils.csv")

#Extracting corpus information so we can exclude atypical children
#this is why we get the wrong number of rows. separate_foils is wrong.
exclusion_four_info_foils <- four_foils_df_trimmed %>% filter(utterance_id %in% four_counts_pasted_targetid_uttid_stem_separate_foils$utterance_id) 
length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #1915
length(unique(exclusion_four_info_foils$stem)) #25
unique(exclusion_four_info_foils$stem) #nouns and empty stems
exclusion_four_info_foils_final <- exclusion_four_info_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle' |
                                                                          stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                                          stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                                          stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy'| stem =='bit')
length(unique(exclusion_four_info_foils_final$stem)) #24 (although its really 23 because of 'bit')
#good this is the one we need to pull frames!
length(exclusion_four_info_foils_final$target_child_id) #1952

exclusion_four_info_foils_final$target_child_id %in% four_child_all_stems_per_child_no_zeros_foils$target_child_id
length(exclusion_four_info_foils_final$target_child_id)
exclusion_four_info_foils_final <- exclusion_four_info_foils_final %>% arrange(target_child_id)
exclusion_four_info_foils_final <- exclusion_four_info_foils_final %>% filter(pos == 'v' | pos == 'part')
length(exclusion_four_info_foils_final$target_child_id) #1915
exclusion_four_info_foils_final[exclusion_four_info_foils_final$stem=='bit', "stem"] <- "bite"
length(exclusion_four_info_foils_final$target_child_id) #1915
write.csv(exclusion_four_info_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/exclusion_four_info_foils_final.csv")
#check the corpra in this df
unique(exclusion_four_info_foils_final$corpus_name)

length(unique(exclusion_four_info_foils_final$target_child_id)) #67
length(unique(filter_by_stem_four_foils$target_child_id)) #67
missing_child_for_four_foils <- exclusion_four_info_foils_final %>% filter(!utterance_id %in% four_counts_pasted_targetid_uttid_stem_separate_foils$utterance_id)
length(unique(missing_child_for_four_foils$target_child_id))
#length(exclusion_four_info_foils_final$target_child_id) #1128

length(exclusion_four_info_foils_final$target_child_id) #1915
length(unique(exclusion_four_info_foils_final$target_child_id)) #67
length(unique(exclusion_four_info_foils_final$stem)) #23
length(unique(exclusion_four_info_foils_final$pos)) #2
exclusion_four_foil_missing <- exclusion_four_info_foils_final %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = length(stem))

sum(four_child_all_stems_per_child_no_zeros_foils$tokens) #1915
length(unique(four_child_all_stems_per_child_no_zeros_foils$target_child_id)) #67
length(unique(four_child_all_stems_per_child_no_zeros_foils$stem)) #23
zeros_four_foil_missing <- four_child_all_stems_per_child_no_zeros_foils %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = sum(tokens))

length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #1915
length(unique(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id)) #67
length(unique(four_counts_pasted_targetid_uttid_stem_separate_foils$stem)) #23

#missing by stem this time
#exclusion_four_foil_missing2 <- exclusion_four_info_foils_final %>% dplyr::group_by(stem) %>%
#dplyr::summarize(tokens = length(stem))

#zeros_four_foil_missing2 <- four_child_all_stems_per_child_no_zeros_foils %>% dplyr::group_by(stem) %>%
#dplyr::summarize(tokens = sum(tokens))

#pasted_four_foil_missing2 <- four_counts_pasted_targetid_uttid_stem_separate_foils %>% dplyr::group_by(stem) %>%
#dplyr::summarize(tokens = length(stem))

#four_no_zeros has the correct number of bites for 2291. Go back to four_foils_filtered_pos_df and append the row
# with utterance 635280. R bind should do the trick. Make it a one row df that you rbind to the other columns after making sure they have
# the same column numbers and names. 
#do this before running the code at the bottom.
#saving 
#save.image("~/GitHub/childesr-corpus-analysis/foils/children/child-production-of-foils-shortened-environment.RData")

#tokens per corpus
corpra_tokens_four_foils <- exclusion_four_info_foils_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = length(corpus_name))
write.csv(corpra_tokens_four_foils,"/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/tokens-from-each-corpus-four-foils.csv")
sum(corpra_tokens_four_foils$tokens) #1915
length(exclusion_four_info_foils_final$target_child_id) #1915

#number of children in each corpus
num_of_children_in_each_corpus_four_foils <- exclusion_four_info_foils_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = length(unique(target_child_id)))
write.csv(num_of_children_in_each_corpus_four_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/num_of_children_in_each_corpus_four_foils.csv")
length(unique(exclusion_four_info_foils_final$target_child_id)) #67

#token frequency per child to check for outliers
token_freq_per_child_four_foils <- exclusion_four_info_foils_final %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = (length(stem)), corpus_name = (corpus_name))
token_freq_per_child_four_foils_final <- unique(token_freq_per_child_four_foils)
write.csv(token_freq_per_child_four_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/token_freq_per_child_four_foils_final.csv")

#combined verb-pairs
combined_three_four_verb_pair_tokens_foils <- rbind(verb_pairs_for_four_child_sheet_foils,verb_pairs_for_three_child_sheet_foils)

combined_three_four_verb_pair_tokens_summed_foils <- combined_three_four_verb_pair_tokens_foils %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = sum(num_chi))
sum(combined_three_four_verb_pair_tokens_summed_foils$tokens) #3630
write.csv(combined_three_four_verb_pair_tokens_summed_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/combined_three_four_verb_pair_tokens_summed_foils.csv")

sum(four_child_all_stems_per_child_no_zeros_foils$tokens) #1915
length(exclusion_four_info_foils_final$target_child_id) #1915

#combined tokens per copra
combined_corpra_tokens_foils <- rbind(corpra_tokens_three_foils, corpra_tokens_four_foils)
combined_corpra_tokens_foils_final <- combined_corpra_tokens_foils %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = sum(tokens))
write.csv(combined_corpra_tokens_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/combined_corpra_tokens_foils_final.csv")
sum(combined_corpra_tokens_foils_final$tokens) #3630

#combined number of children in each corpus
combined_num_of_children_in_each_corpus_foils <- rbind(num_of_children_in_each_corpus_three_foils,num_of_children_in_each_corpus_four_foils)
combined_num_of_children_in_each_corpus_foils_final <- combined_num_of_children_in_each_corpus_foils %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = sum(num_chi))
write.csv(combined_num_of_children_in_each_corpus_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/combined_num_of_children_in_each_corpus_foils_final.csv")

#combined number of unique children in foils and symmetricals
#token_freq_per_child_three_sym_final<- read.csv(file = "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\token_freq_per_child_three_final.csv", header = TRUE)
#token_freq_per_child_three_sym_final$X <- NULL
#token_freq_per_child_three_sym_final$tokens <- NULL
#token_freq_per_child_three_sym_final$corpus_name <- NULL

#token_freq_per_child_four_sym_final <- read.csv(file = "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\token_freq_per_child_four_final.csv", header = TRUE)
#token_freq_per_child_four_sym_final$X <- NULL
#token_freq_per_child_four_sym_final$tokens <- NULL
#token_freq_per_child_four_sym_final$corpus_name <- NULL

combined_num_of_children_in_syms <- rbind(token_freq_per_child_four_sym_final, token_freq_per_child_three_sym_final)
combined_num_of_children_in_syms_unique <- as.data.frame(unique(combined_num_of_children_in_syms$target_child_id))
names(combined_num_of_children_in_syms_unique)[names(combined_num_of_children_in_syms_unique) == "unique(combined_num_of_children_in_syms$target_child_id)"] <- "target_child_id"

length(unique(combined_num_of_children_in_syms_unique$target_child_id)) #71
length(combined_num_of_children_in_syms_unique$target_child_id) #71

#foils

combined_num_of_children_in_foils_redux <-rbind(token_freq_per_child_four_foils_final, token_freq_per_child_three_foils_final)
combined_num_of_children_in_foils_redux_unique <- as.data.frame(unique(combined_num_of_children_in_foils_redux$target_child_id))
names(combined_num_of_children_in_foils_redux_unique)[names(combined_num_of_children_in_foils_redux_unique) == "unique(combined_num_of_children_in_foils_redux$target_child_id)"] <- "target_child_id"
length(unique(combined_num_of_children_in_foils_redux_unique$target_child_id)) #126
length(combined_num_of_children_in_foils_redux_unique$target_child_id) #126
write.csv(combined_num_of_children_in_foils_redux_unique, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/combined_num_of_children_in_foils_redux_unique.csv")

# --
token_freq_per_child_three_foils_final_targetid <- as.data.frame(unique(token_freq_per_child_three_foils_final$target_child_id))
names(token_freq_per_child_three_foils_final_targetid)[names(token_freq_per_child_three_foils_final_targetid) == "unique(token_freq_per_child_three_foils_final$target_child_id)"] <- "target_child_id"


token_freq_per_child_three_foils_final_targetid <- as.data.frame(unique(token_freq_per_child_three_foils_final$target_child_id))
names(token_freq_per_child_three_foils_final_targetid)[names(token_freq_per_child_three_foils_final_targetid) == "unique(token_freq_per_child_three_foils_final$target_child_id)"] <- "target_child_id"

token_freq_per_child_four_foils_final_targetid <- as.data.frame(unique(token_freq_per_child_four_foils_final$target_child_id))
names(token_freq_per_child_four_foils_final_targetid)[names(token_freq_per_child_four_foils_final_targetid) == "unique(token_freq_per_child_four_foils_final$target_child_id)"] <- "target_child_id"

combined_num_of_children_in_foils <- rbind(token_freq_per_child_three_foils_final_targetid, token_freq_per_child_four_foils_final_targetid)
combined_num_of_children_in_foils_unique <- as.data.frame(unique(combined_num_of_children_in_foils$target_child_id))
names(combined_num_of_children_in_foils_unique)[names(combined_num_of_children_in_foils_unique) == "unique(combined_num_of_children_in_foils$target_child_id)"] <- "target_child_id"
length(unique(combined_num_of_children_in_foils_unique$target_child_id)) #126

#checking for duplicates. 
foils_toke_freq_chi <- as.data.frame(token_freq_per_child_four_foils_final_targetid$target_child_id %in% token_freq_per_child_three_foils_final_targetid$target_child_id)

how_many_symsids_in_foils <- as.data.frame(combined_num_of_children_in_foils_unique$target_child_id %in% combined_num_of_children_in_syms_unique$target_child_id)
length(how_many_symsids_in_foils$`combined_num_of_children_in_foils_unique$target_child_id %in% combined_num_of_children_in_syms_unique$target_child_id` == 'TRUE') #126
sum(how_many_symsids_in_foils$`combined_num_of_children_in_foils_unique$target_child_id %in% combined_num_of_children_in_syms_unique$target_child_id`, na.rm = TRUE) #71
#74 shared ids - so 74 children contribute to both sym and foil tokens.

#how many unique foil ids? Not in the shared count
how_many_unique_foil_ids <- as.data.frame(combined_num_of_children_in_foils_unique$target_child_id %in% combined_num_of_children_in_syms_unique$target_child_id)
sum(how_many_unique_foil_ids$`combined_num_of_children_in_foils_unique$target_child_id %in% combined_num_of_children_in_syms_unique$target_child_id`, na.rm = TRUE)
#71

#combined unique target child ids
combined_sym_foil_targetids <- rbind(combined_num_of_children_in_foils_unique, combined_num_of_children_in_syms_unique)
length(unique(combined_sym_foil_targetids$target_child_id))#126

#how many unique sym ids? not in the shared count
how_many_unique_syms_ids <- as.data.frame(combined_num_of_children_in_syms_unique$target_child_id %in% combined_sym_foil_targetids$target_child_id)
sum(how_many_unique_foil_ids$`combined_num_of_children_in_syms_unique$target_child_id %in% combined_sym_foil_targetids$target_child_id`, na.rm = TRUE)
#0 no unique sym ids that are not in the foils.

#unique_foil_ids
unique_foil_ids <- combined_sym_foil_targetids %>% filter(!target_child_id %in% combined_num_of_children_in_syms_unique$target_child_id)
length(unique(unique_foil_ids$target_child_id))
#55 unique foil ids

#unique_sym_ids
unique_sym_ids <- combined_sym_foil_targetids %>% filter(!target_child_id %in% combined_num_of_children_in_foils_unique$target_child_id)
length(unique(unique_sym_ids$target_child_id))
#0 unique sym ids

length(unique(combined_num_of_children_in_syms_unique$target_child_id))
#71 unique sym ids

#combined_exclusion
#combined foil exclusion info
combined_exclusion_foils <- rbind(exclusion_three_info_foils_final, exclusion_four_info_foils_final)
length(combined_exclusion_foils$target_child_id) #3630
length(unique(combined_exclusion_foils$target_child_id)) #126 matches up with "combined_num_of_children_in_foils_redux_unique"
# *** Analysis for combined or other information ** 

#combined foil exclusion info
combined_exclusion_foils <- rbind(exclusion_three_info_foils_final, exclusion_four_info_foils_final)
length(combined_exclusion_foils$target_child_id) # NEW 3630, WAS 2844

#combined exclusion foil and sym
combined_exclusion_foil_sym <- rbind(combined_exclusion_foils,combined_exclusion_syms)
length(unique(combined_exclusion_foil_sym$target_child_id)) #126 children in both sym and foil
length(combined_exclusion_foil_sym$target_child_id) # NEW 4395 - WAS 3494
write.csv(combined_exclusion_foil_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/combined_foil_sym/updated-count/combined_exclusion_foil_sym.csv")

#corpra counts
combined_corpora_tokens_sym_and_foil <- combined_exclusion_foil_sym %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = length(corpus_name))
write.csv(combined_corpora_tokens_sym_and_foil, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/combined_foil_sym/updated-count/combined_corpora_tokens_sym_and_foil.csv")
sum(combined_corpora_tokens_sym_and_foil$tokens)# NEW 4395 - WAS 3494

#how many syms in foils
syms_in_foils <- combined_exclusion_syms %>% filter(target_child_id %in% combined_exclusion_foils$target_child_id)
length(syms_in_foils$target_child_id) #NEW 765 - WAS 650, which is all of the symmetricals.
length(unique(syms_in_foils$target_child_id)) #NEW 71 - WAS 74 syms and foil contribution

#check whether the foil target child id rbind matches the foil exclusion length.


foils_not_in_syms <- combined_exclusion_foils %>% filter(!target_child_id %in% combined_exclusion_syms$target_child_id)
length(unique(foils_not_in_syms$target_child_id)) #NEW 55 - WAS 52 pure foil kids

length(unique(exclusion_four_info_foils_final$target_child_id)) #NEW 67 - WAS 53
length(unique(exclusion_three_info_foils_final$target_child_id)) #SAME 73

length(unique(exclusion_three_info_sym_final$target_child_id)) #NEW 31 - WAS 33 syms
length(unique(exclusion_four_info_sym_final$target_child_id)) #NEW 54 - WAS 41 syms

#how many three-year-olds?
threes_only <- rbind(exclusion_three_info_sym_final,exclusion_three_info_foils_final)
length(unique(threes_only$target_child_id)) #73

threes_in_combined_exclusions <- combined_exclusion_foil_sym %>% filter(between(target_child_age,36,48))
length(unique(threes_in_combined_exclusions$target_child_id)) #73

#how many four-year-olds?
fours_only <- rbind(exclusion_four_info_sym_final, exclusion_four_info_foils_final)
length(unique(fours_only$target_child_id)) #NEW 67 - WAS 53

fours_in_combined_exclusions <- combined_exclusion_foil_sym %>% filter(between(target_child_age,48,60))
length(unique(fours_in_combined_exclusions$target_child_id)) #NEW 67 - WAS 53

#how many males?
exclusions_males <- combined_exclusion_foil_sym %>% filter(target_child_sex == 'male')
length(unique(exclusions_males$target_child_id)) #SAME 74

#how many females?
exclusions_females <- combined_exclusion_foil_sym %>% filter(target_child_sex == 'female')
length(unique(exclusions_females$target_child_id)) #SAME 48

#getting mean age, but first get unique child ids
combined_exclusion_foil_sym_unique_ids <- mean(combined_exclusion_foil_sym$target_child_age)

#how many unique corpra?
length(unique(combined_exclusion_foil_sym$corpus_name)) #SAME 25, CHECK IF SAME CORPORA.
