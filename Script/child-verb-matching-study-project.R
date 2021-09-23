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
length(unique(sym_list$stem)) #29 unique stems. including nouns, etc.

#childesdb search of symmetricals for 3 year olds.
three_year_olds_tokens_sym_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = sym_list$form
)
#checking how many children came up in the search.
length(unique(three_year_olds_tokens_sym_df$target_child_id))#58

#trim the database by selecting the columns we are interested in
three_sym_df_trimmed <- select(three_year_olds_tokens_sym_df, 'target_child_id', 'corpus_name', 'target_child_age',
                               'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')

library(plyr)
three_sym_df_trimmed$stem <- revalue(three_sym_df_trimmed$stem, c("operate"="cooperate")) #this worked.
#renaming columns
names(three_sym_df_trimmed)[names(three_sym_df_trimmed) == "part_of_speech"] <- "pos"
names(three_sym_df_trimmed)[names(three_sym_df_trimmed) == "gloss"] <- "form"
three_sym_df_trimmed

#filter the df by part (past participle??) and v (verb)
three_sym_filtered_pos_df <- three_sym_df_trimmed %>% filter(pos == 'v' | pos == 'part')

unique(three_sym_filtered_pos_df$stem) #operate change to cooperate
length(unique(three_sym_filtered_pos_df$stem)) #19

length(three_sym_filtered_pos_df$target_child_id) #362
#now we want to know what words don't appear in the db
three_sym_not_in_db <- sym_list %>% filter(!form %in% three_sym_filtered_pos_df$form)
three_sym_not_in_db_unique <- as.data.frame(unique(three_sym_not_in_db$stem))
write.csv(three_sym_not_in_db, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/three_sym_not_in_db_v3.csv")

length(three_sym_filtered_pos_df$target_child_id) #453
length(unique(three_sym_filtered_pos_df$target_child_id)) #38

#making df of unique ids in three year olds
ids_for_threes_sym <- as.data.frame(unique(three_sym_filtered_pos_df$target_child_id))
names(ids_for_threes_sym)[names(ids_for_threes_sym) == "unique(three_sym_filtered_pos_df$target_child_id)"] <- "target_child_id"
length(unique(ids_for_threes_sym$target_child_id)) #38 unique ids
length(sym_list$stem) #107
# generate 90 (because our sym_list has 90 rows) instances of an ID (n # of participants x f # of forms )
# we are trying to create a dataframe that has 2,970 rows (33 * 90)
three_many_ids_sym <- ids_for_threes_sym %>% slice(rep(1:n(), each = 107))
length(three_many_ids_sym$target_child_id) #4066
length(unique(sym_list$stem))#29
sym_list

#generate 33 instances of each symmetrical - because we want the ids and sym words to have the same # of rows before
#we merge them together. #33 is the number of participants we have.
n = 38
threes_many_syms <- do.call("rbind", replicate(n, sym_list, simplify = FALSE))
length(threes_many_syms$form) #4066

# Merge many IDS with many_syms
three_sym_and_id <- cbind(threes_many_syms, target_child_id = three_many_ids_sym$target_child_id)
length(three_sym_and_id$target_child_id) #4066

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
library(plyr)
three_sym_filtered_pos_df$stem <- revalue(three_sym_filtered_pos_df$stem, c("operate"="cooperate")) #this worked.

detach(package:plyr)
three_counts_sym <- three_sym_filtered_pos_df %>% group_by(form, target_child_id, target_child_age, target_child_sex,corpus_name,utterance_id) %>%
  summarize(count = sum(unique(length(form))))
length(three_counts_sym$form) #439

three_counts_sym_form <- three_sym_filtered_pos_df %>% group_by(form) %>%
  summarize(count = sum(unique(length(form))))
write.csv(three_counts_sym_form, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/three_counts_sym_form_v3.csv")

three_counts_sym_stem <- three_sym_filtered_pos_df %>% group_by(stem) %>%
  summarize(count = sum(unique(length(stem))))

write.csv(three_counts_sym_stem, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/three_counts_sym_stem_v3.csv")

#checking we still have the same amount of children.
length(unique(three_counts_sym$target_child_id)) #38

#this could be useful later on.
three_counts_pasted_targetid_uttid_stem_sym <- as.data.frame(paste(three_sym_filtered_pos_df$target_child_id, three_sym_filtered_pos_df$stem, three_sym_filtered_pos_df$utterance_id))
names(three_counts_pasted_targetid_uttid_stem_sym)[names(three_counts_pasted_targetid_uttid_stem_sym) == "paste(three_sym_filtered_pos_df$target_child_id, three_sym_filtered_pos_df$stem, three_sym_filtered_pos_df$utterance_id)"] <- "wordstem"
three_counts_pasted_targetid_uttid_stem_separate_sym <- three_counts_pasted_targetid_uttid_stem_sym %>% separate(wordstem, c("target_child_id", "stem", "utterance_id"))
length(unique(three_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id)) #38
unique(three_counts_pasted_targetid_uttid_stem_separate_sym$stem) #ADDED ALL THE SYMS NOW
three_counts_pasted_targetid_uttid_stem_separate_sym <- three_counts_pasted_targetid_uttid_stem_separate_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate'|stem=='cooperate'|stem=='share'|stem=='agree'|stem=='crash'|stem=='rhyme')
#|stem=='operate'|stem=='share'|stem=='agree'|stem=='crash'|stem=='rhyme'
length(unique(three_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id)) #36
length(three_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id) #401 #this number may change depending on if there are any errors in the db or filtering process.
#we dont really use this so we're not going to save it for now.
#write.csv(three_counts_pasted_targetid_uttid_stem_separate, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\three_counts_pasted_targetid_uttid_stem_separate.csv")
#arranging by target_id helps when merging columns.
three_counts_sym <- three_counts_sym %>% arrange(target_child_id)
length(unique(three_counts_sym$target_child_id)) #38

length(three_counts_sym$form) #439, because this only includes counts for words that were produced by the child.Can we use this to plot data for people that have more than one count?
length(three_sym_and_id$form) #4066

three_full_sym <- merge(three_counts_sym, three_sym_and_id, all = TRUE)
three_full_sym <- three_full_sym %>% arrange(target_child_id)
#we dont really need sex, corpus, and utt id here.
three_full_sym$target_child_sex <-NULL
three_full_sym$corpus_name <- NULL
three_full_sym$utterance_id <- NULL

length(unique(three_full_sym$form)) #107
length(unique(three_full_sym$target_child_id)) #38
length(three_full_sym$form) #4316

#now go in and change NAs for age to 3 and NAs for count to 0
three_full_sym$count[is.na(three_full_sym$count)] <- 0
three_full_sym$target_child_age[is.na(three_full_sym$target_child_age)] <- 3
sum(three_full_sym$count) #453 matches with three_sym_filtered_pos_df - this df doesn't count tokens, so we count the rows. 1 row = one token.
write.csv(three_full_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/three_full_sym_v3.csv")

#we select our ALL symmetricals here. 
filter_by_stem_sym <- three_full_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate'|stem=='cooperate'|stem=='share'|stem=='agree'|stem=='crash'|stem=='rhyme')
length(filter_by_stem_sym$target_child_id) #2808 rows
length(unique(filter_by_stem_sym$target_child_id)) #38

#collapsed by stem + the number of counts for each stem
three_child_sum_sym <- aggregate(filter_by_stem_sym$count, by=list(filter_by_stem_sym$stem), sum)
write.csv(three_child_sum_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/collapsed_stem_counts_for_three_v3.csv")

three_child_sum_sym_all <- aggregate(three_full_sym$count, by=list(three_full_sym$stem), sum)


#every stem and their count for each child.
three_child_all_stems_per_child_sym <- filter_by_stem_sym %>% group_by(target_child_id,stem) %>%
  summarize(tokens = sum(count))
write.csv(three_child_all_stems_per_child_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/three_child_all_stems_per_child_sym_v3.csv")

#test2 <- filter_by_stem %>% group_by(stem) %>%
#summarize(num_chi = )


#only_attach <- test %>% filter(stem == 'attach')
#checking <- as.data.frame(only_attach$target_child_id[only_attach$tokens > 0])
#table(checking) #this works but how to scale it up?

three_child_all_stems_per_child_no_zeros_sym <- three_child_all_stems_per_child_sym

three_child_all_stems_per_child_no_zeros_sym <- three_child_all_stems_per_child_no_zeros_sym %>% filter(tokens != 0)
length(unique(three_child_all_stems_per_child_no_zeros_sym$target_child_id)) #36
#eliminating the zeros worked!
verb_pairs_for_three_child_sheet_sym <- three_child_all_stems_per_child_no_zeros_sym %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))
write.csv(verb_pairs_for_three_child_sheet_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/verb_pairs_for_three_child_sheet_sym_v3.csv")

length(unique(three_child_all_stems_per_child_no_zeros_sym$target_child_id)) #36

#Extracting corpus information so we can exclude atypical children
exclusion_three_info_sym <- three_sym_df_trimmed %>% filter(utterance_id %in% three_counts_pasted_targetid_uttid_stem_separate_sym$utterance_id) 
length(unique(exclusion_three_info_sym$stem)) #19 out of how many that we search for?
unique(exclusion_three_info_sym$stem) #nouns and empty stems
exclusion_three_info_sym_final <- exclusion_three_info_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate'|stem=='cooperate'|stem=='share'|stem=='agree'|stem=='crash'|stem=='rhyme')
length(unique(exclusion_three_info_sym_final$stem)) #16
#good this is the one we need to pull frames!
length(exclusion_three_info_sym_final$target_child_id) #406
length(unique(exclusion_three_info_sym_final$target_child_id)) #36

exclusion_three_info_sym_final$target_child_id %in% three_child_all_stems_per_child_no_zeros_sym$target_child_id
exclusion_three_info_sym_final <- exclusion_three_info_sym_final %>% arrange(target_child_id)
exclusion_three_info_sym_final <- exclusion_three_info_sym_final %>% filter(pos == 'v' | pos == 'part')
length(exclusion_three_info_sym_final$target_child_id) #310 should be the same when we add the sum of counts for no zeros three.
sum(three_child_all_stems_per_child_no_zeros_sym$tokens) #310 Yup they match.

#length(unique(exclusion_three_info_sym_final$stem)) #15
#unique(exclusion_three_info_sym_final$stem)
#length(unique(three_child_all_stems_per_child_no_zeros_sym$stem)) #16
#unique(three_child_all_stems_per_child_no_zeros_sym$stem)

length(unique(exclusion_three_info_sym_final$target_child_id))#36

write.csv(exclusion_three_info_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/exclusion_three_info_sym_final_v3.csv")
#check the corpra in this df
unique(exclusion_three_info_sym_final$corpus_name)

#tokens per corpus
corpra_tokens_three_sym <- exclusion_three_info_sym_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = length(corpus_name))
write.csv(corpra_tokens_three_sym,"/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/tokens-from-each-corpus-three-sym_v3.csv")
#do the verb pair tokens add up to the number of tokens in the corpora?
  #verb pair = 310 and the corpora tokens = 310
sum(verb_pairs_for_three_child_sheet_sym$tokens)
sum(corpra_tokens_three_sym$tokens)

#number of children in each corpus
num_of_children_in_each_corpus_three_sym <- exclusion_three_info_sym_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = length(unique(target_child_id)))
write.csv(num_of_children_in_each_corpus_three_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/num-of-children-in-each-corpus-three-sym_v3.csv")

#token frequency per child to check for outliers
token_freq_per_child_three_sym <- exclusion_three_info_sym_final %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = (length(stem)), corpus_name = (corpus_name))
token_freq_per_child_three_sym_final <- unique(token_freq_per_child_three_sym)
write.csv(token_freq_per_child_three_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/three/updated-count/token-freq-per-child-three-sym-final_v3.csv")

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
#checking how many children came up in the search.
length(unique(four_year_olds_tokens_sym_df$target_child_id))#64

#trim the database by selecting the columns we are interested in
four_sym_df_trimmed <- select(four_year_olds_tokens_sym_df, 'target_child_id', 'corpus_name', 'target_child_age',
                              'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')

library(plyr)
four_sym_df_trimmed$stem <- revalue(four_sym_df_trimmed$stem, c("operate"="cooperate")) #this worked.
#renaming columns
names(four_sym_df_trimmed)[names(four_sym_df_trimmed) == "part_of_speech"] <- "pos"
names(four_sym_df_trimmed)[names(four_sym_df_trimmed) == "gloss"] <- "form"
four_sym_df_trimmed

#filter the df by part (past participle??) and v (verb)
four_sym_filtered_pos_df <- four_sym_df_trimmed %>% filter(pos == 'v' | pos == 'part')

unique(four_sym_filtered_pos_df$stem) #operate change to cooperate
length(unique(four_sym_filtered_pos_df$stem)) #21

length(four_sym_filtered_pos_df$target_child_id) #580
#now we want to know what words don't appear in the db
four_sym_not_in_db <- sym_list %>% filter(!form %in% four_sym_filtered_pos_df$form)
four_sym_not_in_db_unique <- as.data.frame(unique(four_sym_not_in_db$stem))
write.csv(four_sym_not_in_db, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/four_sym_not_in_db_v3.csv")

length(four_sym_filtered_pos_df$target_child_id) #580
length(unique(four_sym_filtered_pos_df$target_child_id)) #58

#making df of unique ids in four year olds
ids_for_fours_sym <- as.data.frame(unique(four_sym_filtered_pos_df$target_child_id))
names(ids_for_fours_sym)[names(ids_for_fours_sym) == "unique(four_sym_filtered_pos_df$target_child_id)"] <- "target_child_id"
length(unique(ids_for_fours_sym$target_child_id)) #58 unique ids
length(sym_list$stem) #107
# generate 90 (because our sym_list has 90 rows) instances of an ID (n # of participants x f # of forms )
# we are trying to create a dataframe that has 2,970 rows (33 * 90)
four_many_ids_sym <- ids_for_fours_sym %>% slice(rep(1:n(), each = 107))
length(four_many_ids_sym$target_child_id) #6206
length(unique(sym_list$stem))#29
sym_list

#generate 33 instances of each symmetrical - because we want the ids and sym words to have the same # of rows before
#we merge them together. #33 is the number of participants we have.
n = 58
fours_many_syms <- do.call("rbind", replicate(n, sym_list, simplify = FALSE))
length(fours_many_syms$form) #6206

# Merge many IDS with many_syms
four_sym_and_id <- cbind(fours_many_syms, target_child_id = four_many_ids_sym$target_child_id)
length(four_sym_and_id$target_child_id) #6206

#the number of participants differ from the original code "child-production-of-symmetricals"
#because we searched for nouns and adjectives in that code. Here we focus on verbs. 
#So the total number of children searched between 3 and 4 should be the same.

#replacing age with 4
four_sym_filtered_pos_df$target_child_age <-  replace(four_sym_filtered_pos_df$target_child_age,
                                                      four_sym_filtered_pos_df$target_child_age >= 48.00 &
                                                        four_sym_filtered_pos_df$target_child_age <= 59.99, 4)
length(four_sym_filtered_pos_df$target_child_id)#580
#you can use four_sym_filtered_pos_df to extract sentence frames for four year olds Using utterance_id
#saving

# We are doing this to get a count for the words that are not~ recorded. So that when we merge with
# the words from sym_list all the words not produced, receive an NA, which we later change to 0.
#library(plyr)
#four_sym_filtered_pos_df$stem <- revalue(four_sym_filtered_pos_df$stem, c("operate"="cooperate")) #this worked.

detach(package:plyr)
four_counts_sym <- four_sym_filtered_pos_df %>% group_by(form, target_child_id, target_child_age, target_child_sex,corpus_name,utterance_id) %>%
  summarize(count = sum(unique(length(form))))
length(four_counts_sym$form) #563

four_counts_sym_form <- four_sym_filtered_pos_df %>% group_by(form) %>%
  summarize(count = sum(unique(length(form))))
write.csv(four_counts_sym_form, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/four_counts_sym_form_v3.csv")

four_counts_sym_stem <- four_sym_filtered_pos_df %>% group_by(stem) %>%
  summarize(count = sum(unique(length(stem))))

write.csv(four_counts_sym_stem, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/four_counts_sym_stem_v3.csv")

#checking we still have the same amount of children.
length(unique(four_counts_sym$target_child_id)) #58

#this could be useful later on.
four_counts_pasted_targetid_uttid_stem_sym <- as.data.frame(paste(four_sym_filtered_pos_df$target_child_id, four_sym_filtered_pos_df$stem, four_sym_filtered_pos_df$utterance_id))
names(four_counts_pasted_targetid_uttid_stem_sym)[names(four_counts_pasted_targetid_uttid_stem_sym) == "paste(four_sym_filtered_pos_df$target_child_id, four_sym_filtered_pos_df$stem, four_sym_filtered_pos_df$utterance_id)"] <- "wordstem"
four_counts_pasted_targetid_uttid_stem_separate_sym <- four_counts_pasted_targetid_uttid_stem_sym %>% separate(wordstem, c("target_child_id", "stem", "utterance_id"))
length(unique(four_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id)) #58
unique(four_counts_pasted_targetid_uttid_stem_separate_sym$stem) #ADDED ALL THE SYMS NOW
four_counts_pasted_targetid_uttid_stem_separate_sym <- four_counts_pasted_targetid_uttid_stem_separate_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate'|stem=='cooperate'|stem=='share'|stem=='agree'|stem=='crash'|stem=='rhyme')
#|stem=='operate'|stem=='share'|stem=='agree'|stem=='crash'|stem=='rhyme'
length(unique(four_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id)) #57
length(four_counts_pasted_targetid_uttid_stem_separate_sym$target_child_id) #535 #this number may change depending on if there are any errors in the db or filtering process.
#we dont really use this so we're not going to save it for now.
#write.csv(four_counts_pasted_targetid_uttid_stem_separate, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\four_counts_pasted_targetid_uttid_stem_separate.csv")
#arranging by target_id helps when merging columns.
four_counts_sym <- four_counts_sym %>% arrange(target_child_id)
length(unique(four_counts_sym$target_child_id)) #58

length(four_counts_sym$form) #563, because this only includes counts for words that were produced by the child.Can we use this to plot data for people that have more than one count?
length(four_sym_and_id$form) #6206

four_full_sym <- merge(four_counts_sym, four_sym_and_id, all = TRUE)
four_full_sym <- four_full_sym %>% arrange(target_child_id)
#we dont really need sex, corpus, and utt id here.
four_full_sym$target_child_sex <-NULL
four_full_sym$corpus_name <- NULL
four_full_sym$utterance_id <- NULL

length(unique(four_full_sym$form)) #107
four_full_sym<- four_full_sym %>% filter(form %in% sym_list$form)
length(unique(four_full_sym$form)) #107
unique(four_full_sym$stem)
length(unique(four_full_sym$target_child_id)) #58
length(four_full_sym$form) #6467

#now go in and change NAs for age to 3 and NAs for count to 0
four_full_sym$count[is.na(four_full_sym$count)] <- 0
four_full_sym$target_child_age[is.na(four_full_sym$target_child_age)] <- 4
sum(four_full_sym$count) #579 matches with four_sym_filtered_pos_df - this df doesn't count tokens, so we count the rows. 1 row = one token.
write.csv(four_full_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/four_full_sym_v3.csv")

#we select our ALL symmetricals here. 
filter_by_stem_sym <- four_full_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate'|stem=='cooperate'|stem=='share'|stem=='agree'|stem=='crash'|stem=='rhyme')
length(filter_by_stem_sym$target_child_id) #2808 rows
length(unique(filter_by_stem_sym$target_child_id)) #58

#collapsed by stem + the number of counts for each stem
four_child_sum_sym <- aggregate(filter_by_stem_sym$count, by=list(filter_by_stem_sym$stem), sum)
write.csv(four_child_sum_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/collapsed_stem_counts_for_four_v3.csv")

four_child_sum_sym_all <- aggregate(four_full_sym$count, by=list(four_full_sym$stem), sum)


#every stem and their count for each child.
four_child_all_stems_per_child_sym <- filter_by_stem_sym %>% group_by(target_child_id,stem) %>%
  summarize(tokens = sum(count))
write.csv(four_child_all_stems_per_child_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/four_child_all_stems_per_child_sym_v3.csv")

#test2 <- filter_by_stem %>% group_by(stem) %>%
#summarize(num_chi = )


#only_attach <- test %>% filter(stem == 'attach')
#checking <- as.data.frame(only_attach$target_child_id[only_attach$tokens > 0])
#table(checking) #this works but how to scale it up?

four_child_all_stems_per_child_no_zeros_sym <- four_child_all_stems_per_child_sym

four_child_all_stems_per_child_no_zeros_sym <- four_child_all_stems_per_child_no_zeros_sym %>% filter(tokens != 0)
length(unique(four_child_all_stems_per_child_no_zeros_sym$target_child_id)) #57
#eliminating the zeros worked!
verb_pairs_for_four_child_sheet_sym <- four_child_all_stems_per_child_no_zeros_sym %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))
write.csv(verb_pairs_for_four_child_sheet_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/verb_pairs_for_four_child_sheet_sym_v3.csv")

length(unique(four_child_all_stems_per_child_no_zeros_sym$target_child_id)) #57

#Extracting corpus information so we can exclude atypical children
exclusion_four_info_sym <- four_sym_df_trimmed %>% filter(utterance_id %in% four_counts_pasted_targetid_uttid_stem_separate_sym$utterance_id) 
length(unique(exclusion_four_info_sym$stem)) #18 out of how many that we search for?
unique(exclusion_four_info_sym$stem) #nouns and empty stems
exclusion_four_info_sym_final <- exclusion_four_info_sym %>% filter(stem=='attach'|stem=='connect'|stem=='fight'|stem=='meet'|stem=='touch'|stem=='kiss'|stem=='match'|stem=='trade'|stem=='join'|stem=='marry'|stem=='hug'|stem=='separate'|stem=='cooperate'|stem=='share'|stem=='agree'|stem=='crash'|stem=='rhyme')
length(unique(exclusion_four_info_sym_final$stem)) #16
#good this is the one we need to pull frames!
length(exclusion_four_info_sym_final$target_child_id) #540
length(unique(exclusion_four_info_sym_final$target_child_id)) #57

exclusion_four_info_sym_final$target_child_id %in% four_child_all_stems_per_child_no_zeros_sym$target_child_id
exclusion_four_info_sym_final <- exclusion_four_info_sym_final %>% arrange(target_child_id)
exclusion_four_info_sym_final <- exclusion_four_info_sym_final %>% filter(pos == 'v' | pos == 'part')
length(exclusion_four_info_sym_final$target_child_id) #310 should be the same when we add the sum of counts for no zeros four.
sum(four_child_all_stems_per_child_no_zeros_sym$tokens) #310 Yup they match.

#length(unique(exclusion_four_info_sym_final$stem)) #15
#unique(exclusion_four_info_sym_final$stem)
#length(unique(four_child_all_stems_per_child_no_zeros_sym$stem)) #16
#unique(four_child_all_stems_per_child_no_zeros_sym$stem)

length(unique(exclusion_four_info_sym_final$target_child_id))#36

write.csv(exclusion_four_info_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/exclusion_four_info_sym_final_v3.csv")
#check the corpra in this df
unique(exclusion_four_info_sym_final$corpus_name)

#tokens per corpus
corpra_tokens_four_sym <- exclusion_four_info_sym_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = length(corpus_name))
write.csv(corpra_tokens_four_sym,"/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/tokens-from-each-corpus-four-sym_v3.csv")
#do the verb pair tokens add up to the number of tokens in the corpora?
#verb pair = 310 and the corpora tokens = 310
sum(verb_pairs_for_four_child_sheet_sym$tokens)
sum(corpra_tokens_four_sym$tokens)

#number of children in each corpus
num_of_children_in_each_corpus_four_sym <- exclusion_four_info_sym_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = length(unique(target_child_id)))
write.csv(num_of_children_in_each_corpus_four_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/num-of-children-in-each-corpus-four-sym_v3.csv")

#token frequency per child to check for outliers
token_freq_per_child_four_sym <- exclusion_four_info_sym_final %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = (length(stem)), corpus_name = (corpus_name))
token_freq_per_child_four_sym_final <- unique(token_freq_per_child_four_sym)
write.csv(token_freq_per_child_four_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/four/updated-count/token-freq-per-child-four-sym-final_v3.csv")

#*** Combined child child production of symmetricals ***


#combined verb-pairs
combined_three_four_verb_pair_tokens_sym <- rbind(verb_pairs_for_four_child_sheet_sym,verb_pairs_for_three_child_sheet_sym)
sum(combined_three_four_verb_pair_tokens_sym$tokens) #936

combined_three_four_verb_pair_tokens_summed_sym <- combined_three_four_verb_pair_tokens_sym %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = sum(num_chi))
write.csv(combined_three_four_verb_pair_tokens_summed_sym, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/combined/updated-count/combined-three-four-verb-pair-tokens-summed-sym_v3.csv")
sum(combined_three_four_verb_pair_tokens_summed_sym$tokens) #936

sum(four_child_all_stems_per_child_no_zeros_sym$tokens) #535
length(exclusion_four_info_sym_final$target_child_id) #535

#combined tokens per copra
combined_corpra_tokens_sym <- rbind(corpra_tokens_three_sym, corpra_tokens_four_sym)
combined_corpra_tokens_sym_final <- combined_corpra_tokens_sym %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = sum(tokens))
write.csv(combined_corpra_tokens_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/combined/updated-count/combined-corpra-tokens-sym-final_v3.csv")

#combined number of children in each corpus
combined_num_of_children_in_each_corpus_sym <- rbind(num_of_children_in_each_corpus_three_sym,num_of_children_in_each_corpus_four_sym)
combined_num_of_children_in_each_corpus_sym_final <- combined_num_of_children_in_each_corpus_sym %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = sum(num_chi))
write.csv(combined_num_of_children_in_each_corpus_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/combined/updated-count/combined-num-of-children-in-each-corpus-sym-final_v3.csv")
sum(combined_num_of_children_in_each_corpus_sym_final$num_chi) #93

#combined sym exclusion info
combined_exclusion_syms <- rbind(exclusion_three_info_sym_final, exclusion_four_info_sym_final)
write.csv(combined_num_of_children_in_each_corpus_sym_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/symmetricals/child/combined/updated-count/combined_num_of_children_in_each_corpus_sym_final_v3.csv")
length(combined_exclusion_syms$target_child_id) #936

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
getwd()#

#importing the foil table
foil_list <- read.csv(file = "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Data/foil_list.csv", header = TRUE)
foil_list$unique_stems <- NULL
#how many unique foil stems do we have in total?
length(unique(foil_list$stem)) #40

#childesdb search of foils
three_year_olds_tokens_foil_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = foil_list$form
)

#checking how many children came up in the search.
length(unique(three_year_olds_tokens_foil_df$target_child_id)) #was 82, now 91 with the addition of foils for share and crash.

#trim the database by selecting the columns we are interested in
three_foils_df_trimmed <- select(three_year_olds_tokens_foil_df, 'target_child_id', 'corpus_name', 'target_child_age',
                                 'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')

#rename columns
names(three_foils_df_trimmed)[names(three_foils_df_trimmed) == "part_of_speech"] <- "pos"
names(three_foils_df_trimmed)[names(three_foils_df_trimmed) == "gloss"] <- "form"
three_foils_df_trimmed

#filter the df by part (past participle) and v (verb)
three_foils_filtered_pos_df <- three_foils_df_trimmed %>% filter(pos == 'v' | pos == 'part')
length(three_foils_filtered_pos_df$target_child_id) #was 2037, now 5338
#now we want to know what words don't appear in the db
three_foils_not_in_db <- foil_list %>% filter(!form %in% three_foils_filtered_pos_df$form)
#saving
write.csv(three_foils_not_in_db, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/three_foils_not_in_db_v5.csv")

length(three_foils_filtered_pos_df$target_child_id) #was 2781, now 5338
length(unique(three_foils_filtered_pos_df$target_child_id)) #was 77, now 86

#making df of unique ids in three year olds
ids_for_threes_foils <- as.data.frame(unique(three_foils_filtered_pos_df$target_child_id))
names(ids_for_threes_foils)[names(ids_for_threes_foils) == "unique(three_foils_filtered_pos_df$target_child_id)"] <- "target_child_id"
length(unique(ids_for_threes_foils$target_child_id)) #86 unique ids

# generate 119 (because our sym_list has 119 rows) instances of an ID (n # of participants x f # of forms )
# we are trying to create a dataframe that has 8,925 rows (75 * 119)
length(foil_list$stem) #160
three_many_ids_foils <- ids_for_threes_foils %>% slice(rep(1:n(), each = 214))
length(three_many_ids_foils$target_child_id) #13760

#generate 73 instances of each symmetrical - because we want the ids and sym words to have the same # of rows before
#we merge them together.
length(unique(three_foils_filtered_pos_df$target_child_id)) #86
n = 94
threes_many_foils <- do.call("rbind", replicate(n, foil_list, simplify = FALSE))
length(threes_many_foils$form) #13760

# Merge many IDS with many syms
three_foils_and_id <- cbind(threes_many_foils, target_child_id = three_many_ids_foils$target_child_id)
length(three_foils_and_id$target_child_id) #13760

#the number of participants differ from the original code "child-production-of-foils"
#because we searched for nouns and adjectives in that code. Here we focus on verbs. 
#So the total number of children searched between 3 and 4 should be the same.

#replacing age with 3
three_foils_filtered_pos_df$target_child_age <-  replace(three_foils_filtered_pos_df$target_child_age,
                                                         three_foils_filtered_pos_df$target_child_age >= 36.00 &
                                                           three_foils_filtered_pos_df$target_child_age <= 47.99, 3)
length(three_foils_filtered_pos_df$target_child_id) #5338
#you can use three_foils_filtered_pos_df to extract sentence frames for three year olds Using utterance_id
#saving
#save.image("~/GitHub/childesr-corpus-analysis/symmetricals/children/child-production-of-symmetricals-shortened-environment.RData")

# We are doing this to get a count for the words that are not~ recorded. So that when we merge with
# the words from foil_list all the words not produced, receive an NA, which we later change to 0.
detach(package:plyr)
three_counts_foils <- three_foils_filtered_pos_df %>% group_by(form, target_child_id, target_child_age, target_child_sex,corpus_name,utterance_id) %>%
  summarize(count = sum(unique(length(form)))) #2010
length(three_counts_foils$form)#was 2735, now 5241
#checking we still have the same amount of children.
length(unique(three_counts_foils$target_child_id)) #86

#this could be useful later on.
three_counts_pasted_targetid_uttid_stem_foils <- as.data.frame(paste(three_foils_filtered_pos_df$target_child_id, three_foils_filtered_pos_df$stem, three_foils_filtered_pos_df$utterance_id))
names(three_counts_pasted_targetid_uttid_stem_foils)[names(three_counts_pasted_targetid_uttid_stem_foils) == "paste(three_foils_filtered_pos_df$target_child_id, three_foils_filtered_pos_df$stem, three_foils_filtered_pos_df$utterance_id)"] <- "wordstem"
three_counts_pasted_targetid_uttid_stem_separate_foils <- three_counts_pasted_targetid_uttid_stem_foils %>% separate(wordstem, c("target_child_id", "stem", "utterance_id"))
length(unique(three_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id)) #73
three_counts_pasted_targetid_uttid_stem_separate_foils <- three_counts_pasted_targetid_uttid_stem_separate_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle'
                                                                                                                            | stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                                                                                              stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                                                                                              stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy' | stem== 'stick' | stem== 'glue' | stem== 'hang'|stem=='give'|stem=='take'|stem=='keep'|stem=='bring'|stem=='hog'|stem=='grab'|stem=='hit'|stem=='slam'|stem=='smash'|stem=='knock'|
                                                                                                                              stem=='jam'|stem=='pinch'|stem=='flip'|stem=='turn'|stem=='spin'|stem=='poke'|stem=='press'|stem=='put'|stem=='place'|stem=='set'|stem=='lay'|stem=='wrap'|stem=='discover'|stem=='discriminate')
#|stem=='take'|stem=='keep'|stem=='bring'|stem=='hog'|stem=='grab'|stem=='hit'|stem=='slam'|stem=='smash'|stem=='knock'
#stem=='jam'|stem=='pinch'|stem=='flip'|stem=='turn'|stem=='spin'|stem=='poke'|stem=='press'|stem=='put'|stem=='place'|stem=='set'|stem=='lay'|stem=='wrap'|stem=='discover'|stem=='discriminate'
#change the stems to foil stems!
length(unique(three_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id)) #86
length(three_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #5338 this number might change depending on whether or not there are errors in the filter process!

#arranging by target_id helps when merging columns.
three_counts_foils <- three_counts_foils %>% arrange(target_child_id)
length(unique(three_counts_foils$target_child_id)) #86

length(three_counts_foils$form) #5241  because this only includes counts for words that were produced by the child.Can we use this to plot data for people that have more than one count?
length(three_foils_and_id$form) #13760

three_full_foils <- merge(three_counts_foils, three_foils_and_id, all = TRUE)
three_full_foils <- three_full_foils %>% arrange(target_child_id)
#we dont really need sex, corpus, and utt id here.
three_full_foils$target_child_sex <-NULL
three_full_foils$corpus_name <- NULL
three_full_foils$utterance_id <- NULL

length(unique(three_full_foils$form)) #160
length(unique(three_full_foils$target_child_id)) #86
length(three_full_foils$form) #17907

#now go in and change NAs for age to 3 and NAs for count to 0
three_full_foils$count[is.na(three_full_foils$count)] <- 0
three_full_foils$target_child_age[is.na(three_full_foils$target_child_age)] <- 3
sum(three_full_foils$count) #5338 matches with three_foils_filtered_pos_df? - this df doesn't count tokens, so we count the rows which are equal to one token.
unique(three_full_foils$stem)

#we would select our top 12 foils here, but im going to include all of them.
filter_by_stem_foils <- three_full_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle' |
                                                      stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                      stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                      stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy' | stem== 'stick' | stem== 'glue' | stem== 'hang'|stem=='give'|stem=='take'|stem=='keep'| 
                                                      stem =='bring'|stem=='hog'|stem=='grab'|stem=='hit'|stem=='slam'|stem=='smash'|stem=='knock'|
                                                      stem=='jam'|stem=='pinch'|stem=='flip'|stem=='turn'|stem=='spin'|stem=='poke'|stem=='press'|stem=='put'|stem=='place'|stem=='set'|stem=='lay'|stem=='wrap'|stem=='discover'|stem=='discriminate')
length(filter_by_stem_foils$target_child_id) #17907 rows
length(unique(filter_by_stem_foils$target_child_id)) #86
unique(filter_by_stem_foils$stem)

#*** IDK why 'bring' is being filtered out here. Extra space in your foil list ***#

#collapsed by stem + the number of counts for each stem
three_child_sum_foils <- aggregate(filter_by_stem_foils$count, by=list(filter_by_stem_foils$stem), sum)
write.csv(three_child_sum_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/three_child_sum_foils_v5.csv")

#every stem and their count for each child.
three_child_all_stems_per_child_foils <- filter_by_stem_foils %>% group_by(target_child_id,stem) %>%
  summarize(tokens = sum(count))

#test2 <- filter_by_stem %>% group_by(stem) %>%
#summarize(num_chi = )


#only_attach <- test %>% filter(stem == 'attach')
#checking <- as.data.frame(only_attach$target_child_id[only_attach$tokens > 0])
#table(checking) #this works but how to scale it up?

three_child_all_stems_per_child_no_zeros_foils <- three_child_all_stems_per_child_foils
length(unique(three_child_all_stems_per_child_no_zeros_foils$target_child_id))#86
three_child_all_stems_per_child_no_zeros_foils <- three_child_all_stems_per_child_no_zeros_foils %>% filter(tokens != 0)
length(unique(three_child_all_stems_per_child_no_zeros_foils$target_child_id))#86
length(three_child_all_stems_per_child_no_zeros_foils$target_child_id) #684
#eliminating the zeros worked!
verb_pairs_for_three_child_sheet_foils <- three_child_all_stems_per_child_no_zeros_foils %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))
length(unique(three_child_all_stems_per_child_no_zeros_foils$target_child_id)) #86
#save now or later
write.csv(verb_pairs_for_three_child_sheet_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/verb_pairs_for_three_child_sheet_foils_v5.csv")

#Extracting corpus information so we can exclude atypical children
exclusion_three_info_foils <- three_foils_df_trimmed %>% filter(utterance_id %in% three_counts_pasted_targetid_uttid_stem_separate_foils$utterance_id) 
length(unique(exclusion_three_info_foils$stem)) #37
unique(exclusion_three_info_foils$stem) #nouns and empty stems
exclusion_three_info_foils_final <- exclusion_three_info_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle' |
                                                                            stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                                            stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                                            stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy' | stem== 'stick' | stem== 'glue' | stem== 'hang'|stem=='give'|stem=='take'|stem=='keep'|stem=='bring'|stem=='hog'|stem=='grab'|stem=='hit'|stem=='slam'|stem=='smash'|stem=='knock'|
                                                                            stem=='jam'|stem=='pinch'|stem=='flip'|stem=='turn'|stem=='spin'|stem=='poke'|stem=='press'|stem=='put'|stem=='place'|stem=='set'|stem=='lay'|stem=='wrap'|stem=='discover'|stem=='discriminate')
length(unique(exclusion_three_info_foils_final$stem)) #35 - because we deleted all the foils with zeros.
unique(exclusion_three_info_foils_final$stem)
#good this is the one we need to pull frames!
length(exclusion_three_info_foils_final$target_child_id) #5413
length(unique(exclusion_three_info_foils_final$target_child_id)) #86

exclusion_three_info_foils_final$target_child_id %in% three_child_all_stems_per_child_no_zeros_foils$target_child_id
exclusion_three_info_foils_final <- exclusion_three_info_foils_final %>% arrange(target_child_id)
exclusion_three_info_foils_final <- exclusion_three_info_foils_final %>% filter(pos == 'v' | pos == 'part')
length(exclusion_three_info_foils_final$target_child_id) #5338 should be the same when we add the sum of counts for no zeros three.
sum(three_child_all_stems_per_child_no_zeros_foils$tokens) #5015 Yup they match.
length(unique(exclusion_three_info_foils_final$target_child_id))#was 77, now 86
length(unique(exclusion_three_info_foils_final$stem)) #was 26, now 34

write.csv(exclusion_three_info_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/exclusion_three_info_foils_final_v5.csv")
#check the corpra in this df
unique(exclusion_three_info_foils_final$corpus_name)

#tokens per corpus
corpra_tokens_three_foils <- exclusion_three_info_foils_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = length(corpus_name))
write.csv(corpra_tokens_three_foils,"/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/tokens-from-each-corpus-three-foils_v5.csv")

#number of children in each corpus
num_of_children_in_each_corpus_three_foils <- exclusion_three_info_foils_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = length(unique(target_child_id)))
write.csv(num_of_children_in_each_corpus_three_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/num-of-children-in-each-corpus-three-foils_v5.csv")

#token frequency per child to check for outliers
token_freq_per_child_three_foils <- exclusion_three_info_foils_final %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = (length(stem)), corpus_name = (corpus_name))
token_freq_per_child_three_foils_final <- unique(token_freq_per_child_three_foils)
write.csv(token_freq_per_child_three_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/three/updated-count/token-freq-per-child-three-foils-final_v5.csv")
length(unique(token_freq_per_child_three_foils_final$target_child_id)) #was 77, now 86

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

length(unique(four_year_olds_tokens_foils_df$target_child_id)) #75

four_foils_df_trimmed <- select(four_year_olds_tokens_foils_df, 'target_child_id', 'corpus_name', 'target_child_age',
                                'gloss', 'part_of_speech','target_child_sex','stem','utterance_id')

#renaming columns
names(four_foils_df_trimmed)[names(four_foils_df_trimmed) == "part_of_speech"] <- "pos"
names(four_foils_df_trimmed)[names(four_foils_df_trimmed) == "gloss"] <- "form"
four_foils_df_trimmed

#filter the df by part (participle) and v (verb)
four_foils_filtered_pos_df <- four_foils_df_trimmed %>% filter(pos == 'v' | pos == 'part')
length(four_foils_filtered_pos_df$target_child_id) #6088
#now we want to know what words don't appear in the db
four_foils_not_in_db <- foil_list %>% filter(!form %in% four_foils_filtered_pos_df$form)
write.csv(four_foils_not_in_db, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/four_foils_not_in_db_v5.csv")

length(four_foils_filtered_pos_df$target_child_id) #6088
length(unique(four_foils_filtered_pos_df$target_child_id)) #72

#making df of unique ids in four year olds
ids_for_fours_foils <- as.data.frame(unique(four_foils_filtered_pos_df$target_child_id))
names(ids_for_fours_foils)[names(ids_for_fours_foils) == "unique(four_foils_filtered_pos_df$target_child_id)"] <- "target_child_id"
length(unique(ids_for_fours_foils$target_child_id)) #72 unique ids with duplicate children from the 3 year old foil db

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
four_many_ids_foils <- ids_for_fours_foils %>% slice(rep(1:n(), each = 214))
length(four_many_ids_foils$target_child_id) #was 8092 (68 * 119), now (160 * 72) 11520

#generate 53 (for each unique id) instances of sym_words # i think this (the extra rows) gets filtered out later. 
length(unique(ids_for_fours_foils$target_child_id)) #67 unique ids with duplicate children from the 3 year old foil db
n = 73
four_many_foils <- do.call("rbind", replicate(n, foil_list, simplify = FALSE))
length(four_many_foils$form) #was 8680, now 11520

# Merge many IDS with many syms
four_foils_and_id <- cbind(four_many_foils, target_child_id = four_many_ids_foils$target_child_id)
length(four_foils_and_id$target_child_id) #11520

#replacing age with 4

four_foils_filtered_pos_df$target_child_age <-  replace(four_foils_filtered_pos_df$target_child_age,
                                                        four_foils_filtered_pos_df$target_child_age >= 48.00 &
                                                          four_foils_filtered_pos_df$target_child_age < 59.99, 4)

length(four_foils_filtered_pos_df$target_child_id) #6088
#you can use four_foils_filtered_pos_df to extract sentence frames for three year olds Using utterance_id
#No! Remove the repeats first!

#saving
#save.image("~/GitHub/childesr-corpus-analysis/symmetricals/children/child-production-of-symmetricals-shortened-environment.RData")

#remove comments below if you want to exclude the three year olds that eventually age into the 4-year-old db
#four_foils_filtered_pos_df <- four_foils_filtered_pos_df %>% filter(!target_child_id %in% ids_for_threes_foils$target_child_id)
length(unique(four_foils_filtered_pos_df$target_child_id)) #73
#now you can use it to extract frames.

detach(package:plyr)
four_counts_foils <- four_foils_filtered_pos_df %>% group_by(form, target_child_id, target_child_age, target_child_sex,corpus_name,utterance_id) %>%
  summarize(count = sum(unique(length(form)))) #2175 rows
#checking we still have the same amount of children.
length(unique(four_counts_foils$target_child_id)) #72

#this could be useful later on.
four_counts_pasted_targetid_uttid_stem_foils <- as.data.frame(paste(four_foils_filtered_pos_df$target_child_id, four_foils_filtered_pos_df$stem, four_foils_filtered_pos_df$utterance_id))
names(four_counts_pasted_targetid_uttid_stem_foils)[names(four_counts_pasted_targetid_uttid_stem_foils) == "paste(four_foils_filtered_pos_df$target_child_id, four_foils_filtered_pos_df$stem, four_foils_filtered_pos_df$utterance_id)"] <- "wordstem"
four_counts_pasted_targetid_uttid_stem_separate_foils <- four_counts_pasted_targetid_uttid_stem_foils %>% separate(wordstem, c("target_child_id", "stem", "utterance_id"))
length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #2225
#four_counts_pasted_targetid_uttid_stem_separate <- four_counts_pasted_targetid_uttid_stem_separate %>% filter(utterance_id != 704599)
#four_counts_pasted_targetid_uttid_stem_separate <- four_counts_pasted_targetid_uttid_stem_separate %>% filter(target_child_id != 2591)
four_counts_pasted_targetid_uttid_stem_separate_foils <- four_counts_pasted_targetid_uttid_stem_separate_foils %>% filter(stem=='cover'|stem=='insert'|stem=='tie'|stem=='drop'|stem=='knot'|stem=='punch'|stem=='kick'|stem=='attack'|stem=='tickle'|stem=='pet'|stem=='stroke'|stem=='pull'|stem=='lick'|stem=='bite'|stem=='invite'|stem=='celebrate'|stem=='adopt'|stem=='choose'|stem=='check'|stem=='teach'|stem=='greet'|stem=='push'|stem=='tap'|stem=='hold'|stem=='bump'|stem=='sell'|stem=='buy'|stem=='bit' | stem== 'stick' | stem== 'glue' | stem== 'hang'|stem=='give'|stem=='take'|stem=='keep'|stem=='bring'|stem=='hog'|stem=='grab'|stem=='hit'|stem=='slam'|stem=='smash'|stem=='knock'
                                                                                                                          |stem=='jam'|stem=='pinch'|stem=='flip'|stem=='turn'|stem=='spin'|stem=='poke'|stem=='press'|stem=='put'|stem=='place'|stem=='set'|stem=='lay'|stem=='wrap'|stem=='discover'|stem=='discriminate')

length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id)
four_counts_pasted_targetid_uttid_stem_separate_foils[four_counts_pasted_targetid_uttid_stem_separate_foils$stem=='bit', "stem"] <- "bite"
#fixing the 'bit' stem issue
length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #6088

#USE THIS TO GET THE FRAMES! AND FIND DUPLICATES!
#length(unique(four_counts_pasted_targetid_uttid_stem_separate2$target_child_id))
length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #6088 (THIS SHOULD ADD UP TO THE LENGTH OF EXCLUSION FOR FINAL AND THE SUM OF NO ZEROS DF) it doesn't because this includes words
#not in the top 12. Does it match?

#length(unique(four_counts_pasted_targetid_uttid_stem_separate2$target_child_id))
length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #6088

#arranging by target_id helps when merging columns.
four_counts_foils <- four_counts_foils %>% arrange(target_child_id)
length(unique(four_counts_foils$target_child_id)) #72

length(four_counts_foils$form) #5907
length(four_foils_and_id$form)#11520

#its okay for the rows/columns here to not be equal
four_full_foils <- merge(four_counts_foils, four_foils_and_id, all = TRUE) #Original 
length(four_full_foils$target_child_id) #15927

length(unique(four_full_foils$form)) #160
#deals with the random "thought" that was inserted.
four_full_foils <- four_full_foils %>% filter(form %in% foil_list$form)
length(unique(four_full_foils$form)) #160
length(unique(four_full_foils$stem)) #40
length(unique(four_full_foils$target_child_id)) #72

four_full_foils$target_child_sex <-NULL
four_full_foils$corpus_name <- NULL
four_full_foils$utterance_id <- NULL

length(unique(four_full_foils$form)) #160
length(unique(four_full_foils$target_child_id)) #72
length(four_full_foils$form) #15927

#now go in and change nas for age to 4 and nas for count to 0

four_full_foils$count[is.na(four_full_foils$count)] <- 0
four_full_foils$target_child_age[is.na(four_full_foils$target_child_age)] <- 4
sum(four_full_foils$count) #6088. Matches four_year_olds_tokendf?, ** 
#taking out think here #1129
#four_foils_filtered_pos_df <- four_foils_filtered_pos_df %>% filter(stem != 'think')

#we select our top 12 foils here - going to select all of the foils for now.
filter_by_stem_four_foils <- four_full_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle' |
                                                          stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                          stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                          stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy' | stem == 'bit' | stem== 'stick' | stem== 'glue' | stem== 'hang'|stem=='give'|stem=='take'|stem=='keep'|stem=='bring'|stem=='hog'|stem=='grab'|stem=='hit'|stem=='slam'|stem=='smash'|stem=='knock'|
                                                          stem=='jam'|stem=='pinch'|stem=='flip'|stem=='turn'|stem=='spin'|stem=='poke'|stem=='press'|stem=='put'|stem=='place'|stem=='set'|stem=='lay'|stem=='wrap'|stem=='discover'|stem=='discriminate')
length(filter_by_stem_four_foils$target_child_id) #15927
length(unique(filter_by_stem_four_foils$target_child_id)) #72
# need to add another constraint. 
# the count must be greater than 0 to be in filter_by_stem_four

#collapsed by stem + the number of counts for each stem
four_child_sum_foils <- aggregate(filter_by_stem_four_foils$count, by=list(filter_by_stem_four_foils$stem), sum)
write.csv(four_child_sum_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/four_child_sum_foils_v5.csv")


#every stem and their count for each child.
detach(package:plyr)
four_child_all_stems_per_child_foils <- filter_by_stem_four_foils %>% group_by(target_child_id,stem) %>%
  summarize(tokens = sum(count))
write.csv(four_child_all_stems_per_child_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/four_child_all_stems_per_child_foils_v5.csv")


#this could be useful, once we fix the one extra token for 2291 bite
sum(four_child_all_stems_per_child_foils$tokens) #6088
#test2 <- filter_by_stem %>% group_by(stem) %>%
#summarize(num_chi = )
#write.csv(four_child_all_stems_per_child_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/four_child_all_stems_per_child_foils_v2.csv")

#only_attach <- test %>% filter(stem == 'attach')
#checking <- as.data.frame(only_attach$target_child_id[only_attach$tokens > 0])
#table(checking) #this works but how to scale it up?

four_child_all_stems_per_child_no_zeros_foils <- four_child_all_stems_per_child_foils
length(four_child_all_stems_per_child_foils$stem) #2880
#this is key!
four_child_all_stems_per_child_no_zeros_foils <- four_child_all_stems_per_child_no_zeros_foils %>% filter(tokens != 0)
four_child_all_stems_per_child_no_zeros_foils <- four_child_all_stems_per_child_no_zeros_foils %>% arrange(target_child_id)
#write.csv(four_child_all_stems_per_child_no_zeros_foils, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\foils\\children\\four_child_all_stems_per_child_no_zeros_foils.csv")
length(unique(four_child_all_stems_per_child_no_zeros_foils$target_child_id)) #70
sum(four_child_all_stems_per_child_no_zeros_foils$tokens) #6088
#eliminating the zeros worked!
verb_pairs_for_four_child_sheet_foils <- four_child_all_stems_per_child_no_zeros_foils %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))
length(unique(four_child_all_stems_per_child_no_zeros_foils$target_child_id)) #72
sum(four_child_all_stems_per_child_no_zeros_foils$tokens) #6088
write.csv(verb_pairs_for_four_child_sheet_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/verb_pairs_for_four_child_sheet_foils_v5.csv")

#Extracting corpus information so we can exclude atypical children
#this is why we get the wrong number of rows. separate_foils is wrong.
exclusion_four_info_foils <- four_foils_df_trimmed %>% filter(utterance_id %in% four_counts_pasted_targetid_uttid_stem_separate_foils$utterance_id) 
length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #6088
length(unique(exclusion_four_info_foils$stem)) #38
unique(exclusion_four_info_foils$stem) #nouns and empty stems
exclusion_four_info_foils_final <- exclusion_four_info_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle' |
                                                                          stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                                          stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                                          stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy'| stem =='bit' | stem== 'stick' | stem== 'glue' | stem== 'hang'|stem=='give'|stem=='take'|stem=='keep'|stem=='bring'|stem=='hog'|stem=='grab'|stem=='hit'|stem=='slam'|stem=='smash'|stem=='knock'|
                                                                          stem=='jam'|stem=='pinch'|stem=='flip'|stem=='turn'|stem=='spin'|stem=='poke'|stem=='press'|stem=='put'|stem=='place'|stem=='set'|stem=='lay'|stem=='wrap'|stem=='discover'|stem=='discriminate')
length(unique(exclusion_four_info_foils_final$stem)) #37 (although its really 23 because of 'bit')
#good this is the one we need to pull frames!
length(exclusion_four_info_foils_final$target_child_id) #6170

exclusion_four_info_foils_final$target_child_id %in% four_child_all_stems_per_child_no_zeros_foils$target_child_id
length(exclusion_four_info_foils_final$target_child_id) #6170
exclusion_four_info_foils_final <- exclusion_four_info_foils_final %>% arrange(target_child_id)
exclusion_four_info_foils_final <- exclusion_four_info_foils_final %>% filter(pos == 'v' | pos == 'part')
length(exclusion_four_info_foils_final$target_child_id) #6088
exclusion_four_info_foils_final[exclusion_four_info_foils_final$stem=='bit', "stem"] <- "bite"
length(exclusion_four_info_foils_final$target_child_id) #2225
write.csv(exclusion_four_info_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/exclusion_four_info_foils_final_v5.csv")
#check the corpra in this df
unique(exclusion_four_info_foils_final$corpus_name)

length(unique(exclusion_four_info_foils_final$target_child_id)) #72
length(unique(filter_by_stem_four_foils$target_child_id)) #72
missing_child_for_four_foils <- exclusion_four_info_foils_final %>% filter(!utterance_id %in% four_counts_pasted_targetid_uttid_stem_separate_foils$utterance_id)
length(unique(missing_child_for_four_foils$target_child_id))
#length(exclusion_four_info_foils_final$target_child_id) #1128

length(exclusion_four_info_foils_final$target_child_id) #6088
length(unique(exclusion_four_info_foils_final$target_child_id)) #72
length(unique(exclusion_four_info_foils_final$stem)) #35
length(unique(exclusion_four_info_foils_final$pos)) #2
exclusion_four_foil_missing <- exclusion_four_info_foils_final %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = length(stem))

sum(four_child_all_stems_per_child_no_zeros_foils$tokens) #2225
length(unique(four_child_all_stems_per_child_no_zeros_foils$target_child_id)) #68
length(unique(four_child_all_stems_per_child_no_zeros_foils$stem)) #26
zeros_four_foil_missing <- four_child_all_stems_per_child_no_zeros_foils %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = sum(tokens))

length(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #2225
length(unique(four_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id)) #68
length(unique(four_counts_pasted_targetid_uttid_stem_separate_foils$stem)) #26

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
write.csv(corpra_tokens_four_foils,"/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/tokens-from-each-corpus-four-foils-v5.csv")
sum(corpra_tokens_four_foils$tokens) #9526
length(exclusion_four_info_foils_final$target_child_id) #9526

#number of children in each corpus
num_of_children_in_each_corpus_four_foils <- exclusion_four_info_foils_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = length(unique(target_child_id)))
write.csv(num_of_children_in_each_corpus_four_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/num_of_children_in_each_corpus_four_foils_v5.csv")
length(unique(exclusion_four_info_foils_final$target_child_id)) #72

#token frequency per child to check for outliers
token_freq_per_child_four_foils <- exclusion_four_info_foils_final %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = (length(stem)), corpus_name = (corpus_name))
token_freq_per_child_four_foils_final <- unique(token_freq_per_child_four_foils)
write.csv(token_freq_per_child_four_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/token_freq_per_child_four_foils_final_v5.csv")

#to check for outliers for each stem.
token_freq_per_child_four_foils_v2 <- exclusion_four_info_foils_final %>% dplyr::group_by(target_child_id, stem) %>%
  dplyr::summarize(tokens = (length(stem)))
token_freq_per_child_four_foils_final_v2 <- unique(token_freq_per_child_four_foils_v2)
write.csv(token_freq_per_child_four_foils_final_v2, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/four/updated-count/token_freq_per_child_four_foils_final_v5_new.csv")

#combined verb-pairs
combined_three_four_verb_pair_tokens_foils <- rbind(verb_pairs_for_four_child_sheet_foils,verb_pairs_for_three_child_sheet_foils)

combined_three_four_verb_pair_tokens_summed_foils <- combined_three_four_verb_pair_tokens_foils %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = sum(num_chi))
sum(combined_three_four_verb_pair_tokens_summed_foils$tokens) #19013
write.csv(combined_three_four_verb_pair_tokens_summed_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/combined_three_four_verb_pair_tokens_summed_foils_v5.csv")

sum(four_child_all_stems_per_child_no_zeros_foils$tokens) #1915
length(exclusion_four_info_foils_final$target_child_id) #1915

#combined tokens per copra
combined_corpra_tokens_foils <- rbind(corpra_tokens_three_foils, corpra_tokens_four_foils)
combined_corpra_tokens_foils_final <- combined_corpra_tokens_foils %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = sum(tokens))
write.csv(combined_corpra_tokens_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/combined_corpra_tokens_foils_final_v5.csv")
sum(combined_corpra_tokens_foils_final$tokens) #11426

#combined number of children in each corpus
combined_num_of_children_in_each_corpus_foils <- rbind(num_of_children_in_each_corpus_three_foils,num_of_children_in_each_corpus_four_foils)
combined_num_of_children_in_each_corpus_foils_final <- combined_num_of_children_in_each_corpus_foils %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = sum(num_chi))
write.csv(combined_num_of_children_in_each_corpus_foils_final, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/combined_num_of_children_in_each_corpus_foils_final_v5.csv")

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

length(unique(combined_num_of_children_in_syms_unique$target_child_id)) #79
length(combined_num_of_children_in_syms_unique$target_child_id) #79
write.csv(combined_num_of_children_in_syms_unique, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/combined_num_of_children_in_syms_unique_v5.csv")


#foils

combined_num_of_children_in_foils_redux <-rbind(token_freq_per_child_four_foils_final, token_freq_per_child_three_foils_final)
combined_num_of_children_in_foils_redux_unique <- as.data.frame(unique(combined_num_of_children_in_foils_redux$target_child_id))
names(combined_num_of_children_in_foils_redux_unique)[names(combined_num_of_children_in_foils_redux_unique) == "unique(combined_num_of_children_in_foils_redux$target_child_id)"] <- "target_child_id"
length(unique(combined_num_of_children_in_foils_redux_unique$target_child_id)) #143
length(combined_num_of_children_in_foils_redux_unique$target_child_id) #143
write.csv(combined_num_of_children_in_foils_redux_unique, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/combined_num_of_children_in_foils_redux_unique_v5.csv")

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
write.csv(combined_exclusion_foils, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/combined_exclusion_foil_sym_v3.csv")
length(combined_exclusion_foils$target_child_id) # NEW 3630, WAS 2844, V2 is 4262

id_for_combined_foils_give <- combined_exclusion_foils %>% filter(stem == 'give')
write.csv(id_for_combined_foils_give, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/id_for_combined_foils_give_v3.csv")
#look up the frames
three_ut_foils <- get_utterances(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36,48)
)

#combined outlier check for each participant and stem
combined_token_freq_per_child_foils_v2 <- combined_exclusion_foils %>% dplyr::group_by(target_child_id,stem) %>%
  dplyr::summarize(tokens = (length(stem)))
#combined_token_freq_per_child_foils_final_v2 <- (unique(combined_token_freq_per_child_foils_v2))
write.csv(combined_token_freq_per_child_foils_v2, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/combined_token_freq_per_child_foils_v2.csv")

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

#checking frames for all stems in combined_exclusion_for_foils
ut_id_for_combined_exclusion_foils <- select(combined_exclusion_foils, "utterance_id", "stem", "target_child_id")
length(ut_id_for_combined_exclusion_foils$utterance_id) #4262

child_three_four_ut_foils <- get_utterances(
  collection = "Eng-NA",
  age = c(36,60)
)

child_three_four_ut_foils_filtered_give <- child_three_four_ut_foils %>% filter(id %in% id_for_combined_foils_give$utterance_id)
child_three_four_ut_foils_filtered_give_trimmed <- select(child_three_four_ut_foils_filtered_give,'id','target_child_id','target_child_age','gloss','stem','type','part_of_speech','corpus_name','speaker_role','utterance_order')
write.csv(child_three_four_ut_foils_filtered_give_trimmed, "/Users/abimaelh/Documents/GitHub/child-verb-matching-study/Output/foils/child/combined/updated-count/child_three_four_ut_foils_filtered_give_trimmed.csv")

ut_id_from_combined_exclusion_foils_not_in_utterance_df <- ut_id_for_combined_exclusion_foils %>% filter(!utterance_id %in% child_three_four_ut_foils$id)
#notthing shows up because there are duplicates in the frames! That's why the utterance df and token df lengths do not match.