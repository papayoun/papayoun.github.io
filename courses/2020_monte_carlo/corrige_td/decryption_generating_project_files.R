rm(list = ls())
library(tidyverse)
source("decryption_utils.R")
my_alphabet <- c(LETTERS, "'", ",", ".", " ")


# Processing the transition matrix ----------------------------------------

# full_text <- read_file("seigneur_anneaux.txt")

# transition_matrix <- full_text %>%
#   get_formatted_text() %>%
#   get_count_matrix(my_alphabet, numerical_add_ = 0)
# 
# 
# # Saving the matrix -------------------------------------------------------
# 
# my_path <- "/home/pierre/Documents/web_page/new_web_site/static/courses/2020_monte_carlo/enonces_td/"
# write.table(transition_matrix,
#             row.names = FALSE, col.names = FALSE,
#             sep = ";", file = paste0(my_path, "transition_matrix.txt"))



# Processing the extracts -------------------------------------------------

extracts <- dir(pattern = "extrait")
set.seed(246) # GRAINE A 246
map(extracts, 
    function(extrait){
      initial_text <- read_file(extrait)
      formatted_text <- get_formatted_text(initial_text)
      # print(extrait)
      # print(str_length(formatted_text))
      # print(formatted_text)
      f_permutation <- sample(1:30, replace = F)
      encrypted_text <- get_f_decryption(formatted_text, f_permutation, my_alphabet)
      write_file(encrypted_text, paste0("crypte_", extrait))
    })
