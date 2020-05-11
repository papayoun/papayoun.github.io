get_formatted_text <- function(input_text_) {
  # Remove break lines
  str_replace(input_text_, "\n" , " ") %>% 
    str_replace("\r" , " ") %>%
    str_replace_all("[0123456789]", " ") %>% 
    str_replace_all("[(;:)]", ", ") %>% 
    str_replace_all("[!?]", ". ") %>% 
    str_replace_all("[+]", " ") %>% 
    stringi::stri_trans_general("Latin-ASCII") %>% 
    toupper() %>% 
    str_replace_all("[^ABCDEFGHIJKLMNOPQRSTUVWXYZ',. ]", " ") %>% 
    str_replace(" ,", ",") %>% 
    str_replace(" [[.]]", ".") %>% 
    str_replace("[[.]],", ".") %>% 
    str_replace(",,", ", ") %>% 
    str_squish() %>% 
    str_trim() %>% 
    return()
}

get_transition_tibble <- function(text_, alphabet_){
  text_ %>% 
    str_extract_all(boundary("character")) %>% 
    unlist() %>% 
    tibble(first_letter = .) %>% 
    mutate(second_letter = lead(first_letter)) %>% 
    slice(-nrow(.)) %>% 
    filter(!(first_letter == " " & second_letter %in% c(",", "."))) %>% 
    group_by(first_letter, second_letter, .drop = TRUE) %>% 
    summarise(n_transition = n()) %>% 
    ungroup() %>% 
    mutate(first_letter = factor(first_letter, levels = alphabet_),
           second_letter = factor(second_letter, levels = alphabet_)) %>% 
    complete(first_letter, second_letter, fill = list(n_transition = 0)) %>% 
    na.omit()
}

get_count_matrix <- function(text_, alphabet_, numerical_add_ = 0){
  transition_tibble <- get_transition_tibble(text_, alphabet_)
  count_matrix <- transition_tibble %>% 
    spread(key = second_letter, value = n_transition) %>% 
    select(-first_letter) %>% 
    as.matrix()
  colnames(count_matrix) <- NULL
  count_matrix + numerical_add_
}

get_f_decryption <- function(text_, f_permutation_, alphabet_){
  if(length(f_permutation_) != length(alphabet_)){
    stop("alphabet_ must a vector of same size as permutation_")
  }
  characters_vector <- str_extract_all(text_, boundary("character")) %>% unlist()
  map_chr(characters_vector, 
          function(char){
            start_index <- which(alphabet_ == char)
            alphabet_[f_permutation_[start_index]]
          }) %>% 
    paste(collapse = "")
}

get_initial_guess_permutation<- function(text_, model_matrix_, alphabet_){
  letter_occurences <- text_ %>% 
    str_extract_all(boundary("character")) %>% 
    unlist() %>% 
    factor(levels = alphabet_) %>% 
    table() %>% 
    as.numeric() %>% 
    rank(ties.method = "random")
  grammar_occurences <- rowSums(model_matrix_) %>% rank(ties.method = "random")
  map_dbl(letter_occurences, function(let) which(grammar_occurences == let))
}

get_swapped_permutation <- function(permutation_){
  swapped_indexes <- sample(1:length(permutation_),
                            size = 2, replace = TRUE)
  base::replace(permutation_, swapped_indexes, permutation_[rev(swapped_indexes)])
}

get_log_likelihood <- function(decrypted_text_, model_matrix_, 
                               alphabet_){
  text_transition_matrix <- get_count_matrix(decrypted_text_, alphabet_ , 
                                             0)
  sum(text_transition_matrix * log(model_matrix_))
}

get_mcmc_sample <- function(initial_text_, 
                            model_matrix_,
                            alphabet_,
                            iteration_size_, 
                            initial_f_permutation_ = NULL){
  if(is.null(initial_f_permutation_)){
    initial_f_permutation_ <- 1:30
  }
  initial_decryption <- get_f_decryption(initial_text_, 
                                         initial_f_permutation_, alphabet_)
  first_ll <- get_log_likelihood(initial_decryption, model_matrix_, alphabet_ )
  log_likelihoods <- rep(NA, iteration_size_ + 1)
  guessed_text <- rep(NA, iteration_size_ + 1)
  my_permutations_list <- rep(list(rep(NA, length(alphabet_))), 
                              iteration_size_ + 1)
  log_likelihoods[1] <- first_ll
  guessed_text[1] <- initial_decryption
  my_permutations_list[[1]] <- initial_f_permutation_
  for(i in 1:iteration_size_){
    new_permutation <- get_swapped_permutation(my_permutations_list[[i]])
    new_decryption <- get_f_decryption(initial_text_, 
                                       new_permutation, alphabet_)
    new_log_likelihood <- get_log_likelihood(new_decryption, model_matrix_,
                                             alphabet_ )
    u <- runif(1)
    if(log(u) < new_log_likelihood - log_likelihoods[i]){
      guessed_text[i + 1] <- new_decryption
      log_likelihoods[i + 1] <- new_log_likelihood
      my_permutations_list[[i + 1]] <- new_permutation
    }
    else{
      guessed_text[i + 1] <- guessed_text[i]
      log_likelihoods[i + 1] <- log_likelihoods[i]
      my_permutations_list[[i + 1]] <- my_permutations_list[[i]]
    }
  }
  tibble(iteration = 0:iteration_size_,
         log_likelihood = log_likelihoods,
         decrypted_text = guessed_text,
         permutation = my_permutations_list)
}