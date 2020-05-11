
# my_text %>% 
#   str_extract_all(boundary("character")) %>% 
#   unlist() %>% 
#   as_tibble() %>% 
#   count(value) %>% 
#   as.data.frame()


init <- get_initial_guess_permutation(encrypted_text, my_model_matrix, my_alphabet)
get_f_decryption(encrypted_text, init, my_alphabet)



set.seed(123)
true_f <- sample(1:30, size  = 30, replace = FALSE)
true_text <- "Trois Anneaux pour les Rois Elfes sous le ciel, Sept pour les Seigneurs Nains dans leurs demeures de pierre, Neuf pour les Hommes Mortels destinés au trépas, Un pour le Seigneur des Ténèbres sur son sombre trône Dans le Pays de Mordor où s'étendent les Ombres. Un Anneau pour les gouverner tous, Un Anneau pour les trouver, Un Anneau pour les amener tous et dans les ténèbres les lier Au Pays de Mordor où s'étendent les Ombres." %>% 
  get_formatted_text()
encrypted_text <- get_f_decryption(text_ = true_text, f_permutation = true_f, my_alphabet)

essai <- get_mcmc_sample(true_text, my_model_matrix, my_alphabet, 10000)
ggplot(essai) +
  aes(x = iteration, y = log_likelihood) +
  geom_point()
new_dec <- essai$decrypted_text[10000]
init <- get_initial_guess_permutation(new_start, my_model_matrix, my_alphabet)
essai2 <- get_mcmc_sample(new_start, my_model_matrix, my_alphabet, 100)
ggplot(essai2) +
  aes(x = iteration, y = log_likelihood) +
  geom_point()
new_start <- essai$decrypted_text[which.max(essai$log_likelihood)]

