library(httr)
library(dplyr)
library(pokemon)
library(purrr)
library(rlang)

#' Retrieves evolution data for a specified Pokemon
#'
#' @param pokemon_name Character string with the name of the Pokemon
#'
#' @return A tibble containing evolution data or NULL if retrieval fails
#'
#' @details This function fetches species data and evolution chain information
#' from the PokeAPI, and parses the evolution tree structure.
#'
#' @examples
#' evolution_data <- get_evolution_data("pikachu")
#'
#' @importFrom httr GET http_status content add_headers timeout
#' @importFrom dplyr mutate tibble bind_rows
#' @importFrom purrr map_chr map_int map reduce
#' @importFrom rlang abort inform %||%
#'
get_evolution_data <- function(pokemon_name) {
  # Helper function to get Pokemon species data
  get_pokemon_species <- function(pokemon_name) {
    tryCatch({
      response <- GET(
        paste0("https://pokeapi.co/api/v2/pokemon-species/", tolower(pokemon_name)),
        timeout(10),
        add_headers(`User-Agent` = "EvolutionChainAnalyzer/1.0")
      )
      
      if (http_status(response)$category != "Success") {
        abort(paste("API request failed for", pokemon_name, 
                    "with status:", status_code(response)))
      }
      
      parsed <- content(response, "parsed")
      return(parsed)
    }, error = function(e) {
      inform(paste("Error fetching species data:", e$message))
      return(NULL)
    })
  }
  
  # Helper function to get evolution chain data
  get_evolution_chain <- function(chain_id) {
    tryCatch({
      response <- GET(
        paste0("https://pokeapi.co/api/v2/evolution-chain/", chain_id),
        timeout(10),
        add_headers(`User-Agent` = "EvolutionChainAnalyzer/1.0")
      )
      
      if (http_status(response)$category != "Success") {
        abort(paste("Chain", chain_id, "failed:", status_code(response)))
      }
      
      parsed <- content(response, "parsed")
      return(parsed)
    }, error = function(e) {
      inform(paste("Error fetching chain", chain_id, ":", e$message))
      return(NULL)
    })
  }
  
  # Recursive function to parse evolution tree
  parse_evolution_tree <- function(node) {
    current <- node$species$name
    evolves_to <- node$evolves_to
    
    if (length(evolves_to) == 0) {
      return(tibble(from = character(), to = character()))
    }
    
    base_df <- tibble(
      from = current,
      to = map_chr(evolves_to, ~.x$species$name),
      min_level = map_int(evolves_to, ~.x$evolution_details[[1]]$min_level %||% NA_integer_),
      trigger = map_chr(evolves_to, ~.x$evolution_details[[1]]$trigger$name)
    )
    
    evolved_dfs <- map(evolves_to, parse_evolution_tree)
    
    reduce(evolved_dfs, bind_rows, .init = base_df)
  }
  
  # Main function logic
  species <- get_pokemon_species(pokemon_name)
  if (is.null(species)) return(NULL)
  
  chain_id <- strsplit(species$evolution_chain$url, "/")[[1]][7]
  chain <- get_evolution_chain(chain_id)
  if (is.null(chain)) return(NULL)
  
  evolution_data <- parse_evolution_tree(chain$chain) %>%
    mutate(
      generation = species$generation$name,
      habitat = species$habitat$name %||% "unknown",
      evolution_chain_id = chain_id
    )
  
  return(evolution_data)
}


# Get data ----------------------------------------------------------------
pkmn_info <- pokemon::pokemon %>% 
  select(id, pokemon, gen_id = generation_id, contains("url"))

gen1_pkmn_names <- pkmn_info %>% filter(gen_id == 1) %>% pull(pokemon)

out <- map(gen1_pkmn_names, .f = get_evolution_data, .progress = TRUE)

evolutions <- out %>% 
  bind_rows() %>% 
  select(from, to, evo_id = evolution_chain_id) %>% 
  unique()

dat <- tibble(pokemon = c(evolutions$from, evolutions$to) %>% unique()) %>%
  left_join(pkmn_info, by = "pokemon") %>%
  left_join(evolutions %>% select(from, to, evo_id), by = c("pokemon" = "to")) %>%
  left_join(evolutions %>% select(from, to, evo_id), by = c("pokemon" = "from")) %>%
  mutate(evo_id = coalesce(evo_id.x, evo_id.y), .keep = "unused") %>%
  mutate(evo_id = as.numeric(evo_id)) %>%
  relocate(gen_id, evo_id, contains("url"), .after = everything()) %>%
  arrange(id)

write.csv(x = dat, file = "pokemon/dat_pokemon.csv", row.names = FALSE)
