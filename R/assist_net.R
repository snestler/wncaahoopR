#' Assist Network
#'
#' @description This function produces an assist network visualization for a team for a single game (or collection of games).
#' @usage assist_net(.data)
#' 
#' @param .data play-by-play data frame returned from w_get_pbp_game function
#' @param team Team to create network for. Can be bare home or away variable from play-by-play object or quoted team name
#' @param node_col Color of nodes in network. Can be selected but defaults 
#' to primary team color and then a backup if one is not found.
#' @param three_weights Logical indicating whether to give extra weight for assisted three point shots.
#' If TRUE, assisted three-point shots will be given weight 1.5 (as opposed to weight 1). Default = `TRUE`.
#' @param threshold Number between 0-1 indicating minimum percentage of team assists/baskets a player needs to exceed to be included in network. Default = 0.
#' @param message User supplied plot title to overwrite default plot title, if desired. Default = `NA`.
#' @param listing Allows disabling the return of list of network statistics
#' @return List with network statistics
#' \itemize{
#'  \item{"clust_coeff"} - Network Clustering Coefficient
#'  \item{"page_ranks"} - Player Page Ranks in network
#'  \item{"hub_scores"} - Player Hub Scores in network
#'  \item{"auth_scores"} - Player Authority Scores in network
#'  \item{"ast_freq"} - Player percentage of team's assists
#'  \item{"shot_freq"} - Player percenatge of scoring on team's assisted baskets
#'  }
#'  @importFrom dplyr group_by
#'  @importFrom dplyr summarize
#' @export

assist_net <- function(.data, team, node_col = NULL, three_weights = TRUE, 
                       threshold = 0, message = NA, listing = TRUE) {
  
  pbp_data <- .data
  ### Error Testing
  if(is.na(pbp_data)) {
    stop("pbp_data is missing with no default")
  }
  
  if(is.character(substitute(team)) == FALSE) {
    team <- unique(pbp_data[, deparse(substitute(team))])
  }
  
  
  if(is.null(node_col)) {
    node_col <- ncaa_colors$primary_color[ncaa_colors$espn_name == team]
  }
  
  if(length(node_col) == 0) {
    node_col <- "#ff5501"
    message("There were no colors found for the specified team -- defaulting to something nice")
  }
  
  text_team <- dict$ESPN_PBP[dict$ESPN == team]
  text_team <- text_team[!is.na(text_team)]
  
  ### Warnings
  # if(!"ncaahoopR" %in% .packages()) {
  #   ids <- create_ids_df()
  # }
  if(!team %in% ids$team) {
    warning("Invalid team. Please consult the ids data frame for a list of valid teams, using data(ids).")
    return(NULL)
  }
  if(threshold < 0 | threshold > 1) {
    warning("Threshold for display must be between 0 and 1")
    return(NULL)
  }
    opp <- setdiff(c(pbp_data$away, pbp_data$home), text_team)
    
    factor <- 1.25
  
  ### Get Roster
  roster <- try(w_get_roster(team))
  if(class(roster) == "try-error") {
    warning("Unable to get roster. ESPN is updating CBB files. Check back again soon")
    return(NULL)
  }
  roster$name <- gsub("Jr.", "Jr", roster$name)
  games <- unique(pbp_data$game_id)
  ast <- grep("Assisted", pbp_data$description)
  x <- pbp_data[ast, ]
  
  ### Get Ast/Shot from ESPN Play Description
  # splitplay <- function(description) {
  #   tmp <- strsplit(strsplit(description, "Assisted")[[1]], " ")
  #   n1 <- grep("made", tmp[[1]])
  #   n1 <- n1[length(n1)]
  #   n2 <- length(tmp[[2]])
  #   tmp[[2]][n2] <- substring(tmp[[2]][n2], 1, nchar(tmp[[2]][n2]) - 1)
  #   shot_maker <- paste(tmp[[1]][1:(n1-1)], collapse = " ")
  #   assister <- paste(tmp[[2]][3:n2], collapse = " ")
  #   return(list("shot_maker" = shot_maker, "assister" = assister))
  # }
  
  # x <- dplyr::mutate(x, "ast" = NA, "shot" = NA)
  # for(i in 1:nrow(x)) {
  #   play <- splitplay(x$description[i])
  #   x$ast[i] <- play$assister
  #   x$shot[i] <- play$shot_maker
  # }
  
  x$shot <- regmatches(x$description, 
                       regexpr(".*(?=\\smade)", x$description, perl = TRUE))
  
  x$ast <- regmatches(x$description, 
                      regexpr("(?<=by\\s).*(?=\\.)", x$description, perl = TRUE))
  
  ### Get only shots made by the team in question
  x$ast <- gsub("Jr.", "Jr", x$ast)
  x$shot <- gsub("Jr.", "Jr", x$shot)
  x <- x[is.element(x$ast, roster$name), ]
  
  # sets <- 2 * choose(nrow(roster), 2)
  # network <- data.frame("ast" = rep(NA, sets),
  #                       "shot" = rep(NA, sets),
  #                       "num" = rep(NA, sets))
  
  ### Adjust Three Point Weights in Network
  x$weights <- 1
  if(three_weights){
    threes <- grep("Three Point", x$description)
    x$weights[threes] <- 1.5
  }
  
  ### Aggregate Assists
  # for(i in 1:nrow(roster)) {
  #   ast <- roster$name[i]
  #   tmp <- roster[roster$name != ast,]
  #   for(j in 1:nrow(tmp)) {
  #     index <- j + (i - 1) * nrow(tmp)
  #     network$ast[index] <- ast
  #     network$shot[index] <- tmp$name[j]
  #     network$num[index] <- sum(x$weights[x$ast == ast & x$shot == tmp$name[j]])
  #   }
  # }
  
  network <- x %>% 
    group_by(ast, shot) %>% 
    summarize(num = sum(weights))
  
  network$a_freq <- network$num/sum(network$num)
  network <- dplyr::filter(network, a_freq > 0)
  player_asts <-
    sapply(roster$name, function(name) { sum(network$a_freq[network$ast == name | network$shot == name]) })
  
  ### Team Ast/Shot Distributions
  ast_data <- aggregate(a_freq ~ ast, data = network, sum)
  shot_data <- aggregate(a_freq ~ shot, data = network, sum)
  
  ### Create Temporary Directed Network For Stat Aggregation
  net <- igraph::graph.data.frame(network, directed = T)
  deg <- igraph::degree(net, mode = "all")
  igraph::E(net)$weight <- network$num
  
  ### Compute Clustering Coefficient
  clust_coeff <- round(igraph::transitivity(net, type = "global"), 3)
  
  ### Compute Page Rank
  pagerank <- sort(igraph::page_rank(net)$vector, decreasing = T)
  
  ### Compute Hub Score
  hubscores <- sort(igraph::hub_score(net, scale = F)$vector, decreasing = T)
  
  ### Compute Authority Scores
  auth_scores <- sort(igraph::authority_score(net, scale = F)$vector, decreasing = T)
  
  ### Compute Assist Frequency Data
  ast_freq <- ast_data$a_freq
  names(ast_freq) <- ast_data$ast
  
  ### Compute Shot Frequency Data
  shot_freq <- shot_data$a_freq
  names(shot_freq) <- shot_data$shot
  
  ### Create/Plot Undirected Network
  if(max(player_asts) < threshold) {
    warning("Threshold is too large--no players exceed threshold")
    ### Return Results
    return(list("clust_coeff" = clust_coeff, "page_ranks" = pagerank,
                "hub_scores" = hubscores, "auth_scores" = auth_scores,
                "ast_freq" = ast_freq, "shot_freq" = shot_freq))
  }
  keep <- names(player_asts)[player_asts > threshold]
  network <- dplyr::filter(network, shot %in% keep, ast %in% keep)
  
  net <- igraph::graph.data.frame(network, directed = T)
  deg <- igraph::degree(net, mode="all")
  igraph::E(net)$weight <- network$num
  igraph::E(net)$arrow.size <- 0.7
  igraph::E(net)$edge.color <- "white"
  igraph::E(net)$width <- igraph::E(net)$weight * factor
  igraph::V(net)$color <- node_col
  
  labs <- as.character(network$num)
  
  text <- " Assist Graph"
  
  title <- ifelse(is.na(message), paste0(text_team, ifelse(three_weights, " Weighted", ""), text), text)
  if(length(unique(x$game_id)) == 1 & is.na(message)) {
    title <- paste(title, format(as.Date(x$date[1]), "%B %d, %Y"), sep = "\n")
  }
  
  plot(net, vertex.label.color= "black", vertex.label.cex = 1,
       edge.curved = 0.3, edge.label = labs, edge.label.cex = 1.2,
       edge.label.color = "black",
       layout = igraph::layout_in_circle,
       vertex.label.family = "Arial Black")
  
  par(cex = 0.6)
  title(main = paste("\n", title))
  par(cex = 1)
  
  ### Add Text to Network
  text(-1.5, 1.0, paste(ifelse(three_weights, "Weighted ", ""), "Assist Frequency Leader: ",
                        ast_data$ast[which.max(ast_data$a_freq)], " (",
                        round(100 * max(ast_data$a_freq), 1), "%)", sep = ""),
       cex = 0.6)
  text(-1.5, 0.9, paste(ifelse(three_weights, "Weighted ", ""), "(Assisted) Shot Frequency Leader: ",
                        shot_data$shot[which.max(shot_data$a_freq)], " (",
                        round(100 * max(shot_data$a_freq), 1), "%)", sep = ""),
       cex = 0.6)
  text(-1.5, 0.8, paste("PageRank MVP: ", names(which.max(pagerank)), " (",
                        round(max(pagerank), 3), ")", sep = ""), cex = 0.6)
  text(-1.5, 0.7, paste("Hub Score MVP: ", names(which.max(hubscores)), " (",
                        round(max(hubscores), 3), ")", sep = ""), cex = 0.6)
  text(-1.5, 0.6, paste("Authority Score MVP: ", names(which.max(auth_scores)), " (",
                        round(max(auth_scores), 3), ")", sep = ""), cex = 0.6)
  text(-1.5, 0.5, paste("Team Clustering Coefficient: ", clust_coeff, sep = ""),
       cex = 0.6)
  
  if(three_weights){
    text(0, -1.4, cex = 0.7,
         paste("Weighted Assist Network: Assisted 2 point shots are given weight 1, ",
               "Assisted 3 point shots are given weight 1.5", sep = ""))
  }
  
  
  ### Return Results
  if (listing==TRUE) {
    return(list("clust_coeff" = clust_coeff, "page_ranks" = pagerank,
                "hub_scores" = hubscores, "auth_scores" = auth_scores,
                "ast_freq" = ast_freq, "shot_freq" = shot_freq))
  }
}
