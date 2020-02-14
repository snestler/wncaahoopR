#' Circle Assist Network
#'
#' @description This function produces an assist network visualization for a team for a single game (or collection of games).
#' @usage circle_assist_net(.data, team, highlight_player, highligh_color, three_weights, threshold, message, listing)
#'
#' @param .data play-by-play data frame returned from w_get_pbp_game function 
#' @param team Team to create network for. Can be bare home or away variable from play-by-play object or quoted team name.
#' @param highlight_player Name of player to highlight in assist network. `NA` yields full team assist
#' network with no player highlighting. Default = `NA`.
#' @param highlight_color Color of player links to be highlighted. `NA` if ```highlight_player``` is `NA`.
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
#' @importFrom circlize CELL_META
#' @importFrom circlize chordDiagram
#' @importFrom circlize circos.axis
#' @importFrom circlize circos.text
#' @importFrom circlize circos.track
#' @importFrom circlize get.all.sector.index
#' @importFrom dplyr arrange
#' @importFrom dplyr group_by
#' @importFrom dplyr pull
#' @importFrom dplyr rename
#' @importFrom dplyr summarise
#' @importFrom dplyr lag
#' @importFrom igraph authority_score
#' @importFrom igraph degree
#' @importFrom igraph E
#' @importFrom igraph graph.data.frame
#' @importFrom igraph hub_score
#' @importFrom igraph page_rank
#' @importFrom igraph transitivity
#' @importFrom magrittr %>% 
#' @export
circle_assist_net <- function(.data, team, node_col = NULL, highlight_player = NA, 
                              highlight_color = NA, three_weights = TRUE, 
                              threshold = 0, message = NA, listing = TRUE) {
  pbp_data <- .data
  ### Error Testing
  if(is.null(pbp_data)) {
    stop("pbp_data is missing with no default")
  }
  
  if(is.null(substitute(team)) == TRUE) {
    stop("You need to supply a team (quoted team name, home, or away)")
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
  
  games <- unique(pbp_data$game_id)
  ast <- grep("Assisted", pbp_data$description)
  x <- pbp_data[ast, ]
  
  x <- x[x$whichScored == team, ]
  
  dropVar <- "home_favored_by"
  
  x <- x[, -c(which(names(x) %in% dropVar))]
  
  x <- na.omit(x)
  
  x$shot <- regmatches(x$description, 
                       regexpr(".*(?=\\smade\\s)", x$description, perl = TRUE))
  
  x$ast <- regmatches(x$description, 
                      regexpr("(?<=\\sby\\s).*(?=\\.)", x$description, perl = TRUE))

  ### Adjust Three Point Weights in Network
  x$weights <- 1
  if(three_weights){
    threes <- grep("Three Point", x$description)
    x$weights[threes] <- 1.5
  }
  
  network <- x %>% 
    group_by(ast, shot) %>% 
    summarize(num = sum(weights))
  
  network$a_freq <- network$num/sum(network$num)
  network <- filter(network, a_freq > 0)
  player_asts <-
    sapply(unique(unlist(network[, c("ast", "shot")])), function(name) { 
      sum(network$a_freq[network$ast == name | network$shot == name]) 
    })
  
  ### Team Ast/Shot Distributions
  ast_data <- aggregate(a_freq ~ ast, data = network, sum)
  shot_data <- aggregate(a_freq ~ shot, data = network, sum)
  
  ### Create Temporary Directed Network For Stat Aggregation
  net <- graph.data.frame(network, directed = T)
  deg <- degree(net, mode = "all")
  E(net)$weight <- network$num
  
  ### Compute Clustering Coefficient
  clust_coeff <- round(transitivity(net, type = "global"), 3)
  
  ### Compute Page Rank
  pagerank <- sort(page_rank(net)$vector, decreasing = T)
  
  ### Compute Hub Score
  hubscores <- sort(hub_score(net, scale = F)$vector, decreasing = T)
  
  ### Compute Authority Scores
  auth_scores <- sort(authority_score(net, scale = F)$vector, decreasing = T)
  
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
  network <- filter(network, shot %in% keep, ast %in% keep)
  
  
  
    labs <- as.character(network$num)
  
  text <- " Assist Graph"  
    
  plot_title <-
    ifelse(is.na(message), paste0(text_team, ifelse(three_weights, " Weighted", ""), text), text)
  if(length(unique(x$game_id)) == 1) {
    plot_title <- paste(plot_title, format(as.Date(x$date[1]), "%B %d, %Y"), sep = "\n")
  }
  
  players <- group_by(network, ast) %>%
    summarise("count" = sum(num)) %>%
    rename("player" = ast) %>%
    rbind(
      group_by(network, shot) %>%
        summarise("count" = sum(num)) %>%
        rename("player" = shot)
    ) %>%
    group_by(player) %>%
    summarise("count" = sum(count)) %>%
    arrange(desc(count)) %>%
    pull(player)
  
  if(is.na(highlight_player)) {
    chordDiagram(network[,-4], order = players,
                           grid.col = gg_color_hue(length(players)),
                           annotationTrack = "grid",
                           preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(network))))))
  }else {
    cols <- rep("grey", length(players))
    if(!gsub("Jr.", "Jr", highlight_player) %in% players) {
      warning(paste("Selected highlight_player not in given network.",
                    "Please select a player from the following list"))
      return(sort(players))
    }
    cols[grepl(highlight_player, players)] <- highlight_color
    borders <- filter(network, ast == highlight_player) %>%
      select(ast, shot) %>%
      mutate(graphical = 1)
    chordDiagram(network[,-4], order = players,
                           grid.col = cols,
                           link.lwd = 2,
                           link.border = borders,
                           annotationTrack = "grid",
                           preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(network))))))
  }
  
  for(si in get.all.sector.index()) {
    circos.axis(h = "top", labels.cex = 0.3, sector.index = si, track.index = 2)
  }
  par(cex = 0.6)
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
                          facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  }, bg.border = NA)
  par(cex = 1)
  title(paste("\n\n", plot_title))
  
  ### Return Results
  if (listing == TRUE) {
    return(list("clust_coeff" = clust_coeff, "page_ranks" = pagerank,
                "hub_scores" = hubscores, "auth_scores" = auth_scores,
                "ast_freq" = ast_freq, "shot_freq" = shot_freq))
  }
}
