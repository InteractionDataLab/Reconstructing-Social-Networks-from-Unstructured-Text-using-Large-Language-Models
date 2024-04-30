library(qgraph)
library(igraph)
library(visNetwork)
library(pROC)

plotNetwork <- function(Gsub, layout='default',maine='', coms=NULL, hide = NULL, label = '', colors = FALSE){
  
  
  if (is.null(coms)){
    com <- cluster_walktrap(Gsub)
    V(Gsub)$coms = com$membership
    V(Gsub)$color <- com$membership+1
  } else if(colors == FALSE) {
    #color = fish(n = length(unique(coms)), option = "Koumansetta_rainfordi", end = 0.9)
    color = pals::alphabet(length(unique(coms)))
    
    names(color) = unique(coms)
    
    if (!is.null(hide))
    {
      color[hide] = NA 
    }
    
    V(Gsub)$color <- color[coms]
  }
  else{
    V(Gsub)$color = coms
  }
  
  vertex.frame.color <- V(Gsub)$color
  
  size <- abs(strength(Gsub, loops=T))
  if (max(size, na.rm=T)>0){ 
    size <- 30 * size / max(size, na.rm=T)
  } else {
    size <- 1
  }
  
  if (is.null(E(Gsub)$weight)){
    width <- 1
    edge.color <- NA 
    l <- layout_with_fr
    
  } else{
    weight <- abs(E(Gsub)$weight)
    width <- .01 + 2 * weight / max(weight, na.rm=T)
    
    if ("hidden" %in% names(edge.attributes(Gsub)))
      edge.color <- ifelse(E(Gsub)$hidden == TRUE, rgb(alpha = 0, red = 0, green = 0, blue = 0), 'grey')
    else
      edge.color = 'grey'
    
    e <- get.edgelist(Gsub, names=F)
    e <- cbind(e, E(Gsub)$weight)
    l <- qgraph.layout.fruchtermanreingold(e,
                                           vcount=vcount(Gsub),
                                           area=1*(vcount(Gsub)^2),
                                           repulse.rad=5*(vcount(Gsub)^2))
    
  }
  
  if (layout!='default'){
    l <- layout
  }
  
  plot(Gsub,
       layout=l,
       edge.width = width,
       edge.color=edge.color,
       vertex.label = label,
       vertex.label.color = "black",
       vertex.label.cex = 0.7,
       vertex.size = 3 * sqrt(size),
       edge.curved=0.1,
       vertex.frame.color = vertex.frame.color, 
       edge.arrow.width = 0.1,
       edge.arrow.size = 0.1,
       #main=maine,
       #main.cex = 0.3,
       col='gray'
  )
  
  title(main = maine , cex.main = 3.5)
  #legend(x=-1.5, y=-1.1, legend = names(color), pch=21,
  #       col="#777777", pt.bg=color, pt.cex=2, cex=.8, bty="n", ncol=3)
  
}


########################################################

#Interactive Network

library(visNetwork)

visPlot = function(subgraph, communities = rep(1, length(V(subgraph))), color = "auto", nodesize = 10, edgewidth = 0, title="", 
                   textsize = nodesize, layout = "layout_nicely", directed = TRUE, hidden = rep(FALSE, ecount(subgraph)), seed = 123)
{
  nodes <- data.frame(id = V(subgraph)$name, name = V(subgraph)$label, group = communities)
  nodes$font.size<-textsize 
  if (!color == "auto")
    nodes$color = communities
  nodes$font.color = "grey"
  nodes$font.face = "calibri"
  nodes$label = nodes$name
  nodes$value = nodesize
  edges <- data.frame(get.edgelist(subgraph))
  edges$width = edgewidth
  
  hidden[hidden == FALSE] = "lightblue"
  hidden[hidden == TRUE] = "white"
  edges$color = hidden
  
  colnames(edges)<-c("from","to","width", "color")
  plt = visNetwork(nodes, edges, height = "1000px", width = "100%",main = title)%>%
    visIgraphLayout(layout = layout) %>% visLayout(randomSeed = seed) %>% 
    visPhysics(solver = "repulsion", hierarchicalRepulsion = "nodeDistance") %>%
    visOptions(highlightNearest = TRUE) %>%
    visNodes(scaling = list(min = 10, max = 80)) %>%
    visEdges(smooth = T) %>% #, color = list(color = "lightblue", highlight = "blue")) %>% 
    visGroups(groupname = "2", color = list(background = "lightgray",border = "black")) %>%
    visInteraction(keyboard = T,
                   dragNodes = T, 
                   dragView = T, 
                   zoomView = T)
  
  if (directed == TRUE)
    plt = plt %>% visEdges(arrows ="to")
  
  return(plt)
}

#################################################################

#Finding edges to hide in a network

calculate_hidden = function(g, key = "")
{
  vec = ends(g, es = E(g))
  list = c() 
  
  for (i in 1:nrow(vec))
  {
    if (vec[i,1] %in% key | vec[i,2] %in% key)
      list = c(list, TRUE)
    else
      list = c(list, FALSE)
  }
  
  return(list)
}

####################################################################

#Standard Error

se = function(list)
{
  return(sd(list, na.rm = TRUE)/sqrt(length(list[!is.na(list)])))
}

#####################################################################


process_team_names = function(team_name)
{
  t = trimws(team_name) #Removes trailing whitespaces
  t = tolower(team_name) #Makes lowercase
  t = gsub("_", " ", t) #Substitutes underscore with space
  t = gsub("-", " ", t) #Substitutes hyphen with space
  t = gsub(",", " ", t) #Substitutes comma with space
  
  #t = gsub("team", " ", t)
  #t = gsub("igem", " ", t)
  
  return(trimws(t)) #Returns removing trailing whitespaces 
}

compute_ROC = function(g1, g2)
{
  A1 <- as_adjacency_matrix(g1, attr = "weight")
  A2 <- as_adjacency_matrix(g2)
  
  x <- as.numeric(A1)
  y <- as.numeric(A2)
  
  r <- roc(y ~ x)
  
  print(r)
  
  xx <- r$specificities
  yy <- r$sensitivities
  
  plot(1-xx,yy, 
       xlab='1-Specificity',
       ylab='Sensitivity',
       main=paste0('AUC = ',signif(r$auc,3), '\np < 2.2e-16'),
       cex=0.5,
       col='steelblue',
       type='o', lwd=3)
  abline(0,1,lty=2)
}


populate_network_props = function(g, dir)
{
  degI = log(degree(g, mode = "in", normalized = TRUE),10)
  degI[is.infinite(degI)] = min(degI[!is.infinite(degI)])
  
  degO = log(degree(g, mode = "out", normalized = TRUE),10)
  degO[is.infinite(degO)] = min(degO[!is.infinite(degO)])
  
  deg = log(degree(g, mode = "all", normalized = TRUE), 10)
  deg[is.infinite(deg)] = min(deg[!is.infinite(deg)])
  
  bet = log(betweenness(g, directed = dir, normalized = TRUE),10)
  bet[is.infinite(bet)] = min(bet[!is.infinite(bet)])
  
  eig = log(eigen_centrality(g, directed = dir)$vector,10)
  eig[is.infinite(eig)] = min(eig[!is.infinite(eig)])
  
  auth = log(authority_score(g)$vector,10)
  auth[is.infinite(auth)] = min(auth[!is.infinite(auth)])
  
  core = log(coreness(g), 10)
  core[is.infinite((core))] = min(core[!is.infinite(core)])
  
  trans = transitivity(g, type = "local") #log(coreness(g), 10)
  trans[is.infinite((trans))] = min(trans[!is.infinite(trans)])
  
  df = data.frame(user = V(g)$name, degI = (degI-mean(degI, na.rm = TRUE))/sd(degI, na.rm = TRUE), 
                  degO = (degO-mean(degO, na.rm = TRUE))/sd(degO, na.rm = TRUE), 
                  deg = (deg-mean(deg, na.rm = TRUE))/sd(deg, na.rm = TRUE),
                  bet = (bet-mean(bet, na.rm = TRUE))/sd(bet, na.rm = TRUE), 
                  eig = (eig-mean(eig, na.rm = TRUE))/sd(eig, na.rm = TRUE), 
                  auth = (auth-mean(auth, na.rm = TRUE))/sd(auth, na.rm = TRUE),
                  core = (core-mean(core, na.rm = TRUE))/sd(core, na.rm = TRUE),
                  trans = (trans-mean(trans, na.rm = TRUE)/sd(trans, na.rm = TRUE)))
  
  #df = data.frame(degI_mean = mean(degI, na.rm = TRUE), degI_se = se(degI), degO_mean = mean(degO, na.rm = TRUE), 
  #                degO_se = se(degO), bet_mean = mean(bet, na.rm = TRUE), bet_se = se(bet), eig_mean = mean(eig, na.rm =  TRUE), 
  #                eig_se = se(eig), auth_mean = mean(auth, na.rm = TRUE), auth_se = se(auth))
  
  return(df)
}


