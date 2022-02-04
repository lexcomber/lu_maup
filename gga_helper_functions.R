
#### 1. Declare helper functions and global variables 
## 1.1 Evaluation function
# evaluation functions now positive
how.good.wtR = function(string) {
  sum(eval_data$ScoreR[string] * eval_data$wtR * eval_data$area)}
# make evaluation data - for scale aggregations
make_eval_data = function(g = v200, es.g = g200, fx = NULL) {
  es.g$esID = es.g$gID
  int = st_intersection(g[, "gID"], es.g[, c("esID","ScoreR", "wtR") ])
  int = st_collection_extract(int, "POLYGON") 
  int$area = as.vector(st_area(int))
  int %>% 
    # st_drop_geometry() %>% 
    group_by(gID) %>%
    summarise(wtR = weighted.mean(wtR, area), 
              ScoreR = weighted.mean(ScoreR, area)) -> eval_data
  eval_data$area = as.vector(set_units(st_area(eval_data), ha))
  if(is.null(fx)) {
    eval_data = eval_data
  } else {
    eval_data %>% slice(-fx) -> eval_data
  }
  return(st_drop_geometry(eval_data))
} 


## 1.2 GGA Monitoring function
# see histor.wtR in  small_case_new.R for perfomance plot
## Null history for RStudio
histor.NULL = function(obj) {
  # plot the population
  #eval.xlim = c(round(sum(sort(eval_data$ScoreR) * sort(eval_data$wtR,T)), 0)-1,
  #	round(sum(sort(eval_data$ScoreR) * sort(eval_data$wtR)), 0)+1)
  popn <- obj$population
  itercount <<- itercount + 1
  if (itercount%%100 == 0) cat(itercount, "\t")
}
# declare plot limits for histor.wtR
# eval.xlim = c(round(sum(sort(eval_data$ScoreR) * sort(eval_data$wtR,T)), 0)-10,
#		round(sum(sort(eval_data$ScoreR) * sort(eval_data$wtR)), 0)+10)

## 1.3 Fixing some classes
# get fixed locs
fixed.locs.lu = function(g){
  which(g$lu_class == "Urban" | 
          g$lu_class == "Broadleaf" |
          g$lu_class == "Water" |
          g$lu_class == "Coniferous")}

## 1.4 Results 
# extract best string
best.subset = function(clus) clus$population[which.max(clus$evaluations),]
# get the fixed string for the fixed evaluaton 
get.fixed.string = function(st, fx){
  for (i in fx) {
    bigger.vals.index = which(st >= i)
    st[bigger.vals.index] = st[bigger.vals.index] +1
  }
  index = rep(0, length(c(st, fx)))
  index[fx] = fx
  blank.index = which(index == 0)
  index[blank.index] = st
  index
}
# evaluate the gga result
evaluate.gga = function(st, g = g200, es.g = g200, fx) {
  eval_data.i = make_eval_data(g, es.g, fx = NULL)
  evalFunc.i = function(string) {sum(eval_data.i$ScoreR[string] * eval_data.i$wtR * eval_data.i$area)}
  orig_score = evalFunc.i(string = 1:nrow(g)) 
  if (is.null(fx)){
    opt_set = st
  } else {
    opt_set = get.fixed.string(st, fx)
  }
  opt_score = evalFunc.i(opt_set)
  return(list(originalES = orig_score, 
              optimalES = opt_score, 
              optimalSet = opt_set))}

# create optimised layer from result
create.opt.sf.layer = function(g, new_st, es.g){
  g.copy = g[, c("gID", "lu_class", "cols")]
  g.copy$new_lu = g$lu_class[new_st]
  g.copy$cols = g$cols[new_st]
  # need to do same as create eval data with es.g
  int = st_intersection(g.copy, es.g[, c("ScoreR", "wtR") ])
  int = st_collection_extract(int, "POLYGON") 
  int$area = as.vector(st_area(int))
  int %>% 
    # st_drop_geometry() %>% 
    group_by(gID) %>%
    summarise(wtR = weighted.mean(wtR, area), 
              ScoreR = weighted.mean(ScoreR, area),
              new_lu = unique(new_lu),
              cols = unique(cols)) -> g.copy
  g.copy$area = as.vector(set_units(st_area(g.copy), ha))
  g.copy$ScoreR = g.copy$ScoreR[new_st]
  g.copy$es_score = g.copy$ScoreR * g.copy$wtR * g.copy$area
  g.copy[c("X", "Y")] = st_coordinates(st_centroid(g.copy))
  return(g.copy)
}  




