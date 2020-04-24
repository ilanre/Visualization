rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
library(pacman)
p_load(data.tree,h2o)

#Reading the airline dataset
airlines_data=read.csv("~/R/airline.csv",stringsAsFactors = T)
airlines_data=airlines_data[,-1]
print(paste("The number of variable in the airline dataset are: ",ncol(airlines_data)))
print(paste("The number of records in the airline dataset are: ",nrow(airlines_data)))

#Initializing the H2O session
h2o.init(ip="localhost", port = 54321,nthreads = -1, max_mem_size = "12G")

#importng the read data into H2O platform
system.time({
airlinesHex =as.h2o(airlines_data)


#Setting up the response and exploartory variables for determining Flight departure delay
y = "IsDepDelayed"
obvious_dataleakage_features = c("ArrDelay","DepDelay","Cancelled","CancellationCode","Diverted",
                                 "CarrierDelay","WeatherDelay","NASDelay","SecurityDelay",
                                 "LateAircraftDelay","IsArrDelayed","DepTime","CRSDepTime",
                                 "ArrTime","CRSArrTime","ActualElapsedTime","AirTime",
                                 "CRSElapsedTime")
x = setdiff(h2o.colnames(airlinesHex), c(y, obvious_dataleakage_features))
splits = h2o.splitFrame(data = airlinesHex, ratios = .8, seed = 123)
trainHex = splits[[1]]
validHex = splits[[2]]
})

# GBM hyperparamters
system.time({
gbm_params = list(max_depth = seq(2, 20))

# Train and validate a cartesian grid of GBMs
gbm_grid = h2o.grid("gbm", x = x, y = y,
                    grid_id = "gbm_grid_airline1",training_frame = trainHex,
                    validation_frame = validHex,
                    ntrees =1,min_rows = 1,sample_rate = 1,col_sample_rate = 1,
                    learn_rate = .01,seed = 1111,hyper_params = gbm_params)

gbm_gridperf = h2o.getGrid(grid_id = "gbm_grid_airline1",
                           sort_by = "auc",
                           decreasing = TRUE)
optimal_depth = as.integer(gbm_gridperf@summary_table$max_depth[[1]])
})

# Plot grid model AUC vs. max-depth
library(ggplot2)
library(ggthemes)
ggplot(as.data.frame(sapply(gbm_gridperf@summary_table, as.numeric))) +
  geom_point(aes(max_depth, auc)) +
  geom_line(aes(max_depth, auc, group=1)) +
  labs(x="max depth", y="AUC") +
  theme_pander(base_family = 'Palatino', base_size = 8)


# Train and validate a cartesian grid of GBMs
system.time({
airlines_model.gbm = h2o.gbm(x = x, 
                       y = y, 
                       training_frame = airlinesHex, 
                       ntrees = 10, 
                       max_depth = 6, # optimal_depth, 
                       learn_rate = 0.1, 
                       distribution = "bernoulli")
})

summary(airlines_model.gbm)
h2o.varimp_plot(airlines_model.gbm)

#Obtaining the tree model from GBM and import into the data.tree for visualizing
airlinesTree_model = h2o.getModelTree(model = airlines_model.gbm, tree_number = 1)
source("~/R/datatree/createDataTree.R")
visual_dtree = createDataTree(airlinesTree_model)
GetEdgeLabel <- function(node) {return (node$edgeLabel)}
GetNodeShape <- function(node) {switch(node$type, split = "oval", leaf = "box")}
SetEdgeStyle(visual_dtree, fontname = 'Palatino', label = GetEdgeLabel, labelfloat = TRUE)
SetNodeStyle(visual_dtree, fontname = 'Palatino', shape = GetNodeShape)
SetGraphStyle(visual_dtree, rankdir = "LR", dpi=75.)
plot(visual_dtree, output="graph")
plot(visual_dtree, output = "visNetwork")

