####calculate design-based indices

#read in data
catch<-read.csv("nwfsc_bt_fmp_spp.csv")

#split by year
catch_split<- split(catch, catch$Year)

#create a vector of tows for including or excluding.
tow_fn<-function(x){
  tows<- as.data.frame(x$Trawl_id)
  tows<-unique(tows)
  tows<-as.data.frame(tows[!is.na(tows)])
  names(tows)<-"Trawl_id"
  return(tows)
}

tows<-lapply(catch_split,tow_fn)

#specify how to downsample; for simple random sampling, a proportion of stations should do
include_or_exclude <- function(df, proportions) {
  
  # Get the number of rows in the dataframe
  num_rows <- nrow(df)
  
  # Use lapply to create a list of dataframes
  result_list <- lapply(proportions, function(p) {
    # Generate a random vector of 1s and 0s based on the specified proportion
    random_vector <- sample(c(1, 0), size = num_rows, replace = TRUE, prob = c(p, 1 - p))
    
    # Create a new dataframe with the random assignments
    result_df <- cbind(df, RandomAssignment = random_vector)
    
    return(result_df)
  })
  
  # Set names for the list elements based on proportions
  names(result_list) <- as.character(proportions)
  
  # Return the list of dataframes
  return(result_list)
}

# Assign random 1s and 0s based on the specified proportions to a list of dataframes
props<-as.data.frame(seq(0.1,1.0, by = 0.1))
names(props)<-"Trawl_id"
props<-rep(props,length(tows))

tows_assigned<-map2(tows,props, include_or_exclude)

tows_assigned<-unlist(tows_assigned,recursive = F)

#merge with catch
join_dfs <- function(list_of_dfs, main_df, shared_column) {
  merged_dfs <- lapply(list_of_dfs, function(df) {
    merged_df <- merge(df, main_df, by = shared_column)
    return(merged_df)
  })
  return(merged_dfs)
}

catch_assigned <- join_dfs(tows_assigned, catch, "Trawl_id")

#only keep the 1s
alldata_resampled<-lapply(catch_assigned,function(x){
  x[x$RandomAssignment==1,]
})

#now, split by species
split_spp<-function(x){
  spp_list<- split(x,x$Common_name)
  return(spp_list)
}

adr_split<- lapply(alldata_resampled, split_spp)

adr_split<- unlist(adr_split, recursive = F)


####combine all years for each species
names(adr_split)<-substr(names(adr_split),6,50) #it would be good to replace 50 with a logical indicating the end

species_all_yrs<- adr_split %>%
  bind_rows(.id = "source")

species_all_yrs<-split(species_all_yrs,species_all_yrs$source)

####Run biomass fn for all elements in the list
#first, define the area and strata. Using the basic example first. Refine later and produce an unstratified estimate too.
areaexample <- StrataAreas.fn(data.frame(
  name = LETTERS[1:8],
  Latitude_dd.2 = c(49, 49, 49, 45, 45, 40.5,40.5, 40.5),
  Latitude_dd.1 = c(45, 45, 40.5, 40.5, 40.5, 34.5, 34.5, 34.5),
  Depth_m.1 = c(183, 549, 900, 183, 549, 183, 549, 900),
  Depth_m.2 = c(549, 900, 1280, 549, 900, 549, 900, 1280)
))


indicies<- lapply(species_all_yrs, function(x){Biomass.fn(dir = getwd(),
                                                          dat = x,
                                                          strat.df = areaexample)})


all_indicies<-unlist(indicies,recursive = F)

#just extract the global estimates and variance metrics
global_ests <- all_indicies[grep("All", names(all_indicies))]

age<- unlist(global_ests,recursive = F)

global_ests_total <- age[grep("Total", names(age))]

#name them
add_column_based_on_names <- function(df_list) {
  df_list <- lapply(seq_along(df_list), function(i) {
    # Extract the name of the data frame
    df_name <- names(df_list)[i]
    
    # Add a new column with the name of the data frame
    df_list[[i]]$species <- df_name
    
    return(df_list[[i]])
  })
  
  # Return the modified list of data frames
  return(df_list)
}

global_ests_total<-add_column_based_on_names(global_ests_total)

#seperate out proportions and species crudely... will fix later
fix_columns<-function(x){
  x$Proportion<-gsub("[[:alpha:]]", "", x$species)
  x$Proportion<-gsub("\\s", "", x$Proportion)
  x$Proportion<-gsub("(?<![0-9])\\.|\\.(?![0-9])", "", x$Proportion, perl = TRUE)
  x$species<-gsub(".All.Total","", x$species)
  x$species<- gsub("[[:punct:]0-9]", "", x$species)
  return(x)
}

global_ests_total<-lapply(global_ests_total,fix_columns)

ests_df<-do.call(rbind,global_ests_total)
ests_df$Proportion<- as.numeric(ests_df$Proportion)

#make a year column
ests_df<- rownames_to_column(ests_df, var = "Year")
ests_df$Year<-substr(ests_df$Year,5,8)

#write csv
setwd(output)
write.csv(ests_df,"design_based_estimates.csv",row.names = F)

#make plots: index CV ###############################
ggplot(data=ests_df, aes(x=Proportion, y=cv, color = Year)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = 'black') + facet_wrap(~species) +
  theme_classic() + theme(axis.text=element_text(color = "black", size=8),
                          axis.title = element_text(color="black",size=16)) +
  labs(x="Proportion of tows kept", y = "Index CV")

setwd(output)
#ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_design_index_CV.tiff',plot = last_plot(), path = output, width = 12, height = 8, device = 'tiff', dpi = 150)

#make plots: biomass ests ###################################
#many species
ggplot(data=ests_df, aes(x=Proportion, y=Bhat, color = Year)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 3), se = T, na.rm = T, aes(group = 1), color = "black") + facet_wrap(~species, scales = "free") +
  #geom_errorbar(aes(ymin=ifelse(Bhat-seBhat < 0, 0, Bhat-seBhat), ymax=Bhat+seBhat),
  #              width=.2, position=position_dodge(.9)) +
  theme_classic() + theme(axis.text=element_text(color = "black", size=8),
                          axis.title = element_text(color="black",size=16)) +
  labs(x="Proportion of tows kept", y = "Biomass estimate")

setwd(output)
#ggsave(filename = 'NWFSC_BT_FMP_focal_spp_by_year_design_biomass_ests.tiff',plot = last_plot() , path = output, width = 14, height = 8, device = 'tiff', dpi = 150)
