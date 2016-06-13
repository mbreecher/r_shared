get_office <- function(users = NULL){
  if (is.null(users) | !is.null(dim(users))){
    print ("function requires a user vector")
  }else{
    user_df <- data.frame(User = users, Order = seq(1, length(users), 1))
    user_df$User <- as.character(user_df$User)
    #get locations
    setwd("C:/R/workspace/source")
    locations <- read.csv("PS_Users_report_pivot.csv")
    locations <- locations[,c("User", "User.Job.code", "User.Office.Location")]
    
    #reverse User names from "last, first" to "first last"
    locations$User <- as.character(locations$User)
    locations$User.Office.Location <- as.character(locations$User.Office.Location)
    resources <- read.csv(textConnection(locations[,1]), header = F, strip.white=T)
    locations$User <- paste(resources[,2], resources[,1], sep = " ")
    locations <- unique(locations[,c("User", "User.Office.Location")])
    
    #manual reconciliation of names between salesforce and openair
    oa_names <- c("Brad Kramer","Chris Klobuka","Chris Murphy",
                  "Kimberly Gresham","Leo Panchuck","Olalaken Jimi-Salami","Chris Hockaday", 
                  "Dan Collins", "Julie Peng","Sammi Azoulay")
    sf_names <- c("Bradley Kramer","Christopher Klobuka","Christopher Murphy",
                  "Kim Gresham","Leonid Panchuck","Olalekan Jimi-Salami","Christopher Hockaday", 
                  "Daniel Collins", "Julia Swinehart", "Samantha Azoulay")
    for(i in 1:length(oa_names)){
      if(length(locations[locations$User %in% oa_names[i],]$User) > 0){
        locations[locations$User %in% oa_names[i],]$User <- sf_names[i]   
      }
    }
    result <- merge(user_df, locations, by = "User", all.x = T)
    
    #manual office add for pre-OA peeps
    pre_oa <- c("Amy Cooperman","David Wiggins","Diana Morales","Jessica Hamburg",
                "Katherine Marion","Meghan Willeke","Sheetal Shah", "Curtis Schroeder",
                "Kendra Laubenthal","Nathan Bell","Sam Messina", "Jason Basteyns", 
                "Zhi Chen")
    pre_location <- c("Chicago Office","Dallas Office","Scottsdale Office","New York City Office",
                      "Scottsdale Office","Denver Office","Scottsdale Office", "Ames Office",
                      "Ames Office", "New York City Office", "Scottsdale Office", "Scottsdale Office", 
                      "Mountain View Office")
    for(i in 1:length(pre_oa)){
      if(length(result[result$User %in% pre_oa[i],]$User) > 0){
        result[result$User %in% pre_oa[i],]$User.Office.Location <- pre_location[i]   
      }
    }
    
    if(dim(result[is.na(result[!is.na(result$User),]$User.Office.Location),])[1] > 0){
      print("Please Fix:")
      print(unique(result[is.na(result$User.Office.Location),]$User))
    }
    result[order(result$Order),]$User.Office.Location
  } 
}