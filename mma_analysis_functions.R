library(ggplot2)
metrics_for_individual_fighters <- function(data_frame,variable,bar_width){
  # Plots overlaid histograms of the distribution of a given variable, 
  # enabling comparison between the two time eras.
  # 
  # Args:
  #   data_frame: The data frame containing the data to be used.
  #   variable: A string which gives the variable we wish to examine with the histograms.
  #   bar_width: A numeric which is the desired bar width of the histograms.
  # 
  # Returns:
  #   Plots the overlaid histograms.
  if(variable == "Sig.Strikes"){
    type = "significant strikes"
  }
  else if(variable == "Sub.Attempts"){
    type = "submission attempts"
  }
  else if(variable == "Guard.Passes"){
    type = "guard passes"
  }
  else{
    type = "takedowns"
  }
  ggplot(data_frame,aes(x=data_frame[,variable],fill=before_or_after_announcement))+
    geom_histogram(aes(y=..density..),alpha=0.5,binwidth = bar_width,position = "identity")+
    labs(fill = "Time period", x = type, y = "Percent", title = paste0("Percentage of fights containing a given number of ",type))
}

fight_time_minutes <- function(rounds,time_in_round){
  # Computes the length of a fight in expressed in minutes.
  # 
  # Args:
  #   rounds: The round in which a fight was finished.
  #   time_in_round: The time at which the fight was stopped in a round.
  #   
  # Returns:
  #   The fight length expressed in minutes.
  ifelse(rounds == 1,
         as.numeric(substr(time_in_round,1,1))+(1/60)*as.numeric(substr(time_in_round,3,4)),
         5*(rounds-1)+as.numeric(substr(time_in_round,1,1))+
           (1/60)*as.numeric(substr(time_in_round,4,5))
  )
}

shorten_method_names <- function(name){
  # Abbreviates the name of the method by which a fight was finished.
  # 
  # Args:
  #   name: The string which gives the name of the method.
  # 
  # Returns:
  #   An abbreviated version of the method name.
  if(substr(name,1,3) == "SUB"){
    return("SUB")
  }
  else if(substr(name,3,5) == "DEC"){
    return(paste0(substr(name,1,1),"D"))
  }
  else if(substr(name,1,2) == "KO"){
    return("KO/TKO")
  }
  else if(name == "CNC"){
    return("NC")
  }
  else if(substr(name,1,2) == "DQ"){
    return("DQ")
  }
  else if(substr(name,1,10) == "Overturned"){
    return("OT")
  }
  else{
    return("Other")
  }
}

format_knockout_name <- function(name){
  # Formats the specific knockout method to a shorter name.
  # 
  # Args:
  #   name: A string giving the knockout type's name to be shortened.
  # 
  # Returns:
  #   A shortened version of the knockout type's name.
  if(name == "KO/TKO"){
    return("KO/TKO")
  }
  else if(substr(name,1,2) == "KO"){
    char_name=as.character(name)
    short_name = substr(char_name,7,nchar(char_name))
    return(short_name)
  }
  else{
    return(as.character(name))
  }
}

format_submission_name <- function(name){
  # Formats the name of a submission so it's easier to read in plots.
  # 
  # Args:
  #   name: The string consisting of the entire submission name. 
  # 
  # Returns:
  #   A shortened version of the submission name. Names ending in "choke" are
  #   shortened by removing the substring "choke".
  char_name = as.character(name)
  if(substr(char_name,1,3)=="SUB"){
    short_name = substr(char_name,4,nchar(char_name))
    if(substr(short_name,nchar(short_name)-5,nchar(short_name)) == " Choke"){
      short_name = substr(short_name,1,nchar(short_name)-5)
    }
    # Take care of the "Other"-cases
    short_name = gsub("-","",short_name)
  }
  else{
    short_name = char_name
  }
  return(short_name)
}

format_decision_name <- function(name){
  # Formats the abbreviated decision type name so it has a clearer meaning.
  # 
  # Args:
  #   name: A string which is the abbreviated version of a decision type name.
  # 
  # Returns:
  #   A string which gives a clearer meaning of the decision type.
  if(name == "U-DEC"){
    return("Unanimous decision")
  }
  else if(name == "S-DEC"){
    return("Split decision")
  }
  else if(name == "M-DEC"){
    return("Majority decision")
  }
}

outcomes_within_method_type <- function(type,data_frame){
  # Shows the percentages of fights ending in a certain type of method.
  # 
  # Args:
  #   type: The type of method we're interested in.
  #   data_frame: The data frame containing the data we're interested in.
  # 
  # Returns:
  #   Plots overlaid bar plots showing the percentage of fights ending by a specific
  #   type of method given a general method.
  if(type == "KO"){
    method_specific_data_frame = data_frame[substr(data_frame$Method,1,2) == "KO",]
    method_specific_data_frame$Method = sapply(method_specific_data_frame$Method,
                                               format_knockout_name)
  }
  else if(type == "Submission"){
    method_specific_data_frame = data_frame[substr(data_frame$Method,1,3)=="SUB",]
    method_specific_data_frame$Method = sapply(method_specific_data_frame$Method,
                                               format_submission_name)
  }
  else if(type == "Decision"){
    method_specific_data_frame = data_frame[substr(data_frame$Method,3,5)=="DEC",]
    method_specific_data_frame$Method = sapply(method_specific_data_frame$Method,
                                               format_decision_name)
  }
  before_announcement = method_specific_data_frame[method_specific_data_frame$before_or_after_announcement == "Before",]
  after_announcement = method_specific_data_frame[method_specific_data_frame$before_or_after_announcement == "After",]
  ggplot()+
    geom_bar(data = before_announcement,
             aes(x = before_announcement$Method, y = (..count..)/sum(..count..),color="Before"),
             alpha=0.5,fill="blue",position="dodge")+
    coord_flip()+
    geom_bar(data = after_announcement,
             aes(x = after_announcement$Method, y = (..count..)/sum(..count..),color="After"),
             alpha=0.5,fill="red",position="dodge")+
    coord_flip()+
    scale_color_manual(name = "",values=c("Before" = "blue","After"="red"),guide="legend")+
    guides(colour=guide_legend(override.aes = list(fill=c("red","blue"))))+
    labs(fill = "Time period", x = paste0(type, " type"), y = "Percent", title = paste0("Relative frequency of ",type, " types"))
}