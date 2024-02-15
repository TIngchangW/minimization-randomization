# Function for minimization randomization
randomize = function(input){
  
  unrandomized_pt = data.frame(input)
  # Convert the input to a single row data frame. (may be unnecessary)
  
  unrandomized_pt[, 'Arm'] = NA
  # Column "Arm" is waiting for assignment
  
  all_pt_file = grep("all_patients.csv", list.files(output_dir), value = TRUE)
  # Find all randomized patients. The object folder should have a file 
  # (file automated created in test script)
  
  randomized_pt = read.csv(all_pt_file, sep = ',', header = TRUE)
  # Read in the csv file
  
  if(unrandomized_pt[, 'ID'] %in% randomized_pt[, 'ID']){
    # If the new patient ID exists in randomized patient
    
    info = paste0("Patient ", unrandomized_pt[, 'ID'], " has been randomized.")
    # Return error: Patient [ID] has been randomized.
    
    return(info)
  }

  # Combine the randomized patients and the new
  if(nrow(randomized_pt) == 0){
    # If the randomized_pt is empty
    
    data = unrandomized_pt
    # There is no randomized patient, data would be the new patient.
    
  }else{
    # Otherwise
    
    data = rbind(randomized_pt, unrandomized_pt)
    # Union the randomized patient and new patient as data (data frame). 
    # New patient is at the bottom of data
  }
  
  data = data[!duplicated(data[, 'ID']), ]
  # Remove duplicated IDs in data (unnecessary, in case anything unexpected)
  
  # Randomize the new patient
  which_patient = which(is.na(data[, 'Arm']))
  # which_patient locates the new patient in data
  
  dynamic_seed = as.numeric(gsub("\\D", "", data[which_patient, 'ID']))
  # Use the numerical values in patient ID as random seed
  
  dynamic_seed = ifelse(is.na(dynamic_seed), nrow(randomized_pt), dynamic_seed)
  # If the random seed is invalid, use the number of rows in data instead
  
  if (is.na(data[1, 'Arm'])){
    # If the first patient in data is the new patient.
    
    set.seed(dynamic_seed)
    # Call random seed
    
    data[1, 'Arm'] = sample(arm, 1, prob = ratio)
    # Randomly assign the first patient to either arm following ratio 
    # (prob = 2/3 to Treatment, prob = 1/3 to Control)
    
  } else{
    # Otherwise
    
    data_randomized_patients = data[which(!is.na(data[, 'Arm'])),]
    # Extract the subset of randomized patient (equivalent with randomized_pt)
    
    imbalance_score = rep(0, n_arm)
    # Initialize the imbalance score as 0. Vector length as the number of arms
    
    names(imbalance_score) = arm
    # Give the names of arms to imbalance score as key names.
    
    for(j in arm){
      # Go through each arm, update its imbalance score
      for (i in covariance) {
        # Go through each covariance (stratification factor)
        
        sum_for_treatment = sum(
          data_randomized_patients[data_randomized_patients[, 'Arm']==j,i]==
            data[which_patient,i])
        # Per the new patient's covariance value, count the number of replicates in that arm
        # Store the number as sum_for_treatment
        
        sum_for_treatment = sum_for_treatment * weight[i]
        # Adjust sum_for_treatment through its covariance weight
        
        imbalance_score[j] = imbalance_score[j] + sum_for_treatment
        # Add sum_for_treatment to the imbalance score of that arm
      }
    }
    # Loop finished, now we have the imbalance score for each arm
    imbalance_score = imbalance_score/ratio
    # Normalize the imbalance score by ratio.
    
    ## Note:
    ## Imagine arms as tanks, imbalance score is the water in each tank.
    ## We want to make the water in tanks equal
    ## so we prefer to assign the new patient to the most shallow tanks (could be more than 1)
    ## but we also wish to maintain some randomness in the process.
    
    if(sum(!is.na(imbalance_score))==0 ){
      # If the imbalance score are all NA
      
      return("Error: New Patient with Incomplete Data") 
      # Patient information in incomplete, return error
    }
    
    if(length(unique(imbalance_score))==1 ){
      #if all imbalance score are the same
      
      set.seed(dynamic_seed)
      # Call random seed
      
      data[which_patient, 'Arm'] = sample(arm, 1, prob = ratio)
      # randomize follow ratio
      
    }else {
      # Otherwise, there is imbalance
      
      imbalance = (imbalance_score == min(imbalance_score))
      # find arms with minimal imbalance score
      
      imbalance_min_arm = names(imbalance_score)[imbalance]
      # extract arms with minimal imbalance score as imbalance_min_arm
      
      imbalance_other_arm = names(imbalance_score)[!imbalance]
      # extract other arms imbalance_other_arm
      
      alpha = var(imbalance_score)
      # Calculate alpha as the variance of imbalance scores
      
      probability = 1-.5*(.5^alpha)
      # Define probability with alpha
      
      set.seed(dynamic_seed)
      # call random seed
      
      min_or_max = rbinom(1, 1, probability)
      # min_or_max follows a binomial distribution
      # P(min_or_max = 1) = probability 
      # i.e. min_or_max ~ Binom(probability)
      
      if(min_or_max == 1){
        # If min_or_max == 1
        
        data[which_patient, 'Arm'] = sample(imbalance_min_arm, 1)
        # Assign the new patient to any in imbalance_min_arm equally
        
      }else{
        # Otherwise
        
        data[which_patient, 'Arm'] = sample(imbalance_other_arm, 1)
        # Assign the new patient to any in imbalance_other_arm equally
      }
    }
  }
  ## Randomization finished
  
  write.table(data, all_pt_file, na='NA', row.names = FALSE, sep = ',')
  # Overwrite the randomized patient csv file with new data
  
  randtable = data[data[, 'ID'] %in% unrandomized_pt[, 'ID'],]
  # Save the newly randomized patient as random table
  
  return(randtable)
  # Return the random table for display on the interface
}


