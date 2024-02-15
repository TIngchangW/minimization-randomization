server<-function(input, output) {
  
  # read the variable names from the dataset, select covariance if null
  observeEvent(input$file_rand, {
    file <- input$file_rand
    data <- read.table(file$datapath, sep = ',', header = TRUE)
    
    # Get the variable names from the uploaded file
    variables <- variable.names(data)
    
    # Update the choices of the covariances, patient id, treatment colunmns
    updateSelectInput(inputId = "covariance", choices = variables)
    updateSelectInput(inputId = "pt_id", choices = variables)
    updateSelectInput(inputId = "Treatment.col", choices = variables)
    
    # Update the treatment
    unique_trt <- unique(data[, Treatment.col])
    unique_trt <- unique_trt[!is.na(unique_trt)]
    updateTextInput(inputId = "trt", value = unique_trt)
  })
  
  
  
  
  
  # Function for minimization randonmization
  randomize = function(file_rand, pt_id, trt, Treatment.col, covariance, seed){
    
    data <- read.table(file_rand$datapath, sep = ',', header = TRUE)
    #trt = "MIS, Open"
    trt = str_split(trt, ",")[[1]]
    trt <- trimws(trt)
    ntrt = length(trt)
    # covariance = str_split(covariance, ",")[[1]]
    # covariance <- trimws(covariance)
    n.cov = length(covariance)
    
    
    # Randomize the last patient
    which.patient <- sum(!is.na(data[,Treatment.col]))+1
    
    dynamic_seed <- ifelse(is.na(seed), data[which.patient, pt_id], seed)
    dynamic_seed <- as.numeric(gsub("\\D", "", dynamic_seed))
    
    if (is.na(data[1,Treatment.col])){
      set.seed(dynamic_seed)
      data[1, Treatment.col] = sample(trt, 1)
      data$Patient[1] = 1
    } else{
      
      data.randomized.patients <- data[1:(which.patient-1),]
      total.sum.for.treatment <- rep(0, ntrt)
      names(total.sum.for.treatment) <- trt
      
      # number of the same covariance with the new patient in each trt.
      for(i in covariance){
        for (j in trt) {
          sum.for.treatment <- sum(
            data.randomized.patients[data.randomized.patients[, Treatment.col]==j,i]==data[which.patient,i])
          total.sum.for.treatment[j] <- total.sum.for.treatment[j] + sum.for.treatment
        }
      }
      if(sum(!is.na(total.sum.for.treatment))==0 ){ 
        return("Error: New Patient with Incomplete Data") 
      }else if(length(unique(total.sum.for.treatment))==1 ){
        #if there is no imbalance in any treatment allocation
        set.seed(dynamic_seed)
        data[which.patient, Treatment.col] = sample(trt, 1)
        
      }else {
        # if there is imbalance, select the minimum ones and randomize
        alpha <- sd(total.sum.for.treatment)
        imbalance <- (total.sum.for.treatment ==min(total.sum.for.treatment))
        imbalance_mintrt <- names(total.sum.for.treatment)[imbalance]
        imbalance_maxtrt <- names(total.sum.for.treatment)[!imbalance]
        set.seed(dynamic_seed)
        # decide whether to assign treatment with minimum summation, 1 = Yes
        min_or_max <- rbinom(1,1,(1-.5*(.5^alpha)))==1
        if(min_or_max){
          data[which.patient, Treatment.col] = sample(imbalance_mintrt, 1)
        }else{
          data[which.patient, Treatment.col] = sample(imbalance_maxtrt, 1)
        }
        
      }
      
    } 
    return(data)
  }
  
  #   Show the final calculated values from RAND table
  mydata <- reactive({
    req(input$file_rand)
    randomize(input$file_rand,
              input$pt_id,
              input$trt,
              input$Treatment.col,
              input$covariance, 
              input$seed)
  })
  
  output$randTable <- renderTable({
    # read.table(input$file_rand$datapath, sep = ",", header = TRUE)
    mydata()
  })
  
  # info <- sessionInfo() 
  # output$version <- renderText({ 
  #   paste(info$R.version[c(13, 2)]$version.string, info$R.version[c(13, 2)]$arch,
  #         sep=", ")   
  # })
  
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$title, 'random_table.csv', sep='-') },
    content = function(file) {
      write.table(mydata(), file, na="")
    }
  )
  
  
} # end server
