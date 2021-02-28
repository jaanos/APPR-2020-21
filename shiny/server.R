shinyServer(function(input,output){
  output$prvigraf <- renderPlot({
    
    if(input$oblika_e=="reg"){
      reg_ear(input$leto)}
    
    else{
      loe_ear(input$leto)}
    })
    
  output$drugigraf <- renderPlot({
      
      prihodnost_dobicek_na_delnico_s <- data.frame(Leto=c(input$leto))
      if(input$check == "osn"){
        if(input$oblika_e=="reg"){
          a=data.frame(Leto=c(input$leto))
          reg_pov(a,model_dobicek)}
        
        else{
         
          reg_pov(input$leto,model_earning)}
      }
      else{
        if(input$oblika_e=="reg"){
          a=data.frame(Leto=c(input$leto))
          opt_pov(a,model_dobicek)}
        else{
          opt_pov(input$leto,model_earning)}
      }
    })
  }
) 
