library(shiny)
library(GenomicAlignments)
source("parse_BAM_for_debug.R")

function(input, output) {
  
  # Debug printouts to see how BAM files look like
  #input_BAM_for_debug_1 = readBAM("wgEncodeUwRepliSeqMcf7S1AlnRep1.bam")
  #print(input_BAM_for_debug_1)
  
  #input_BAM_for_debug_2 = readBAM("wgEncodeUwRepliSeqMcf7S2AlnRep1.bam")
  #print(input_BAM_for_debug_2)
  
  
  BAM1 = readBAM("wgEncodeUwRepliSeqMcf7S1AlnRep1.bam")
  BAM2 = readBAM("wgEncodeUwRepliSeqMcf7S2AlnRep1.bam")
  

  # Defining action on button press
  
  
  # Drawing the coverage plot
  output$coverage_plot <- renderPlot({
    chr1 <- BAM1[BAM1$rname == input$name,'pos']
    chr2 <- BAM2[BAM2$rname == input$name,'pos']
    
    chr1_density = density(chr1)
    chr2_density = density(chr2)
    
    plot(chr1_density,
         ylim = range(c(chr1_density$y, chr2_density$y)),
         main = paste("Coverage plot of chromosome",input$name,sep=" "),
         xlab = input$name,
         col = 'blue',
         lwd=2.5)
    lines(chr2_density, lwd=2.5, col = 'red')
  })
  
}