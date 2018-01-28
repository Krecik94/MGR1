library(shiny)
library(GenomicAlignments)
source("parse_BAM_for_debug.R")

function(input, output) {
  
  # Debug printouts to see how BAM files look like
  #input_BAM_for_debug_1 = readBAM("wgEncodeUwRepliSeqMcf7S1AlnRep1.bam")
  #print(input_BAM_for_debug_1)
  
  #input_BAM_for_debug_2 = readBAM("wgEncodeUwRepliSeqMcf7S2AlnRep1.bam")
  #print(input_BAM_for_debug_2)
  
  
  
  # Reading input files
  input_file_1 <- BamFile("wgEncodeUwRepliSeqMcf7S1AlnRep1.bam")
  input_file_2 <- BamFile("wgEncodeUwRepliSeqMcf7S2AlnRep1.bam")
  

  # Defining action on button press
  observeEvent(input$button, {
    # Creating ranges from which to extract sequences
    input_rages <- GRanges(input$name,IRanges(input$rangeStart, input$rangeEnd))
    
    # Reading sequences
    reads_1 <- scanBam(input_file_1, param=ScanBamParam(what=c("rname", "seq", "pos"), which=input_rages))
    reads_2 <- scanBam(input_file_2, param=ScanBamParam(what=c("rname", "seq", "pos"), which=input_rages))
    
    names(reads_1) <- "val"
    names(reads_2) <- "val"
    
    # Extracting first sequence hit
    first_string_1 <-reads_1$val$seq[1]
    first_string_2 <-reads_2$val$seq[1]
    
    # Debug printouts
    print(toString(first_string_1))
    print(toString(first_string_2))
    
    # Assigning values to output fields
    output$sequence1 <- renderText(toString(first_string_1))
    output$sequence2 <- renderText(toString(first_string_2))
  })
  
}