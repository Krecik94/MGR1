library(shiny)
library(GenomicAlignments)
source("test_BAM_load.R")

function(input, output) {
  
  bamFile <- "wgEncodeUwRepliSeqBg02esG1bAlnRep1.bam"
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  
  
  #output$distPlot <- renderPlot({
  #  
  #  x    <- faithful$waiting
  #  bins <- seq(min(x), max(x), length.out = input$bins + 1)
  #  
  #  hist(x, breaks = bins, col = "#75AADB", border = "white",
  #       xlab = "Waiting time to next eruption (in mins)",
  #       main = "Histogram of waiting times")
  #  
  #})
  
  bf <- BamFile("wgEncodeUwRepliSeqMcf7S1AlnRep1.bam")
  
  gr <- GRanges("chr1",IRanges(10499500, 14933500))
  
  reads <- scanBam(bf, param=ScanBamParam(what=c("rname", "seq", "pos"), which=gr))
  #BAM = readBAM("wgEncodeUwRepliSeqMcf7S1AlnRep1.bam")
  
  #gal <- readGAlignments("wgEncodeUwRepliSeqMcf7S1AlnRep1.bam", param = ScanBamParam(which = GRanges("chr1",IRanges(143500,154500))))
  
  #test <- gal$start
  #print(class(reads))
  #single_stirng <- reads$`chr1:10499500-14933500`$seq[1]
  #print(class(single_stirng))
  #print(toString(single_stirng))
  
  #output$sequence1 <- renderText({if(input$button == 1)
  #  {input$rangeStart}})
  

  observeEvent(input$button, {
    #print(toString(single_stirng))
    
    #print(toString(single_stirng))
    gr <- GRanges("chr1",IRanges(input$rangeStart, input$rangeEnd))
    #print(toString(single_stirng))
    reads <- scanBam(bf, param=ScanBamParam(what=c("rname", "seq", "pos"), which=gr))
    #print(toString(single_stirng))
    print(input$rangeStart)
    print(input$rangeEnd)
    single_stirng <- reads$`chr1:10499500-14933500`$seq[1]
    print(toString(single_stirng))
    output$sequence1 <- renderText(toString(single_stirng))
  })
  

  
  #getSeq(gal)
  #print(gal)
  #print(mcols(BAM)$seq)
  #print (BAM$seq)
  
}