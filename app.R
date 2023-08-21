require(magick)
require(shiny)
require(keras)
require(tensorflow)
require(reticulate)
py_install("h5py")
#reticulate::virtualenv_create(envname = 'r-tensorflow', python = 'virtualenv_starter()')
#reticulate::virtualenv_install('r-tensorflow', c('h5py', ' keras'), ignore_installed = FALSE)#
#reticulate::use_virtualenv(virtualenv = 'r-tensorflow', required = TRUE)

    

model<-load_model_hdf5("tufts3.hdf5")
options(digits=3)
colnames<-c("Mild","None","Severe")
process_pix <- function(lsf) {
  img <- lapply(lsf, image_load, grayscale = TRUE) # grayscale the image
  arr <- lapply(img, image_to_array) # turns it into an array
  arr <- lapply(arr,function(x) x/255)
  #arr_normalized <- normalize(arr, axis = 1) #normalize to make small numbers 
  au<-array(unlist(arr),dim=c(length(arr),31,32,1))
  return(au) }


library(shiny)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      fluidRow(column(1,
                      fileInput("ul1", "Open Editing Image", accept = c(".jpg")), 
                      fileInput("ul2", "Open Base Image", accept = c(".jpg")),
                      actionButton("ul3","Predict Editing"))
      )),
    mainPanel(
      fluidRow(
        column(
          5,
          textOutput("eim")
        ),  column 
        (4,
          textOutput("bim")
        )),
      fluidRow(
        column(
          5,
          imageOutput("photo1",brush=brushOpts(id="pb1"))
        ),  column 
        (4,
          imageOutput("photo3",brush=brushOpts(id="pb2"))
        )),
      fluidRow(actionButton("fw","Fill White"),actionButton("fb","Fill Black"),actionButton("rtl","<-")),
      fluidRow(
        
        column
        (5,textOutput("source1")),
        column
        (4,textOutput("source2"))
      )
    ))
)
server <- function(input, output, session) {
  imgul<<-process_pix("195_14.jpg")
  im1<<-image_read("195_14.jpg")
  im2<<-image_read("1161_80.jpg")
  awh<<-image_read("aw.jpg")
  abk<<-image_read("abk.jpg")
  output$eim<-renderText("Editing image")
  output$bim<-renderText("Base image")
  lim <- function(source)
  {   output$photo1<-renderImage({
    list(
      src = file.path(source),
      contentType = "image/jpeg",
      width = 64*5,
      height = 64*5
    )}, deleteFile = FALSE)
  }
  output$coords1<-renderText({
    apb<-array(unlist(input$pb1))
    au<-paste(apb[1],"\n",apb[2],"\n",apb[3],"\n",apb[4])
    au
  })
  output$photo1<-renderImage({
    list(
      src = file.path("195_14.jpg"),
      contentType = "image/jpeg",
      width = 64*5,
      height = 64*5
    )}, deleteFile = FALSE)
  output$photo3<-renderImage({
    list(
      src = file.path("1161_80.jpg"),
      contentType = "image/jpeg",
      width = 64*5,
      height = 64*5
    )}, deleteFile = FALSE)
  output$source2<-renderText({
    img<-process_pix("1161_80.jpg")
    mp<-predict(model,img)
    predind<-which.max(mp)
    pred<-colnames[predind]
    outtext2<-paste("Prediction:",pred,max(mp)*100,"%")
    outtext2
  })
  observeEvent(input$ul1, {
    # Check if a file is selected
    
    if (is.null(input$ul1))
      return()
    
    imgul<<-process_pix(input$ul1$datapath)
    
    output$photo1 <- renderImage({
      list(
        src = input$ul1$datapath,
        contentType = "image/jpeg",
        width = 64*5,
        height = 64*5
      )}, deleteFile = FALSE)  
  })
  
  
  observeEvent(input$ul3, {
    output$source1<-renderText({
      mp<-predict(model,imgul)
      print(mp)
      predind<-which.max(mp)
      pred<-colnames[predind]
      outtext<-paste("Prediction:",pred,max(mp)*100,"%")
      outtext
    })
  })
  
  observeEvent(input$rtl, { 
    apb<-array(unlist(input$pb2))
    x1<-as.numeric(apb[1])
    x2<-as.numeric(apb[2])
    y1<-as.numeric(apb[3])
    y2<-as.numeric(apb[4])
    w<-x2-x1
    h<-y2-y1
    region<-paste(w,"x",h,"+",x1,"+",y1,sep="")
    print(region)
    reg<-image_crop(im2,region)
    off<-paste("+",x1,"+",y1)
    im1<<-image_composite(im1,reg,offset=off)
    image_write(im1,"leftim.jpg")
    imgul<<-process_pix("leftim.jpg")
    lim("leftim.jpg")
    
  })
  
  observeEvent(input$fw, {
    if (is.null(input$pb1))
      return()
    apb<-array(unlist(input$pb1))
    x1<-as.numeric(apb[1])
    x2<-as.numeric(apb[2])
    y1<-as.numeric(apb[3])
    y2<-as.numeric(apb[4])
    w<-x2-x1
    h<-y2-y1
    region<-paste(w,"x",h,"+",x1,"+",y1,sep="")
    reg<-image_crop(awh,region)
    off<-paste("+",x1,"+",y1)
    im1<<-image_composite(im1,reg,offset=off)
    image_write(im1,"leftim.jpg")
    imgul<<-process_pix("leftim.jpg")
    lim("leftim.jpg")
    
    
  })
  
  observeEvent(input$fb, {
    if (is.null(input$pb1))
      return()
    apb<-array(unlist(input$pb1))
    x1<-as.numeric(apb[1])
    x2<-as.numeric(apb[2])
    y1<-as.numeric(apb[3])
    y2<-as.numeric(apb[4])
    w<-x2-x1
    h<-y2-y1
    region<-paste(w,"x",h,"+",x1,"+",y1,sep="")
    reg<-image_crop(abk,region)
    off<-paste("+",x1,"+",y1)
    im1<<-image_composite(im1,reg,offset=off)
    image_write(im1,"leftim.jpg")
    imgul<<-process_pix("leftim.jpg")
    lim("leftim.jpg")
    
      
  })
}
    shinyApp(ui, server)
  
