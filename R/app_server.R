#' main server of app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return shiny server
#' @export
#'

app_server <- function(input, output, session) {
    exams=sort(unique(hotakapackage::AA$Exam))
    `%>%` <- magrittr::`%>%`

    F1=function(x){
      x=unname(x)
      if(x>.99) x=.99
      if(x<0.01) x=.01
      gsub("^0.",".",format(x,digits=0,nsmall=2))
    }

    GetData <- shiny::reactive({
      hotakapackage::AA %>%
        dplyr::filter(Exam %in% as.character(c(input$Exam1,input$Exam2)))
    })

    # observe({
    #   AA0=AA %>%
    #     filter(Exam %in% as.character(c(input$Exam1,input$Exam2)))
    #
    #   updateSliderInput(session, "graddate", #value = c(min(AA0$YEAR_GRADUATED),max(AA0$YEAR_GRADUATED)),
    #                     min = min(AA0$YEAR_GRADUATED), max = max(AA0$YEAR_GRADUATED))
    #   })

    output$plot1 <- renderPlot({
      # labb=as.character(unlist(getinput()))
      # preddat=as.numeric(labb)
      # names(preddat)=c(paste0("\u2265",seq(400,650,50)))
      # AA1=AA[AA$Exam==input$Exam1 &
      #          AA$YEAR_GRADUATED >= input$graddate[1] &
      #          AA$YEAR_GRADUATED <= input$graddate[2] &
      #          AA$EXAMDATE >= input$Exam2date[1] &
      #          AA$EXAMDATE <= input$Exam2date[2]
      #          ,]
      # AA2=AA[AA$Exam==input$Exam2 &
      #          AA$YEAR_GRADUATED >= input$graddate[1] &
      #          AA$YEAR_GRADUATED <= input$graddate[2] &
      #          AA$EXAMDATE >= input$Exam2date[1] &
      #          AA$EXAMDATE <= input$Exam2date[2] ,]
      #

      ### Data
      shiny::updateSliderInput(session, "graddate", #value = c(min(AA0$YEAR_GRADUATED),max(AA0$YEAR_GRADUATED)),
                               min = min(GetData()$YEAR_GRADUATED), max = max(GetData()$YEAR_GRADUATED))

      AA1=GetData() %>%
        dplyr::filter(Exam==input$Exam1 &
                        YEAR_GRADUATED >= input$graddate[1] &
                        YEAR_GRADUATED <= input$graddate[2] &
                        as.Date(EXAMDATE) >= input$Exam1date[1] &
                        as.Date(EXAMDATE) <= input$Exam1date[2])
      AA2=GetData() %>%
        dplyr::filter(Exam==input$Exam2 &
                        YEAR_GRADUATED >= input$graddate[1] &
                        YEAR_GRADUATED <= input$graddate[2] &
                        as.Date(EXAMDATE) >= input$Exam2date[1] &
                        as.Date(EXAMDATE) <= input$Exam2date[2])
      BB=merge(AA1,AA2,by="NBOME_ID")
      nnn=nrow(BB)

      ########## If no data
      if(nnn==0){
        ggplot2::ggplot(cars)+
          ggplot2::labs(title="NO DATA")+
          ggplot2::theme(text = ggplot2::element_text(size=20))
      } else {

        ########## If theres data

        ### Calculations
        Corr=F1(cor(BB$score.x,BB$score.y))
        Coef=coef(lm(BB$score.y~BB$score.x))

        ### Edit graph
        if(input$Outliers){
          BB=BB %>%
            filter(score.x<=mean(score.x)+sd(score.x)*4
                   & score.x>=mean(score.x)-sd(score.x)*4
                   & score.y<=mean(score.y)+sd(score.y)*4
                   & score.y>=mean(score.y)-sd(score.y)*4)
        }
        set.seed(1)
        if(input$Jitter){
          BB$score.x = BB$score.x + rnorm(nrow(BB),sd=sd(BB$score.x)/30)
          BB$score.y = BB$score.y + rnorm(nrow(BB),sd=sd(BB$score.y)/30)
        }
        set.seed(1)
        if(input$LimitN) BB[sample(1:nrow(BB),min(nrow(BB),1000)),]

        # AA1=AA[AA$Exam=="EM",]
        # AA2=AA[AA$Exam=="OP",]
        # BB=merge(AA1,AA2,by="NBOME_ID")[1:1000,]

        TITLE=paste0("N = ",
                     format(nnn,big.mark=",",scientific=FALSE),
                     "\nCorrelation = ",
                     Corr,
                     "\ny = ",
                     round(Coef[1],3),
                     " + ",
                     round(Coef[2],3),"x")
        ggplot2::ggplot(BB,ggplot2::aes(y=score.y,x=score.x))+
          ggplot2::geom_point()+
          ggplot2::geom_abline(intercept=Coef[1],slope=Coef[2],color="red",size=1.5)+
          ggplot2::labs(title=TITLE,
                        y=paste0(input$Exam1," Score"),
                        x=paste0(input$Exam2," Score"))+
          ggplot2::theme_bw()+
          ggplot2::theme(text = ggplot2::element_text(size=15))+
          ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .01))+
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = .01))
      }
    }, height = 600)

    output$note <- shiny::renderText({
      paste("Note. Only 1st time attempt data are included.")
    })
}
