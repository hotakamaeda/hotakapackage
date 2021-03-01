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
    # Read data
    ShinyData=readRDS("//ord-isi-data/public/Staff_Folders/Hotaka_Maeda/RShiny/Data/ShinyData_0.rds")
    exams=sort(unique(ShinyData$Exam))
    `%>%` <- magrittr::`%>%`

    ### Formatting functions
    F1=function(x){
      x=unname(x)
      if(x>.99) x=.99
      if(x<0.01) x=.01
      gsub("^0.",".",format(x,digits=0,nsmall=2))
    }
    F2=function(x){
      x=unname(x)
      format(x,digits=0,nsmall=1)
    }
    F3=function(x){
      x=unname(x)
      format(x,big.mark=",",scientific=FALSE)
    }

    ######################################
    ### Data - reactive

    # get 2 exams data
    GetData <- shiny::reactive({
      ShinyData %>%
        dplyr::filter(Exam %in% as.character(c(input$Exam1,input$Exam2)))
    })

    # Render selectInput
    output$choose_graddate <- shiny::renderUI({
      yr.names <- as.vector( sort(unique(GetData()$YEAR_GRADUATED),decreasing = TRUE) )
      selectInput("graddate","Expected Graduation Year", choices=yr.names, multiple=TRUE)
    })

    # filter exam date, grad date
    GetDataE1 <- shiny::reactive({
      temp=GetData()
      AA1=temp %>%
        dplyr::filter(Exam==input$Exam1 &
                        as.Date(EXAMDATE) >= input$Exam1date[1] &
                        as.Date(EXAMDATE) <= input$Exam1date[2])
      if(length(input$graddate)>0) {
        AA1=AA1 %>%
          dplyr::filter(YEAR_GRADUATED %in% input$graddate)
      }
      AA1
    })

    GetDataE2 <- shiny::reactive({
      temp=GetData()
      AA2=temp %>%
        dplyr::filter(Exam==input$Exam2 &
                        as.Date(EXAMDATE) >= input$Exam2date[1] &
                        as.Date(EXAMDATE) <= input$Exam2date[2])
      if(length(input$graddate)>0) {
        AA2=AA2 %>%
          dplyr::filter(YEAR_GRADUATED %in% input$graddate)
      }
      AA2
    })

    GetDataMerged <- shiny::reactive({
      merge(GetDataE1(),GetDataE2(),by="NBOME_ID")
    })

    ##############################
    # Univariate

    output$tableU1 <- renderTable({
      BB=GetDataE1()
      Exam1=c(N=F3(length(BB$score)),
              Mean=F2(mean(BB$score)),
              SD=F2(sd(BB$score)),
              Max=max(BB$score),
              "90th"=unname(quantile(BB$score,probs=.9)),
              "75th"=unname(quantile(BB$score,probs=.75)),
              "Median"=unname(quantile(BB$score,probs=.5)),
              "25th"=unname(quantile(BB$score,probs=.25)),
              "10th"=unname(quantile(BB$score,probs=.1)),
              Min=min(BB$score)
        )
      BB=GetDataE2()
      Exam2=c(N=F3(length(BB$score)),
              Mean=F2(mean(BB$score)),
              SD=F2(sd(BB$score)),
              Max=max(BB$score),
              "90th"=unname(quantile(BB$score,probs=.9)),
              "75th"=unname(quantile(BB$score,probs=.75)),
              "Median"=unname(quantile(BB$score,probs=.5)),
              "25th"=unname(quantile(BB$score,probs=.25)),
              "10th"=unname(quantile(BB$score,probs=.1)),
              Min=min(BB$score)
      )
      tab1=data.frame(Var=names(Exam1),Exam1,Exam2)
      colnames(tab1)=c("",input$Exam1,input$Exam2)
      tab1
    })

    output$plotU1 <- renderPlot({
      BB=GetDataE1()
      Median=median(BB$score)
      if(input$Outliers){
        BB=BB %>%
          dplyr::filter(score<=mean(score)+sd(score)*4
                 & score>=mean(score)-sd(score)*4)
      }
      ggplot2::ggplot(BB, ggplot2::aes(x=score)) +
        ggplot2::geom_histogram(bins=30)+
        ggplot2::labs(title=paste0("Histogram of ",input$Exam1," scores"))+
        ggplot2::geom_vline(xintercept=Median,color="deepskyblue",size=1)+
        ggplot2::theme_bw()+
        ggplot2::theme(text = ggplot2::element_text(size=15))+
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .01))+
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0,.01)))
    },height=500)

    output$plotU2 <- renderPlot({
      BB=GetDataE2()
      Median=median(BB$score)
      if(input$Outliers){
        BB=BB %>%
          dplyr::filter(score<=mean(score)+sd(score)*4
                 & score>=mean(score)-sd(score)*4)
      }
      ggplot2::ggplot(BB, ggplot2::aes(x=score)) +
        ggplot2::geom_histogram(bins=30)+
        ggplot2::labs(title=paste0("Histogram of ",input$Exam2," scores"))+
        ggplot2::geom_vline(xintercept=Median,color="deepskyblue",size=1)+
        ggplot2::theme_bw()+
        ggplot2::theme(text = ggplot2::element_text(size=15))+
        ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .01))+
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0,.01)))
    },height=500)


    ##############################
    # Bivariate

    # table
    output$table1 <- renderTable({
      BB=GetDataMerged()
      Exam1=c(N=F3(length(BB$score.x)),
              Mean=F2(mean(BB$score.x)),
              SD=F2(sd(BB$score.x)),
              Max=max(BB$score.x),
              "90th"=unname(quantile(BB$score.x,probs=.9)),
              "75th"=unname(quantile(BB$score.x,probs=.75)),
              "Median"=unname(quantile(BB$score.x,probs=.5)),
              "25th"=unname(quantile(BB$score.x,probs=.25)),
              "10th"=unname(quantile(BB$score.x,probs=.1)),
              Min=min(BB$score.x)
      )
      Exam2=c(N=F3(length(BB$score.y)),
              Mean=F2(mean(BB$score.y)),
              SD=F2(sd(BB$score.y)),
              Max=max(BB$score.y),
              "90th"=unname(quantile(BB$score.y,probs=.9)),
              "75th"=unname(quantile(BB$score.y,probs=.75)),
              "Median"=unname(quantile(BB$score.y,probs=.5)),
              "25th"=unname(quantile(BB$score.y,probs=.25)),
              "10th"=unname(quantile(BB$score.y,probs=.1)),
              Min=min(BB$score.y)
      )
      tab1=data.frame(Var=names(Exam1),Exam1,Exam2)
      colnames(tab1)=c("",input$Exam1,input$Exam2)
      tab1
    })

    # Plot
    output$plot1 <- renderPlot({

      ### Data
      # shiny::updateSliderInput(session, "graddate",
      #                          min = min(GetData()$YEAR_GRADUATED), max = max(GetData()$YEAR_GRADUATED))

      BB=GetDataMerged()
      nnn=nrow(BB)

      # quantiles
      Exam1q=quantile(BB$score.x,probs=c(.1,.25,.5,.75,.9))
      Exam2q=quantile(BB$score.y,probs=c(.1,.25,.5,.75,.9))

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
        if(input$LimitN) BB[sample(1:nrow(BB),min(nrow(BB),500)),]

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
          ggplot2::geom_vline(xintercept=Exam1q[3],color="deepskyblue",size=1)+
          ggplot2::geom_hline(yintercept=Exam2q[3],color="deepskyblue",size=1)+
          ggplot2::geom_abline(intercept=Coef[1],slope=Coef[2],color="red",size=1.5)+
          ggplot2::labs(title=TITLE,
                        y=paste0(input$Exam1," Score"),
                        x=paste0(input$Exam2," Score"))+
          ggplot2::theme_bw()+
          ggplot2::theme(text = ggplot2::element_text(size=15))+
          ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = .01))+
          ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = .01))
      }
    },height=500)

}
