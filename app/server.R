## Server
## setwd("/Users/Ollie/Documents/Uni/Auckland University/2015/Summer Scholarship/RShiny")
## shinyapps::deployApp('Areas')
## shinysky::run.shinysky.example()
## To get shinysky
##if (require(devtools)) install.packages("devtools")#if not alrady installed
##devtools::install_github("AnalytixWare/ShinySky")
##library(shinysky)
##shinysky::run.shinysky.example()

## Require package shiny
require(shiny)

## Require package shinysky (colour actionButton)
require(shinysky)

## Require RColorBrewer
require(RColorBrewer)

## Require prettyR
require(prettyR)

## Source Rfunc.R
#source("RfuncShiny.R")

## Useful functions
## Create the trimming function (trims leading and trailling spaces)
trim.func <- function(x){
        gsub("^\\s+|\\s+$", "", x)
}

## String-breaking function by Rachel:
strbreak.func <- function(xvec,  max.char){
        ## There is an R function strwrap that looks like it does much the same as this.
        ##
        ## xvec is a vector of character strings which typically includes spaces: we can break at
        ## any space.  For example "Peanut Butter & Treacle" can be broken at any of the spaces within.
        ## max.char is the maximum length of a string segment desired.
        xsplit <- strsplit(xvec, split=" ")
        ## Number of characters in each split section:
        xnchar <- lapply(xsplit, function(x) nchar(x))
        nx <- length(xvec)
        xvec.out <- character(length(nx))
        ## Now go through each element of xnchar and decide on the splits:
        for(i in 1:nx){
                str.x <- xsplit[[i]][1]
                j <- 1
                seg.nchar <- nchar(str.x)
                while(j < length(xsplit[[i]])){
                        j <- j+1

                        ## See if segment j can be pasted to the current segment without exceeding the max length:
                        if(seg.nchar + xnchar[[i]][j] + 1 <= max.char){
                                str.x <- paste0(str.x, " ", xsplit[[i]][j])
                                seg.nchar <- seg.nchar + xnchar[[i]][j] + 1  ## Add 1 for the interim space
                        }
                        else{
                                ## If not, start a new segment:
                                str.x <- paste0(str.x, "\n", xsplit[[i]][j])
                                seg.nchar <- xnchar[[i]][j]
                        }
                }  ## End of segments j of element i of xvec
                xvec.out[i] <- str.x
        } ## End of element i of xvec.
        xvec.out

}

## Create colour schemes
colour.diff <- brewer.pal(9, "Pastel1")
colour.diff2 <- brewer.pal(8, "Pastel2")
colour.paired <- brewer.pal(12, "Paired")

########### SERVER START ##########

shinyServer(
            function(input, output) {

                    ## Reactive function of the data
                    Area.data <- reactive({

                            ## Update when select Area is updated
                            input$selectArea

                            isolate({
                                    ## print("Isolate ui.data Loading - Area")

                                    ## Area name.csv
                                    csvname <- paste(input$selectArea, ".csv", sep = "")

                                    ## Read the all-time data and get the calender years and months
                                    dat <- read.csv(csvname, as.is = TRUE)

                                    ## REFORMAT THE DATA FROM SCHOOLS FORMAT INTO CATCHIT FORMAT:
                                    dat$FirstName = dat$Child.Name

                                    ## Rachel: changed this to auto-detect the column-names with "Yes" in them:
                                    ## First remove column Sprung.no.kill because we won't analyse this:
                                    dat$Sprung.no.kill <- NULL
                                    ## Now delete any rows that don't contain "Yes" or "yes" at all:
                                    keep.rows <- apply(dat, 1, function(y) any(y=="Yes" | y=="yes"))
                                    keep.rows[is.na(keep.rows)] <- FALSE
                                    dat <- dat[keep.rows,]

                                    ## Then dat$Species is given by which record is "Yes" in each row.
                                    ## If there is more than one - quick fix for now - just take the first one.
                                    dat$Species <- apply(dat, 1, function(y) names(dat)[min(which(y=="Yes" | y=="yes"))])

                                    ## NOW ADDRESS BAIT.1 AND BAIT.2 IF NEEDED TO MAKE A SINGLE COLUMN Bait
                                    dat$Bait <- dat$Bait.1
                                    ## Paste on dat$Bait.2 only where there is a record in dat$Bait.2:
                                    ## If Bait.2 or Bait.3 are completely empty, they will be read in as NA
                                    ## rather than "", so fix this first:
                                    dat$Bait.2[is.na(dat$Bait.2)] <- ""
                                    dat$Bait.3[is.na(dat$Bait.3)] <- ""
                                    dat$Bait[dat$Bait.2 != ""] <- paste(dat$Bait[dat$Bait.2 != ""],
                                                                        dat$Bait.2[dat$Bait.2 != ""], sep=" & ")
                                    dat$Bait[dat$Bait.3 != ""] <- paste(dat$Bait[dat$Bait.3 != ""],
                                                                        dat$Bait.3[dat$Bait.3 != ""], sep=" & ")

                                    ## Select cases where a species was caught
                                    datcatch = dat[dat$Species != "", ]
                                    ## Get unique species, names, lines and traps
                                    unique.species <- unique(datcatch$Species)

                                    unique.names <- unique(datcatch$FirstName)
                                    unique.placement <- unique(datcatch$Placement)
                                    unique.traps <- unique(datcatch$TrapName)
                                    unique.bait <- unique(datcatch$Bait)
                                    unique.years <- unique(datcatch$calyear)

                                    ## Re-define unique.names and dat
                                    unique.names <- unique(dat$FirstName)

                                    ## Reactive list to return
                                    list(dat = dat, datcatch = datcatch, unique.species = unique.species,
                                         unique.names = unique.names, unique.placement = unique.placement,
                                         unique.traps = unique.traps, unique.bait = unique.bait, unique.years = unique.years)
                            })

                    })

                    ## Overall.plot function


                    ## ######## Reactive UI ##########

                    ## Reactive colour title
                    output$colourtitle <- renderUI({
                            plurals <- c(Species="Species", Bait="Baits", Placement="Placements", Person="People",
                                         "Trap Type"="Trap Types")
                            conditionalPanel(
                                             condition = "input.selectArea != ''",
                                             h4(paste0("Which ", plurals[input$selectSubject], " do you want to include?"))
                                             )

                    })

                    ## Reactive 'subject' checkbox
                    output$subject <- renderUI({

                            ## Define 'subject' : this is Colour Catches By:
                            subject <- input$selectSubject
                            if(input$selectSubject == "Person"){
                                    subject <- "FirstName"
                            }
                            if(input$selectSubject == "Trap Type"){
                                    subject <- "Trap.Type"
                            }

                            ## Define the data as where a species was caught
                            datcatch <- Area.data()$datcatch
                            datcatch$subject <- datcatch[[subject]]

                            ## Remove cases where subject has been left blank (namely for when 'Sex' is selected)
                            datcatch <- datcatch[datcatch$subject != "", ]

                            ## Sort subject in descending order
                            sort.table <- table(datcatch$subject)
                            sort.table <- sort(sort.table, decreasing = TRUE)

                            ## Find unique subject
                            unique.subject <- names(sort.table)

                            ## print("List of Subjects Loading")

                            ## Conditional scrolling selection
                            selectInput('subject',
                                        label = "", #h5(paste("Select ", input$selectSubject, ":", sep = "")),
                                        choices = unique.subject,
                                        selected = unique.subject,
                                        multiple = TRUE,
                                        selectize = FALSE,
                                        size = 5)

                    })


                    ## Reactive breakdown title
                    output$breakdowntitle <- renderUI({
                            plurals <- c(Species="Species", Bait="Baits", Placement="Placements",
                                         Person="People", "Trap Type"="Trap Types")
                            conditionalPanel(
                                             condition = "input.selectBy != 'No Selection' && input.selectArea != ''",
                                             ## h3("Specific plot options:")
                                             h4(paste0("Which ", plurals[input$selectBy], " do you want to see?"))
                                             )
                    })



                    ## Reactive 'by' radio buttons
                    output$by <- renderUI({

                            if(input$selectBy == "No Selection"){
                                    return(NULL)
                            }

                            ## print("Select All/List")

                            ## Define 'by'
                            if(input$selectBy == "Person"){
                                    by.vec <- "People"
                            }
                            if(input$selectBy == "Placement"){
                                    by.vec <- "Placements"
                            }
                            if(input$selectBy == "Species"){
                                    by.vec <- "Species"
                            }
                            if(input$selectBy == "Bait"){
                                    by.vec <- "Baits"
                            }
                            if(input$selectBy == "Trap Type"){
                                    by.vec <- "Trap Types"
                            }

                            ## 'by' dropdown
                            conditionalPanel(
                                             condition = "input.selectBy == 'No Selection'",
                                             ## Rachel: changed this with a hack so that an impossible option is picked,
                                             ## because I deleted the No Selection option.  This means that these
                                             ## unwanted radio buttons will not display.
                                             ## condition = "input.selectBy != 'No Selection'",
                                             radioButtons("by",
                                                          label = h5(paste("Specify a subset of ", by.vec, " to view:", sep = "")),
                                                          choices = c(paste("All ", by.vec, sep = ""), paste("List ", by.vec, sep = "")),
                                                          selected=paste("List ", by.vec, sep = "")

                                                          )
                                             )

                    })


                    ## Reactive 'by' checkbox
                    isolate({input$subject
                             output$subby <- renderUI({

                                     ## Gets rid of initial error message
                                        #if(is.null(input$subject)){
                                        #  return(NULL)
                                        #}

                                     ## print("List of SubBy Start...")

                                     ## Define 'subject' and 'by'
                                     subject <- input$selectSubject
                                     by.vec <- input$selectBy

                                     ## Correct for extra spaces if subject is 'Person' or 'Trap Type'
                                     if(input$selectSubject == "Person"){
                                             subject <- "FirstName"
                                     }
                                     if(input$selectSubject == "Trap Type"){
                                             subject <- "Trap.Type"
                                     }

                                     ## Define the data as where a species was caught
                                     datcatch <- Area.data()$datcatch
                                     datcatch$subject <- datcatch[[subject]]

                                     ## Select the year (if necessary) or return an error if no data in select year
                                        #if(input$selectAggregate == "Month in a selected year"){
                                        #  datcatch <- datcatch[datcatch$calyear == input$selectYear, ]
                                        #  if(is.na(datcatch[1, 1]) == TRUE){
                                        #    return(NULL)
                                        #  }
                                        #}

                                     ## Extract and trim unique First Names
                                     FirstName <- Area.data()$unique.names
                                     alias <- trim.func(FirstName)

                                     ## Determine which (if any) are duplicate First Names
                                     which.dups <- which(duplicated(alias) | rev(duplicated(rev(alias))))

                                     ## Add username to duplicates
                                     if(length(which.dups) != 0){
                                             alias[which.dups] <- paste(alias[which.dups], unique(datcatch$Username)[which.dups], sep= ", ")
                                             ## print("DUPLICATES!")
                                             ## Assign each person their alias
                                             for(i in 1:length(datcatch[, 1])){
                                                     person.counter <- match(datcatch$FirstName[i], FirstName)
                                                     datcatch$alias[i] <- alias[person.counter]
                                             }
                                     }else{
                                             datcatch$alias <- datcatch$FirstName
                                     }

                                     ## Extract input$selectBy
                                     datcatch$by <- datcatch[[by.vec]]
                                     if(by.vec == "Person"){
                                             datcatch$by <- datcatch$alias
                                     }
                                     if(by.vec == "Trap Type"){
                                             datcatch$by <- datcatch$Trap.Type
                                     }

                                     ## Remove cases where 'by' has been left blank (e.g. no line or person entered)
                                     datcatch <- datcatch[datcatch$by != "", ]

                                     ## Extract cases only regarding those specified in input$subject
                                     unique.subject <- input$subject

                                     ## Remove secondary errors
                                        #if(unique.subject[1] %in% unique(datcatch$subject) == FALSE){
                                        #  return(NULL)
                                        #}

                                     store.vec <- data.frame()
                                     for(i in 1:length(unique.subject)){
                                             datvec <- datcatch[datcatch$subject == unique.subject[i], ]
                                             store.vec <- rbind(store.vec, datvec)
                                     }
                                     datcatch <- store.vec

                                     ## Sort input$selectBy in descending order
                                     sort.table <- table(datcatch$by)
                                     sort.table <- sort(sort.table, decreasing = TRUE)

                                     ## Should be in descending order
                                     unique.by <- names(sort.table)

                                     ## Adapt 'Person' and Trap Type plural
                                     if(input$selectBy == "Person"){
                                             by.vec <- "People"
                                     }

                                     ## print("List of SubBy End...")

                                     ## Conditional scrolling selection
                                     conditionalPanel(
                                                      condition = "input.selectBy != 'No Selection' && input.by == 'List People' || input.by == 'List Placements' || input.by == 'List Species' || input.by == 'List Baits' || input.by == 'List Trap Types'",
                                                      selectInput("subby",
                                                                  label = h5(paste("Select as many options as you like:")),
                                                                  choices = unique.by,
                                                                  selected = unique.by,
                                                                  multiple = TRUE,
                                                                  selectize = FALSE,
                                                                  size = 10)
                                                      )

                             })
                     })


                    ## Help Text
                    output$helptext <- renderUI({

                            if(input$selectArea == ""){
                                    return(NULL)
                            }

                            if(input$selectBy == "No Selection"){
                                    return(NULL)
                            }

                            if(input$by == "All People" | input$by == "All Placements" | input$by == "All Species" | input$by == "All Trap.Types" | input$by == "All Baits"){
                                    return(NULL)
                            }

                    })





                    ## Load blank when Area is changed
                                        #observeEvent(input$selectArea, priority = 17, {
                    observe({input$selectArea

                             output$overall <- renderPlot({
                                     return(NULL)
                             })
                             output$stackedplot <- renderPlot({
                                     return(NULL)
                             })
                             ## print("Blank due to Area change")
                     })

                    ## Load blank when 'colour by' is changed
                                        #observeEvent(input$selectSubject, priority = 16, {
                    observe({input$selectSubject

                             output$overall <- renderPlot({
                                     return(NULL)
                             })
                             output$stackedplot <- renderPlot({
                                     return(NULL)
                             })
                             ## print("Blank due to colour by change")
                     })

                    ## Load blank when 'breakdown by' is changed
                                        #observeEvent(input$selectBy, priority = 15, {
                    observe({input$selectBy

                             output$overall <- renderPlot({
                                     return(NULL)
                             })
                             output$stackedplot <- renderPlot({
                                     return(NULL)
                             })
                             ## print("Blank due to select by change")
                     })


                    ## Load blank when subject elements are changed
                    observeEvent(input$subject, priority = 12, {
                            output$overall <- renderPlot({
                                    return(NULL)
                            })
                            output$stackedplot <- renderPlot({
                                    return(NULL)
                            })
                    })


                    ## Load blank when "select by" is changed
                    observeEvent(input$subby, priority = 10, {

                            output$overall <- renderPlot({
                                    return(NULL)
                            })
                            output$stackedplot <- renderPlot({
                                    return(NULL)
                            })
                    })


                    ## ######## START OF OVERALL PLOT ###########

                    ## Generate code required to plot the Overall Bar Plot
                    observeEvent(input$submit, priority = 9, {

                            ## print("Overall pre-code is running")
                            output$overall <- renderPlot({

                                    ## Set the raw data
                                    dat <- Area.data()$dat

                                    ## Set the subject, year, aggregate
                                    subject <- input$selectSubject
                                    if(input$selectSubject == "Person"){
                                            subject <- "FirstName"
                                    }
                                    if(input$selectSubject == "Trap Type"){
                                            subject <- "Trap.Type"
                                    }
                                    subject.elements <- input$subject
                                    year <- input$selectYear
                                        #agg <- input$selectAggregate
                                    agg <- "Year"
                                    byvec <- input$subby

                                    ## Extract the calender month, month name, year and specified subject from the data
                                    datcatch <- Area.data()$datcatch
                                    datcatch$subject <- datcatch[[subject]]

                                    ## Extract and trim unique First Names
                                    FirstName <- Area.data()$unique.names
                                    alias <- trim.func(FirstName)

                                    ## Determine which (if any) are duplicates
                                    which.dups <- which(duplicated(alias) | rev(duplicated(rev(alias))))

                                    ## Add username to duplicates
                                    alias[which.dups] <- paste(alias[which.dups], unique(datcatch$Username)[which.dups],
                                                               sep= ", ")

                                    ## Assign each person their alias
                                    for(i in 1:length(datcatch[, 1])){
                                            person.counter <- match(datcatch$FirstName[i], FirstName)
                                            datcatch$alias[i] <- alias[person.counter]
                                    }

                                    ## Extract a specific year of data (e.g. 2014) or years depending on aggregate
                                    if(agg == "Month in a selected year"){
                                            datYear <- datcatch[datcatch$calyear == year, ]
                                    }else{
                                            datYear <- datcatch
                                    }

                                    ## Extact cases from that year(s) regarding the subject where a species was caught by
                                    ## people in personvec/linevec
                                    datSubject <- datYear[datYear$subject != "", ]
                                    ## Select all cases if no 'by' selection is made
                                    if(input$subby[1] == "No Selection"){
                                            datBy <- datSubject
                                    }
                                    ## Otherwise extract cases only regarding those specified in input$selectBy
                                    store.byvec <- data.frame()
                                    for(i in 1:length(byvec)){
                                            if(input$selectBy == "Person"){
                                                    datBy <- datSubject[datSubject$alias == byvec[i], ]
                                            }
                                            if(input$selectBy == "Placement"){
                                                    datBy <- datSubject[datSubject$Placement == byvec[i], ]
                                            }
                                            if(input$selectBy == "Species"){
                                                    datBy <- datSubject[datSubject$Species == byvec[i], ]
                                            }
                                            if(input$selectBy == "Bait"){
                                                    datBy <- datSubject[datSubject$Bait == byvec[i], ]
                                            }
                                            if(input$selectBy == "Trap Type"){
                                                    datBy <- datSubject[datSubject$Trap.Type == byvec[i], ]
                                            }
                                            store.byvec <- rbind(store.byvec, datBy)
                                    }
                                    datCounts <- store.byvec

                                    datTable = table(factor(datCounts$subject, levels = subject.elements))
                                    print(datTable)
                                    ## print("Overall pre-code finished")


                                    ## ######## Plotting commands ############

                                    ## Plot the overall data and add non-dynamic labels
                                        #output$overall <- renderPlot({
                                    ## print("Overall is plotting")
                                    ## print(input$submit)
                                    ## Busy indicator
                                    if (input$submit == 0)
                                            return()
                                        #Sys.sleep(2)
                                    ## Alternate method
                                    layout(matrix(1:2, ncol = 2), widths = c(0.8, 0.2))
                                    par(mar = c(6, 4, 3, 0))
                                    ## Legend auto-cex
                                    ## Rachel: putting in code here to split long character strings optimally:
                                    legend.string <- strbreak.func(names(datTable), max.char=15)
                                    ##
                                    min.cex <- 0.8
                                    max.cex <- 2

                                    ## Find the maximum segment length for the legend strings, where segments are
                                    ## separated by "\n" :
                                    legend.string.rows <- strsplit(legend.string, split="\n")
                                    nchar.legend.max <- unlist(lapply(legend.string.rows, function(x)max(nchar(x))))
                                    auto.cex <- max(min.cex, min(c(20/length(legend.string)), 20/nchar.legend.max))
                                    auto.cex <- min(auto.cex, max.cex)
                                    ## Add colours if need more than 9
                                    if(length(legend.string) > 9){
                                            colour.diff <- colorRampPalette(brewer.pal(brewer.pal.info["Pastel1", "maxcolors"],
                                                                                       "Pastel1"))(length(legend.string))
                                    }

                                    ## Hold until graphics are ready to be plotted
                                    dev.hold()
                                    par(mar=c(5 + max(nchar.legend.max)/3, 4, 4, 2))
                                    barplot(datTable, beside = TRUE, col = colour.diff[1:length(rownames(datTable))],
                                            las=2, xlab="", cex.axis=1.4,
                                            names=legend.string, cex.names=auto.cex*0.8)#cex.names=1.1)

                                        # ylim = c(0, max(colSums(datTable)) + 0.1*(max(colSums(datTable)))))
                                    dev.flush()

                                    ## Non-dynamic labels
                                    title(ylab = "Number Caught", cex.lab = 1.4)
                                    titlestr <- paste("Number of catches for", paste(byvec, collapse=", "))
                                    titlestr <- strbreak.func(titlestr, max.char=50)
                                    ## If there are more than 2 rows of titlestr, just cite what category it corresponds to:
                                    titlestr.rows <- unlist(strsplit(titlestr, split="\n"))
                                    plurals <- c(Species="Species", Bait="Baits", Placement="Placements",
                                                 Person="People", "Trap Type"="Trap Types")
                                    if(length(titlestr.rows)>2)
                                            titlestr <- paste0("Number of catches for the specified ", plurals[input$selectBy])
                                    title(main = titlestr, xlab = "", cex.main = 1.5, cex.lab = 1.4)

                                    ## Add dynamic legend
                                    par(mar=c(0, 0, 0, 0), mgp=c(0, 0, 0))
                                    plot(1, type="n", bty="n", xaxt="n", yaxt="n")
                                    legend("topleft", legend = legend.string, col = 1,
                                           pt.bg = colour.diff[1:length(input$subject)], pch = 22,
                                           cex = auto.cex*0.8, pt.cex = auto.cex*1.75, xpd = TRUE, inset = c(0, 0),
                                           y.intersp=1.4)

                            })
                    })

                    ## ######## END OF OVERALL PLOT #########


                    ## End of Observes

                    ## Download Graphics
                    observeEvent(input$download, {

                            ## print("downloading...")
                            ## print(length(input$subby))

                    })

            }
            )
