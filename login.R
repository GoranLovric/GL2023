    vals <- reactiveValues(UserType = "User")
    
    # Return the UI for a modal dialog with data selection input. If 'failed' is
    # TRUE, then display a message that the previous value was invalid.
    dataModal_login <- function(failed = FALSE) {
      modalDialog(
        textInput("UserId", "UserId:"),
        passwordInput("password", "Password:"),
        if (failed)
          div(tags$b("Invalid username or password", style = "color: red;")),
        
        footer = tagList(
          actionButton("ok", "Login"),
          br(),
          br(),
          actionLink("register","Change password"),
          br(),
          br(),
          actionLink("ooops","Go back")

        )
      )
    }
    
    
    #
    data2Modal <- function(failed = FALSE) {
      modalDialog(
        textInput("UserId2", "UserId:"),
        passwordInput("password_old", "Old password:"),
        passwordInput("password_new", "New password:"),
        if (failed)
          div(tags$b("Invalid username or password", style = "color: red;")),
        
        footer = tagList(
          actionButton("okay", "OK")
        )
      )
    }
    
    #observeEvent(input$logout, {
    #  session$reload()
    #})
    
    # Show modal when button is clicked.
    observe({
     # if (input$tabs == "intro") {
        showModal(dataModal_login())
      #}

    })
    
    # When OK button is pressed, attempt to load the data set. If successful,
    # remove the modal. If not show another modal, but this time with a failure
    # message.
    observeEvent(input$ok, {
      con <- dbConnect(SQLite(), dbname = "data/test.db")
      Users <- dbGetQuery(con, "SELECT * FROM tab")
      dbDisconnect(con)
      # Check that data object exists and is data frame.
      if (input$UserId %in% Users$UserId && input$password == Users$password[which(Users$UserId == input$UserId)]) {
        vals$UserType <- Users$Type[which(Users$UserId == input$UserId)]
        removeModal()
      } else {
        showModal(dataModal_login(failed = TRUE))
      }
    })
    
    observeEvent (input$register,{
      showModal(data2Modal())
    })
    
    observeEvent (input$ooops,{
      updateTabItems(session, "tabs", selected = "intro")
    })
    
    observeEvent(input$okay,{
      #con <- dbConnect(SQLite(), dbname = "Users.sqlite")
      #Users <- dbGetQuery(con, "SELECT * FROM tab")
      #dbDisconnect(con)
      con <- dbConnect(SQLite(), dbname = "data/test.db")
      Users <- dbGetQuery(con, "SELECT * FROM tab")
      dbDisconnect(con)
      if (input$UserId2 %in% Users$UserId && input$password_old == Users$password[which(Users$UserId == input$UserId2)]) {
        con <- dbConnect(SQLite(), dbname = "data/test.db")
        ff <- sprintf("UPDATE tab SET '%s' = '%s' WHERE UserId = '%s'", "password", input$password_new, input$UserId2)
        dbGetQuery(con, ff)
        dbDisconnect(con)
        vals$UserType <- Users$Type[which(Users$UserId == input$UserId2)]
        removeModal()
      } else {
        showModal(data2Modal(failed = TRUE))
      }  
    })


    