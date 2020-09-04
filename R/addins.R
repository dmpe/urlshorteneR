#' Shorten an URL
#' 
#' @description Call this function as an addin to prompt a url shortener.
#'   bit.ly accepts a custom domain, see \code{\link[urlshorteneR]{bitly_shorten_link}}.
#' 
#' @export
shortenerAddin <- function() {
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Shorten URL"),
    miniUI::miniContentPanel(
      padding = 10,
      shiny::textInput("url", label = "Long URL", value = "", width = "100%"),
      shiny::selectInput(
        "provider",
        label = "Provider",
        choices = c("bit.ly", "is.gd", "v.gd"),
        selected = "bit.ly",
        width = "100%"
      ),
      shiny::uiOutput("bitly_domain"),
      shiny::actionButton("shorten", label = "Shorten"),
      shiny::actionButton("expand", label = "Expand")
    )
  )
  
  server <- function(input, output, session) {
  
    shortener_provider <- shiny::reactive({
      shiny::req(input$provider)
      input$provider
    })
    
    # bit.ly supports custom domains
    output$bitly_domain <- shiny::renderUI({
      if (shortener_provider() == "bit.ly") {
        shiny::tagList(
          shiny::textInput("domain", label = "Bit.ly domain", value = "bit.ly",
                           width = "100%")
        )
      } else {
        return(NULL)
      }
    })
    
    # Shorten links
    shiny::observeEvent(input$shorten, {
      
      shiny::req(input$url)
      
      shortener <- get_shortener(provider_name = input$provider)

      # Conditionally pass custom bit.ly custom domain
      if (input$provider == 'bit.ly') {
        res <- shortener(longUrl = input$url, domain = input$domain)
      } else {
        res <- shortener(longUrl = input$url)
      }
      
      tryCatch({
        parse_response(res) %>% 
          copy_and_notify()
      }, error = function(e) {
        cli::cli_alert_danger(e)
        shiny::showNotification(
          ui = paste("Something went wrong:", e, sep = " "),
          duration = 3L,
          closeButton = TRUE,
          type = "message"
        )
      })
      
    })
    
    # Expand links
    shiny::observeEvent(input$expand, {
      
      shiny::req(input$url)
      
      expander <- get_expander(provider_name = input$provider)
      
      # bitly_expand_link uses bitly_id instead of a short URL - strip protocol accordingly
      if (input$provider == "bit.ly") {
        short_url <- gsub(pattern = "https://", x = input$url, replacement = "")
      } else {
        short_url <- input$url
      }
      
      tryCatch({
        expander(shorturl = short_url) %>% 
          parse_response(expand = TRUE) %>%
          copy_and_notify()
      }, error = function(e) {
        cli::cli_alert_danger(e)
        shiny::showNotification(
          ui = paste("Something went wrong:", e, sep = " "),
          duration = 3L,
          closeButton = TRUE,
          type = "warning"
        )
      })
      
    })
    
    shiny::observeEvent(input$done, {
      invisible(shiny::stopApp())
    })

  }
  
  viewer <- shiny::dialogViewer(dialogName = "URL shortener", width = 600, height = 400)
  shiny::runGadget(ui, server, viewer = viewer)
}

#' Shorten an URL from clipboard
#' 
#' @export
clipShortenerAddin <- function() {
  long_url <- clipr::read_clip()
  
  tryCatch({
    short_url <- bitly_shorten_link(long_url = long_url) %>% 
      parse_response()
    clipr::write_clip(short_url)
    cli::cli_alert_success(paste(short_url, "copied to clipboard", sep = " "))
  }, error = function(err) {
    cli::cli_alert_danger(err)
  })
}

#' Expand an URL from clipboard
#' 
#' @export
clipExpanderAddin <- function() {
  bitly_id <- clipr::read_clip() %>% 
    gsub(pattern = "https://", replacement = "")
  
  tryCatch({
    long_url <- bitly_expand_link(bitlink_id = bitly_id) %>% 
      parse_response(expand = TRUE)
    clipr::write_clip(long_url)
    cli::cli_alert_success(paste(long_url, "copied to clipboard", sep = " "))
  }, error = function(e) {
    cli::cli_alert_danger(e)
  })
  
}


#' Select shortener function
#' 
#' @param provider_name (str) url shortener provider
#' @return A function
#' 
#' @noRd
get_shortener <- function(provider_name) {
  switch (provider_name,
          "bit.ly" = bitly_shorten_link_,
          "is.gd" = isgd_LinksShorten,
          "v.gd" = vgd_LinksShorten
  )
}

#' Select expander function
#' 
#' @inheritParams get_shortener
#' @noRd
get_expander <- function(provider_name) {
  switch (provider_name,
          "bit.ly" = bitly_expand_link_,
          "is.gd" = isgd_LinksExpand,
          "v.gd" = vgd_LinksExpand
  )
}

#' Copy result to clipboard and fire notifications
#' 
#' @noRd
copy_and_notify <- function(url) {
  clipr::write_clip(url)
  shiny::showNotification(
    ui = paste(url, "copied to clipboard", sep = " "),
    duration = 3L,
    closeButton = TRUE,
    type = "message",
  )
  cli::cli_alert_success(paste(url, "copied to clipboard", sep = " "))
}

#' Parse response
#' 
#' @description url shortener functions return results in different formats,
#'   this helper checks for length and grabs values accordingly.
#'   For example, \code{bitly_shorten_link} returns a \code{data.frame}.
#'   
#' @noRd
parse_response <- function(res, expand = FALSE) {
  if (length(res) > 1 && expand == FALSE) {
    res$link[[1]]
  } else if (length(res) > 1 && expand == TRUE) { 
    res$long_url[[1]]
  } else {
    res
  }
}
