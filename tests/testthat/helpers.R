http_app <- function(wd = getwd()) {
  `%||%` <- function(l, r) if (is.null(l)) r else l
  force(test_dir)
  app <- webfakes::httpbin_app()

  # An error with a JSON response that has a 'message'
  app$get("/rhub-error", function(req, res) {
    msg <- req$query[["msg"]]
    status <- as.integer(req$query[["status"]] %||% 401)
    res$
      set_status(status)$
      send_json(object = list(message = msg))
  })

  # An error with an invalid JSON response
  app$get("/rhub-error2", function(req, res) {
    status <- as.integer(req$query[["status"]] %||% 401)
    res$
      set_status(status)$
      send_json(text = "[this is not valid json]")
  })

  # An error with a JSON response, without a 'message'
  app$get("/rhub-error3", function(req, res) {
    status <- as.integer(req$query[["status"]] %||% 401)
    res$
      set_status(status)$
      send_json(object = list(foo = "bar"))
  })

  app$get(
    c(
      "/rhub.yaml",
      "/platforms.json",
      "/platforms2.json",
      "/manifest.json"
    ),
    function(req, res) {
      yaml <- testthat::test_path("fixtures", basename(req$path))
      res$send_file(yaml)
    }
  )

  # SSE
  sse <- function(req, res) {
    `%||%` <- function(l, r) if (is.null(l)) r else l
    if (is.null(res$locals$sse)) {
      progress <- !is.null(req$query$progress)
      error <- !is.null(req$query$error)
      duration <- as.double(req$query$duration %||% 2)
      delay <- as.double(req$query$delay %||% 0)
      numevents <- as.integer(req$query$numevents %||% 5)
      pause <- max(duration / numevents, 0.01)
      res$locals$sse <- list(
        sent = 0,
        numevents = numevents,
        pause = pause,
        progress = progress,
        error = error
      )

      res$
        set_header("cache-control", "no-cache")$
        set_header("content-type", "text/event-stream")$
        set_header("access-control-allow-origin", "*")$
        set_header("connection", "keep-alive")$
        set_status(200)

      if (delay > 0) {
        return(res$delay(delay))
      }
    }

    msg <- paste0(
      "event: ", res$locals$sse$sent + 1L, "\n",
      "message: live long and prosper\n\n"
    )
    res$locals$sse$sent <- res$locals$sse$sent + 1L
    res$write(msg)

    if (res$locals$sse$progress) {
      msg <- paste0(
        "event: progress\n",
        "data: \"This is {.code it}: ", res$locals$sse$sent, "\"\n\n"
      )
      res$write(msg)
    }

    if (res$locals$sse$sent == res$locals$sse$numevents) {
      if (res$locals$sse$progress) {
        msg <- if (res$locals$sse$error) {
          paste0(
            "event: error\n",
            "data: \"This is a {.code failure}.\"\n\n"
          )
        } else {
          paste0(
            "event: result\n",
            "data: \"All is {.code good}.\"\n\n"
          )
        }
        res$write(msg)
      }
      res$send("")
    } else {
      res$delay(res$locals$sse$pause)
    }
  }
  app$get("/sse", sse)
  app$post("/sse", sse)

  app
}

http <- webfakes::new_app_process(http_app())

redact_port <- function(x) {
  gsub(":[0-9]+", ":<port>", x)
}

redact_ae_header <- function(x) {
  gsub(
    "\"Accept[-]Encoding\": \"[^\"]*\"",
    "\"Accept-Encoding\": \"<encodings>\"",
    x
  )
}

redact_abs_path <- function(x) {
  wd <- normalizePath(getwd())
  wd2 <- normalizePath(getwd(), winslash = "/")
  x2 <- gsub(wd, "<wd>", x, fixed = TRUE)
  x3 <- gsub(wd2, "<wd>", x2, fixed = TRUE)
  x3
}

# for the rematch tests
df <- function(...) {
  args <- list(...)
  structure(
    args,
    names = names(args),
    row.names = seq_along(args[[1]]),
    class = c("data.frame")
  )
}

asdf <- function(...) {
  as.data.frame(df(...))
}
