# cli_status

    Code
      pid <- cli_status("This is a status message")
    Message
      > This is a status message
    Code
      cli::cli_status_clear(pid, result = "clear")

---

    Code
      pid <- cli_status("This is a status message")
    Message
      > This is a status message
    Code
      cli::cli_status_clear(pid, result = "failed")
    Message
      x This is a status message
      

