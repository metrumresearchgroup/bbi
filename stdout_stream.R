print_to_stdout_short <- function() {
    message("first message, about to send messages at 0.5 second intervals")
    for (i in 1:4) {
        Sys.sleep(0.5)
        message("message: ", i, " - going back to sleep now...")
    }
    return(TRUE)
}

print_to_stdout_short()
