print_to_stdout_short <- function() {
    message("first message, about to send messages at 0.5 second intervals")
    print("a printed message")
    warning("a warning")
    for (i in 1:4) {
        Sys.sleep(0.5)
        message("message: ", i, " - going back to sleep now...")
    }
    return(TRUE)
}
sink(stdout(), type = "message") # sink messages to stdout

print_to_stdout_short()
