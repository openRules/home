prostatecancerrisk <- function(fmh = 'no', race = 'w', pageformat = 'factsbox'){

if (race == "w"){prob = 0.0258}
if (race == "h"){prob = 0.0314}
if (race == "a"){prob = 0.0208}
if (race == "n"){prob = 0.0248}
if (race == "b"){prob = 0.0443}

if (fmh == "y"){prob = 0.0272 * 2.53} #So FMH is applied to risk for all races pooled. Unclear what to do with AA male and pos FMH

msg = NULL
#data(pages)
#msg <- subset(pages, pagename==pageformat)[, "content"]

# https://github.com/jeroenooms/opencpu/issues/162
factsbox <- system.file("www/prostatecancer-factsbox.html", package = "home")
#stop(factsbox)
nc <- nchar(factsbox)
msg <- readChar(factsbox,10000)

prob = prob * 100
msg <- sub("XCONTROLX", sprintf("%.1f",prob),msg)

prob = prob * 0.78
msg <- sub("XPSAX", sprintf("%.1f",prob),msg)

msg <- paste(msg,"<div>&nbsp;</div><div style=\"text-align:center\">		<button id=\"startover\" type=\"button\" onclick=\"location.reload()\">Start over</button></div>", sep="")
list(message = msg)
}