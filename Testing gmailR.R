library(googlesheets)
library(gmailr)
library(data.table)
library(stringr)
library(purrr)


#trying out Friday, 23 Sep 2016 17:19 and all that

testemail <- data.table(first=c("Jacob", "Adam", "Farrel", "Jennifer"), last=c("Buchinsky", "Buchinsky", "Buchinsky", "Buchinsky"), To=c("jacob.buchinsky@gmail.com", "adam.s.buchinsky@gmail.com", "farrel.buchinsky@ahn.org", "jbuchinsky@gmail.com"), owe=c(0.53, 0.54, 0.25, 1.65), From = "Farrel Buchinsky<fjbuch@gmail.com")



body <- "Dear %s

I was going through Quicken and it looks as if I owe you %s USD. Is that possible? If you think this question is odd then you are correct. I am just trying something out.

Bye
Farrel"
testemail[, body:=sprintf(body, first, owe)]

getreadyemail <- testemail[, list(To, From, Subject = "Do I owe you money", body)]

emails <- pmap(.l = getreadyemail, .f = mime)
map(.x = emails, .f = send_message)

# 	To = "jbuchinsky@gmail.com",
# 	From = "fjbuch@gmail.com",
# 	Subject = "this is just a gmailr test",
# 	body = "Can you hear me now?")
# ))
# dat  <- mime(
# 	To = "jbuchinsky@gmail.com",
# 	From = "fjbuch@gmail.com",
# 	Subject = "this is just a gmailr test",
# 	body = "Can you hear me now?")
# send_message(test_email)



check.details.reminder[, emailsend := mime( To = email, From  = "fjbuch@gmail.com", Subject = "You have not yet checked your details", body = str_c("Dear ", first, ", Six collaborators have entered the date (yyyy-mm-dd) on which they checked their details on the collaborators spreadhseet. https://docs.google.com/spreadsheets/d/1h01etGN9mrN6Tu5rlE_A0w027sC_2KR2LE7BZpRuoaM/edit?usp=sharing    You were not one of them. The abstract will be submitted on Monday 2016-09-12. Please make sure I have correct information on you. If you encounter an error please edit the spreadsheet directly. Feel free to email me or call me on my mobile (412) 567-7870 if you have any questions. Thanks, Farrel"))]

check.details.reminder[, body := as.character(str_c("Dear ", first, ", Six collaborators have entered the date (yyyy-mm-dd) on which they checked their details on the collaborators spreadhseet. https://docs.google.com/spreadsheets/d/1h01etGN9mrN6Tu5rlE_A0w027sC_2KR2LE7BZpRuoaM/edit?usp=sharing    You were not one of them. The abstract will be submitted on Monday 2016-09-12. Please make sure I have correct information on you. If you encounter an error please edit the spreadsheet directly. Feel free to email me or call me on my mobile (412) 567-7870 if you have any questions. Thanks, Farrel"))]


suppressPackageStartupMessages(library(gmailr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
library(readr)
getwd()
addresses <- read_csv("addresses.csv")
marks <- read_csv("marks.csv")
my_dat <- left_join(marks, addresses)

this_hw <- "The Fellowship Of The Ring"
email_sender <- 'Peter Jackson <peter@tolkien.example.org>' # your Gmail address
optional_bcc <- 'Anonymous <anon@palantir.example.org>'     # for me, TA address
body <- "Hi, %s.
Your mark for %s is %s.
Thanks for participating in this film!
"

edat <- my_dat %>%
	mutate(
		To = sprintf('%s <%s>', name, email),
		Bcc = optional_bcc,
		From = email_sender,
		Subject = sprintf('Mark for %s', this_hw),
		body = sprintf(body, name, this_hw, mark)) %>%
	select(To, Bcc, From, Subject, body)
edat
data.table(edat)
write_csv(edat, "composed-emails.csv")

emails <- edat %>%
	pmap(mime)

# email2 <- pmap(edat, mime)
#
# identical(emails, email2)
# str(email2)
## optional: use if you've created your own client id
#use_secret_file("gmailr-tutorial.json")

safe_send_message <- safely(send_message)
sent_mail <- emails %>%
	map(safe_send_message)

saveRDS(sent_mail,
				paste(gsub("\\s+", "_", this_hw), "sent-emails.rds", sep = "_"))

errors <- sent_mail %>%
	transpose() %>%
	.$error %>%
	map_lgl(Negate(is.null))
sent_mail[errors]
