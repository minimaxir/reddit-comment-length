source("Rstart.R")

df <- read_csv("reddit_score_comment_length.csv") %>%
filter(score >= -200, score <= 2000) %>%
mutate(se_comment_length = as.numeric(se_comment_length))

sum(df$num_comments) # 1.66 Billion

plot <- ggplot(df, aes(x=score, y=avg_comment_length)) +
	geom_line(size=0.25,
		color = '#e74c3c') +
	geom_ribbon(aes(
		ymin = avg_comment_length - 1.96 * se_comment_length,
		ymax = avg_comment_length + 1.96 * se_comment_length
		),
		fill = "#e74c3c",
		alpha = 0.5,
		data = df) +
	fte_theme() +
	theme(panel.grid.minor = element_line(color="#FAFAFA")) +
	scale_y_continuous(limits = c(0,500)) +
	scale_x_continuous(breaks = seq(-200, 2000, by=200),
						minor_breaks = seq(-100, 1900, by=200),
						labels = comma) +
	labs(x = expression("Comment Score" ~
			scriptstyle(italic("(# Upvotes - # Downvotes)"))
			),
			y = expression("Avg. Length of Comment For Each Score" ~
				scriptstyle(italic("(# of Characters)"))
				),
			title = "Relationship between Reddit Comment Score and Comment Length for 1.66 Billion Comments"
			)
			
max_save(plot,"reddit_comment_length", "Reddit Comment Dump by /u/Stuck_In_the_Matrix",w=6.5)