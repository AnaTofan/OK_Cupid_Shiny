.TITLE <- "OK Cupid Explorer"
folder <-  getwd()

jobs <- levels(as.factor(cupid_data$job))
stack_x <- c("body_type", "diet", "drinks", "drugs", "education",
             "income", "job", "sex", "smokes", "status",
             "religion", "religious_affil",
             "agecat")

stack_y <- c("smokes", "drinks", "drugs", "body_type")
over_bar_x <- c("age", "sex", "education", "job", "income", "diet",
                "drinks","orientation", "pets", "smokes", "drugs", "status",
                "offspring", "religion", "religious_affil", "agecat")


density_x <- c("status", "offspring", "drugs", "smokes", "drinks"
)
