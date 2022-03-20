install.packages("tidyverse")
install.packages('showtext', dependencies = TRUE)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggthemes)

#Installing different fonts
library(ggplot2)
library(extrafont)
loadfonts(device = "win")



#Importing CSV downloaded from google forms
Reg <- read.csv("C:\\Users\\Hephzibah Akindele\\Documents\\COMPSSA-Election\\Valid Registrations.csv")
Votes <- read.csv("C:\\Users\\Hephzibah Akindele\\Documents\\COMPSSA-Election\\Election Day Votes.csv")


#Converting Matriculation Number variable type from character to integer
Reg$Matriculation.number <- as.integer(Reg$Matriculation.number)

# Total Number of Registered Voters Per Department Per Level
Total <- Reg %>%
  group_by(Department.x, Level.x) %>%
  count()
write.csv(Total, "C:\\Users\\Hephzibah Akindele\\Documents\\COMPSSA-Election\\Total Reg Dept.csv")

#Selecting only useful data columns 
Reg <-  select(Reg, Matriculation.number, Password)




# Validating votes with matching passwords, and removing invalid votes
Valid_votes <- Votes %>%
  left_join(Reg, "Matriculation.number") %>%
  arrange(Department, Level, Matriculation.number) %>%
  mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
  filter(Valids == TRUE) %>%
  select(-Matriculation.number, -Level, -Department, -Password..Voting.Passkey.submitted., -Password)

Null_registration <-  Votes %>%
  left_join(Reg, "Matriculation.number") %>%
  arrange(Department, Level, Matriculation.number) %>%
  mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
  filter(is.na(Valids)) %>%
  select(-Matriculation.number, -Level, -Department, -Password..Voting.Passkey.submitted., -Password)

# Count NAs in the data frame
# NAs = Unregistered Votes
# False = Void Votes

# Counting votes per level
Votes_per_level <- Votes %>%
  left_join(Reg, "Matriculation.number") %>%
  arrange(Department, Level, Matriculation.number) %>%
  mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Department, Level)%>%
  count()




# Invalid Votes
Invalid_Votes <- Votes %>%
  left_join(Reg, "Matriculation.number") %>%
  arrange(Department, Level, Matriculation.number) %>%
  mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
  filter(Valids == FALSE) %>%
  count()

# Counting votes per department and adding invalid Votes
Votes_per_dept <- Votes %>%
  left_join(Reg, "Matriculation.number") %>%
  arrange(Department, Level, Matriculation.number) %>%
  mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Department)%>%
  count() %>%
  ungroup() %>%
  add_row(Department = "Invalid Votes", n = Invalid_Votes$n)
  



# Counting total votes for all positions and candidates
Vote_Count <- lapply(Valid_votes, table)


# length(Vote_Count[["PRESIDENT."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["VICE.PRESIDENT."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["GENERAL.SECRETARY."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["ASSISTANT.GENERAL.SECRETARY."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["FINANCIAL.SECRETARY."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["TREASURER."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["SOCIAL.SECRETARY."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["SPORTS.SECRETARY."]]) <- length(Vote_Count[["PRESIDENT."]])
# length(Vote_Count[["PRO"]]) <- length(Vote_Count[["PRESIDENT."]])


President <- as.data.frame(Vote_Count[["PRESIDENT"]])
VP <- as.data.frame(Vote_Count[["VICE.PRESIDENT"]])
GenSec <- as.data.frame(Vote_Count[["GENERAL.SECRETARY"]])
AGS <- as.data.frame(Vote_Count[["ASSISTANT.GENERAL.SECRETARY"]])
Fin_Sec <- as.data.frame(Vote_Count[["FINANCIAL.SECRETARY"]])
Treasurer <- as.data.frame(Vote_Count[["TREASURER"]])
Social_Sec <- as.data.frame(Vote_Count[["SOCIAL.SECRETARY"]])
Sports_Sec <-as.data.frame(Vote_Count[["SPORTS.SECRETARY"]])
PRO <- as.data.frame(Vote_Count[["PRO"]])
Welfare_sec <- as.data.frame(Vote_Count[["WELFARE.SECRETARY"]])


# Export Total Vote COunt as csv
capture.output(Vote_Count, file = "Vote Count.csv")   



# Exporting data frames to working directory
write.csv(Votes_per_level,"C:\\Users\\Hephzibah Akindele\\Documents\\COMPSSA-Election\\Votes Per Level.csv", row.names = FALSE)
write.csv(Votes_per_dept,"C:\\Users\\Hephzibah Akindele\\Documents\\COMPSSA-Election\\Votes Per Department.csv", row.names = FALSE)

#### Visualizations
#Vote Summary
Votes_per_dept %>%
  ggplot(aes(Department, n, fill = Department)) +
  geom_col(width =  0.5) +
  xlab("\nDepartment") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Votes Per Department") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave("Votes per department.png")



Votes %>%
  left_join(Reg, "Matriculation.number") %>%
  arrange(Department, Level, Matriculation.number) %>%
  mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
  filter(Valids == TRUE) %>%
  group_by(Gender)%>%
  count() %>%
  ggplot(aes(Gender, n, fill = Gender)) +
  geom_col(width =  0.5) +
  xlab("\nGender") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Votes Per Gender") +F
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave("Votes per gender.png")

Votes %>%
  left_join(Reg, "Matriculation.number") %>%
  arrange(Department, Level, Matriculation.number) %>%
  mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
  filter(Valids == TRUE) %>%
  mutate(Level = as.factor(Level)) %>%
  group_by(Level)%>%
  count() %>%
  ggplot(aes(Level, n, fill = Level)) +
  geom_col(width =  0.5) +
  xlab("\nLevel") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Votes Per Level") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
ggsave("Votes per level 2.png")





## Visualization of Vote counts for The President
President %>%
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
  geom_col(width =  0.5) +
  xlab("\nPresidential Candidates") +
  ylab("Vote Count") +
  labs(fill = "Vote Count", title = "Voting Results for\n the Presidential Candidates") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
        legend.title = element_text(face = "bold"))
  ggsave("President.png")

  
  # Visualization by gender statistics 
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    group_by(Gender, PRESIDENT)%>%
    count() %>%
    ggplot(aes(PRESIDENT, n)) +
    geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nPresidential Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Presidential Votes by Gender") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("President Gender.png")
  
  
  # Visualization by Class statistics 6 bar charts
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Level, PRESIDENT)%>%
    count() %>%
    ggplot(aes(PRESIDENT, n)) +
    geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nPresidential Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Presidential Votes by Level") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("President Level.png")
  
  # Visualization by Department statistics
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Department, PRESIDENT)%>%
    count() %>%
    ggplot(aes(PRESIDENT, n)) +
    geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nPresidential Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Presidential Votes by Department") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("President Department.png")
  

  

## Visualization of Vote counts for the vice president
VP %>% 
  ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
    geom_col(width =  0.5) +
    xlab("\nVice Presidential Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Voting Results for\n the Vice Presidential Candidates") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Vice President.png")
  
  
  
  # Visualization by gender statistics 
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    group_by(Gender, VICE.PRESIDENT)%>%
    count() %>%
    ggplot(aes(VICE.PRESIDENT, n)) +
    geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nVice Presidential Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Vice Presidential Votes by Gender") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Vice President Gender.png")
  
  
  # Visualization by Class statistics 6 bar charts
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Level, VICE.PRESIDENT)%>%
    count() %>%
    ggplot(aes(VICE.PRESIDENT, n)) +
    geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nVice Presidential Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Vice Presidential Votes by Level") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Vice President Level.png")
  
  # Visualization by Department statistics
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Department, VICE.PRESIDENT)%>%
    count() %>%
    ggplot(aes(VICE.PRESIDENT, n)) +
    geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nVice Presidential Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Vice Presidential Votes by Department") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Vice President Department.png")


  

# Visualization of Vote counts for The General Secretary
  GenSec %>% 
    ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
    geom_col(width =  0.5) +
    xlab("\nGeneral Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Voting Results for\n the General Secretary Candidates") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("General Secretary.png")
  
  
  
  # Visualization by gender statistics 
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    group_by(Gender, GENERAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(GENERAL.SECRETARY, n)) +
    geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nGeneral Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "General Secretary Votes by Gender") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("General Secretaryt Gender.png")
  
  
  # Visualization by Class statistics 6 bar charts
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Level, GENERAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(GENERAL.SECRETARY, n)) +
    geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nGeneral Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "General Secretary Votes by Level") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("General Secretary Level.png")
  
  # Visualization by Department statistics
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Department, GENERAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(GENERAL.SECRETARY, n)) +
    geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nGeneral Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "General Secretary Votes by Department") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("General Secretary Department.png")
  
  

# Visualization of Vote counts for the Assistant General Secretary
  AGS %>% 
    ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
    geom_col(width =  0.5) +
    xlab("\nAssistant General Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Voting Results for\n the Assistant General Secretary Candidates") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Assistant General Secretary.png")
  
  
  # Visualization by gender statistics 
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    group_by(Gender, ASSISTANT.GENERAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(ASSISTANT.GENERAL.SECRETARY, n)) +
    geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nAssistant General Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Assistant General Secretary Votes by Gender") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Assistant General Secretary Gender.png")
  
  
  # Visualization by Class statistics 6 bar charts
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Level, ASSISTANT.GENERAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(ASSISTANT.GENERAL.SECRETARY, n)) +
    geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nAssistant General Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Assistant General Secretary Votes by Level") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Assistant General Secretary Level.png")
  
  # Visualization by Department statistics
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Department, ASSISTANT.GENERAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(ASSISTANT.GENERAL.SECRETARY, n)) +
    geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nAssistant General Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Assistant General Secretary Votes by Department") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Assistant General Secretary Department.png")
  
  
  
  
  

# Visualization of Vote counts for the Sports Secretary
  Sports_Sec %>% 
    ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
    geom_col(width =  0.5) +
    xlab("\nSports Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Voting Results for\n the Sports Secretary Candidates") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Sports Secretary.png")
  
  
  # Visualization by gender statistics 
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    group_by(Gender, SPORTS.SECRETARY)%>%
    count() %>%
    ggplot(aes(SPORTS.SECRETARY, n)) +
    geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nSports Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Sports Secretary Votes by Gender") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Sports Secretary Gender.png")
  
  
  # Visualization by Class statistics 6 bar charts
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Level, SPORTS.SECRETARY)%>%
    count() %>%
    ggplot(aes(SPORTS.SECRETARY, n)) +
    geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nSports Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Sports Secretary Votes by Level") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Sports Secretary Level.png")
  
  # Visualization by Department statistics
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Department, SPORTS.SECRETARY)%>%
    count() %>%
    ggplot(aes(SPORTS.SECRETARY, n)) +
    geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nSports Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Sports Secretary Votes by Department") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Sports Secretary Department.png")


# Visualization of Vote counts for the Social Secretary
  Social_Sec %>% 
    ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
    geom_col(width =  0.5) +
    xlab("\nSocial Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Voting Results for\n the Social Secretary Candidates") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Social Secretary.png")
  
  
  # Visualization by gender statistics 
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    group_by(Gender, SOCIAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(SOCIAL.SECRETARY, n)) +
    geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nSocial Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Social Secretary Votes by Gender") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Social Secretary Gender.png")
  
  
  # Visualization by Class statistics 6 bar charts
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Level, SOCIAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(SOCIAL.SECRETARY, n)) +
    geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nSocial Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Social Secretary Votes by Level") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Social Secretary Level.png")
  
  # Visualization by Department statistics
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Department, SOCIAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(SOCIAL.SECRETARY, n)) +
    geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nSocial Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Social Secretary Votes by Department") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Social Secretary Department.png")
  
  
  


# Visualization of Vote counts for the Public Relations Officer
  PRO %>% 
    ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
    geom_col(width =  0.5) +
    xlab("\nPublic Relations Officer Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Voting Results for\n the Public Relations Officer Candidates") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Public Relations Officer.png")
  
  # Visualization by gender statistics 
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    group_by(Gender, PRO)%>%
    count() %>%
    ggplot(aes(PRO, n)) +
    geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nPublic Relations Officer Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Public Relations Officer Votes by Gender") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Public Relations Officer Gender.png")
  
  
  # Visualization by Class statistics 6 bar charts
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Level, PRO)%>%
    count() %>%
    ggplot(aes(PRO, n)) +
    geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nPublic Relations Officer Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Public Relations Officer Votes by Level") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Public Relations Officer Level.png")
  
  # Visualization by Department statistics
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Department, PRO)%>%
    count() %>%
    ggplot(aes(PRO, n)) +
    geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nPublic Relations Officer Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Public Relations Officer Votes by Department") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Public Relations Officer.png")
  
  
  
  
  
  
  

# Visualization of Vote counts for the Treasurer
  Treasurer %>% 
    ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
    geom_col(width =  0.5) +
    xlab("\nTreasurer Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Voting Results for\n the Treasurer Candidates") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Treasurer.png")
  

  # Visualization by gender statistics 
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    group_by(Gender, TREASURER)%>%
    count() %>%
    ggplot(aes(TREASURER, n)) +
    geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nTreasurer Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Treasurer Votes by Gender") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Treasurer Gender.png")
  
  
  # Visualization by Class statistics 6 bar charts
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Level, TREASURER)%>%
    count() %>%
    ggplot(aes(TREASURER, n)) +
    geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nTreasurer Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Treasurer Votes by Level") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Treasurer Level.png")
  
  # Visualization by Department statistics
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Department, TREASURER)%>%
    count() %>%
    ggplot(aes(TREASURER, n)) +
    geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nTreasurer Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Treasurer Votes by Department") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Treasurer Department.png")
  
  
  
  
  

                         
# Visualization of Vote counts for the Financial Secretary
  Fin_Sec %>% 
    ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
    geom_col(width =  0.5) +
    xlab("\nFinancial Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Voting Results for\n the Financial Secretary Candidates") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Financial Secretary.png")
  
  # Visualization by gender statistics 
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    group_by(Gender, FINANCIAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(FINANCIAL.SECRETARY, n)) +
    geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nFinancial Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Financial Secretary Votes by Gender") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Financial Secretary Gender.png")
  
  
  # Visualization by Class statistics 6 bar charts
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Level, FINANCIAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(FINANCIAL.SECRETARY, n)) +
    geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nFinancial Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Financial Secretary Votes by Level") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Financial Secretary Level.png")
  
  # Visualization by Department statistics
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Department, FINANCIAL.SECRETARY)%>%
    count() %>%
    ggplot(aes(FINANCIAL.SECRETARY, n)) +
    geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nFinancial Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Financial Secretary Votes by Department") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Financial Secretary Department.png")
  
  

# Visualization of Vote counts for the Welfare Secretary
  Welfare_sec %>% 
    ggplot(aes(x = Var1, y = Freq, fill = Var1 )) +
    geom_col(width =  0.5) +
    xlab("\nWelfare Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Voting Results for\n the Welfare Secretary Candidates") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Welfare Secretary.png")
  
  # Visualization by gender statistics 
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    group_by(Gender, WELFARE.SECRETARY)%>%
    count() %>%
    ggplot(aes(WELFARE.SECRETARY, n)) +
    geom_bar(aes(fill = Gender), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nWelfare Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Welfare Secretary Votes by Gender") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Welfare Secretary Gender.png")
  
  
  # Visualization by Class statistics 6 bar charts
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Level, WELFARE.SECRETARY)%>%
    count() %>%
    ggplot(aes(WELFARE.SECRETARY, n)) +
    geom_bar(aes(fill = Level), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nWelfare Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Welfare Secretary Votes by Level") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Welfare Secretary Level.png")
  
  # Visualization by Department statistics
  Votes %>%
    left_join(Reg, "Matriculation.number") %>%
    arrange(Department, Level, Matriculation.number) %>%
    mutate(Valids = Password..Voting.Passkey.submitted. == Password) %>%
    filter(Valids == TRUE) %>%
    mutate(Level = as.factor(Level)) %>%
    group_by(Department, WELFARE.SECRETARY)%>%
    count() %>%
    ggplot(aes(WELFARE.SECRETARY, n)) +
    geom_bar(aes(fill = Department), stat = "identity", position = "dodge", width = 0.5) +
    xlab("\nWelfare Secretary Candidates") +
    ylab("Vote Count") +
    labs(fill = "Vote Count", title = "Welfare Secretary Votes by Department") +
    theme_fivethirtyeight() +
    theme(axis.title = element_text(face = "bold"),
          plot.title = element_text(family = "Bangers", hjust = .5, lineheight = .8),
          legend.title = element_text(face = "bold"))
  ggsave("Welfare Secretary Department.png")
  