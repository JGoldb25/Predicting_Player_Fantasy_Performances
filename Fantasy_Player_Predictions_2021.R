setwd("~/Desktop/")
rm(list=ls())
library(dplyr)
library(ggplot2)
library(devtools)
library(stringr)
library(lattice)

#######################
##Catchers
Catchers <- read.csv(file = 'FG_Catchers.csv')
head(Catchers)

C_specific_cols <- Catchers %>% 
  select(Player = Name, playerid, Team, FG_Average_Draft_Position = ADP, Total_Hits = H, 
         Doubles = X2B, Triples = X3B, Home_Runs = HR, Runs_Batted_In = RBI, 
         Runs_Scored = R, Walks = BB, Strikeouts = SO, Stolen_Bases = SB, Caught_Stealing = CS,
         Hit_By_Pitch = HBP)
head(C_specific_cols)

C_rank_ADP <- C_specific_cols %>%
  mutate(FG_Average_Draft_Position_New = round(FG_Average_Draft_Position, 0)) %>% 
  arrange(FG_Average_Draft_Position_New) %>% 
  relocate(FG_Average_Draft_Position_New, .before = FG_Average_Draft_Position)
head(C_rank_ADP)

C_rank_ADP_two <- select(C_rank_ADP, -FG_Average_Draft_Position)
head(C_rank_ADP_two)

C_rank_ADP_updated_three <- C_rank_ADP_two %>% 
  mutate(FG_Catcher_Rank= row_number()) %>% 
  relocate(FG_Catcher_Rank, .before = FG_Average_Draft_Position_New)
head(C_rank_ADP_updated_three)


C_Total_Hits <- C_rank_ADP_updated_three %>% 
  mutate(Singles = Total_Hits - (Doubles + Triples + Home_Runs)) %>% 
  relocate(Singles, .before = Doubles)
head(C_Total_Hits)


C_Total_Bases <- C_Total_Hits %>% 
  mutate(Total_Bases = (1 * Singles) + (2 * Doubles) + (3 * Triples) + (4 * Home_Runs)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(C_Total_Bases)

C_strikeouts <- C_Total_Bases %>% 
  mutate(Strikeouts = (-1 * Strikeouts)) %>% 
  relocate(Total_Bases, .before = Stolen_Bases)
head(C_strikeouts)


C_cs <- C_strikeouts %>% 
  mutate(Caught_Stealing = (-1 * Caught_Stealing)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(C_cs)


C_proj_points <- C_cs %>% 
  mutate(Proj_Points = (Runs_Scored + Total_Bases + Runs_Batted_In + Walks + Strikeouts + 
                        Hit_By_Pitch + Stolen_Bases + Caught_Stealing)) %>% 
  relocate(Proj_Points, .before = Total_Bases) %>% 
  arrange(desc(Proj_Points))
head(C_proj_points)


C_league_ranks <- C_proj_points %>% 
  mutate(League_Proj_Points_Rank = row_number()) %>% 
  relocate(League_Proj_Points_Rank, .before = FG_Average_Draft_Position_New)
head(C_league_ranks)


C_league_rank_comp <- C_league_ranks %>% 
  mutate(League_Rank_Comparison_to_FG = FG_Catcher_Rank - League_Proj_Points_Rank) %>% 
  relocate(League_Rank_Comparison_to_FG, .before = FG_Average_Draft_Position_New)


C_league_rank_comp$League_Rank_Comparison_to_FG <- lapply(C_league_rank_comp$League_Rank_Comparison_to_FG, function(x) {
  x.c <- paste0(ifelse(x >= 0, "+", ""), x, "")
})
head(C_league_rank_comp)

C_league_rank_comp$Team <- sub("^$", "Free_Agent", C_league_rank_comp$Team)
head(C_league_rank_comp)

C_league_rank_total <- C_league_rank_comp %>% 
  mutate(Player_Position = "C") %>% 
  relocate(Player_Position, .before = FG_Catcher_Rank)

C_league_rank_comp <- C_league_rank_total %>% 
  mutate(Team_Player = paste0(Team, " - ", C_league_rank_total$Player_Position, " - ", Player)) %>% 
  relocate(Team_Player, .before = Player)
head(C_league_rank_comp)

C_league_ranks_top <- C_league_rank_comp %>% 
  filter(League_Proj_Points_Rank > 0 & League_Proj_Points_Rank < 50 & Team != "Free_Agent") %>% 
  arrange(desc(Proj_Points))
head(C_league_ranks_top)


C_points_comp <- C_league_rank_comp %>% 
  select(Team_Player, Proj_Points)


dotchart(C_league_ranks_top$Proj_Points, labels = C_league_ranks_top$Team_Player,
         cex=.8,
         main="Catcher Projected Points",
         xlab="Projected Points")


##Dataset: C_points_comp
rm(C_league_rank_comp)
rm(C_specific_cols)
rm(Catchers)
rm(C_cs)
##rm(C_league_rank_comp)
rm(C_league_rank_total)
rm(C_league_ranks)
rm(C_league_ranks_top)
rm(C_proj_points)
rm(C_rank_ADP)
rm(C_rank_ADP_two)
rm(C_rank_ADP_updated_three)
rm(C_strikeouts)
rm(C_Total_Bases)
rm(C_Total_Hits)
#################


##First Base
First_Base <- read.csv(file = 'FG_First_Base.csv')
head(First_Base)

FB_specific_cols <- First_Base %>% 
  select(Player = Name, playerid, Team, FG_Average_Draft_Position = ADP, Total_Hits = H, 
         Doubles = X2B, Triples = X3B, Home_Runs = HR, Runs_Batted_In = RBI, 
         Runs_Scored = R, Walks = BB, Strikeouts = SO, Stolen_Bases = SB, Caught_Stealing = CS,
         Hit_By_Pitch = HBP)
head(FB_specific_cols)

FB_rank_ADP <- FB_specific_cols %>%
  mutate(FG_Average_Draft_Position_New = round(FG_Average_Draft_Position, 0)) %>% 
  arrange(FG_Average_Draft_Position_New) %>% 
  relocate(FG_Average_Draft_Position_New, .before = FG_Average_Draft_Position)
head(FB_rank_ADP)

FB_rank_ADP_two <- select(FB_rank_ADP, -FG_Average_Draft_Position)
head(FB_rank_ADP_two)

FB_rank_ADP_updated_three <- FB_rank_ADP_two %>% 
  mutate(FG_FB_Rank= row_number()) %>% 
  relocate(FG_FB_Rank, .before = FG_Average_Draft_Position_New)
head(FB_rank_ADP_updated_three)


FB_Total_Hits <- FB_rank_ADP_updated_three %>% 
  mutate(Singles = Total_Hits - (Doubles + Triples + Home_Runs)) %>% 
  relocate(Singles, .before = Doubles)
head(FB_Total_Hits)


FB_Total_Bases <- FB_Total_Hits %>% 
  mutate(Total_Bases = (1 * Singles) + (2 * Doubles) + (3 * Triples) + (4 * Home_Runs)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(FB_Total_Bases)

FB_strikeouts <- FB_Total_Bases %>% 
  mutate(Strikeouts = (-1 * Strikeouts)) %>% 
  relocate(Total_Bases, .before = Stolen_Bases)
head(FB_strikeouts)


FB_cs <- FB_strikeouts %>% 
  mutate(Caught_Stealing = (-1 * Caught_Stealing)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(FB_cs)


FB_proj_points <- FB_cs %>% 
  mutate(Proj_Points = (Runs_Scored + Total_Bases + Runs_Batted_In + Walks + Strikeouts + 
                          Hit_By_Pitch + Stolen_Bases + Caught_Stealing)) %>% 
  relocate(Proj_Points, .before = Total_Bases) %>% 
  arrange(desc(Proj_Points))
head(FB_proj_points)


FB_league_ranks <- FB_proj_points %>% 
  mutate(League_Proj_Points_Rank = row_number()) %>% 
  relocate(League_Proj_Points_Rank, .before = FG_Average_Draft_Position_New)
head(FB_league_ranks)


FB_league_rank_comp <- FB_league_ranks %>% 
  mutate(League_Rank_Comparison_to_FG = FG_FB_Rank - League_Proj_Points_Rank) %>% 
  relocate(League_Rank_Comparison_to_FG, .before = FG_Average_Draft_Position_New)


FB_league_rank_comp$League_Rank_Comparison_to_FG <- lapply(FB_league_rank_comp$League_Rank_Comparison_to_FG, function(x) {
  x.c <- paste0(ifelse(x >= 0, "+", ""), x, "")
})
head(FB_league_rank_comp)


FB_league_rank_comp$Team <- sub("^$", "Free_Agent", FB_league_rank_comp$Team)
head(FB_league_rank_comp)


FB_league_rank_total <- FB_league_rank_comp %>% 
  mutate(Player_Position = "1B") %>% 
  relocate(Player_Position, .before = FG_FB_Rank)


FB_league_rank_comp <- FB_league_rank_total %>% 
  mutate(Team_Player = paste0(Team, " - ", FB_league_rank_total$Player_Position, " - ", Player)) %>% 
  relocate(Team_Player, .before = Player)
head(FB_league_rank_comp)

FB_league_ranks_top <- FB_league_rank_comp %>% 
  filter(League_Proj_Points_Rank > 0 & League_Proj_Points_Rank < 50 & Team != "Free_Agent") %>% 
  arrange(desc(Proj_Points))
head(FB_league_ranks_top)


FB_points_comp <- FB_league_rank_comp %>% 
  select(Team_Player, Proj_Points)


dotchart(FB_league_ranks_top$Proj_Points, labels = FB_league_ranks_top$Team_Player,
         cex=.8,
         main="First Base Projected Points",
         xlab="Projected Points")


##Dataset: FB_points_comp
rm(FB_league_rank_comp)
rm(FB_specific_cols)
rm(First_Base)
rm(FB_cs)
##rm(FB_league_rank_comp)
rm(FB_league_rank_total)
rm(FB_league_ranks)
rm(FB_league_ranks_top)
rm(FB_proj_points)
rm(FB_rank_ADP)
rm(FB_rank_ADP_two)
rm(FB_rank_ADP_updated_three)
rm(FB_strikeouts)
rm(FB_Total_Bases)
rm(FB_Total_Hits)
#################


##Second Base
Second_Base <- read.csv(file = 'FG_Second_Base.csv')
head(Second_Base)

SB_specific_cols <- Second_Base %>% 
  select(Player = Name, playerid, Team, FG_Average_Draft_Position = ADP, Total_Hits = H, 
         Doubles = X2B, Triples = X3B, Home_Runs = HR, Runs_Batted_In = RBI, 
         Runs_Scored = R, Walks = BB, Strikeouts = SO, Stolen_Bases = SB, Caught_Stealing = CS,
         Hit_By_Pitch = HBP)
head(SB_specific_cols)


SB_rank_ADP <- SB_specific_cols %>%
  mutate(FG_Average_Draft_Position_New = round(FG_Average_Draft_Position, 0)) %>% 
  arrange(FG_Average_Draft_Position_New) %>% 
  relocate(FG_Average_Draft_Position_New, .before = FG_Average_Draft_Position)
head(SB_rank_ADP)


SB_rank_ADP_two <- select(SB_rank_ADP, -FG_Average_Draft_Position)
head(SB_rank_ADP_two)


SB_rank_ADP_updated_three <- SB_rank_ADP_two %>% 
  mutate(FG_SB_Rank= row_number()) %>% 
  relocate(FG_SB_Rank, .before = FG_Average_Draft_Position_New)
head(SB_rank_ADP_updated_three)


SB_Total_Hits <- SB_rank_ADP_updated_three %>% 
  mutate(Singles = Total_Hits - (Doubles + Triples + Home_Runs)) %>% 
  relocate(Singles, .before = Doubles)
head(SB_Total_Hits)


SB_Total_Bases <- SB_Total_Hits %>% 
  mutate(Total_Bases = (1 * Singles) + (2 * Doubles) + (3 * Triples) + (4 * Home_Runs)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(SB_Total_Bases)

SB_strikeouts <- SB_Total_Bases %>% 
  mutate(Strikeouts = (-1 * Strikeouts)) %>% 
  relocate(Total_Bases, .before = Stolen_Bases)
head(SB_strikeouts)


SB_cs <- SB_strikeouts %>% 
  mutate(Caught_Stealing = (-1 * Caught_Stealing)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(SB_cs)


SB_proj_points <- SB_cs %>% 
  mutate(Proj_Points = (Runs_Scored + Total_Bases + Runs_Batted_In + Walks + Strikeouts + 
                          Hit_By_Pitch + Stolen_Bases + Caught_Stealing)) %>% 
  relocate(Proj_Points, .before = Total_Bases) %>% 
  arrange(desc(Proj_Points))
head(SB_proj_points)


SB_league_ranks <- SB_proj_points %>% 
  mutate(League_Proj_Points_Rank = row_number()) %>% 
  relocate(League_Proj_Points_Rank, .before = FG_Average_Draft_Position_New)
head(SB_league_ranks)


SB_league_rank_comp <- SB_league_ranks %>% 
  mutate(League_Rank_Comparison_to_FG = FG_SB_Rank - League_Proj_Points_Rank) %>% 
  relocate(League_Rank_Comparison_to_FG, .before = FG_Average_Draft_Position_New)


SB_league_rank_comp$League_Rank_Comparison_to_FG <- lapply(SB_league_rank_comp$League_Rank_Comparison_to_FG, function(x) {
  x.c <- paste0(ifelse(x >= 0, "+", ""), x, "")
})
head(SB_league_rank_comp)


SB_league_rank_comp$Team <- sub("^$", "Free_Agent", SB_league_rank_comp$Team)
head(SB_league_rank_comp)


SB_league_rank_total <- SB_league_rank_comp %>% 
  mutate(Player_Position = "2B") %>% 
  relocate(Player_Position, .before = FG_SB_Rank)


SB_league_rank_comp <- SB_league_rank_total %>% 
  mutate(Team_Player = paste0(Team, " - ", SB_league_rank_total$Player_Position, " - ", Player)) %>% 
  relocate(Team_Player, .before = Player)
head(SB_league_rank_comp)

SB_league_ranks_top <- SB_league_rank_comp %>% 
  filter(League_Proj_Points_Rank > 0 & League_Proj_Points_Rank < 40 & Team != "Free_Agent") %>% 
  arrange(desc(Proj_Points))
head(SB_league_ranks_top)


SB_points_comp <- SB_league_rank_comp %>% 
  select(Team_Player, Proj_Points)


dotchart(SB_league_ranks_top$Proj_Points, labels = SB_league_ranks_top$Team_Player,
         cex=.8,
         main="Second Base Projected Points",
         xlab="Projected Points")


##Dataset: SB_points_comp
rm(SB_league_ranks_top)
rm(SB_league_rank_comp)
rm(SB_specific_cols)
rm(Second_Base)
rm(SB_cs)
##rm(SB_league_rank_comp)
rm(SB_league_rank_total)
rm(SB_league_ranks)
rm(SB_proj_points)
rm(SB_rank_ADP)
rm(SB_rank_ADP_two)
rm(SB_rank_ADP_updated_three)
rm(SB_strikeouts)
rm(SB_Total_Bases)
rm(SB_Total_Hits)
#################


##Third Base
Third_Base <- read.csv(file = 'FG_Third_Base.csv')
head(Third_Base)

TB_specific_cols <- Third_Base %>% 
  select(Player = Name, playerid, Team, FG_Average_Draft_Position = ADP, Total_Hits = H, 
         Doubles = X2B, Triples = X3B, Home_Runs = HR, Runs_Batted_In = RBI, 
         Runs_Scored = R, Walks = BB, Strikeouts = SO, Stolen_Bases = SB, Caught_Stealing = CS,
         Hit_By_Pitch = HBP)
head(TB_specific_cols)

TB_rank_ADP <- TB_specific_cols %>%
  mutate(FG_Average_Draft_Position_New = round(FG_Average_Draft_Position, 0)) %>% 
  arrange(FG_Average_Draft_Position_New) %>% 
  relocate(FG_Average_Draft_Position_New, .before = FG_Average_Draft_Position)
head(TB_rank_ADP)

TB_rank_ADP_two <- select(TB_rank_ADP, -FG_Average_Draft_Position)
head(TB_rank_ADP_two)

TB_rank_ADP_updated_three <- TB_rank_ADP_two %>% 
  mutate(FG_TB_Rank= row_number()) %>% 
  relocate(FG_TB_Rank, .before = FG_Average_Draft_Position_New)
head(TB_rank_ADP_updated_three)


TB_Total_Hits <- TB_rank_ADP_updated_three %>% 
  mutate(Singles = Total_Hits - (Doubles + Triples + Home_Runs)) %>% 
  relocate(Singles, .before = Doubles)
head(TB_Total_Hits)


TB_Total_Bases <- TB_Total_Hits %>% 
  mutate(Total_Bases = (1 * Singles) + (2 * Doubles) + (3 * Triples) + (4 * Home_Runs)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(TB_Total_Bases)

TB_strikeouts <- TB_Total_Bases %>% 
  mutate(Strikeouts = (-1 * Strikeouts)) %>% 
  relocate(Total_Bases, .before = Stolen_Bases)
head(TB_strikeouts)


TB_cs <- TB_strikeouts %>% 
  mutate(Caught_Stealing = (-1 * Caught_Stealing)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(TB_cs)


TB_proj_points <- TB_cs %>% 
  mutate(Proj_Points = (Runs_Scored + Total_Bases + Runs_Batted_In + Walks + Strikeouts + 
                          Hit_By_Pitch + Stolen_Bases + Caught_Stealing)) %>% 
  relocate(Proj_Points, .before = Total_Bases) %>% 
  arrange(desc(Proj_Points))
head(TB_proj_points)


TB_league_ranks <- TB_proj_points %>% 
  mutate(League_Proj_Points_Rank = row_number()) %>% 
  relocate(League_Proj_Points_Rank, .before = FG_Average_Draft_Position_New)
head(TB_league_ranks)


TB_league_rank_comp <- TB_league_ranks %>% 
  mutate(League_Rank_Comparison_to_FG = FG_TB_Rank - League_Proj_Points_Rank) %>% 
  relocate(League_Rank_Comparison_to_FG, .before = FG_Average_Draft_Position_New)


TB_league_rank_comp$League_Rank_Comparison_to_FG <- lapply(TB_league_rank_comp$League_Rank_Comparison_to_FG, function(x) {
  x.c <- paste0(ifelse(x >= 0, "+", ""), x, "")
})
head(TB_league_rank_comp)


TB_league_rank_comp$Team <- sub("^$", "Free_Agent", TB_league_rank_comp$Team)
head(TB_league_rank_comp)


TB_league_rank_total <- TB_league_rank_comp %>% 
  mutate(Player_Position = "3B") %>% 
  relocate(Player_Position, .before = FG_TB_Rank)


TB_league_rank_comp <- TB_league_rank_total %>% 
  mutate(Team_Player = paste0(Team, " - ", TB_league_rank_total$Player_Position, " - ", Player)) %>% 
  relocate(Team_Player, .before = Player)
head(TB_league_rank_comp)


TB_league_ranks_top <- TB_league_rank_comp %>% 
  filter(League_Proj_Points_Rank > 0 & League_Proj_Points_Rank < 40 & Team != "Free_Agent") %>% 
  arrange(desc(Proj_Points))
head(TB_league_ranks_top)


TB_points_comp <- TB_league_rank_comp %>% 
  select(Team_Player, Proj_Points)


dotchart(TB_league_ranks_top$Proj_Points, labels = TB_league_ranks_top$Team_Player,
         cex=.8,
         main="Third Base Projected Points",
         xlab="Projected Points")


##Dataset: TB_points_comp
rm(TB_league_ranks_top)
rm(TB_league_rank_comp)
rm(TB_specific_cols)
rm(Third_Base)
rm(TB_cs)
#rm(TB_league_rank_comp)
rm(TB_league_rank_total)
rm(TB_league_ranks)
rm(TB_proj_points)
rm(TB_rank_ADP)
rm(TB_rank_ADP_two)
rm(TB_rank_ADP_updated_three)
rm(TB_strikeouts)
rm(TB_Total_Bases)
rm(TB_Total_Hits)



###################

##Short Stop
Shortstop <- read.csv(file = 'FG_Short_Stop.csv')
head(Shortstop)

SS_specific_cols <- Shortstop %>% 
  select(Player = Name, playerid, Team, FG_Average_Draft_Position = ADP, Total_Hits = H, 
         Doubles = X2B, Triples = X3B, Home_Runs = HR, Runs_Batted_In = RBI, 
         Runs_Scored = R, Walks = BB, Strikeouts = SO, Stolen_Bases = SB, Caught_Stealing = CS,
         Hit_By_Pitch = HBP)
head(SS_specific_cols)

SS_rank_ADP <- SS_specific_cols %>%
  mutate(FG_Average_Draft_Position_New = round(FG_Average_Draft_Position, 0)) %>% 
  arrange(FG_Average_Draft_Position_New) %>% 
  relocate(FG_Average_Draft_Position_New, .before = FG_Average_Draft_Position)
head(SS_rank_ADP)

SS_rank_ADP_two <- select(SS_rank_ADP, -FG_Average_Draft_Position)
head(SS_rank_ADP_two)

SS_rank_ADP_updated_three <- SS_rank_ADP_two %>% 
  mutate(FG_SS_Rank= row_number()) %>% 
  relocate(FG_SS_Rank, .before = FG_Average_Draft_Position_New)
head(SS_rank_ADP_updated_three)


SS_Total_Hits <- SS_rank_ADP_updated_three %>% 
  mutate(Singles = Total_Hits - (Doubles + Triples + Home_Runs)) %>% 
  relocate(Singles, .before = Doubles)
head(SS_Total_Hits)


SS_Total_Bases <- SS_Total_Hits %>% 
  mutate(Total_Bases = (1 * Singles) + (2 * Doubles) + (3 * Triples) + (4 * Home_Runs)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(SS_Total_Bases)

SS_strikeouts <- SS_Total_Bases %>% 
  mutate(Strikeouts = (-1 * Strikeouts)) %>% 
  relocate(Total_Bases, .before = Stolen_Bases)
head(SS_strikeouts)


SS_cs <- SS_strikeouts %>% 
  mutate(Caught_Stealing = (-1 * Caught_Stealing)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(SS_cs)


SS_proj_points <- SS_cs %>% 
  mutate(Proj_Points = (Runs_Scored + Total_Bases + Runs_Batted_In + Walks + Strikeouts + 
                          Hit_By_Pitch + Stolen_Bases + Caught_Stealing)) %>% 
  relocate(Proj_Points, .before = Total_Bases) %>% 
  arrange(desc(Proj_Points))
head(SS_proj_points)


SS_league_ranks <- SS_proj_points %>% 
  mutate(League_Proj_Points_Rank = row_number()) %>% 
  relocate(League_Proj_Points_Rank, .before = FG_Average_Draft_Position_New)
head(SS_league_ranks)


SS_league_rank_comp <- SS_league_ranks %>% 
  mutate(League_Rank_Comparison_to_FG = FG_Catcher_Rank - League_Proj_Points_Rank) %>% 
  relocate(League_Rank_Comparison_to_FG, .before = FG_Average_Draft_Position_New)


SS_league_rank_comp$League_Rank_Comparison_to_FG <- lapply(SS_league_rank_comp$League_Rank_Comparison_to_FG, function(x) {
  x.c <- paste0(ifelse(x >= 0, "+", ""), x, "")
})
head(SS_league_rank_comp)


SS_league_rank_comp$Team <- sub("^$", "Free_Agent", SS_league_rank_comp$Team)
head(SS_league_rank_comp)


SS_league_rank_total <- SS_league_rank_comp %>% 
  mutate(Player_Position = "SS") %>% 
  relocate(Player_Position, .before = FG_SS_Rank)


SS_league_rank_comp <- SS_league_rank_total %>% 
  mutate(Team_Player = paste0(Team, " - ", SS_league_rank_total$Player_Position, " - ", Player)) %>% 
  relocate(Team_Player, .before = Player)
head(SS_league_rank_comp)

SS_league_ranks_top <- SS_league_rank_comp %>% 
  filter(League_Proj_Points_Rank > 0 & League_Proj_Points_Rank < 40 & Team != "Free_Agent") %>% 
  arrange(desc(Proj_Points))
head(SS_league_ranks_top)


SS_points_comp <- SS_league_rank_comp %>% 
  select(Team_Player, Proj_Points)


dotchart(SS_league_ranks_top$Proj_Points, labels = SS_league_ranks_top$Team_Player,
         cex=.8,
         main="Shortstop Projected Points",
         xlab="Projected Points")


##Dataset: SS_point_comp
rm(SS_league_ranks_top)
rm(SS_league_rank_comp)
rm(SS_specific_cols)
rm(Shortstop)
rm(SS_cs)
#rm(SS_league_rank_comp)
rm(SS_league_rank_total)
rm(SS_league_ranks)
rm(SS_proj_points)
rm(SS_rank_ADP)
rm(SS_rank_ADP_two)
rm(SS_rank_ADP_updated_three)
rm(SS_strikeouts)
rm(SS_Total_Bases)
rm(SS_Total_Hits)



###########
##Outfield
Outfield <- read.csv(file = 'FG_Outfield.csv')
head(Outfield)

OF_specific_cols <- Outfield %>% 
  select(Player = Name, playerid, Team, FG_Average_Draft_Position = ADP, Total_Hits = H, 
         Doubles = X2B, Triples = X3B, Home_Runs = HR, Runs_Batted_In = RBI, 
         Runs_Scored = R, Walks = BB, Strikeouts = SO, Stolen_Bases = SB, Caught_Stealing = CS,
         Hit_By_Pitch = HBP)
head(OF_specific_cols)

OF_rank_ADP <- OF_specific_cols %>%
  mutate(FG_Average_Draft_Position_New = round(FG_Average_Draft_Position, 0)) %>% 
  arrange(FG_Average_Draft_Position_New) %>% 
  relocate(FG_Average_Draft_Position_New, .before = FG_Average_Draft_Position)
head(OF_rank_ADP)

OF_rank_ADP_two <- select(OF_rank_ADP, -FG_Average_Draft_Position)
head(OF_rank_ADP_two)

OF_rank_ADP_updated_three <- OF_rank_ADP_two %>% 
  mutate(FG_OF_Rank = row_number()) %>% 
  relocate(FG_OF_Rank, .before = FG_Average_Draft_Position_New)
head(OF_rank_ADP_updated_three)


OF_Total_Hits <- OF_rank_ADP_updated_three %>% 
  mutate(Singles = Total_Hits - (Doubles + Triples + Home_Runs)) %>% 
  relocate(Singles, .before = Doubles)
head(OF_Total_Hits)


OF_Total_Bases <- OF_Total_Hits %>% 
  mutate(Total_Bases = (1 * Singles) + (2 * Doubles) + (3 * Triples) + (4 * Home_Runs)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(OF_Total_Bases)

OF_strikeouts <- OF_Total_Bases %>% 
  mutate(Strikeouts = (-1 * Strikeouts)) %>% 
  relocate(Total_Bases, .before = Stolen_Bases)
head(OF_strikeouts)


OF_cs <- OF_strikeouts %>% 
  mutate(Caught_Stealing = (-1 * Caught_Stealing)) %>% 
  relocate(Total_Bases, .before = Total_Hits)
head(OF_cs)


OF_proj_points <- OF_cs %>% 
  mutate(Proj_Points = (Runs_Scored + Total_Bases + Runs_Batted_In + Walks + Strikeouts + 
                          Hit_By_Pitch + Stolen_Bases + Caught_Stealing)) %>% 
  relocate(Proj_Points, .before = Total_Bases) %>% 
  arrange(desc(Proj_Points))
head(OF_proj_points)


OF_league_ranks <- OF_proj_points %>% 
  mutate(League_Proj_Points_Rank = row_number()) %>% 
  relocate(League_Proj_Points_Rank, .before = FG_Average_Draft_Position_New)
head(OF_league_ranks)


OF_league_rank_comp <- OF_league_ranks %>% 
  mutate(League_Rank_Comparison_to_FG = FG_OF_Rank - League_Proj_Points_Rank) %>% 
  relocate(League_Rank_Comparison_to_FG, .before = FG_Average_Draft_Position_New)


OF_league_rank_comp$League_Rank_Comparison_to_FG <- lapply(OF_league_rank_comp$League_Rank_Comparison_to_FG, function(x) {
  x.c <- paste0(ifelse(x >= 0, "+", ""), x, "")
})
head(OF_league_rank_comp)


OF_league_rank_comp$Team <- sub("^$", "Free_Agent", OF_league_rank_comp$Team)
head(OF_league_rank_comp)


OF_league_rank_total <- OF_league_rank_comp %>% 
  mutate(Player_Position = "OF") %>% 
  relocate(Player_Position, .before = FG_OF_Rank)


OF_league_rank_comp <- OF_league_rank_total %>% 
  mutate(Team_Player = paste0(Team, " - ", OF_league_rank_total$Player_Position, " - ", Player)) %>% 
  relocate(Team_Player, .before = Player)
head(OF_league_rank_comp)

OF_league_ranks_top <- OF_league_rank_comp %>% 
  filter(League_Proj_Points_Rank > 0 & League_Proj_Points_Rank < 40 & Team != "Free_Agent") %>% 
  arrange(desc(Proj_Points))
head(OF_league_ranks_top)


OF_points_comp <- OF_league_rank_comp %>% 
  select(Team_Player, Proj_Points)


dotchart(OF_league_ranks_top$Proj_Points, labels = OF_league_ranks_top$Team_Player,
         cex=.8,
         main="Outfield Projected Points",
         xlab="Projected Points")


##Dataset: OF_point_comp
rm(OF_league_ranks_top)
rm(OF_league_rank_comp)
rm(OF_specific_cols)
rm(Outfield)
rm(OF_cs)
#rm(OF_league_rank_comp)
rm(OF_league_rank_total)
rm(OF_league_ranks)
rm(OF_proj_points)
rm(OF_rank_ADP)
rm(OF_rank_ADP_two)
rm(OF_rank_ADP_updated_three)
rm(OF_strikeouts)
rm(OF_Total_Bases)
rm(OF_Total_Hits)



####################

##Pitchers
Pitchers_One <- read.csv(file = 'FG_Pitchers_One.csv')
Pitchers_Two <- read.csv(file = 'FG_Pitchers_Two.csv')

Pitchers <- Pitchers_One %>% left_join(Pitchers_Two, by = "Name")

P_specific_cols <- Pitchers %>% 
  select(Player = Name, Player_ID = playerid.x, Team = Team.x, FG_Average_Draft_Position = ADP.x, 
         Innings_Pitched = IP.x, Hits_Allowed = H.x, Earned_Runs = ER.x,
         Walks_Issued = BB.x, Strikeouts = SO.x, Quality_Starts = QS,
         Wins = W.x, Losses = L.x, Saves = SV.x, Holds = HLD)
head(P_specific_cols)


P_rank_ADP <- P_specific_cols %>%
  mutate(FG_Average_Draft_Position_New = round(FG_Average_Draft_Position, 0)) %>% 
  arrange(FG_Average_Draft_Position_New) %>% 
  relocate(FG_Average_Draft_Position_New, .before = FG_Average_Draft_Position)
head(P_rank_ADP)


P_rank_ADP_two <- select(P_rank_ADP, -FG_Average_Draft_Position)
head(P_rank_ADP_two)


P_rank_ADP_updated_three <- P_rank_ADP_two %>% 
  mutate(FG_Pitcher_Rank = row_number()) %>% 
  relocate(FG_Pitcher_Rank, .before = FG_Average_Draft_Position_New)
head(P_rank_ADP_updated_three)


P_IP <- P_rank_ADP_updated_three %>% 
  mutate(Innings_Pitched = (3 * Innings_Pitched)) %>% 
  relocate(Innings_Pitched, .before = Hits_Allowed)
head(P_IP)


P_HA <- P_IP %>% 
  mutate(Hits_Allowed = (-1 * Hits_Allowed)) %>% 
  relocate(Hits_Allowed, .before = Earned_Runs)
head(P_HA)


P_ER <- P_HA %>% 
  mutate(Earned_Runs = (-2 * Earned_Runs)) %>% 
  relocate(Earned_Runs, .before = Walks_Issued)
head(P_ER)

P_BB <- P_ER %>% 
  mutate(Walks_Issued = (-1 * Walks_Issued)) %>% 
  relocate(Walks_Issued, .before = Strikeouts)
head(P_BB)


P_Wins <- P_BB %>% 
  mutate(Wins = (4 * Wins)) %>% 
  relocate(Wins, .before = Losses)
head(P_Wins)


P_Losses <- P_Wins %>% 
  mutate(Losses = (-5 * Losses)) %>% 
  relocate(Losses, .before = Saves)
head(P_Losses)


P_Saves <- P_Losses %>% 
  mutate(Saves = (4 * Saves)) %>% 
  relocate(Saves, .before = Holds)
head(P_Saves)


P_proj_points <- P_Saves %>% 
  mutate(Proj_Points = (Innings_Pitched + Hits_Allowed + Earned_Runs + Walks_Issued + 
                          Strikeouts + Quality_Starts + Wins + Losses + Saves + Holds)) %>% 
  relocate(Proj_Points, .before = Innings_Pitched) %>% 
  arrange(desc(Proj_Points))
head(P_proj_points)


P_league_ranks <- P_proj_points %>% 
  mutate(League_Proj_Points_Rank = row_number()) %>% 
  relocate(League_Proj_Points_Rank, .before = FG_Average_Draft_Position_New)
head(P_league_ranks)


P_league_rank_comp <- P_league_ranks %>% 
  mutate(League_Rank_Comparison_to_FG = FG_Pitcher_Rank - League_Proj_Points_Rank) %>% 
  relocate(League_Rank_Comparison_to_FG, .before = FG_Average_Draft_Position_New)


P_league_rank_comp$League_Rank_Comparison_to_FG <- lapply(P_league_rank_comp$League_Rank_Comparison_to_FG, function(x) {
  x.c <- paste0(ifelse(x >= 0, "+", ""), x, "")
})
head(P_league_rank_comp)


P_league_rank_comp$Team <- sub("^$", "Free_Agent", P_league_rank_comp$Team)
head(P_league_rank_comp)


P_league_rank_total <- P_league_rank_comp %>% 
  mutate(Player_Position = "Pitcher") %>% 
  relocate(Player_Position, .before = FG_Pitcher_Rank)


P_league_rank_comp <- P_league_rank_total %>% 
  mutate(Team_Player = paste0(Team, " - ", P_league_rank_total$Player_Position, " - ", Player)) %>% 
  relocate(Team_Player, .before = Player)
head(P_league_rank_comp)


P_league_ranks_top <- P_league_rank_comp %>% 
  filter(League_Proj_Points_Rank > 0 & League_Proj_Points_Rank < 400 & Team != "Free_Agent") %>% 
  arrange(desc(Proj_Points))
head(P_league_ranks_top)


P_points_comp <- P_league_rank_comp %>% 
  select(Team_Player, Proj_Points)


P_SP <- P_league_ranks_top %>% 
  filter(Saves <= 20 & League_Proj_Points_Rank > 0 & League_Proj_Points_Rank < 80)


P_RP <- P_league_ranks_top %>% 
  filter(Saves >= 20 & League_Proj_Points_Rank > 0 & League_Proj_Points_Rank < 200)


dotchart(P_SP$Proj_Points, labels = P_SP$Team_Player,
         cex=.8,
         main="Starting Pitcher Projected Points",
         xlab="Projected Points")

dotchart(P_RP$Proj_Points, labels = P_RP$Team_Player,
         cex=.8,
         main="Relief Pitcher Projected Points",
         xlab="Projected Points")




##Dataset: P_points_comp
rm(P_SP)
rm(P_RP)
rm(P_league_ranks_top)
rm(P_league_rank_comp)
rm(P_specific_cols)
rm(Pitchers)
rm(P_league_rank_total)
rm(P_league_ranks)
rm(P_proj_points)
rm(P_rank_ADP)
rm(P_rank_ADP_two)
rm(P_rank_ADP_updated_three)
rm(P_Wins)
rm(P_Losses)
rm(P_ER)
rm(P_BB)
rm(P_HA)
rm(P_IP)
rm(P_Saves)
rm(Pitchers_One)
rm(Pitchers_Two)



#####All Players Rankings
All_Players <- do.call("rbind", list(C_points_comp, FB_points_comp, SB_points_comp, TB_points_comp,
                                     SS_points_comp, OF_points_comp, P_points_comp))


All_Players <- All_Players %>% 
  filter(Team_Player != "Free_Agent") %>% 
  arrange(desc(Proj_Points))
    
  
All_Players <- All_Players %>% 
    mutate(Proj_Points_Rank = row_number()) %>% 
    filter(Proj_Points_Rank > 0 & Proj_Points_Rank < 50)
  

dotchart(All_Players$Proj_Points, labels = All_Players$Team_Player,
             cex = 0.8,
             main = "All Players Projected 2021 Points - ESPN ATML League",
             xlab = "Projected Points")










#######Testing examples###########
league_ranks_top$Player = str_wrap(league_ranks_top$Player, width = 3)

ggplot(league_ranks_top, aes(x = reorder(Player, -Proj_Points), y = Proj_Points)) +
  geom_bar(stat = "identity")

ggplot(league_ranks_top, aes(x = Player, y = Proj_Points)) +
  geom_dotplot(binaxis = "y", stackdir = "center")





