



library(tidyverse)
library(MCOE)
library(here)


con <- mcoe_sql_con()


classenroll <- tbl(con, "STAFF_CLASSENROLL") %>%
    head(20) %>%
    collect()



coursetaught <- tbl(con, "STAFF_COURSETAUGHT") %>%
    head(20) %>%
    collect()





### -----


staffassign <- tbl(con, "STAFF_ASSIGN") %>%
    filter(CountyName == "MONTEREY",
           CourseCode %in% c("2535", "2538"),
           AcademicYear == "1819") %>%
    collect()



health.teachers <- staffassign %>%
    select(RecID:SchoolName) %>%
    distinct()



summ.teacher <- health.teachers %>%
    group_by(DistrictName, SchoolName) %>%
    count()






codebook <- tbl(con, "CODEBOOK_ASSIGNMENT") %>%
    collect()





courseenroll <- tbl(con, "STAFF_COURSEENROLL") %>%
    filter(CountyName == "MONTEREY",
           CourseCode %in% c("1000"),  #b CourseCode %in% health,
           AcademicYear == "1819") %>%
        select(DistrictCode:GradeLevelCode) %>%
    distinct() %>%
collect()


courseenroll2 <- courseenroll %>%
    mutate(grades = case_when(GradeLevelCode %in% c("PS","KN","01","02", "03") ~ "PK-3",
                              GradeLevelCode %in% c("04","05","06") ~ "4-6",
                              GradeLevelCode %in% c("07","08") ~ "7-8",
                              GradeLevelCode %in% c("09","10","11","12") ~ "9-12",
                              TRUE ~ "Other")) %>%
    select(-GradeLevelCode) %>%
    distinct()




health <- c("1000", "2535", "2538")


coursetaught <- tbl(con, "STAFF_COURSETAUGHT") %>%
    filter(CountyName == "MONTEREY",
           AcademicYear == "1819") %>%
    collect() %>%
    mutate(AssignmentCode = as.character(CourseCode))



joint <- coursetaught %>%
    filter(AssignmentCode %in% c("1000"),
           Enrollment > 1) %>%
    rename(SchoolCode = schoolCode) %>%
    left_join(codebook, by = c("AssignmentCode" = "AssignmentCode")) %>%
#    select(-YEAR,-DistrictCode,-schoolCode,-CountyName) %>%
    left_join(courseenroll2) %>%
    select(DistrictName, SchoolName, AssignmentName, grades, Enrollment)



summ <- joint %>%
    group_by(DistrictName, SchoolName, AssignmentName, grades) %>%
    count()

summ2 <- summ %>%
    ungroup() %>%
    group_by(grades) %>%
    summarise(total = sum(n))



write_csv(summ, "Self-Contained Classes.csv")
write_csv(summ.teacher, "Teachers of Health Education Courses.csv")



split by K-3,  4-5, 6-8,  9-12  
seperate self-contained and departmentalized 
how many middle and high school Health teachers 
grandtotals  



