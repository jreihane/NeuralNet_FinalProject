
create_train_data <- function(data_csv){
        train1_data <- subset(data_csv,!(project_id %in% c(1,7,13,19,25,31,37,43,49,55,61)))
        train2_data <- subset(data_csv,!(project_id %in% c(2,8,14,20,26,32,38,44,50,56,62)))
        train3_data <- subset(data_csv,!(project_id %in% c(3,9,15,21,27,33,39,45,51,57,63)))
        train4_data <- subset(data_csv,!(project_id %in% c(4,10,16,22,28,34,40,46,52,58)))
        train5_data <- subset(data_csv,!(project_id %in% c(5,11,17,23,29,35,41,47,53,59)))
        train6_data <- subset(data_csv,!(project_id %in% c(6,12,18,24,30,36,42,48,54,60)))
        
        train_data <- list(train1_data,train2_data,train3_data,train4_data,train5_data,train6_data)
        
        train_data
}

create_test_data <- function(data_csv){
        test1_data <- subset(data_csv,project_id %in% c(1,7,13,19,25,31,37,43,49,55,61))
        test2_data <- subset(data_csv,project_id %in% c(2,8,14,20,26,32,38,44,50,56,62))
        test3_data <- subset(data_csv,project_id %in% c(3,9,15,21,27,33,39,45,51,57,63))
        test4_data <- subset(data_csv,project_id %in% c(4,10,16,22,28,34,40,46,52,58))
        test5_data <- subset(data_csv,project_id %in% c(5,11,17,23,29,35,41,47,53,59))
        test6_data <- subset(data_csv,project_id %in% c(6,12,18,24,30,36,42,48,54,60))
        
        test_data <- list(test1_data,test2_data,test3_data,test4_data,test5_data,test6_data)
        
        test_data
}