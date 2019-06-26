list1 <- list()
list1[1] = 1
list1[2] = 2
list1[3] = 3
list2 <- list()
list2[1] = 'a'
list2[2] = 'b'
list2[3] = 'c'
list2[4] = 'd'
list_all <- list(list1, list2)

list3 <- list()
list3[1] = 'Z'

list_all <- NULL
list_all <- append(list_all, x=list1)
#list_all <- list(list1)
list_all <- append(list_all, list2)

list_all <- NULL
list_all <- list(list1)
list_all <- append(list_all, list(list2))
list_all <- append(list_all, list(list3))

list_all <- NULL
list_all <- append(list_all, list(list1))
list_all <- append(list_all, list(list2))
list_all <- append(list_all, list(list3))



