declare void @printInt(i32) ;  w innym module
define i32 @main() {
       %i1 = alloca i32                               
       store i32 44, i32* %i1                         
       %i3 = alloca i32                               
       store i32 2, i32* %i3                          
       %i5 = load i32, i32* %i1                      
       %i6 = load i32, i32* %i3
       %i7 = sub i32 %i5, %i6
       call void @printInt(i32 %i7)
       ret i32 0
}

