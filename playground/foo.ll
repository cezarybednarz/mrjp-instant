declare void @printInt(i32)
define i32 @main() {
 %i1 = add i32 2, 0
 call void @printInt(i32 %i1)
 %i3 = add i32 1, 0
 call void @printInt(i32 %i3)
 ret i32 0
}
