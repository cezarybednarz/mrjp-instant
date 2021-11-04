declare void @printInt(i32)
define i32 @main() {
 %i1 = add i32 42, 0
 call void @printInt(i32 %i1)
 ret i32 0
}
