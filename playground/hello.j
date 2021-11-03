.class public Hello
.super java/lang/Object
.method public <init>()V
  aload_0
  invokespecial java/lang/Object/<init>()V
  return
.end method

.method public static main([Ljava/lang/String;)V
.limit stack 3
.limit locals 3
  getstatic java/lang/System/out Ljava/io/PrintStream;
  bipush 44
  istore 1
  iconst_2
  istore_2
  iload_1
  iload_2
  isub 
  invokevirtual  java/io/PrintStream/println(I)V
  return
.end method