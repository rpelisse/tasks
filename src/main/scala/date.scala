val format = new java.text.SimpleDateFormat("dd-MM-yyyy")
val d = format.format(new java.util.Date())
println("date:" + d)
println("class:" + d.getClass)
val d2 = format.parse("15-10-2016")
println("class d2:"  + d2.getClass)