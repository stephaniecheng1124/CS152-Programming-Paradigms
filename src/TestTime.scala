package time1


object TestTime extends App {
  try {
    val t1 = Time(3, 45)
    val t2 = Time(3)
    println("t1 = " + t1)
    println("t2 = " + t2)
    println("t1 < t2 = " + t1.before(t2))
    println("t1 minutes since midnight = " + t1.minutesSinceMidNight)
    val t3 = Time(14, 60) // oops!
  } catch {
    case e: IllegalArgumentException => println(e)
  }
  
}